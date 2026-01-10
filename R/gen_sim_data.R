library(truncnorm)
library(mice)
library(dplyr)

# data path
datapath <- "data-r-objects/inputs"
# parameters
n.units <- 10000

# seed
set.seed(1234)

# generate covariates
# time point 1: SS - start of the semester
# gender, race, previous GPA,
# number of classes, previous test score,
# student ESL indicator,
# indicator for in advanced classes
# time point 2: MS - middle of the semester
# current GPA, absences
data <- data.frame(
  student_id = seq(1:n.units),
  gender_SS = sample(c(0, 1, 2), n.units, replace = TRUE),
  race_SS = sample(c('race1', 'race2', 'race3'), n.units, replace = TRUE), # inducing MCAR for race
  prev_GPA_SS = rtruncnorm(n.units, a = 0, b = 4, mean = 3, sd = 1),
  num_classes_SS = rpois(n.units, lambda = 6),
  prev_testscore = rlnorm(n.units, meanlog = 10),
  ESL_SS = rbinom(n.units, size = 1, prob = 0.4),
  advanced_classes_SS = rbinom(n.units, size = 1, prob = 0.1)
)

# having a slightly different distribution for ESL students for absences
absences_MS_ESL <- data.frame(student_id = data$student_id[data$ESL_SS == 1],
                                  ESL_SS = 1,
                                  absences_MS = rpois(sum(data$ESL_SS), lambda = 5))
absences_MS_Not_ESL <- data.frame(student_id = data$student_id[data$ESL_SS == 0],
                                      ESL_SS = 0,
                                      absences_MS = rpois(abs(nrow(data) - sum(data$ESL_SS)), lambda = 3))

# combining absences for mid-semester
absences_MS <- dplyr::bind_rows(absences_MS_ESL,
                                absences_MS_Not_ESL)

data <- data %>%
          dplyr::inner_join(
            absences_MS,
            by = c(
              "student_id",
              "ESL_SS"))

# having a slightly different distribution for student ESL for GPA
cur_GPA_MS_ESL <- data.frame(
  student_id = data$student_id[data$ESL_SS == 1],
  ESL_SS = 1,
  cur_GPA_MS = rtruncnorm(sum(data$ESL_SS), a = 0, b = 4, mean = 2.75, sd = 1.5))

cur_GPA_MS_Not_ESL <- data.frame(
  student_id = data$student_id[data$ESL_SS == 0],
  ESL_SS = 0,
  cur_GPA_MS = rtruncnorm(abs(nrow(data) - sum(data$ESL_SS)), a = 0, b = 4, mean = 3.25, sd = 0.5))

cur_GPA_MS <- dplyr::bind_rows(cur_GPA_MS_ESL,
                               cur_GPA_MS_Not_ESL)

data <- data %>%
  dplyr::inner_join(
    cur_GPA_MS,
    by = c(
      "student_id",
      "ESL_SS"))

data$race_SS <- factor(data$race_SS)

# generate outcome based on logistic regression model
lin.predictor <-
  0.03 +
  0.05  * data$gender_SS +
  0.1   * data$num_classes_SS +
  0.2   * data$prev_GPA_SS +
  -0.6 * data$ESL_SS +
  0.2   * data$advanced_classes_SS +
  -0.25  * data$absences_MS +
  0.7   * data$cur_GPA_MS

# sigmoid function to bound the probabilities to be between -1 and 1
prob <- 1/(1 + exp(-lin.predictor))
data$graduate <- rbinom(n.units, size = 1, prob = prob)
data$dropout <- 1 - data$graduate
# Inducing Missing Not At Random (MNAR) using MICE's ampute function

## Initial amputation of data
amp_data_list_01 <- mice::ampute(
  data = data,
  prop = 0.1, # the total number of missing rows of the whole data set
  mech = "MNAR" # the type of missing we plan to see
)

## Check the patterns of missing.
## 0s are where missing occurs.
md.pattern(amp_data_list_01$amp)

## Checking the missing patterns of the data set
mnarPatterns <- amp_data_list_01$patterns
## Taking out missing values from student_id and outcome (let's assume we have complete data on both)
mnarPatterns$student_id <- 1
mnarPatterns$graduate <- 1
mnarPatterns$dropout <- 1

## Adjusting the pattern to further to make missing pattern more random
mnarPatterns$gender_SS[3] <- 0
mnarPatterns$ESL_SS[3] <- 0
mnarPatterns$ESL_SS[7] <- 0
mnarPatterns$ESL_SS[10] <- 0
mnarPatterns$cur_GPA_MS[3] <- 0
mnarPatterns$cur_GPA_MS[4] <- 0
mnarPatterns$cur_GPA_MS[5] <- 0
mnarPatterns$cur_GPA_MS[7] <- 0

## Updating the amputation of data with new pattern
amp_data_list_02 <- mice::ampute(
  data = data,
  prop = 0.25, # the total number of missing rows of the whole data set
  mech = "MNAR", # the type of missing we plan to see
  patterns = mnarPatterns # the pattern of missing values
)

# pulling out the amputed data alone!
amp_data <- amp_data_list_02$amp

# save out all data
saveRDS(amp_data, file = here::here(datapath, 'student.rds'))

# split into train/test
data.split <- rsample::initial_split(amp_data, prop = 0.8)
data.train <- rsample::training(data.split)
data.test  <- rsample::testing(data.split)

saveRDS(data.train, file = here::here(datapath, 'studentTrain.rds'))
saveRDS(data.test,   file = here::here(datapath, 'studentTest.rds'))

# meta data file

# columns
# [1] "student_id"     "gender_SS"      "race_SS"        "prev_GPA_SS"    "num_classes_SS""prev_testscore"  "ESL_SS"
# [8] "advanced_SS"    "absences_MS"    "cur_GPA_MS"     "graduate"
meta <- data.frame(
  row.names = colnames(amp_data),
  min = c(NA, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  max = c(NA, 1, NA, 4, 17, 100000, 1, 1, 11, 4, 1, 1),
  med = c(NA, median(data$gender_SS), NA, apply(data[,c(4:12)], 2, function(x) median(as.numeric(x)))),
  var.category = c('id', rep('predictor', 9), 'outcome', 'outcome'),
  var.type = c('category', 'binary', 'category', 'numeric', 'integer', 'numeric', 'binary', 'binary', 'integer', 'numeric', 'binary', 'binary'),
  data.source = c('record', 'record', 'record', 'gradebook', 'gradebook', 'record', 'record', 'record', 'record', 'gradebook', 'record','binary'),
  pred_SS = c(NA, 1, 1, 1, 1, 1, 1, 1, 0, 0, NA, NA),
  pred_MS = c(NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA)
)

saveRDS(meta, file = here::here(datapath, 'studentMeta.rds'))

