######################################################
# get_model_spec
######################################################

#' For a given machine learning method, this function uses the parsnip package
#' to get corresponding specifications
#'
#' @param ML machine learning method
#' @return modelSpec model specifications from parsnip


get_model_spec <- function(ML){

    # for generalized linear model
  if (ML == "glm"){

    # setting glm Model Specifications
    model_spec <-
      parsnip::logistic_reg() %>%
      parsnip::set_engine("glm") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(penalty = 0)

  } else if (ML == "lasso"){

    model_spec <-
      parsnip::logistic_reg() %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(penalty = tune(),
                        mixture = 1)

  } else if (ML == "ridge"){

    model_spec <-
      parsnip::logistic_reg() %>%
      parsnip::set_engine("glmnet") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(penalty = tune(),
                        mixture = 0)

  } else if (ML == "random_forest"){

    model_spec <-
      parsnip::rand_forest() %>%
      parsnip::set_engine("ranger", num.threads = parallel::detectCores() - 1) %>%  # Enable parallel processing
      parsnip::set_mode("classification") %>%
      parsnip::set_args(mtry = tune(),         # Number of predictors to sample
                        trees = 100,           # Fixed at 100 (default 500 is overkill for learning)
                        min_n = tune(),        # Minimum node size
                        importance = "permutation")

  } else if (ML == "naive_bayes"){

    model_spec <-
      parsnip::naive_Bayes() %>%
      parsnip::set_engine("naivebayes") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(smoothness = tune(),
                        Laplace = tune())

  } else if (ML == "xgboost"){

    model_spec <-
      parsnip::boost_tree() %>%
      parsnip::set_engine("xgboost", nthread = parallel::detectCores() - 1) %>%  # Enable parallel processing
      parsnip::set_mode("classification") %>%
      parsnip::set_args(tree_depth = 4,
                        trees = 50,              # Fixed at 50 (default 15 is too low, 500+ is overkill)
                        learn_rate = 0.1)        # Fixed at reasonable default

  } else if (ML == "svm_poly"){
    model_spec <-
      parsnip::svm_poly() %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(cost = tune(),
                        degree = 2,              # Fixed at 2 (higher degrees = much slower)
                        scale_factor = 1,        # Fixed at default
                        margin = 0.1)            # Fixed at default
    
  } else if (ML == "svm_rbf"){
    model_spec <-
      parsnip::svm_rbf() %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(cost = tune(),
                        rbf_sigma = tune(),      # Keep tuning this - it's important
                        margin = 0.1)            # Fixed at default

  } else if (ML == "neural_network"){

    model_spec <-
      parsnip::mlp() %>%
      parsnip::set_engine("nnet") %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_args(hidden_units = tune(),
                        penalty = tune(),
                        epochs = tune())

  } else
  {
    print(paste0("Invalid ML:", ML))
  }

  return(model_spec)
}


######################################################
# pick_threshold
# 1/8/25 KP replaced with new function - find_threshold
######################################################

#' For a given machine learning method, this function uses the parsnip package
#' to get corresponding specifications

pick_threshold <- function(
    learnerMetricsThres, learnerNameSelect,
    metricName, metricNumber, below
)
{
  learnerRows <- learnerMetricsThres %>%
    dplyr::filter(learnerName == learnerNameSelect)

  if(below)
  {
    candidates <- learnerRows[learnerRows[,metricName] < metricNumber,]
    thres <- candidates$threshold[which.max(candidates[,metricName])]
  } else
  {
    candidates <- learnerRows[learnerRows[,metricName] > metricNumber,]
    thres <- candidates$threshold[which.min(candidates[,metricName])]
  }

  return(thres)
}

#' Find Threshold for Each Learner
#'
#' This function calculates the threshold for predicted probabilities that achieves
#' a specified target value for a given classification metric, separately for each learner.
#'
#' @param data A dataframe containing the predicted probabilities, ground truths,
#' and learner names.
#' @param predicted_probs_col The name of the column with predicted probabilities (string).
#' @param ground_truths_col The name of the column with ground truth labels (string). Must be binary.
#' @param learner_col The name of the column with learner names (string).
#' @param metric A string specifying the metric to optimize. Supported metrics:
#'        "tpr" (True Positive Rate), "fpr" (False Positive Rate), "precision", "recall", "specificity".
#' @param target_proportion A numeric value specifying the target value for the metric (e.g., 0.8).
#' @return A dataframe with one row per learner, showing the optimal threshold for the specified metric.
#' @export
#'
#' @examples
#' # Example data
#' data <- tibble(
#'   predicted_probs = c(0.1, 0.4, 0.35, 0.8, 0.9, 0.65, 0.2, 0.5, 0.7, 0.85),
#'   ground_truths = c(0, 0, 0, 1, 1, 1, 0, 0, 1, 1),
#'   learnerName = rep(c("Learner1", "Learner2"), each = 5)
#' )
#'
#' # Find the threshold for TPR = 0.8 for each learner
#' find_threshold(data, "predicted_probs", "ground_truths", "learnerName", metric = "tpr", target_proportion = 0.8)
find_threshold <- function(data, predicted_probs_col, ground_truths_col, learner_col, metric, target_proportion) {
  # Check if inputs are valid
  if (!all(c(predicted_probs_col, ground_truths_col, learner_col) %in% colnames(data))) {
    stop("One or more specified columns are not present in the data.")
  }
#  if (!all(data[[ground_truths_col]] %in% c(0, 1))) {
#    stop("ground_truths column must be binary (0 or 1).")
#  }

  # Map shorthand metric names to full names
  metric <- tolower(metric)
  metric_map <- list(
    "tpr" = "true_positive_rate",
    "fpr" = "false_positive_rate",
    "precision" = "precision",
    "recall" = "recall",
    "specificity" = "specificity"
  )
  if (!metric %in% names(metric_map)) {
    stop("Unsupported metric. Choose from 'tpr', 'fpr', 'precision', 'recall', or 'specificity'.")
  }
  metric_full <- metric_map[[metric]]
  
  data <- data %>%
    mutate(!!sym(outcomeName) := ifelse(!!sym(outcomeName) == "yes", 1, 0))

  # Helper function to calculate classification metrics
  calculate_metric <- function(threshold, predicted_probs, ground_truths, metric) {
    predictions <- ifelse(predicted_probs >= threshold, 1, 0)
    tp <- sum(predictions == 1 & ground_truths == 1)  # True Positives
    fp <- sum(predictions == 1 & ground_truths == 0)  # False Positives
    tn <- sum(predictions == 0 & ground_truths == 0)  # True Negatives
    fn <- sum(predictions == 0 & ground_truths == 1)  # False Negatives

    # Metrics
    if (metric == "true_positive_rate") {  # TPR
      return(tp / (tp + fn))
    } else if (metric == "false_positive_rate") {  # FPR
      return(fp / (fp + tn))
    } else if (metric == "precision") {
      return(tp / (tp + fp))
    } else if (metric == "recall") {  # Alias for TPR
      return(tp / (tp + fn))
    } else if (metric == "specificity") {
      return(tn / (tn + fp))
    } else {
      stop("Unsupported metric. This should never happen.")
    }
  }

  # Function to find threshold for a single learner
  find_for_learner <- function(learner_data) {
    thresholds <- sort(unique(learner_data[[predicted_probs_col]]))
    best_threshold <- NULL
    for (threshold in thresholds) {
      value <- calculate_metric(
        threshold,
        learner_data[[predicted_probs_col]],
        learner_data[[ground_truths_col]],
        metric_full
      )
      if (!is.na(value) && value >= target_proportion) {
        best_threshold <- threshold
        break
      }
    }
    return(best_threshold)
  }

  # Group by learner and calculate thresholds
  results <- data %>%
    group_by_at(learner_col) %>%
    summarise(
      criteriaThreshold = find_for_learner(cur_data()),
      .groups = "drop"
    )

  return(results)
}



# filter out learner results to just a subset of learners
filter_results <- function(learnersResults, learnerNamesFocus)
{
  # first make a copy
  learnersResultsFocus <- learnersResults

  # filter out to just relevant learners from data frames
  learnersResultsFocus$modelResults <- learnersResultsFocus$modelResults %>%
    dplyr::filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$predProbs <- learnersResultsFocus$predProbs %>%
    dplyr::filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$rocResults <- learnersResultsFocus$rocResults %>%
    dplyr::filter(learnerName %in% learnerNamesFocus)
  learnersResultsFocus$prResults <- learnersResultsFocus$prResults %>%
    dplyr::filter(learnerName %in% learnerNamesFocus)
  # learnersResultsFocus$learnersMetrics <- learnersResultsFocus$learnersMetrics %>%
  #   dplyr::filter(learnerName %in% learnerNamesFocus)

  # list of modelFits
  # ugly code :(
  learnersResultsFocus$modelFits <- list()
  learnersResultsFocus$finalWorkflows <- list()
  for(l in learnerNamesFocus)
  {
    learnersResultsFocus$modelFits[[l]] <- learnersResults$modelFits[[
      which(names(learnersResults$modelFits) == l)
    ]]
    learnersResultsFocus$finalWorkflows[[l]] <- learnersResults$finalWorkflows[[
      which(names(learnersResults$finalWorkflows) == l)
    ]]
  }

  # add in additional info
  learnersResultsFocus$learnerSpec <- learnerSpec
  learnersResultsFocus$learnerNames <- learnerNamesFocus

  return(learnersResultsFocus)
}

# convert metrics from long to wide
convert_metrics_wide <- function(modelResults)
{
  learnersMetricsMean <- tidyr::pivot_wider(
    modelResults,
    id_cols = c("learnerName", "predSet", "ML"),
    names_from = ".metric",
    values_from = "mean",
    names_prefix = "mean_"
  )

  learnersMetricsStdError <- tidyr::pivot_wider(
      modelResults,
      id_cols = c("learnerName", "predSet", "ML"),
      names_from = ".metric",
      values_from = "std_err",
      names_prefix = "se_"
    ) %>%
    dplyr::select(c("se_pr_auc", "se_roc_auc"))
    # KP removed "se_accuracy" since removed it from metrics returned in learner_training

  learnersMetricsWide <- dplyr::bind_cols(
    learnersMetricsMean,
    learnersMetricsStdError
  )

  return(learnersMetricsWide)
}

# create a version of a data.frame with
# rounded digits for pretty printing
round_table <- function(table)
{
  roundTable <- table %>%
    dplyr::mutate_if(is.numeric, round, digits = 5)

  return(roundTable)
}

#' Helper function to split the incoming learners
#' into machine learning, predictor set
#'
#' @param selectedLearners enter selected Learners saved from the previous 02_01_Learner_Training Rmd

split_learner_names <- function(selectedLearners)
{
  # setting up null vectors to store the split ml,
  # predSet and tuningSet

  ML <- NULL
  predSet <- NULL

  for (i in 1:length(selectedLearners))
  {
    learnerSplit <- unlist(stringr::str_split(selectedLearners[i], "_predSet_"))

    ML[i] <- learnerSplit[1]
    predSet[i] <- paste0("predSet_", learnerSplit[2])
  }

  return(list(ML = ML,
              predSet = predSet))
} # learnerNameSplits


