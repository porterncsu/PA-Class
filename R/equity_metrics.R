#' Equity Metrics function
#'
#' The function takes in a dataframe with all the predictive probabilities and true results and then return a data frame
#' with the performance metrics by the chosen protected variable of interest.
#'
#' @param predProbs data frame with predictive probabilities
#' @param outcomeName the column name for the true value of the outcome
#' @param equityVars a vector of protected variables we would like to estimate the performance metrics for
#' @author Brit Henderson, Zarni Htet, Kristin Porter, Kristen Hunter
calc_equity_metrics <- function(predProbs, equityVar, outcomeName, threshold){

  equityResults <- NULL

  # pulling out the categories of the focus equity/protected variable of interest
  catVals <- levels(factor(predProbs[[equityVar]]))
  numCats <- length(catVals)

  # This loop is looping through categories of equity var[i] which has n*j categories
  for(i in 1:numCats)
  {
    # category
    cat <- catVals[i]

    # pull the datasets for each of the category for the equity/protected variable of interest
    predProbsCat <- predProbs[predProbs[[equityVar]] == cat,]
    truth.factor <- as.factor(as.character(predProbsCat[[outcomeName]]))
    # KP commenting this out because it's only grabbing obs with value 1 and creating error in computation of AUC
    #levels(truth.factor) <- base::ifelse(levels(truth.factor) == "no", "0", "1")
    truth.numeric <- 1*(as.character(truth.factor)=="yes") # need to change to truth.factor within as.character() as the actual outcome is now "yes" or "no"
    truth.factor <- as.factor(truth.numeric)
    #    truth.factor <- relevel(as.factor(truth.factor), ref = "yes")
    predictions <- predProbsCat$.pred_yes

    # check
    if(max(truth.numeric) > 1 | min(truth.numeric) < 0)
    {
      stop('Error in coding of the outcome variable.')
    }
    if(min(predictions) < 0 | max(predictions) > 1)
    {
      stop('Error in coding of the outcome variable.')
    }
    equityResultsCat <- data.frame(
      equityVar = equityVar,
      category  = cat,
      threshold = threshold,
      tp      = calc_tp_man(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      tpr     = calc_tpr(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      tn      = calc_tn_man(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      tnr     = calc_tnr(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      fp      = calc_fp_man(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      fpr     = calc_fpr(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      fn      = calc_fn_man(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      fnr     = calc_fnr(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      fdr     = calc_fdr(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      ppv     = calc_precision(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      acc     = calc_accuracy_man(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      pp      = calc_pp_man(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      pn      = calc_pn_man(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      on      = calc_on_man(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      op      = calc_op_man(predictions = predictions, outcomes = truth.factor, threshold = threshold),
      total   = length(predictions),
      auc_roc = yardstick::roc_auc_vec(truth = fct_relevel(truth.factor,"0","1"), estimate = predictions, event_level="second"),
      auc_pr  = yardstick::pr_auc_vec(truth = fct_relevel(truth.factor,"0","1"), estimate = predictions, event_level="second")
    )

    equityResults <- bind_rows(equityResults, equityResultsCat)
  }
  return(equityResults)
}

#' Calculate t tests for equity metrics across all folds
#'
#' @param equityResults data frame of equity results (tpr, fpr, etc) across folds
#' @export
#'
#' @examples
calc_equity_test_overall <- function(equityResults)
{
  # prepare to save out results
  equityTests <- equityResults %>%
    group_by(learnerName, equityVar, threshold) %>%
    dplyr::select(learnerName, equityVar, threshold) %>%
    summarise() %>%
    mutate(fnr_pVal = NA, fpr_pVal = NA, fdr_pVal = NA)

  # summarise learner data across folds
  learnerOverall <- equityResults %>%
    group_by(learnerName, equityVar, category, threshold) %>%
    summarise(fp = sum(fp),
              fn = sum(fn),
              pn = sum(tn + fn),
              pp = sum(tp + fp),
              op = sum(tp + fn),
              on = sum(tn + fp))

  # determine number of categories to check for type of test
  numCats <- length(unique(equityResults$category))

  # t test
  if(numCats == 2)
  {
    # calculate t tests
    for(l in unique(learnerOverall$learnerName))
    {
      learnerData <- learnerOverall %>%
        filter(learnerName == l)
      selectRows <- equityTests$learnerName == l

      an.error.occured <- FALSE
      # false negative t test
      tryCatch({
          tTest_fn <- prop.test(
            x = c(learnerData$fn[1], learnerData$fn[2]),
            n = c(learnerData$op[1], learnerData$op[2])
        )}, error = function(e) {an.error.occured <<- TRUE}
    )
      if(an.error.occured)
      {
        tTest_fn <- list()
        tTest_fn$p.value <- NaN
      } else
      {
        equityTests$fnr_pVal[selectRows] <- tTest_fn$p.value
      }

      an.error.occured <- FALSE
      # false positive t test
      tryCatch({
        tTest_fp <- prop.test(
          x = c(learnerData$fp[1], learnerData$fp[2]),
          n = c(learnerData$on[1], learnerData$on[2])
        )}, error = function(e) {an.error.occured <<- TRUE}
      )

      if(an.error.occured)
      {
        tTest_fp <- list()
        tTest_fp$p.value <- NaN
      } else
      {
        equityTests$fpr_pVal[selectRows] <- tTest_fp$p.value
      }

      an.error.occured <- FALSE
      # fdr t test
      tryCatch({
        tTest_fd <- prop.test(
          x = c(learnerData$fp[1], learnerData$fp[2]),
          n = c(learnerData$pp[1], learnerData$pp[2])
        )}, error = function(e) {an.error.occured <<- TRUE}
      )

      if(an.error.occured)
      {
        tTest_fd <- list()
        tTest_fd$p.value <- NaN
      } else
      {
        equityTests$fdr_pVal[selectRows] <- tTest_fd$p.value
      }
    }
  # anova: ZH needs to review this
  } else
  {
    # calculate t tests
    for(l in unique(learnerOverall$learnerName))
    {
      learnerData <- learnerOverall %>%
        filter(learnerName == l)
      selectRows <- equityTests$learnerName == l

      # false negative anova
      glm_full_fn <- glm(
        data = learnerData,
        formula = cbind(fn, op - fn) ~ category,
        family = "binomial"
      )
      glm_intercept_fn <- glm(
        data = learnerData,
        formula = cbind(fn, op - fn) ~ 1,
        family = "binomial"
      )
      aov_fn <- anova(glm_intercept_fn, glm_full_fn, test = "Chisq")
      equityTests$fnr_pVal[selectRows] <- aov_fn$"Pr(>Chi)"[2]

      # false positive anova
      glm_full_fp <- glm(
        data = learnerData,
        formula = cbind(fp, on - fp) ~ category,
        family = "binomial"
      )
      glm_intercept_fp <- glm(
        data = learnerData,
        formula = cbind(fp, on - fp) ~ 1,
        family = "binomial"
      )
      aov_fp <- anova(glm_intercept_fp, glm_full_fp, test = "Chisq")
      equityTests$fpr_pVal[selectRows] <- aov_fp$"Pr(>Chi)"[2]

      # false discovery anova
      glm_full_fd <- glm(
        data = learnerData,
        formula = cbind(fp, pp - fp) ~ category,
        family = "binomial"
      )
      glm_intercept_fd <- glm(
        data = learnerData,
        formula = cbind(fp, pp - fp) ~ 1,
        family = "binomial"
      )
      aov_fd <- anova(glm_intercept_fd, glm_full_fd, test = "Chisq")
      equityTests$fdr_pVal[selectRows] <- aov_fd$"Pr(>Chi)"[2]
    }
  }

  return(equityTests)
}


#' Calculate equity metrics across learners, thresholds, equityVars
#'
#' @param predProbs
#' @param learnerNames vector of learnerNames
#' @param thresholdsSelect vector of thresholds to calculate equity metrics for
#' @param equityVars
#' @param outcomeName
#'
calc_equity_metrics_all <- function(
    predProbs, learnerNames,
    thresholdsSelect, equityVars, outcomeName
)
{
  equityResults <- NULL

  for(l in learnerNames)
  {
    for(t in thresholdsSelect)
    {
      for(e in equityVars)
      {
        equityResultsLearner <- calc_equity_metrics(
          predProbs = predProbs[predProbs$learnerName==l,],
          outcomeName = outcomeName,
          equityVar = e,
          threshold = t
        )
        equityResultsLearner$learnerName <- l
        equityResultsLearner$threshold <- t

        equityResults <- bind_rows(equityResults, equityResultsLearner)

      } # equityVars
    } # thresholds
  } #learners

  equityResults <- equityResults %>%
    relocate(learnerName, equityVar, category, threshold)

  return(equityResults)

} # calc_equity_metrics_all_test


#' Calculate equity test across learners, thresholds, equityVars
#'
#' @param learnersSelect vector of learnerNames
#' @param equityVarsSelect vector of equityVars
#' @param thresholdsSelect vector of thresholds to calculate equity metrics for
#'
calc_equity_tests_all <- function(
    equityResults,
    learnersSelect,
    equityVarsSelect,
    thresholdsSelect
)
{
  # calculate fairness metric tests
  equityTests <- NULL
  for(l in learnersSelect)
  {
    for(e in equityVarsSelect)
    {
      for(t in thresholdsSelect)
      {
        equityResultsIter <- equityResults %>%
          filter(learnerName == l) %>%
          filter(equityVar == e) %>%
          filter(threshold == t)

        equityTestsIter <- calc_equity_test_overall(equityResults = equityResultsIter)
        equityTests <- bind_rows(equityTests, equityTestsIter)
      }
    }
  }
  return(equityTests)
}






