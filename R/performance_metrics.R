conf_mat_man <- function(predictions, outcomes, threshold){

  predictions <- as.vector(predictions)
  outcomes <- as.vector(outcomes)
  outcomes <- factor(outcomes)

  predictions_threshold <- ifelse(predictions >= threshold, 1,0)
  predictions_threshold <- factor(predictions_threshold)

  tp <- sum(predictions_threshold == 1 & outcomes == 1)
  fp <- sum(predictions_threshold == 1 & outcomes == 0)
  tn <- sum(predictions_threshold == 0 & outcomes == 0)
  fn <- sum(predictions_threshold == 0 & outcomes == 1)

  conf_mat_contents <- list(tp, fp, tn,fn)
  names(conf_mat_contents) <- list("tp", "fp", "tn", "fn")

  return(conf_mat_contents)

} # conf_mat_man

calc_fp_man <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  fp <- CM$fp[[1]]

  return(fp)

} # false positive

calc_tp_man <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  tp <- CM$tp[[1]]

  return(tp)

} # true positive

calc_fn_man <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions,outcomes,threshold)

  fn <- CM$fn[[1]]

  return(fn)

} # false negative

calc_tn_man <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  tn <- CM$tn[[1]]

  return(tn)

} # true negative

calc_on_man <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  on <- CM$fp[[1]] + CM$tn[[1]]

  return(on)

} # all ground truth/observed negatives

calc_op_man <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  op <- CM$fn[[1]] + CM$tp[[1]]

  return(op)

} # all ground truth/observed positives

calc_pp_man <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  pp <- CM$tp[[1]] + CM$fp[[1]]

  return(pp)

} # all predicted positives

calc_pn_man <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  pn <- CM$tn[[1]] + CM$fn[[1]]

  return(pn)

} # all predicted negatives

calc_accuracy_man <- function(predictions, outcomes, threshold){

  predictions <- as.vector(predictions)
  outcomes <- as.factor(as.vector(outcomes))
  predictions_threshold <- as.factor(ifelse(predictions >= threshold, 1,0))

  CM <- conf_mat_man(predictions, outcomes, threshold)
  accuracy_ <- (CM$tp[[1]] + CM$tn[[1]])/(CM$tp[[1]] + CM$tn[[1]] + CM$fp[[1]] + CM$fn[[1]])

  return(accuracy_)

} # accuracy

#' Calculates the False Positive Rate (FPR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the False Positive Rate.  Given that predictions need to be binary for the FPR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' FPR = FP / (FP + TN)
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, false positive rate
#' @export
#' @examples
#' fpr(predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin, threshold = .5)

calc_fpr <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  fp <- CM$fp[[1]]
  tn <- CM$tn[[1]]

  fpr <- fp / (fp + tn)
  return(fpr)
}

#' Calculates the False discovery rate (FDR)
#'
#' FDR = FP / (FP + TP)

calc_fdr <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  fp <- CM$fp[[1]]
  tp <- CM$tp[[1]]

  fdr <- fp / (fp + tp)
  return(fdr)
}

#' Calculates the False Negative Rate (FNR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the False Negative Rate.  Given that predictions need to be binary for the FNR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' FNR = FN / (FN + TP)
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, false negative rate
#' @export
#' @examples
#' fnr(predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin, threshold = .5)

calc_fnr <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  fn <- CM$fn[[1]]
  tp <- CM$tp[[1]]

  fnr <- fn / (fn + tp)
  return(fnr)
}

#' Calculates the True Positive Rate (TPR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the True Positive Rate.  Given that predictions need to be binary for the TPR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' TPR = TP / (FN + TP)
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, true positive rate
#' @export
#' @examples
#' tpr(predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin, threshold = .5)

calc_tpr <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  fn <- CM$fn[[1]]
  tp <- CM$tp[[1]]

  tpr <- tp / (fn + tp)
  return(tpr)
}

#' Calculates the True Negative Rate (TNR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the True Negative Rate.  Given that predictions need to be binary for the TNR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' TNR = TN / (TN + FP)
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, true negative rate
#' @export
#' @examples
#' tnr(predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin, threshold = .5)

calc_tnr <- function(predictions, outcomes, threshold){

  CM <- conf_mat_man(predictions, outcomes, threshold)

  fp <- CM$fp[[1]]
  tn <- CM$tn[[1]]

  tnr <- tn / (fp + tn)
  return(tnr)

}

#' Calculates the True Negative Rate (TNR)
#'
#' This function takes the predictions of a model,  (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the True Negative Rate.  Given that predictions need to be binary for the TNR to be calculated
#' you need to pass in a threshold to cut the predictions off.  If the predictions are already binary, then pass in .5
#' TNR = TN / (TN + FP)
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, true negative rate
#' @export
#' @examples
#' tnr(predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin, threshold = .5)

calc_tn <- function(predictions, outcomes, threshold, reference){

  CM <- confusion_matrix(predictions, outcomes, threshold, reference)

  table <- CM$table

  fp <- table[1,2]
  tn <- table[2,2]

  tn <- tn
  return(tn)

}

#' Calculate the Precision for predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the precision based on the predictions.  Given that predictions need to binary for the precision calculates
#' you need to pass in a threshold to but the predictions off.  If the predictions are already binary, then pass in .5
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, returns precision value
#' @export
#' @examples
#' precision(predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin, threshold = .5)

calc_precision <- function(predictions, outcomes, threshold){

  tp <- calc_tp_man(predictions = predictions, outcomes = outcomes, threshold = threshold)
  fp <- calc_fp_man(predictions = predictions, outcomes = outcomes, threshold = threshold)

  precision <- tp/(tp + fp)

  return(precision)
} # precision

#' Calculate the Recall for predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the recall based on the predictions.  Given that predictions need to binary for the recall calculates
#' you need to pass in a threshold to but the predictions off.  If the predictions are already binary, then pass in .5
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, returns recall value
#' @export
#' @examples
#' recall(predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin, threshold = .5)

calc_recall <- function(predictions, outcomes, threshold){

  predictions <- as.vector(predictions)

  outcomes <- as.vector(outcomes)
  outcomes <- factor(outcomes)

  predictions_threshold <- ifelse(predictions >= threshold, 1, 0)
  predictions_threshold <- factor(predictions_threshold)

  recall <- caret::sensitivity(predictions_threshold, outcomes,  positive = "1")
  return(recall)

}

#' Calculate the Accuracy for predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continous numeric [0,1]) and
#' calculates the accuracy based on the predictions.  Given that predictions need to binary for the accuracy calculates
#' you need to pass in a threshold to but the predictions off.  If the predictions are already binary, then pass in .5
#' Note: if there are imbalanced positives and negatives this metric might not be that useful
#'
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut  predictions that are continous within binary 0s and 1s
#' @return numeric, returns accuracy value
#' @export

calc_accuracy <- function(outcomes, predictions, threshold, reference){

  predictions <- as.vector(predictions)
  outcomes <- as.factor(as.vector(outcomes))

  # converting outcome factor and re-leveling to put the positive class as first level
  outcomes <- relevel(as.factor(outcomes), ref = reference)

  predictions_threshold <- as.factor(ifelse(predictions >= threshold, 1, 0))
  # converting prediction_threshold factor and re-leveling to put the positive class as first level
  predictions_threshold <- relevel(predictions_threshold, ref = reference)
  print(levels(predictions_threshold))  # Inspect levels


  CM <- caret::confusionMatrix(predictions_threshold, outcomes, mode = "prec_recall")
  accuracy_ <- as.vector(CM$overall["Accuracy"])
  return(accuracy)
}

#' Calculate the F1 Score for predictions
#'
#' This function takes the predictions of a model, (can be either binary 0 or 1, or continuous numeric [0,1]) and
#' calculates the F1 score based on the predictions. Given that predictions need to be binary for the F1 calculation,
#' you need to pass in a threshold to cut predictions that are continuous into binary 0s and 1s. If the predictions
#' are already binary, then pass in .5. The F1 score is the harmonic mean of precision and recall.
#'
#' @param predictions list of numerics, predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @param threshold numeric, value between 0 - 1 to cut predictions that are continuous within binary 0s and 1s
#' @return numeric, returns F1 score value
#' @export
#' @examples
#' f1_score(predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin, threshold = .5, reference = "1")
# calc_f1_score <- function(predictions, outcomes, threshold, reference= "1") {
#   predictions <- as.vector(predictions)
#   outcomes <- as.factor(as.vector(outcomes))
#   # converting outcome factor and re-leveling to put the positive class as first level
#   outcomes <- relevel(outcomes, ref = reference)
#   predictions_threshold <- as.factor(ifelse(predictions >= threshold, 1, 0))
#   # converting prediction_threshold factor and re-leveling to put the positive class as first level
#   predictions_threshold <- relevel(predictions_threshold, ref = reference)
#   print(levels(predictions_threshold))

  # Using caret's confusionMatrix function to get precision and recall
  #CM <- caret::confusionMatrix(predictions_threshold, outcomes, mode = "prec_recall")
# Extract precision and recall from confusion matrix
# precision <- as.numeric(CM$byClass["Precision"])
# recall <- as.numeric(CM$byClass["Recall"])

# Calculate F1 score
# f1_score <- 2 * (precision * recall) / (precision + recall)
#
# return(f1_score)
#}

  #' Calculate the F1 Score for predictions
  #'
  #' This function takes the predictions of a model, (can be either binary 0 or 1, or continuous numeric [0,1]) and
  #' calculates the F1 score based on the predictions. Given that predictions need to be binary for the F1 calculation,
  #' you need to pass in a threshold to cut predictions that are continuous into binary 0s and 1s. If the predictions
  #' are already binary, then pass in .5. The F1 score is the harmonic mean of precision and recall.
  #'
  #' @param predictions list of numerics, predicted values
  #' @param outcomes list of numerics, actual values/outcomes
  #' @param threshold numeric, value between 0 - 1 to cut predictions that are continuous within binary 0s and 1s
  #' @return numeric, returns F1 score value
  #' @export
  #' @examples
  #' f1_score(predictions = FakePredictionResults$est.risk.score,
  #' outcomes = FakePredictionResults$true.risk.bin, threshold = .5)
calc_f1_score <- function(predictions, outcomes, threshold) {
    # Calculate true positives and false positives using the existing functions
    tp <- calc_tp_man(predictions = predictions, outcomes = outcomes, threshold = threshold)
    fp <- calc_fp_man(predictions = predictions, outcomes = outcomes, threshold = threshold)
    fn <- calc_fn_man(predictions = predictions, outcomes = outcomes, threshold = threshold)

    # Calculate precision and recall
    precision <- tp/(tp + fp)
    recall <- tp/(tp + fn)

    # Calculate F1 score
    f1_score <- 2 * (precision * recall) / (precision + recall)

    return(f1_score)
  }

#' Calculate the Brier Score for predictions
#'
#' This function calculates the Brier Score, which measures the accuracy of probabilistic predictions.
#' It is the mean squared error between predicted probabilities and actual binary outcomes.
#' Lower scores indicate better calibration and discrimination, with 0 being perfect and 1 being worst.
#' Unlike threshold-dependent metrics, Brier Score evaluates the raw probability estimates directly.
#'
#' @param predictions numeric vector, predicted probabilities between 0 and 1
#' @param outcomes numeric vector, actual binary outcomes (0 or 1)
#' @return numeric, Brier Score value between 0 (perfect) and 1 (worst)
#' @export
#' @examples
#' calc_brier_score(predictions = c(0.9, 0.8, 0.3, 0.1),
#'                  outcomes = c(1, 1, 0, 0))
calc_brier_score <- function(predictions, outcomes) {
  predictions <- as.numeric(predictions)
  outcomes <- as.numeric(outcomes)

  # Handle factor outcomes (convert "yes"/"no" to 1/0)
  if (all(outcomes %in% c(0, 1)) == FALSE) {
    if (is.factor(outcomes)) {
      levels_vec <- levels(outcomes)
      outcomes <- as.numeric(outcomes == levels_vec[2])
    }
  }

  brier_score <- mean((predictions - outcomes)^2, na.rm = TRUE)
  return(brier_score)
}



#' Finds the threshold to cut continous predictions to maximize a given metric
#'
#' This function takes a given performance metric (precision, accuracy, or recall) that requires a threshold
#' and returns the threshold value that maximizes the specified metric
#'
#' @param func function, takes in other functions as arguments such as precision, accuracy and recall
#' @param predictions list of numerics,  predicted values
#' @param outcomes list of numerics, actual values/outcomes
#' @return numeric, value from 0 to 1 that is the threshold that maximizes the metric func
#' @export
#' @examples
#' threshold_finder(precision, predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin)
#' threshold_finder(recall, predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin)
#' threshold_finder(accuracy, predictions = FakePredictionResults$est.risk.score,
#' outcomes = FakePredictionResults$true.risk.bin)

threshold_finder <- function(func, predictions,outcomes){

  i <- 0
  res <- data.frame()
  idx <- 1
  while(i <= 100){

    temp <- func(predictions,outcomes,i/100)
    res <- rbind(res, c(i/100, temp))


    i <- i +1
    idx <- idx +1
  }
  colnames(res)<- c("threshold", "res_")
  res <- res[order(-res$res_),]
  return(as.numeric(res$threshold[1]))
}

#' Confusion matrix threshold
#'
#' Generate confusion matrix on a specific range of thresholds
#'
#' @param predictions predictive probabilities
#' @param outcomes the outcomes
#' @param thresSeq sequence of thresholds
conf_metrics_thres <- function(predictions, outcomes, thresSeq){

  interpretMetrics <- NULL

  for (i in 1:length(thresSeq)){

    interpretMetricsThres <- data.frame(
      threshold   = thresSeq[i],
      tpr = calc_tpr(   predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      tpc = calc_tp_man(predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      tnr = calc_tnr(   predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      tnc = calc_tn_man(predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      fpr = calc_fpr(   predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      fpc = calc_fp_man(predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      fdr = calc_fdr(   predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      fnr = calc_fnr(   predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      fnc = calc_fn_man(predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      # precision (added by KP)
      ppv = calc_precision(predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      # accuracy (added by KP)
      acc = calc_accuracy_man(predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),
      # f1 (added by KP 01/2025)
      f1 = calc_f1_score(predictions = predictions, outcomes = outcomes, threshold = thresSeq[i]),

      # observed negatives
      on = sum(outcomes == 0),
      # observed positives
      op = sum(outcomes == 1),
      auc_roc = yardstick::roc_auc_vec(truth = fct_relevel(as.factor(outcomes),"0","1"), estimate = predictions, event_level="second"),
      auc_pr  = yardstick::pr_auc_vec(truth = fct_relevel(as.factor(outcomes),"0","1"), estimate = predictions, event_level="second"),
      # Brier score (added by KP)
      brier = calc_brier_score(predictions = predictions, outcomes = outcomes)
    )
    # add additional metrics
    # predicted positives
    interpretMetricsThres$pp <- interpretMetricsThres$tpc + interpretMetricsThres$fpc
    # predicted negatives
    interpretMetricsThres$pn <- interpretMetricsThres$tnc + interpretMetricsThres$fnc

    interpretMetrics <- bind_rows(interpretMetrics, interpretMetricsThres)

  } # end of for loop
  return(interpretMetrics)
} # Confusion matrix threshold

#' Confusion matrix threshold learner
#'
#' Wrapper around conf_metrics_thres_learner
#' to estimate confusion matrix results for
#' all the selected learner
#'
#' @param predProbs object with predictive probabilities of all the selected learners
#' @param outcomeName name of the outcome
#' @param thresSeq sequence of thresholds to test over
#'
#' @return
#' @export
#'
#' @examples
conf_metrics_thres_learner <- function(predProbs, outcomeName, thresSeq, equityVar = NULL)
{
  learnerNames <- unique(predProbs$learnerName)
  confOut <- NULL

  # converting the outcomeName variable to be 1 when yes and to be 0 when no.
  # KP: outcomeName variable is already 1 when yes and 0 when no so this line overwrites all to zero
 predProbs <-
   predProbs %>%
     dplyr::mutate(!!sym(outcomeName) := ifelse(as.character(!!sym(outcomeName)) == "yes", 1,0))

  if(is.null(equityVar))
  {
    # select only relevant columns
    confData <- predProbs %>%
      dplyr::select(learnerName,
                    .pred_yes,
                    !!outcomeName,
                    ML,
                    predSet)

    for(learnerNameSelect in learnerNames)
    {
      confDataLearner <- confData %>%
        filter(learnerName == learnerNameSelect)

      confOutLearner <- conf_metrics_thres(
        predictions = confDataLearner$.pred_yes,
        outcomes = confDataLearner[[outcomeName]],
        thresSeq = thresSeq
      )
      confOutLearner$learnerName <- learnerNameSelect
      confOutLearner <- confOutLearner %>%
        relocate(learnerName)

      confOut <- dplyr::bind_rows(confOut, confOutLearner)
    }
  } else
  {
    # select only relevant columns
    confData <- predProbs %>%
      dplyr::select(learnerName,
                    .pred_yes,
                    !!outcomeName,
                    ML,
                    predSet,
                    all_of(equityVar))

    # make sure equityVars are factors

    for(learnerNameSelect in learnerNames)
    {
      equityVarCats <- unique(confData[[equityVar]])

      for(equityVarCat in equityVarCats)
      {
        confDataLearnerEquity <- confData %>%
          filter(learnerName == learnerNameSelect) %>%
          filter(get(equityVar) == equityVarCat)

        confOutLearner <- conf_metrics_thres(
          predictions = confDataLearnerEquity$.pred_yes,
          outcomes = confDataLearnerEquity[[outcomeName]],
          thresSeq = thresSeq
        )

        confOutLearner$learnerName <- learnerNameSelect
        confOutLearner[[equityVar]] <- equityVarCat
        confOutLearner <- confOutLearner %>%
          relocate(learnerName)

        confOut <- dplyr::bind_rows(confOut, confOutLearner)
      }
    }
  }

  return(confOut)
} # conf_metrics_thres_learner




