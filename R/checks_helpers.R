#' Matching Suffix Function
#'
#' The purpose of this function is to extract suffixes followed by an underscore.
#' We define suffix as the character string after the last underscore.
#'
#' @param colRowNames a vector containing column or row names that we want to extract suffixes
#' @return
#' @export
#'
#' @examples
extractSuffix <- function(colRowNames){

   ext_suffixes <- sub('.*\\_', '', colRowNames)

return(ext_suffixes)
}

#' Returning relevant timepoints for your predictor sets
#'
#' This function will take in your timepoint and the timepoint data set you have created
#' and it will return relevant timepoints for your predictor. By relevant time points,
#' it will return the timepoint you designated and all the time points before as they
#' are permissible to be included in the predictor set.
#'
#' @param timepoint the timepoint you have specified
#' @param timepointDat the timepoint data set you have created
#'
#' @return
#' @export
#'
#' @examples
checkRelevantTime <- function(timepoint, timepointDat){

  # Convert matrics to data frames
  trainDat <- as.data.frame(trainDat)
  timepointDat <- as.data.frame(timepointDat)

  # Run the checks on the timepointData (i.e for instance if the order is in the correct order)

  # Pull out the time point you have created and all the time point before
  currentTimepoint <- as.numeric(as.character(timepointDat[as.character(timepointDat$label) == timepoint,]$order))
  relevantTimepoint <- as.character(timepointDat[which(as.numeric(as.character(timepointDat$order))
                                                       <= currentTimepoint),]$label)

return(relevantTimepoint)
} # end of checkPredictorTime

#' Checking if specified Predictors are in sync with the relevant time points
#'
#' @param predSet a vector of specified sets of predictors
#' @param relevantTimepoint a vector of relevant time points
#'
#' @return
#' @export
#'
#' @examples
checkPredRelevantTime <- function(predSet,relevantTimepoint){

  # Extract Suffixes of the time point of the specified predictors
  predSuffixes <- extractSuffix(predSet)

  # Check if the suffixes match with relevant time point
  checkSuffix <- predSuffixes %in% relevantTimepoint

  # pulling out Predictors that are missing the timepoint
  # !checkSuffix to pull out the predictor with missing suffix
  missingTimePointPreds <- predSet[!checkSuffix]

  # Missing Timepoint predictor sets

  # Return error warnings
  if(sum(checkSuffix %in% FALSE) >= 1){
    message(paste("You have specified a predictor set with predictors that are not available at or before the timepoint
                  you specified in step 01-01. The predictor/s you need to review is printed below."))
    print(missingTimePointPreds)
  }

} # checkPredRelevantTime

#' Check if the specified predictor set is in the training Data
#'
#' @param predSet a specified predictor set
#' @param trainDat training data set you have created
#'
#' @return
#' @export
#'
#' @examples
checkPredTraining <- function(predSet, trainDat){

  # converts matrix to dataframe
  trainDat <- as.data.frame(trainDat)

  # pull out columns of Training Data
  predTrain <- colnames(trainDat)

  # check if predSet and colnames of trainData matches
  checkPred <- predSet %in% predTrain

  # check if statement to print out Predictors that do not match
  if (sum(checkPred) != length(checkPred)){

    typoPredSet <- predSet[which(checkPred == FALSE)]
    message(paste("The following predictors do not exist
                  in your training data set. Please check."))

    # printing the Predictor set that do not match
    print(typoPredSet)


  } # end of if statement
} # end of checkPredTraining

