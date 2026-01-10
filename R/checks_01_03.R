#############################################
# Source Libraries &  Helper Function Files #
#############################################
# source(here::here("R", "checks_helpers.R"))
#########################################################

#' Checking Outcome Variable
#'
#' In this function, we are checking if the data & meta data file have the specified outcome variable.
#' Further, we are checking if the outcome is specified correctly and of the correct data type.
#'
#' @param outcome the outcome we would like to predict for a particular data set
#' @param trainDat
#' @param metaDat
#'
#' @return it does not a return an R object. It will return error warnings!
#' @export
#'
#' @examples
checkOutcomeColumn <- function(outcome, trainDat) {

  # Function Qs: Do we need to have a standard column of var.category and var.type?

  # is the outcome in the data file?
  checkData <- outcome %in% colnames(trainDat)
  if (checkData == FALSE)
  {
    message("The outcome you specified is not found in the training data file.
             Please review the documentation for the data model and correct.")
  }

  # if the outcome variable exist?
  if (checkData == TRUE)
  {

    # check that var.type is binary
    checkType <- metaDat[outcome,"var.type"]
    if(checkType != "binary")
    {
      message("The learner training process only supports binary variables.
              The metadata file lists a type other than binary.
              Please review the documentation for the data model and correct.")
    }
    if (checkType == "binary")
    {
      check1 <- min(trainDat[,outcome]) == 0 & max(trainDat[,outcome]) == 1
      # note: NA values are allowed
      check2 <- length(unique(trainDat[,outcome])) <= 3
      checkBinary <- check1 & check2
    }
    if (checkBinary == FALSE)
    {
      message("The outcome is not a binary outcome with two levels: 0 and 1.
              Please recode the variable to be a character or factor variable
              with levels 0 and 1 (and missing values if appropriate).")
    }
  }
} #checkOutcomeColumn

#' Check Timepoint Function
#'
#' This function checks three things:
#' - if the timePoint  that you have specified exists in the timePoint data table.
#' - if the rownames and column names have the _timepoint in meta data file.
#' - If the column names have _timepoint in the training data file.
#'
#' @param timepoint a short hand for the timepoint when you received the data
#' @param trainDat the training data set you have prepared
#' @param metaDat the meta data that you have created
#' @param timepointDat the time point data set you have created
#'
#' @return
#' @export
#'
#' @examples
checkTimepointLabel <- function(timepoint, trainDat, metaDat, timepointDat){

  #Convert the matrixes to data frames for easier manipulation later on
  trainDat <- as.data.frame(trainDat)
  metaDat <- as.data.frame(metaDat)
  timepointDat <- as.data.frame(timepointDat)

  # Check the time point data frame's correctness using a helper function
  # i.e same as the TimePointLabel check data frame

  # is the timepoint in the timepointDat dataset? as.character as the predictors are returned as factors.
  timeLabelDat <- timepoint %in% as.character(timepointDat$label)
  if (timeLabelDat == FALSE) {
    message(paste0("The timepoint label you specified is not found in the timepointTable.Rdata
                   you created with 01_01_Specifying_Time_Points_R_Notebook markdown."))
  } # end if

  # are there predictors for this timepoint in the training dataset?
  trainingPredictors <- extractSuffix(colnames(trainDat))
  timetrainDat <- timepoint %in% trainingPredictors
  if (timetrainDat == FALSE){
    message(paste0("There are no predictors corresponding to the timepoint label you specified in the training dataset. Please review the
                   documentation for the data model and correct."))
  }

  # are there predictors for this timepoint in the meta dataset?
  metaColumns <- extractSuffix(colnames(metaDat))
  metaRows <- extractSuffix(rownames(metaDat))

  timeMetaColumns <- timepoint %in% metaColumns
  timeMetaRows <- timepoint %in% metaRows

  if (timeMetaRows == FALSE){
    message(paste0("There are no predictors corresponding to the timepoint label you specified in the meta dataset. Please review the
                   documentation for the data model and correct."))
  } # first if to check if the specified time point predictors are in the meta data frame.

  if (timeMetaColumns == FALSE){
    message(paste0("Your meta dataset is missing a column to specify which predictors are in the timepoint
                   with the label you specified. Please review the documentation for the data model and correct."))
  } # second if to check if the columns to indicate which time point exists for which predictors exist or not.
} #checkTimepointLabel


#' Check Population Function
#'
#' @param timepoint a short hand for the timepoint when you received the data
#' @param trainDat the training data set you have prepared
#' @param populationDat a population data set you have created
#' @param metaDat the meta data that you have created
#'
#' @return
#' @export
#'
#' @examples
checkPopulationSelect <- function(timepoint, trainDat, metaDat, populationDat){

}

#' Multiple checks on the specified predictor set for a base model
#'
#' This function checks if the predictor set has
#' - relevant Time points (a predictor set cannot have predictors from later time points but only current & earlier)
#' - check if the predictors exist in a training data set. If not print out the rest.
#'
#' @param predSet specified predictor set
#' @param timepoint specified time point
#' @param timepointDat timepointData you have created
#' @param trainDat trainData you have created
#'
#' @return
#' @export
#'
#' @examples
checkSpecifypredset_BM <- function(
  predSet, timepoint = NULL, timepointDat = NULL, trainDat)
{

  # Check if the time points that have been specified are from the current one as well as the previous one.
  if(!is.null(timepoint))
  {
    relevantTime <- checkRelevantTime(timepoint = timepoint, timepointDat = timepointDat)
    checkPredRelevantTime(predSet = predSet, relevantTime = relevantTime)
  }

  # Check if the predictors exist in the Training Data set and print out the non-matching predictors.
  checkPredTraining(predSet = predSet, trainDat = trainDat)
} # end of checkSpecifypredset_BM

#' Multiple checks on if the list of predictors meet our requirements
#' TODO
#'
#' @param allPredsets a list of specified predictors
#' @param timepoint specified time point
#' @param timepointDat timepointData you have created
#' @param trainDat trainData you have created
#'
#' @return
#' @export
#'
#' @examples

checkAllPredSets <- function(allPredsets, timepoint, timepointDat, trainDat) {

  # Check if each of list of predictors meet our basic requires
  tmp <- lapply(allPredsets, checkSpecifypredset_BM,
         timepoint = timepoint, timepointDat = timepointDat, trainDat = trainDat)

} # checkAllPredSets

#' Check if user has specified a Specific Algorithm
#'
#' This function checks if the user specified algorithms are currently available
#' in the toolset engine. If not, which algorithms are not matching.
#'
#' @param specifiedLearners specified learners by user
#' @param availableModels the available ML algorithms
#'
#' @return
#' @export
#'
#' @examples
checkSpecifyModels <- function(availableModels, specifiedLearners) {

  # find which specifiedAlgorithm matches with the availableAlgos.
  # if not match, spell out NAs
  matchLearners <- lapply(specifiedLearners, match, as.character(availableModels))

  # if there algorithms that do not match
  if(sum(is.na(unlist(matchLearners))) >= 1){

    # find the index of the algorithm that do not match
    indexList <- lapply(lapply(matchLearners, is.na),which)
    dataList <-specifiedLearners

    # return the list of unmatched algoritms
    unMatchedList <- map2(dataList, indexList, `[`)

    # Print out a list of unMatched Algorithms
    message(paste0("The following modeling approaches are not available as of this moment. Please review the
                    modeling approach table and correct."))
    print(unMatchedList)
  } #end of if

} # checkSpecifyModels











