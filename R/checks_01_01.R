#' Check Timepoint Table Creation
#'
#' 1) Check the number of columns of a data table
#' 2) Check if the column names are correct. If not, spit out which one is the right one.
#' 2) Check if the Labels are Unique
#' 3) Check if the Time is unique
#' 4) Check if the Time are properly ordered (Optional)
#'
#' @param timePointTable a matrix where the program time points are collected
#'
#' @return
#' @export
#'
#' @examples
checkTimePointTableCreation <- function(timePointTable){

  # desired column names
  desiredColNames <- c("order","label", "description","notes")

  # convert to a data frame
  timePointTable <- as.data.frame(timePointTable)

  # Order to a numeric vector
  # Levels need to be directly translated from character to numeric
  Order <- as.numeric(as.character(timePointTable$order))

  # columNames of timePointTable
  colTimePointTable <- colnames(timePointTable)

  # Check lenghth of columns to see if it's below the required 4
  if(length(colnames(timePointTable)) != 4 & length(colnames(timePointTable)) < 4){

    message(paste0("Your timePointTable is missing some columns.
                   Please check the next warning for correction"))

  # Check length of columns to see if it's above the required 4
  } else if(length(colnames(timePointTable)) != 4 & length(colnames(timePointTable)) > 4){

    message(paste0("Your timePointTable has more columns than it should.
                    Please check the next warning for correction."))
  }# First code block is done

  # Check which columns are not matching
  if (sum(is.na(match(colTimePointTable, desiredColNames))) >= 1){

    # save the unmatched columns
    unMatchedCols <- colTimePointTable[which(is.na(match(colTimePointTable, desiredColNames)))]

    # save the unmatched index
    unMatchedIndex <- which(is.na(match(colTimePointTable, desiredColNames)))

    # save the correct version of the Column Label Names
    correctColumnNames <- desiredColNames[unMatchedIndex]

    # save the errorTable
    errorTable <- data.frame(
      "CurrentColName" = unMatchedCols,
      "CorrectColName" = correctColumnNames,
      "ColNumber" = unMatchedIndex)

    message(paste0("The following columns names are not matching the
                   desired column names for the
                   time point table. Please check:"))

    print(errorTable) # printing out the unmatched columns
  }# Second check block

  # If all the columns are matching then check if the timePoint Labels are unique
  if (sum(is.na(match(colTimePointTable, desiredColNames))) == 0){

    if (sum(duplicated(as.character(timePointTable$label))) >=1 ){

      # save the character vector
      timepointLabel <- as.character(timePointTable$label)

      # save the duplicated records
      duplicatedLabelValue <- timepointLabel[which(duplicated(as.character(timePointTable$label)))]

      # save the duplicatedLabelValue Index
      duplicatedLabelIndex <- which(duplicated(as.character(timePointTable$label)))

      # table for duplicatedLabel
      errorTable <- data.frame(dfindex = duplicatedLabelIndex , value = duplicatedLabelValue)

      # print out warning message
      message(paste0("The following time point labels are duplicated in the time point table.
                     Please review the 01_01_Specifying_Time_Points_R_Notebook markdown and correct."))

      # print duplicated labels
      print(errorTable)
    }

  } # Third check block

  ######################################################
  # Check if the time is in an ascending order first.
  ######################################################

  if(is.unsorted(Order, strictly = TRUE)){

    message(paste0("The time point table's order column may not be in an ascending order.
                   Please review the 01_01_Specifying_Time_Points_R_Notebook markdown and correct."))

    if (sum(duplicated(as.character(timePointTable$order))) >=1 ){

      # save the character vector
      timepointOrder <- as.character(timePointTable$order)

      # save the duplicated records
      duplicatedOrderValues <- timepointOrder[which(duplicated(as.character(timePointTable$order)))]

      # save the duplicated records index
      duplicatedOrderIndex <- which(duplicated(as.character(timePointTable$order)))

      # create Error Table
      errorTable <- data.frame(dfindex = duplicatedOrderIndex, value = duplicatedOrderValues)

      # print out warning message
      message(paste0("The following time point labels are duplicated in the time point table.
                     Please review the 01_01_Specifying_Time_Points_R_Notebook markdown and correct."))

      # print duplicated Order error Table
      print(errorTable)
    }

  } # Fourth check block

} # Check Timepoint Table Creation
