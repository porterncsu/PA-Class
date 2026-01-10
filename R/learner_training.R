##########################################################################
# train_learners
##########################################################################

#' This function takes in a model specification object saved from a
#' previous step 01_03_Specifying Learners for Targeting Objective R Notebook
#' and returns an R object with all the performance metrics estimated
#'
#' @param learnerSpec all specifications form Notebook 1
#' @param recipes data preprocessing steps by learner
#' @param mainRecipes data preprocessing steps
#' @return modelResults tibble of mean and std_err of pr_auc & roc_auc by learner across folds
#' @return predProbs tibble of stacked copies of the training data (one copy for each learner) with CV predictions appended
#' @return rocResults data for computing ROC curve (sens & spec at many thresholds) stacked by learner
#' @return prResults data for computing PR curve (sens & spec at many thresholds) stacked by learner
#' @return modelFits fit info for all learners refit on full training data after tuning
#' @return finalWorkflows workflows for all learners
#' @author Kristen Hunter & Zarni Htet
#'
train_learners <- function(trainDat,
                           learnerSpec,
                           recipes,
                           mainRecipes){

  # outcome name
  outcomeName <- learnerSpec$outcomeName
  
  # Fix recipe scoping - rebuild recipes with outcome variable in current environment
  # and ensure they use the trainDat passed to this function (not original data)
  for (i in seq_along(recipes)) {
    pred_formula <- as.formula(paste(outcomeName, "~", paste(learnerSpec$allPredsets[[i]], collapse = "+")))
    recipes[[i]] <- recipe(pred_formula, data = trainDat) %>%
      step_bin2factor(!!sym(outcomeName))
  }
  
  # Rebuild mainRecipes with the trainDat passed to this function
  pred_formula_main <- as.formula(paste(outcomeName, "~", "."))
  mainRecipes <- recipe(pred_formula_main, data = trainDat) %>%
    step_bin2factor(!!sym(outcomeName))

  # predictor sets
  allPredsets <- learnerSpec$allPredsets

  # the seed for the model object. This is used to set the cross folds in place
  seed <- learnerSpec$cvParams$seed
  set.seed(seed)

  # the number of folds for cross validation
  # this could be a numeric number of a column of character/factor values
  # if it's a column of character/factor values, this would kick in group K folds
  cvFolds <- learnerSpec$cvParams$cvFolds

  # create folds outside loop so all comparisons made on same data
  # KP moved from inside j loop 3/25
  folds <- rsample::vfold_cv(
    trainDat,
    v = cvFolds,
    strat = learnerSpec$cvParams$stratVar
  )

  # save out model results across folds
  modelResults <- NULL

  # save out predicted probabilities across folds
  predProbs <- NULL

  # save out ROC and PR curves
  rocResults <- NULL
  prResults <- NULL

  # save out fitted models
  modelFits <- NULL
  finalWorkflows <- NULL

  # looping through predictor sets
  for (i in 1:length(allPredsets)){

    # name of predictor set
    predSetName <- names(allPredsets)[i]

    # parsing out the current predictor set
    predSet <- allPredsets[[i]] # i is for the current predictor set the loop is in

    # unlist the ML algorithm names for the ith predictor set to make it
    # flat and be able to count the number of item
    # all.learners is a named list of predictorsetA = c("glm", "random_forest")
    # Please refer to 01_03 specifications if you are confused.
    algorithmList <- unlist(learnerSpec$specifiedLearners[predSetName])

    predSetRecipe <- recipes[[i]]

    # looping through machine learning algorithms
    for (j in 1:length(algorithmList)){

      # parsing out each machine learning algorithm for each of the predictor set
      ML <- algorithmList[[j]]

      # save out learner name
      learnerName <- paste(ML, predSetName, sep = "_")

      # get_model_spec is our internal function stored under learner_training_helpers.R
      modelSpec <- get_model_spec(ML)

      # set up workflow - workflow allows us to combine recipe, multiple models and model tuning
      # How to set up workflows: https://workflows.tidymodels.org/
      # Different stages of the workflow: https://workflows.tidymodels.org/articles/stages.html
      workflow <- workflows::workflow() %>%
        workflows::add_recipe(predSetRecipe) %>%
        workflows::add_model(modelSpec)

      # conduct tuning
      tuneOut <- workflow %>%
        tune::tune_grid(
          resamples = folds,
          grid = learnerSpec$cvParams$gridSize,
          metrics = metric_set(roc_auc, pr_auc)
      )
      # select best model on metric user specified in Notebook 1 (currently either AUC-ROC or AUC-PR)
      # select_best {tune} finds the tuning parameter combination with the best performance values
      bestFit <- tuneOut %>%
        tune::select_best(metric = learnerSpec$cvParams$tuneMetrics)

      # finalize the workflow - set the object with hyper parameters of the best selected model
      # The ⁠finalize_workflow {tune}takes a list or tibble of tuning parameter values and updates workflow with those values.
      finalWorkflow <- workflow %>%
        tune::finalize_workflow(bestFit)

      # refit on cross-validated data - here using the optimal tuning parameters
      bestRefitCVModel <- finalWorkflow %>%
        tune::fit_resamples(
                folds,
                metrics = metric_set(roc_auc, pr_auc),
                control = control_resamples(verbose = FALSE, # not logging results (default)
                                            allow_par = TRUE, # allows parallel processing (default)
                                            save_pred = TRUE, # whether  out-of-sample predictions are saved (overriding default)
                                            pkgs = NULL, # not loading packages for parallel processing
                                            save_workflow = FALSE, # whether workflow should be appended to output (default)
                                            event_level = "first", # refers to options for parallel processing (default)
                                            parallel_over = NULL, # refers to options for parallel processing (default)
                                            backend_options = NULL) # refers to options for parallel processing (default)
              )

      # calculate metrics on CV fit - roc_auc, pr_auc: this is the mean and standard error calculation
      bestRefitCVMetrics <-
        tune::collect_metrics(bestRefitCVModel)

      # pull predicted probabilities from v-fold cross-validation
      predProbsLearner <-
        tune::collect_predictions(bestRefitCVModel) %>%  #  extract predictions from each fold of a cross-validated model, so prediction for each ob when it had turn in validation fold
        dplyr::select(-sym(learnerSpec$outcomeName)) %>% # excludes column "outcomeName"
        dplyr::arrange(.row) %>% # arranges the rows of the data frame based on the values in the .row column
        dplyr::rename(fold_id = id)

      # recipe - prep - bake the train Dat here
      trainDatPrepBaked <-
        mainRecipes %>%
          recipes::prep() %>%   # prepares the recipe for use - any calculations or transformations based on the training data
          recipes::bake(new_data = NULL) # applies recipe to training data

      # append predicted probabilities to original train data
      # adds these columns to training data: "fold_id",".pred_1",".pred_0",".row",".config"
      predProbsLearner <-
        dplyr::bind_cols(trainDatPrepBaked, predProbsLearner)

      # save out ROC and PR data
      # ground truth in factor form
      truth.factor <- as.factor(
        as.character(predProbsLearner[[outcomeName]])
      )
      # in case there is only one observed level
      levels(truth.factor) <- c("0", "1")
      # set the positive class to be the reference level (necessary for metrics below)
      truth.factor <- relevel(truth.factor, ref = "1")

      # KP ADDED NEXT LINE
      # DUE TO ERROR IN roc_curve() when var not in passed in data.frame
      predProbsLearner$truth.factor <- truth.factor

      rocResultsLearner <- yardstick::roc_curve(
        data = predProbsLearner, truth = truth.factor, .pred_yes
      )
      prResultsLearner <- yardstick::pr_curve(
        data = predProbsLearner, truth = truth.factor, .pred_no
      )

      # save out relevant info for objects
      rocResultsLearner$ML            <- prResultsLearner$ML               <- ML
      rocResultsLearner$predSet       <- prResultsLearner$predSet          <- predSetName
      rocResultsLearner$learnerName   <- prResultsLearner$learnerName      <- learnerName

      bestRefitCVMetrics$learnerName  <- predProbsLearner$learnerName <- learnerName
      bestRefitCVMetrics$predSet      <- predProbsLearner$predSet     <- predSetName
      bestRefitCVMetrics$ML           <- predProbsLearner$ML          <- ML

      # saving out for each iteration of the learner
      modelResults   <- dplyr::bind_rows(modelResults, bestRefitCVMetrics)
      predProbs      <- dplyr::bind_rows(predProbs, predProbsLearner)
      rocResults     <- dplyr::bind_rows(rocResults, rocResultsLearner)
      prResults      <- dplyr::bind_rows(prResults, prResultsLearner)

      # refit on NON cross-validated data
      # KP: Why?
      bestRefitModel <- finalWorkflow %>%
        parsnip::fit(data = trainDat)
        # bestRefitModel is a workflow that contains the following
        # "pre" -> preprocessing steps
        # "fit" -> specifies model and hyperparameters to be tuned
        # "post" - > steps involved in evaluating the trained model
        # "train" -> final model obtained after fitting the workflow to the training data

      # save out model fit on full training data
      modelFits[[learnerName]] <- bestRefitModel

      # save out final workflows to be reutilized with testing set
      finalWorkflows[[learnerName]] <- finalWorkflow
      
      cat("finished training and validation for ", learnerName, "\n")
      

    } # Loop for learners
  } # Loop for predictor sets

  return(list(modelResults = modelResults, #tibble of mean and std_err of pr_auc & roc_auc by learner across folds
              predProbs = predProbs, # tibble of stacked copies of the training data (one copy for each learner) with CV predictions appended
              rocResults = rocResults, # data for computing ROC curve (sens & spec at many thresholds) stacked by learner
              prResults = prResults,  # data for computing PR curve (sens & spec at many thresholds) stacked by learner
              modelFits = modelFits, # fit info for all learners refit on full training data after tuning
              finalWorkflows = finalWorkflows)) # workflows for all learners
} # end train_learners

##########################################################################
# test_learners
##########################################################################

# get learner fits and results on test set
#' @param trainResults
#' @param learnerSpec
#' @param mainRecipes
#' @return testMetrics
#' @return testPredProbs
#' @return testRocResults
#' @return testPrResults
#' @return testFits
#' @author Kristen Hunter & Zarni Htet

test_learners <- function(trainResults,
                          learnerSpec,
                          mainRecipes,
                          trn_data,
                          tst_data)
{
  
  # save out test model results
  testMetricsLong <- NULL
  
  # save out roc results
  testRocResults  <- NULL
  
  # save out pr results
  testPrResults   <- NULL
  
  # save predictive probabilities
  testPredProbs   <- NULL
  
  # save the fitted test results
  testFits        <- NULL
  
  learnerNames   <- trainResults$learnerNames
  
  # KP commenting this out to replace since don't have split data
  # # load the split data
  # dataSplit <- readRDS(here::here(
  #   learnerSpec$dataInputPath, learnerSpec$specDataName
  # ))
  
  # replacing lines 257 to 261
  # tidymodels::last_fit requires split object so re-engineering that from test and train datasets
  # copying: https://stackoverflow.com/questions/64004132/tidymodels-creating-an-rsplit-object-from-training-and-testing-data
  #trn_data <- readRDS(here::here(learnerSpec$dataInputPath, learnerSpec$trainDataName))
  #tst_data <- readRDS(here::here(learnerSpec$dataInputPath, learnerSpec$testDataName))
  combined_data <- bind_rows(trn_data,tst_data)
  ind <- list(analysis = seq(nrow(trn_data)), assessment = nrow(trn_data) + seq(nrow(tst_data)))
  dataSplit <- make_splits(ind, combined_data)
  
  # load test data.
  #testDat <- readRDS(here::here(
  #  learnerSpec$dataInputPath, learnerSpec$testDataName
  #))
  testDat <- tst_data
  
  for(learnerName in learnerNames)
  {
    # extract finalize workflow which is fitted model with all
    # the hyperparameters on the entire trained data
    finalWorkflowLearner <-
      trainResults$finalWorkflows[[learnerName]]
    
    # final learner fit
    # supply a data split object to last_fit function & it will pick up the test data's specified metric results
    testFitLearner <-
      finalWorkflowLearner %>%
      tune::last_fit(dataSplit, metrics = metric_set(accuracy, roc_auc, pr_auc))
    
    # metrics
    testMetricsLearner <- testFitLearner %>%
      tune::collect_metrics()
    
    # recipe - prep - bake the test Dat here
    testDatPrepBaked <-
      mainRecipes %>%
      recipes::prep() %>%
      recipes::bake(new_data = testDat)
    
    # predictions/predictive probabilities
    testPredProbsLearner <- tune::collect_predictions(testFitLearner)  %>%
      dplyr::select(-sym(learnerSpec$outcomeName)) %>%
      dplyr::arrange(.row) %>%
      dplyr::rename(fold_id = id)
    
    # append predicted probabilities to original test data
    testPredProbsLearner <-
      dplyr::bind_cols(testDatPrepBaked, testPredProbsLearner)
    
    # save out ROC and PR data
    # ground truth in factor form
    truth.factor <- as.factor(
      as.character(testPredProbsLearner[[learnerSpec$outcomeName]])
    )
    # in case there is only one observed level
    levels(truth.factor) <- c("0", "1")
    # set the positive class to be the reference level (necessary for metrics below)
    truth.factor <- relevel(truth.factor, ref = "1")
    
    # KP ADDED NEXT LINE (same as added in train_learners())
    # DUE TO ERROR IN roc_curve() when var not in passed in data.frame
    testPredProbsLearner$truth.factor <- truth.factor
    
    rocResultsLearner <- yardstick::roc_curve(
      data = testPredProbsLearner, truth = truth.factor, .pred_yes
    )
    prResultsLearner <- yardstick::pr_curve(
      data = testPredProbsLearner, truth = truth.factor, .pred_no
    )
    
    
    # add relevant info
    learnerNameSplit <- split_learner_names(learnerName)
    
    testMetricsLearner$learnerName <- testPredProbsLearner$learnerName <- learnerName
    testMetricsLearner$predSet     <- testPredProbsLearner$predSet     <- learnerNameSplit$predSet
    testMetricsLearner$ML          <- testPredProbsLearner$ML          <- learnerNameSplit$ML
    
    rocResultsLearner$learnerName  <- prResultsLearner$learnerName <- learnerName
    rocResultsLearner$predSet      <- prResultsLearner$predSet     <- learnerNameSplit$predSet
    rocResultsLearner$ML           <- prResultsLearner$ML          <- learnerNameSplit$ML
    
    # append
    testMetricsLong <- dplyr::bind_rows(testMetricsLong, testMetricsLearner)
    testRocResults  <- dplyr::bind_rows(testRocResults, rocResultsLearner)
    testPrResults   <- dplyr::bind_rows(testPrResults, prResultsLearner)
    testPredProbs   <- dplyr::bind_rows(testPredProbs, testPredProbsLearner)
    
    testFits[[learnerName]] <- testFitLearner
  }
  
  testMetrics <- tidyr::pivot_wider(
    testMetricsLong,
    id_cols = c("learnerName", "predSet", "ML"),
    names_from = ".metric",
    values_from = ".estimate"
  ) %>%
    dplyr::relocate(learnerName, predSet, ML)
  
  return(list(testMetrics = testMetrics,
              testPredProbs = testPredProbs,
              testRocResults = testRocResults,
              testPrResults = testPrResults,
              testFits = testFits))
} # model wrapper function

