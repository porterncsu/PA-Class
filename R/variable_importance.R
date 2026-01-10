# Sourcing in get_model_specs function
# source(here::here("R", "learner_training.R"))

#' @param dat Dataframe in analysis
#' @param learnerSpec summary of all learner specifications
#' @param learnerName name of learner
#' @param varInfo dataframe of variables, high values,
#' and low values
#' @param stratVar Variable to stratify on for bootstrap
#' @param bootDraws number of bootstrap draws
#' @author Kristen Hunter

get_all_vi_ests <- function(
  dat, learnerName, learnerSpec,
  varInfo,
  removeVars = NULL,
  stratVar = NULL, bootDraws = 100,
  parallel = TRUE
)
{

  # get learner properties and model spec
  learnerProp <- split_learner_names(selectedLearners = learnerName)
  ML <- learnerProp$mlSet
  tuningFrame <- learnerSpec$tuningSets[[ML]]
  predsetName <- learnerProp$predSet
  predset <- learnerSpec$allPredsets[[predsetName]]
  browser()
  model_spec <- get_model_spec(ML = ML)
  outcomeName <- learnerSpec$outcomeName

  # remove variables
  predset <- predset[!(predset %in% removeVars)]

  boot_vi <- get_boot_vi_est(
    dat = dat,
    predset = predset,
    outcomeName = outcomeName,
    model_spec = model_spec,
    learnerProp = learnerProp,
    tuningFrame = tuningFrame,
    varInfo = varInfo,
    stratVar = stratVar,
    bootDraws = bootDraws,
    parallel = parallel
  )

  return(boot_vi)
}

#' Get all bootstrap samples of variable importance
#'
#' @param dat Dataframe in analysis
#' @param predset full vector of predictors in learner
#' @param outcomeName name of outcome in dataset
#' @param model_spec specification of learner
#' @param varInfo dataframe of variables, high values,
#' and low values
#' @param stratVar Variable to stratify on for bootstrap
#' @param bootCount bootstrap sample iteration (e.g. 1, 2, 3, etc.)
#'
#' @return Dataframe of the all variables and the mean difference from a bootstrap sample
#' @author Kristin Porter, Benjamin Jacques-Leslie, Alissa Stover, Kristen Hunter, Zarni Htet
#'
#' @examples
get_boot_vi_est <- function(
  dat, predset, outcomeName, model_spec,
  tuningFrame, learnerProp,
  varInfo,
  stratVar, bootDraws, parallel
)
{

  # parallelize
  if(parallel)
  {
    future::plan(multisession, workers = future::availableCores() - 1)
  }

  boot_vi_ests <- 1:bootDraws %>%
    furrr::future_map_dfr(~get_single_boot_est(
      dat = dat,
      predset = predset,
      outcomeName = outcomeName,
      model_spec = model_spec,
      learnerProp = learnerProp,
      tuningFrame = tuningFrame,
      varInfo = varInfo,
      stratVar = stratVar,
      bootCount = .))

  return(boot_vi_ests)
}


#' One boot strap of a variable importance
#'
#' @param dat Dataframe in analysis
#' @param predset full vector of predictors in learner
#' @param outcomeName name of outcome in dataset
#' @param model_spec specification of learner
#' @param varInfo dataframe of variables, high values,
#' and low values
#' @param stratVar Variable to stratify on for bootstrap
#' @param bootCount bootstrap sample iteration (e.g. 1, 2, 3, etc.)
#'
get_single_boot_est <- function(dat, predset, outcomeName, model_spec,
                                tuningFrame, learnerProp,
                                varInfo,
                                stratVar, bootCount)
{

  ### Set seed for random bootstrap sample
  set.seed(1218111 + bootCount)

  ### Create stratified bootstrap sample
  if(!is.null(stratVar))
  {
    boot <- rsample::bootstraps(dat, times = 1, strata = all_of(stratVar))
    bootSample <- as.data.frame(boot$splits[[1]])
  } else
  {
    # simple bootstrap sample
    boot <- rsample::bootstraps(dat, times = 1)
    bootSample <- as.data.frame(boot$splits[[1]])
  }

  vi_est <- get_single_vi_est(
    dat = bootSample, predset = predset,
    outcomeName = outcomeName, model_spec = model_spec,
    tuningFrame = tuningFrame, learnerProp = learnerProp,
    varInfo = varInfo
  ) %>%
  dplyr::mutate(bootDraw = bootCount)

  return(vi_est)
}



#' Calculates the difference in mean predicted values from a model
#' between a target indicator variable being 0 or 1
#'
#' @param dat Dataframe in analysis
#' @param predset full vector of predictors in learner
#' @param outcomeName name of outcome in dataset
#' @param model_spec specification of learner
#' @param varSelect variable to conduct VIP analysis
#' @param varLowVal low value of variable; for binary variables, 0
#' @param varHighVal high value of variable; for binary variables, 1
#'
#' @return Dataframe of the selected variable and the mean difference
#' @author Kristin Porter, Kristen Hunter, Benjamin Jacques-Leslie, Alissa Stover, Zarni Htet
#'
#' @examples
get_single_vi_est <-
  function(dat, predset, outcomeName, model_spec, tuningFrame, learnerProp,
           varInfo)
  {

    # prepare data for model training
    dat_pred <- dat %>%
      dplyr::select(dplyr::all_of(predset))
    dat_pred <- dat_pred %>%
      mutate_if(sapply(dat_pred, is.character), as.factor)

    dat_outcome <- dat %>%
      dplyr::select(dplyr::all_of(outcomeName))
    dat_outcome[,outcomeName] <- as.factor(as.character(dat_outcome[,outcomeName]))
    dat_outcome <- as.data.frame(dat_outcome)

    # fit model
    model <- fit_model(
      trainDat_pred    = dat_pred,
      trainDat_outcome = dat_outcome,
      model_spec       = model_spec
    )

    vi_ests <- NULL

    get_est <- function(varSelect, varLowVal, varHighVal)
    {
      # new data with low value of predictor
      newDatVarLow <- dat_pred
      newDatVarLow[[varSelect]] <- varLowVal

      # new data with high value of predictor
      newDatVarHigh <- dat_pred
      newDatVarHigh[[varSelect]] <- varHighVal

      # make predictions
      predVarLow <- predict(model$model_train, new_data = newDatVarLow, type = "prob")
      predVarLow <- as.data.frame(predVarLow)
      colnames(predVarLow) <- c("pred_0", "pred_1")

      predVarHigh <- predict(model$model_train, new_data = newDatVarHigh, type = "prob")
      predVarHigh <- as.data.frame(predVarHigh)
      colnames(predVarHigh) <- c("pred_0", "pred_1")

      # save out results
      est <- tibble(
        predictor = varSelect,
        lowValue = varLowVal,
        highValue = varHighVal,
        est = mean(predVarHigh$pred_1) - mean(predVarLow$pred_1)
      )

      return(est)
    }

    vi_ests <- varInfo %>%
      purrr::pmap_dfr(get_est)

    # Return estimate
    return(vi_ests)
  }
