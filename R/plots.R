# loading my favorite color palette
redGreen0102 <- wesanderson::wes_palette("Darjeeling1")

get_

selectedMetrics <- function(metricType)
{
  if(metricType == "count")
  {
    selectedMetrics = c("tnc", "fnc", "fpc", "tpc")
  } else if(metricType == "rate")
  {
    selectedMetrics = c("tnr", "fnr", "fpr", "tpr")
  } else if(metricType == "fdr")
  {
    selectedMetrics = "fdr"
  }
  return(selectedMetrics)
}

# MDRC Color Palette
mdrc_colors <- data.frame(a = c("#cae3eb", "#F9DABC"    , "#D9C89E"    , "#DAE6D6"    , "#e6e7e7", NA_character_),
                          b = c("#63b6cf", "#EFA967"    , "#A89968"    , "#A1BD7A"    , "#b1b3b5", NA_character_),
                          c = c("#00558C", "#D57800"    , "#816040"    , "#538549"    , "#808284", "#B7312C"    ),
                          d = c("#002B49", NA_character_, NA_character_, NA_character_, "#000000", NA_character_),
                          row.names = c("blue", "orange", "brown", "green", "grey", "red"))

# MDRC Theme
theme_mdrc <- function(base_size = 9, base_family= "ArialMT") {

  # All text should appear in Arial 9-point font, except axis labels and titles.
  theme_bw(base_size = base_size, base_family = base_family) %+replace%

    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),

          # No legend - use labels directly in graph
          # legend.position = "none",

          # Show only x and y borders
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),

          # All labels should be horizontal
          axis.title.y = element_text(angle = 360),

          # Making axis values darker
          axis.text = element_text(colour = "black", size = 9),

          # Center and bold titles should (Arial Bold 11-point)
          plot.title = element_text(face = "bold",
                                    hjust = 0.5,
                                    size = 11,
                                    margin= ggplot2::margin(0,0,1,0, unit = "line")),

          # Left-align source/notes/footnotes
          plot.caption = element_text(hjust= 0,
                                      margin= ggplot2::margin(1,0,0,0, unit = "line")),
          # Additional formatting added
          # Remove tickmarks
          axis.ticks.y =element_blank(),
          axis.ticks.x =element_blank(),

          # Remove background color for panel labels created by facet_*
          strip.background = element_blank(),

          # Make panel label horizontal
          strip.text = element_text(size = 9),
          strip.text.y.right = element_text(angle = 0),
          strip.text.y.left = element_text(angle = 0),

          # Make panel label outside of the axis line (outside/inside)
          strip.placement = "outside"

    )
}

#' Wrapper to produce the mdrc themed graph
#' appropriating from RTU
#'
#' @param plotData prepared Long Data with confusion matrix rates or counts along with learner name and thresholds
#' @param outcomeDescription predictive negative and positive descriptions
#' @param metricType to plot a count or a rate graph
#' @param graphTitle title of a graph
#'
#' @return
#' @export
#'
#' @examples
conf_mat_bar_plot <- function(
  plotData, equityVar = NULL, metricType, outcomeDescription, graphTitle
)
{
  if(metricType == "count")
  {
    metrics_labels = c(
      "tnc" = "True \nNegative",
      "fnc" = "False\nNegative",
      "fpc" = "False\nPositive",
      "tpc" = "True \nPositive"
    )
    axis.title.y <- "Count"
  } else if(metricType == "rate")
  {
    metrics_labels = c(
      "tnr" = "True \nNegative",
      "fnr" = "False\nNegative",
      "fpr" = "False\nPositive",
      "tpr" = "True \nPositive"
    )
    axis.title.y <- "Rate"
  } else if(metricType == "fdr")
  {
    metrics_labels = c(
      "fdr" = "False\nDiscovery"
    )
    axis.title.y <- "False Discovery Rate"
  } else if(metricType == "fnr"){

    metrics_labels = c(
      "fnr" = "False\nNegative"
    )
    axis.title.y <- "False Negative Rate"
  } else if(metricType == "fpr"){

    metrics_labels = c(
      "fpr" = "False\nPositive"
    )
    axis.title.y <- "False Positive Rate"
  } else if(metricType == "tpr"){

    metrics_labels = c(
      "tpr" = "True\nPositive"
    )
    axis.title.y <- "True Positive Rate"
  } else if(metricType == "tnr"){

    metrics_labels = c(
      "tnr" = "True\nNegative"
    )
    axis.title.y <- "True Negative Rate"
  } else if(metricType == "ppv"){

    metrics_labels = c(
      "ppv" = "Precision"
    )
    axis.title.y <- "Precision"
  }

  if(metricType %in% c("count", "rate"))
  {
    # data preparation for plotting
    plotData <- plotData %>%
      dplyr::mutate(
        metricType_fct = ordered(metricType,
                                 levels = names(metrics_labels),
                                 labels = metrics_labels),
        predict_cat4 = ordered(metricType,
                               levels = names(metrics_labels),
                               labels = c(outcomeDescription[1],
                                          " ",
                                          outcomeDescription[2],
                                          "  ")))

  }

  # setting the mdrc theme
  pal <- c(mdrc_colors["blue",3],
           mdrc_colors["blue",1],
           mdrc_colors["orange",1],
           mdrc_colors["orange",3])

  if(is.null(equityVar))
  {
    learnerNames <- unique(plotData$learnerName)
    p <- plotData %>%
      ggplot2::ggplot(aes(x = learnerName, y = value, fill = learnerName)) +
      ggplot2::scale_fill_manual("Learner", values = pal)
  } else
  {
    # ensure it is a factor
    plotData[[equityVar]] <- as.factor(as.character(plotData[[equityVar]]))
    equityVarCats <- unique(plotData[[equityVar]])

    p <- plotData %>%
      ggplot2::ggplot( aes(x = .data[[equityVar]], y = value, fill = .data[[equityVar]])) +
      ggplot2::scale_fill_manual("Category", values = pal)
  }

  p <- p +
    ggplot2::geom_bar(stat = "identity") +
    # Extend the y/x axis by a multiplier
    scale_y_continuous(position = "right") +
    # Prevent ggplot from cutting off text
    ggplot2::coord_cartesian(clip = "off") +
    # Label around text
    labs(title = graphTitle, x = "Threshold", y = axis.title.y) +
    theme_mdrc() +
    # Additional modifications specific this plot
    theme(
      axis.text.x = element_blank(),
      # Adjusting spacing between panels
      panel.spacing.y = unit(2, "lines"),
      panel.spacing.x = unit(.5, "lines"),
      # Adjusting spacing around title text
      plot.title = element_text(margin= ggplot2::margin(0,0,3,0, unit = "line")),
      strip.text.y = element_text(margin= ggplot2::margin(0,1,0,0, unit = "line"))
    )

  if(metricType %in% c("rate", "count"))
  {
    p <- p +
      ggplot2::facet_grid(predict_cat4 + metricType_fct  ~  threshold  ,
                          # Move strip label from top to bottom ("x"), right to left ("y")
                          # Or do for both ("both")
                          switch = "both",
                          scales = "fixed",
                          space = "fixed")
  } else
  {
    p <- p +
      ggplot2::facet_grid(~  threshold  ,
                          # Move strip label from top to bottom ("x"), right to left ("y")
                          # Or do for both ("both")
                          switch = "both",
                          scales = "fixed",
                          space = "fixed")
  }

  return(p)
}

#' Predictive Probability Graph function
#'
#' This function will plot the predictive probabilities of the learner of choice when we specify
#' the ML, predSet, tuningSet and predProbs
#'
#' @param ML learner's ML
#' @param predSet learner's predictor set
#' @param tuningSet learner's tuning set
#' @param predProbs the dataset with predictive probabilities of all learners
#' @param splitCategory NULL or character; specify whether to split data by a certain category,
#' such as success/failure or a protected class like gender
#' @param learnerName learnerName (if you have inserted this, ML, predSet and tuningSet could be null)
#' @param probGroups observations lumped into different probability groups such as low, medium, high
#'
#' @return
#' @export
#'
#' @examples
pred_prob_graph <- function(
  predProbs,
  ML = NULL, predSet = NULL,
  learnerName = NULL,
  splitCategory = NULL,
  probGroups = NULL)
{

  if(!is.null(learnerName) & (!is.null(ML) | !is.null(predSet) )
     & (!is.null(probGroups)))
  {
    stop("Please provide either the learnerName OR the combination of ML, predSet, and tuningSet OR
         the probability group")
  }
  if(is.null(learnerName) & is.null(probGroups))
  {
    if(is.null(ML) | is.null(predSet))
    {
      stop("Please provide all: ML, predSet, tuningSet")
    }
    else
    {
      learnerName <- paste(ML, predSet, sep = "_")
    }
  }

  # Pulling the predictive probabilities of the learner of choice or probability group of choice

  if(!is.null(learnerName)){

    choice_learner <- predProbs %>%
      dplyr::filter(learnerName == !!learnerName)
    labelName <- learnerName # for graph title label

  } else if(!is.null(probGroups)) {

    choice_learner <- predProbs %>%
      dplyr::filter(probGroups == !!probGroups)
    labelName <- paste0("probability group ", probGroups)

  }

  if(is.null(splitCategory)){

    # ggplot for the learner of choice
    choice_learner_graph <-
      ggplot(choice_learner, aes(.pred_yes)) +
      geom_histogram(fill = redGreen0102[2]) +
      xlab ("Predicted Probabilities") +
      ylab ("Count") +
      ggtitle(paste("Distribution of probabilities for\n", labelName)) +
      scale_x_continuous(limits = c(0,1)) +
      theme (plot.title = element_text(size = 9,
                                       face = "bold",
                                       vjust = 1,
                                       hjust = 0.5))
  } else {

    choice_learner <- choice_learner %>%
        mutate(., {{splitCategory}}:= factor(.data[[splitCategory]]))

    # ggplot for the learner of choice
    choice_learner_graph <-
      ggplot(choice_learner,
             aes_string(x = ".pred_yes", fill = splitCategory, alpha = 0.5)) +
      geom_density() +
      xlab ("Predicted Probabilities") +
      ggtitle (paste("Distribution of probabilities for\n", labelName)) +
      scale_x_continuous(limits = c(0,1)) +
      theme(plot.title = element_text(size = 9,
                                      face = "bold",
                                      vjust = 1,
                                      hjust = 0.5)) +
      guides(alpha = "none")
  }

  return(choice_learner_graph)

}

# generate variable importance graphs
#' @param learnerFits list of fitted model parsnip objects
generate_vip_graphs <- function(learnerFits)
{

  # error handling for ML algorithms where VIP is not supported.
  robustVip <- function(learnerName, fit)
  {
    tryCatch(vip::vip(fit),
             warning = function(w) { print("Warning"); return(x) },
             error = function(e) { print(paste("Error:", e, learnerName)); return(NULL) })
  }

  vipGraphs <- list()
  counter <- 0

  for(i in 1:length(learnerFits)){

    learnerName <- names(learnerFits)[[i]]
    fit <- tune::extract_model(learnerFits[[i]])

    # try to create plot
    vipGraph <- robustVip(learnerName, fit)

    if(!is.null(vipGraph))
    {
      counter <- counter + 1
      vipGraph <- vipGraph +
        ggtitle(paste0("Variable Importance for\n", learnerName)) +
        theme(plot.title = element_text(size = 12,
                                        face = "bold",
                                        vjust = 1,
                                        hjust = 0.5))


      vipGraphs[[counter]] <- vipGraph
    }
  }
  return(vipGraphs)
}


# generate predictor plots by prediction category
generate_pred_plots <- function(predProbs, predictors)
{
  predPlots <- NULL

  for(i in 1:length(predictors))
  {
    predictor <- predictors[i]

    # base plot
    p <- ggplot(predProbs) +
      aes_string(predictor)

    # try to determine if categorical
    if(length(unique(predProbs[[predictor]])) <= 10)
    {
      predProbs[[predictor]] <- as.factor(as.character(predProbs[[predictor]]))
      p <- p + geom_bar(aes(fill = predCat), position = position_dodge())
    } else
    {
      p <- p + geom_density(aes(color = predCat))
    }
    predPlots[[i]] <- p
  }

  return(predPlots)
}


# plot histograms of all predictors
generate_predictor_histograms <- function(dat, allPredictors)
{
  # categorize into categorical or numeric predictors
  predictorTypes <- categorizePredictors(dat = trainingDat, predictors = allPredictors)
  numericPredictors <- predictorTypes$numericPredictors

  # plot histograms of numeric predictors
  predictorPlots <- NULL
  for(i in 1:length(numericPredictors))
  {
    predictor <- numericPredictors[i]

    # histogram plot
    p <- ggplot(trainingDat) +
      aes_string(predictor) +
      geom_histogram()

    predictorPlots[[i]] <- p
  }

  return(predictorPlots)
}


