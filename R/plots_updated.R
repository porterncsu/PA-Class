# loading color palette
redGreen0102 <- wesanderson::wes_palette("Darjeeling1")

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


#' Plot Predicted Probabilities by Learner
#'
#' This function generates a density plot to visualize the distributions of
#' predicted probabilities across multiple learners (models). Each learner's
#' distribution is represented by a separate density curve.
#'
#' @param predProbs A data frame containing predicted probabilities and learner identifiers.
#'   It must have at least the following columns:
#'   - `.pred_yes`: The predicted probabilities for the positive class.
#'   - `learnerName`: The name of the learner (model) associated with each prediction.
#'
#' @return A ggplot object representing the density curves for predicted probabilities.
#'
#' @examples
#' # Assuming `learnersResults$predProbs` contains the required columns:
#' plot_predicted_probabilities(learnersResults$predProbs)
#'
#' @export
plot_predicted_probabilities <- function(predProbs) {
  # Ensure the input data has the required columns
  if (!all(c(".pred_yes", "learnerName") %in% colnames(predProbs))) {
    stop("The input data frame must contain '.pred_yes' and 'learnerName' columns.")
  }

  # Combine probabilities and add learner as a factor
  combinedProbs <- predProbs %>%
    dplyr::mutate(learner = factor(learnerName))

  # Plot density curves
  ggplot(combinedProbs, aes(x = .pred_yes)) +
    geom_density(fill = "steelblue", alpha = 0.2) + # Single color, no aes()
    facet_wrap(~ learner, scales = "free_y") +
    labs(
      title = "Density of Predicted Likelihoods by Learner",
      x = "Predicted Probability",
      y = "Density"
    ) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend
}

#' Plot Predicted Probabilities for Training and Test Sets
#'
#' This function generates a density plot to visualize the distributions of
#' predicted probabilities across training and test sets. Each set's
#' distribution is represented by a separate density curve.
#'
#' @param predProbs A data frame containing predicted probabilities and set identifiers.
#'   It must have at least the following columns:
#'   - `.pred_yes`: The predicted probabilities for the positive class.
#'   - `set`: The name of the data set associated with each prediction.
#'
#' @return A ggplot object representing the density curves for predicted probabilities.
#'
#' @export
plot_predicted_probabilities_bySet <- function(predProbs) {
  # Ensure the input data has the required columns
  if (!all(c(".pred_yes", "set") %in% colnames(predProbs))) {
    stop("The input data frame must contain '.pred_yes' and 'set' columns.")
  }

  # Combine probabilities and add set as a factor
  combinedProbs <- predProbs %>%
    dplyr::mutate(set = factor(set))

  # Plot density curves
  ggplot(combinedProbs, aes(x = .pred_yes, color = set, fill = set)) +
    geom_density(alpha = 0.2) + # Semi-transparent fill
    labs(
      title = "Density of Predicted Likelihoods by Data Set",
      x = "Predicted Probability",
      y = "Density"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())
}

#' Plot Predicted Probabilities by Learner, Split by True Outcome
#'
#' This function generates a density plot to visualize the distributions of
#' predicted probabilities across multiple learners (models). Optionally, it
#' splits the probabilities by the true outcome (e.g., 0 or 1) for comparison.
#'
#' @param predProbs A data frame containing predicted probabilities and learner identifiers.
#'   It must have at least the following columns:
#'   - `.pred_yes`: The predicted probabilities for the positive class.
#'   - `learnerName`: The name of the learner (model) associated with each prediction.
#'   - The true outcome column, specified by `outcomeName`.
#' @param outcomeName The name of the column representing the true outcome (binary: 0 or 1).
#'
#' @return A ggplot object representing the density curves for predicted probabilities, optionally split by true outcome.
#'
#' @examples
#' # Assuming `learnersResults$predProbs` contains the required columns:
#' plot_predicted_probabilities_split(learnersResults$predProbs, outcomeName)
#'
#' @export
plot_predicted_probabilities_split <- function(predProbs, outcomeName) {
  # Ensure the input data has the required columns
  required_columns <- c(".pred_yes", "learnerName", outcomeName)
  if (!all(required_columns %in% colnames(predProbs))) {
    stop(paste("The input data frame must contain the following columns:", paste(required_columns, collapse = ", ")))
  }

  # Combine probabilities and add learner and true outcome as factors
  combinedProbs <- predProbs %>%
    dplyr::mutate(
      learner = factor(learnerName),
      true_outcome = factor(!!sym(outcomeName), levels = c("no", "yes"), labels = c("Outcome: 0", "Outcome: 1"))
    )

  # Plot density curves, split by true outcome
  ggplot(combinedProbs, aes(x = .pred_yes, color = true_outcome, fill = true_outcome)) +
    geom_density(alpha = 0.2) +
    facet_wrap(~ learner, scales = "free_y") + # Separate panels for each learner
    labs(
      title = "Density of Predicted Likelihoods by Learner, Split by True Outcome",
      x = "Predicted Probability",
      y = "Density",
      color = "True Outcome",
      fill = "True Outcome"
    ) +
    theme_minimal()
}


#' Plot ROC Curves by Predictor Set with a Reference Learner
#'
#' This function generates ROC curves faceted by prediction set (`predSet`),
#' including a reference prediction set (e.g., `predSet_bm`) in all plots for comparison.
#' The reference prediction set is displayed with a solid line, while other learners
#' are displayed with dashed lines. A solid diagonal line (slope = 1) is included as a baseline.
#'
#' @param rocResults A data frame containing the ROC results. It must include the following columns:
#'   - `specificity`: The specificity values for the ROC curve.
#'   - `sensitivity`: The sensitivity values for the ROC curve.
#'   - `ML`: The name of the machine learning model (learner).
#'   - `predSet`: The prediction set associated with each learner.
#' @param referencePredSet The name of the reference prediction set to include in all facets. Defaults to `"predSet_bm"`.
#' @param title The title of the plot. Defaults to "ROC Curves by Prediction Set".
#'
#' @return A ggplot object representing the ROC curves faceted by prediction set, with a reference curve included in all facets.
#'
#' @examples
#' # Assuming `learnersResults$rocResults` contains the required columns:
#' plot_roc_curve_with_reference(learnersResults$rocResults, referencePredSet = "predSet_bm")
#'
#' @export
plot_roc_curve_with_reference <- function(rocResults, referencePredSet = "predSet_bm", referenceLearner = "glm", title = "ROC Curves by Prediction Set") {
  library(dplyr)
  library(ggplot2)
  library(viridis)

  # Ensure required columns exist
  required_columns <- c("specificity", "sensitivity", "ML", "predSet")
  if (!all(required_columns %in% colnames(rocResults))) {
    stop(paste("The input data frame must contain the following columns:", paste(required_columns, collapse = ", ")))
  }

  # Extract the reference data for the combination of predSet_bm and glm
  reference_data <- rocResults %>%
    filter(predSet == referencePredSet & ML == referenceLearner) %>%
    mutate(ML = "Benchmark")  # Label the benchmark curve as "Benchmark"

  # Expand the reference data to include all unique predSets
  reference_data_expanded <- rocResults %>%
    distinct(predSet) %>%  # Get all unique predSets
    cross_join(reference_data) %>%  # Perform cross join to replicate reference data
    mutate(predSet = predSet.x) %>%  # Update predSet for all facets
    select(-predSet.x, -predSet.y)

  # Combine expanded reference data with the original data
  rocResults_combined <- bind_rows(
    if (nrow(rocResults %>% filter(ML != referenceLearner)) > 0) rocResults else NULL,  # Keep original learners if they exist
    reference_data_expanded  # Add benchmark data
  )

  # Generate a color palette for all learners (excluding Benchmark)
  unique_learner_names <- unique(rocResults_combined$ML)
  non_benchmark_learners <- setdiff(unique_learner_names, "Benchmark")

  learner_colors <- if (length(non_benchmark_learners) > 0) {
    viridis_pal(option = "plasma")(length(non_benchmark_learners))
  } else {
    character(0)  # Empty vector if there are no non-benchmark learners
  }
  names(learner_colors) <- non_benchmark_learners

  # Add red color for Benchmark if present
  if ("Benchmark" %in% unique_learner_names) {
    learner_colors <- c(learner_colors, "Benchmark" = "red")
  }

  # Create base plot
  p <- ggplot()

  # Add learner curves (only if there are non-benchmark learners)
  if (length(non_benchmark_learners) > 0) {
    p <- p + geom_path(
      data = rocResults_combined %>% filter(ML != "Benchmark"),
      aes(x = 1 - specificity, y = sensitivity, color = ML),
      linewidth = 1, alpha = 0.8
    )
  }

  # Add benchmark curves
  if ("Benchmark" %in% unique_learner_names) {
    p <- p + geom_path(
      data = rocResults_combined %>% filter(ML == "Benchmark"),
      aes(x = 1 - specificity, y = sensitivity),
      color = "red", linewidth = 1.2, alpha = 1
    )
  }

  # Finalize plot with additional formatting
  p <- p +
    geom_abline(slope = 1, intercept = 0, color = "gray50", linewidth = 0.8) + # Solid diagonal line
    facet_wrap(~ predSet, ncol = min(length(unique(rocResults_combined$predSet)), 3)) + # Dynamic columns
    scale_color_manual(values = learner_colors) + # Assign colors
    labs(
      title = title,
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)",
      color = "Learner"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.minor = element_blank()
    )

  return(p)
}

#   # Ensure required columns exist
#   required_columns <- c("specificity", "sensitivity", "ML", "predSet")
#   if (!all(required_columns %in% colnames(rocResults))) {
#     stop(paste("The input data frame must contain the following columns:", paste(required_columns, collapse = ", ")))
#   }
#
#   # Initialize combined results with the original data
#   rocResults_combined <- rocResults
#
#   # Add benchmark data only if a valid referencePredSet is provided
#   if (!is.null(referencePredSet) && referencePredSet != "none") {
#     reference_data <- rocResults %>%
#       filter(predSet == referencePredSet & ML == referenceLearner) %>%
#       mutate(ML = "Benchmark")  # Label the benchmark curve as "Benchmark"
#
#     if (nrow(reference_data) > 0) {  # Only proceed if there is valid benchmark data
#       reference_data_expanded <- rocResults %>%
#         distinct(predSet) %>%
#         cross_join(reference_data) %>%
#         mutate(predSet = predSet.x) %>%
#         select(-predSet.x, -predSet.y)
#
#       # Append benchmark data
#       rocResults_combined <- bind_rows(rocResults_combined, reference_data_expanded)
#     }
#   }
#
#   # Generate unique learner names (excluding Benchmark if it doesn’t exist)
#   unique_learner_names <- unique(rocResults_combined$ML)
#   unique_learner_names <- setdiff(unique_learner_names, "Benchmark")  # Exclude Benchmark if not present
#
#   # Assign colors only if there are non-benchmark learners
#   learner_colors <- if (length(unique_learner_names) > 0) {
#     viridis_pal(option = "plasma")(length(unique_learner_names))
#   } else {
#     character(0)  # Empty vector to avoid errors
#   }
#   names(learner_colors) <- unique_learner_names
#
#   # Add red color for Benchmark only if it exists
#   if ("Benchmark" %in% rocResults_combined$ML) {
#     learner_colors <- c(learner_colors, "Benchmark" = "red")
#   }
#
#   # Plot learners and benchmark (if present)
#   p <- ggplot() +
#     geom_path(
#       data = rocResults_combined %>% filter(ML != "Benchmark"),
#       aes(x = 1 - specificity, y = sensitivity, color = ML),
#       linewidth = 1, alpha = 0.8
#     ) +
#     if ("Benchmark" %in% rocResults_combined$ML) {
#       geom_path(
#         data = rocResults_combined %>% filter(ML == "Benchmark"),
#         aes(x = 1 - specificity, y = sensitivity),
#         color = "red", linewidth = 1.2, alpha = 1
#       )
#     } else NULL +
#     geom_abline(slope = 1, intercept = 0, color = "gray50", linewidth = 0.8) +
#     facet_wrap(~ predSet, ncol = min(length(unique(rocResults_combined$predSet)), 3)) +
#     scale_color_manual(values = learner_colors) +
#     labs(
#       title = title,
#       x = "1 - Specificity (False Positive Rate)",
#       y = "Sensitivity (True Positive Rate)",
#       color = "Learner"
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       legend.position = "bottom",
#       legend.box = "horizontal",
#       panel.grid.minor = element_blank()
#     )
#
#   return(p)
# }

#' Plot Precision-Recall (PR) Curves with a Benchmark
#'
#' This function plots Precision-Recall (PR) curves for multiple machine learning models (learners)
#' across different prediction sets. It includes a benchmark curve for a specified prediction
#' set and learner in all facets.
#'
#' @param prResults A data frame containing the PR curve results. The data must include the following columns:
#'   - `recall`: Recall values for the PR curve.
#'   - `precision`: Precision values for the PR curve.
#'   - `ML`: The name of the machine learning model (learner).
#'   - `predSet`: The prediction set associated with each learner.
#' @param referencePredSet A string specifying the prediction set to use for the benchmark curve. Default is `"predSet_bm"`.
#' @param referenceLearner A string specifying the learner to use for the benchmark curve. Default is `"glm"`.
#' @param title A string specifying the title of the plot. Default is `"PR Curves by Prediction Set"`.
#'
#' @return A ggplot object representing the PR curves faceted by prediction set, with a benchmark curve included in all facets.
#'
#' @examples
#' # Example usage
#' plot_pr_curve_with_reference(
#'   prResults = learnersResults$prResults,
#'   referencePredSet = "predSet_bm",
#'   referenceLearner = "glm"
#' )
#'
#' @export
plot_pr_curve_with_reference <- function(prResults, referencePredSet = "predSet_bm", referenceLearner = "glm", title = "PR Curves by Prediction Set") {
  library(dplyr)
  library(ggplot2)
  library(viridis)

  # Ensure required columns exist
  required_columns <- c("recall", "precision", "ML", "predSet")
  if (!all(required_columns %in% colnames(prResults))) {
    stop(paste("The input data frame must contain the following columns:", paste(required_columns, collapse = ", ")))
  }

  # Extract the reference data for the specified benchmark
  reference_data <- prResults %>%
    filter(predSet == referencePredSet & ML == referenceLearner) %>%
    mutate(ML = "Benchmark")  # Label as "Benchmark"

  # Expand the reference data to include all unique predSets
  reference_data_expanded <- prResults %>%
    distinct(predSet) %>%
    cross_join(reference_data) %>%
    mutate(predSet = predSet.x) %>%
    select(-predSet.x, -predSet.y)

  # Combine expanded reference data with the original data
  prResults_combined <- bind_rows(
    if (nrow(prResults %>% filter(ML != referenceLearner)) > 0) prResults else NULL,  # Keep original learners if they exist
    reference_data_expanded  # Add benchmark data
  )

  # Generate unique learner names (excluding Benchmark if not present)
  unique_learner_names <- unique(prResults_combined$ML)
  non_benchmark_learners <- setdiff(unique_learner_names, "Benchmark")

  # Assign colors only if there are non-benchmark learners
  learner_colors <- if (length(non_benchmark_learners) > 0) {
    viridis_pal(option = "plasma")(length(non_benchmark_learners))
  } else {
    character(0)  # Empty vector if no non-benchmark learners exist
  }
  names(learner_colors) <- non_benchmark_learners

  # Add red color for Benchmark if present
  if ("Benchmark" %in% unique_learner_names) {
    learner_colors <- c(learner_colors, "Benchmark" = "red")
  }

  # Create base plot
  p <- ggplot()

  # Add learner curves (only if there are non-benchmark learners)
  if (length(non_benchmark_learners) > 0) {
    p <- p + geom_path(
      data = prResults_combined %>% filter(ML != "Benchmark"),
      aes(x = recall, y = precision, color = ML),
      linewidth = 1, alpha = 0.8
    )
  }

  # Add benchmark curve
  if ("Benchmark" %in% unique_learner_names) {
    p <- p + geom_path(
      data = prResults_combined %>% filter(ML == "Benchmark"),
      aes(x = recall, y = precision),
      color = "red", linewidth = 1.2, alpha = 1
    )
  }

  # Finalize plot with additional formatting
  p <- p +
    facet_wrap(~ predSet, ncol = min(length(unique(prResults_combined$predSet)), 3)) + # Dynamic columns
    scale_color_manual(values = learner_colors) + # Assign colors
    labs(
      title = title,
      x = "Recall",
      y = "Precision",
      color = "Learner"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.minor = element_blank()
    )

  return(p)
}

#' Create Confusion Matrix Bar Plot
#'
#' Creates a bar plot visualization of confusion matrix metrics with flexible
#' display options for rates, counts, and other classification metrics.
#'
#' @param plotData data.frame. Must contain columns:
#'   \itemize{
#'     \item learnerName - model identifier
#'     \item value - metric values
#'     \item threshold - classification threshold
#'     \item metricType - type of metric being plotted
#'   }
#' @param equityVar character or NULL. Optional column name for equity analysis grouping
#' @param metricType character. One of:
#'   \itemize{
#'     \item "count" - confusion matrix counts (tnc, fnc, fpc, tpc)
#'     \item "rate" - confusion matrix rates (tnr, fnr, fpr, tpr)
#'     \item "fdr" - false discovery rate
#'     \item "fnr" - false negative rate
#'     \item "fpr" - false positive rate
#'     \item "tpr" - true positive rate
#'     \item "tnr" - true negative rate
#'     \item "ppv" - precision/positive predictive value
#'   }
#' @param outcomeDescription character vector of length 2. Labels for negative and
#'   positive classes in order c("negative_label", "positive_label")
#' @param graphTitle character. Title for the plot
#'
#' @return A ggplot2 object
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' conf_mat_bar_plot(
#'   plotData = metrics_data,
#'   metricType = "rate",
#'   outcomeDescription = c("Not Graduate", "Graduate"),
#'   graphTitle = "Classification Performance Metrics"
#' )
#'
conf_mat_bar_plot <- function(plotData,
                              equityVar = NULL,
                              metricType,
                              outcomeDescription,
                              graphTitle) {

  # Input validation
  validate_inputs(plotData, metricType, outcomeDescription)

  # Get metric configuration
  metric_config <- get_metric_config(metricType)
  metrics_labels <- metric_config$labels
  axis.title.y <- metric_config$axis_title

  # Data preparation for count and rate plots
  if(metricType %in% c("count", "rate")) {
    plotData <- prepare_plot_data(plotData, metrics_labels, outcomeDescription)
  }

  # Create base plot
  p <- create_base_plot(plotData, equityVar, get_plot_palette())

  # Add common plot elements
  p <- p +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(position = "right") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = graphTitle,
                  x = "Threshold",
                  y = axis.title.y) +
    get_plot_theme()

  # Add faceting based on metric type
  p <- add_faceting(p, metricType)

  return(p)
}

#' Validate function inputs
#' @noRd
validate_inputs <- function(plotData, metricType, outcomeDescription) {
  if (!is.data.frame(plotData)) {
    stop("plotData must be a data frame")
  }

  valid_metrics <- c("count", "rate", "fdr", "fnr", "fpr", "tpr", "tnr", "ppv")
  if (!metricType %in% valid_metrics) {
    stop(sprintf("metricType must be one of: %s",
                 paste(valid_metrics, collapse = ", ")))
  }

  if (length(outcomeDescription) != 2) {
    stop("outcomeDescription must be a vector of length 2")
  }

  required_cols <- c("learnerName", "value", "threshold")
  missing_cols <- setdiff(required_cols, names(plotData))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s",
                 paste(missing_cols, collapse = ", ")))
  }
}

#' Get metric configuration
#' @noRd
get_metric_config <- function(metricType) {
  configs <- list(
    "count" = list(
      labels = c(
        "tnc" = "True \nNegative",
        "fnc" = "False\nNegative",
        "fpc" = "False\nPositive",
        "tpc" = "True \nPositive"
      ),
      axis_title = "Count"
    ),
    "rate" = list(
      labels = c(
        "tnr" = "True \nNegative",
        "fnr" = "False\nNegative",
        "fpr" = "False\nPositive",
        "tpr" = "True \nPositive"
      ),
      axis_title = "Rate"
    ),
    "fdr" = list(
      labels = c("fdr" = "False\nDiscovery"),
      axis_title = "False Discovery Rate"
    ),
    "fnr" = list(
      labels = c("fnr" = "False\nNegative"),
      axis_title = "False Negative Rate"
    ),
    "fpr" = list(
      labels = c("fpr" = "False\nPositive"),
      axis_title = "False Positive Rate"
    ),
    "tpr" = list(
      labels = c("tpr" = "True\nPositive"),
      axis_title = "True Positive Rate"
    ),
    "tnr" = list(
      labels = c("tnr" = "True\nNegative"),
      axis_title = "True Negative Rate"
    ),
    "ppv" = list(
      labels = c("ppv" = "Precision"),
      axis_title = "Precision"
    )
  )

  return(configs[[metricType]])
}

#' Prepare plot data for count and rate metrics
#' @noRd
prepare_plot_data <- function(plotData, metrics_labels, outcomeDescription) {
  plotData %>%
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

#' Get color palette for plot
#' @noRd
get_plot_palette <- function() {
  c(mdrc_colors["blue",3],
    mdrc_colors["blue",1],
    mdrc_colors["orange",1],
    mdrc_colors["orange",3])
}

#' Create base plot
#' @noRd
create_base_plot <- function(plotData, equityVar, pal) {
  if(is.null(equityVar)) {
    learnerNames <- unique(plotData$learnerName)
    plotData %>%
      ggplot2::ggplot(aes(x = learnerName, y = value, fill = learnerName)) +
      ggplot2::scale_fill_manual("Learner", values = pal)
  } else {
    plotData[[equityVar]] <- as.factor(as.character(plotData[[equityVar]]))
    plotData %>%
      ggplot2::ggplot(aes(x = .data[[equityVar]],
                          y = value,
                          fill = .data[[equityVar]])) +
      ggplot2::scale_fill_manual("Category", values = pal)
  }
}

#' Get plot theme settings
#' @noRd
get_plot_theme <- function() {
  theme_mdrc() +
    theme(
      axis.text.x = element_blank(),
      panel.spacing.y = unit(2, "lines"),
      panel.spacing.x = unit(.5, "lines"),
      plot.title = element_text(
        margin = ggplot2::margin(0, 0, 3, 0, unit = "line")
      ),
      strip.text.y = element_text(
        margin = ggplot2::margin(0, 1, 0, 0, unit = "line")
      )
    )
}

#' Add faceting to plot
#' @noRd
add_faceting <- function(p, metricType) {
  if(metricType %in% c("rate", "count")) {
    p + ggplot2::facet_grid(
      predict_cat4 + metricType_fct ~ threshold,
      switch = "both",
      scales = "fixed",
      space = "fixed"
    )
  } else {
    p + ggplot2::facet_grid(
      ~ threshold,
      switch = "both",
      scales = "fixed",
      space = "fixed"
    )
  }
}

#' Calibration Plot for Binary Outcomes by Learner
#'
#' This function generates a calibration plot for binary outcomes, drawing separate
#' calibration curves for each learner specified in the `learnerName` column.
#'
#' @param data A data frame containing predicted probabilities, ground truths,
#' and learner names.
#' @param predicted_probs_col The name of the column with predicted probabilities (string).
#' @param ground_truths_col The name of the column with ground truth labels (string). Must be binary.
#' @param learner_col The name of the column with learner names (string).
#' @param n_bins An integer specifying the number of bins to group predicted probabilities. Default is 10.
#'
#' @return A ggplot object representing the calibration plot.
#' @export
#'
#' @examples
#' # Example data
#' data <- data.frame(
#'   predicted_probs = c(0.1, 0.4, 0.35, 0.8, 0.9, 0.65, 0.2, 0.5, 0.7, 0.85),
#'   ground_truths = c(0, 0, 0, 1, 1, 1, 0, 0, 1, 1),
#'   learnerName = rep(c("Learner1", "Learner2"), each = 5)
#' )
#'
#' # Generate the calibration plot
#' calibration_plot(data, "predicted_probs", "ground_truths", "learnerName")
#'
#' @import ggplot2
#' @importFrom dplyr group_by summarise
calibration_plot <- function(data, predicted_probs_col, ground_truths_col, learner_col, n_bins = 10) {
  # Check if inputs are valid
  if (!all(c(predicted_probs_col, ground_truths_col, learner_col) %in% colnames(data))) {
    stop("One or more specified columns are not present in the data")
  }

  # If ground truths are factors, convert to numeric based on levels
  if (is.factor(data[[ground_truths_col]])) {
    levels_vec <- levels(data[[ground_truths_col]])
    data[[ground_truths_col]] <- as.numeric(data[[ground_truths_col]] == levels_vec[2])
  }

  # Convert predicted probabilities to numeric if they aren't already
  data[[predicted_probs_col]] <- as.numeric(as.character(data[[predicted_probs_col]]))

  # Create bins
  data$bin <- cut(data[[predicted_probs_col]],
                  breaks = seq(0, 1, length.out = n_bins + 1),
                  include.lowest = TRUE)

  # Calculate the average predicted probability and observed proportion for each learner and bin
  calibration_data <- data %>%
    group_by(.data[[learner_col]], bin) %>%
    summarise(
      avg_predicted_prob = mean(.data[[predicted_probs_col]], na.rm = TRUE),
      observed_prob = mean(.data[[ground_truths_col]], na.rm = TRUE),
      .groups = "drop"
    )

  # Create the calibration plot with one curve per learner
  ggplot(calibration_data, aes(x = avg_predicted_prob, y = observed_prob, color = .data[[learner_col]])) +
    geom_point(size = 3) +
    geom_line(aes(group = .data[[learner_col]]), linetype = "solid") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    labs(
      x = "Average Predicted Probability",
      y = "Observed Proportion",
      color = "Learner",
      title = "Calibration Plot"
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_fixed() +
    theme_minimal()
}

#' Plot Predicted Probabilities for a Single Learner by Group
#'
#' This function generates a density plot to visualize the distributions of predicted probabilities
#' for a single learner, grouped by a specified variable (e.g., race, gender). The grouping variable
#' should already be recoded if descriptive labels are desired.
#'
#' @param predProbs A data frame containing predicted probabilities, learner identifiers,
#'   and grouping variables. It must include:
#'   - `.pred_yes`: The predicted probabilities for the positive class.
#'   - `learnerName`: The name of the learner (model) associated with each prediction.
#'   - Any additional column(s) used as the grouping variable.
#' @param learner A string specifying the name of the learner (model) to plot.
#' @param group_var A string specifying the name of the grouping variable (e.g., "race", "gender").
#'   This column must exist in the `predProbs` data frame.
#'
#' @return A ggplot object representing the density curves of predicted probabilities for the
#'   specified learner, grouped by the specified variable.
#'
#' @examples
#' # Assuming learnersResults$predProbs contains .pred_yes, learnerName, and recoded group variables:
#' learner <- "Model1"
#' group_var <- "race"
#' plot_pred_probs(learnersResults$predProbs, learner, group_var)
#'
#' @export
plot_pred_probs <- function(predProbs, learner, group_var) {
  # Check if the grouping variable exists in the data
  if (!group_var %in% colnames(predProbs)) {
    stop(paste("The grouping variable", group_var, "does not exist in the data frame."))
  }

  # Check if the learner exists in the data
  if (!learner %in% unique(predProbs$learnerName)) {
    stop(paste("The learner", learner, "does not exist in the data frame."))
  }

  # Filter data for the specified learner
  learner_data <- predProbs %>%
    dplyr::filter(learnerName == learner)

  # Plot density curves grouped by the specified variable
  ggplot(learner_data, aes(
    x = .pred_yes,
    color = .data[[group_var]],
    fill = .data[[group_var]],
    group = .data[[group_var]]  # Explicitly specify the group aesthetic
  )) +
    geom_density(alpha = 0.3) + # Semi-transparent fill for better visualization
    labs(
      title = paste("Predicted Probabilities for Learner:", learner),
      subtitle = paste("Grouped by", group_var),
      x = "Predicted Probability",
      y = "Density",
      color = group_var,
      fill = group_var
    ) +
    theme_minimal() +
    theme(legend.title = element_text(size = 10))
}


#' Calibration Plot for Binary Outcomes by Group for a Specific Learner
#'
#' This function generates a calibration plot for binary outcomes, drawing separate
#' calibration curves for each group specified in the `group_col` column, for a single specified learner.
#'
#' @param data A data frame containing predicted probabilities, ground truths,
#' group names, and learner names.
#' @param predicted_probs_col The name of the column with predicted probabilities (string).
#' @param ground_truths_col The name of the column with ground truth labels (string). Must be binary.
#' @param group_col The name of the column with group names (string).
#' @param learner_col The name of the column with learner names (string).
#' @param learnerName The name of the learner to filter the data for (string).
#' @param n_bins An integer specifying the number of bins to group predicted probabilities. Default is 10.
#'
#' @return A ggplot object representing the calibration plot.
#' @export
#'
#' @examples
#' # Example data
#' data <- data.frame(
#'   predicted_probs = c(0.1, 0.4, 0.35, 0.8, 0.9, 0.65, 0.2, 0.5, 0.7, 0.85),
#'   ground_truths = c(0, 0, 0, 1, 1, 1, 0, 0, 1, 1),
#'   learnerName = rep(c("Learner1", "Learner2"), each = 5),
#'   race = rep(c("Group1", "Group2"), each = 5)
#' )
#'
#' # Generate the calibration plot for a specific learner
#' calibration_plot_by_group(data, "predicted_probs", "ground_truths", "race", "learnerName", "Learner1")
#'
#' @import ggplot2
#' @importFrom dplyr group_by summarise filter mutate
calibration_plot_by_group <- function(data, predicted_probs_col, ground_truths_col, group_col, learner_col, learnerName, n_bins = 10) {
  # Check if inputs are valid
  if (!all(c(predicted_probs_col, ground_truths_col, group_col, learner_col) %in% colnames(data))) {
    stop("One or more specified columns are not present in the data")
  }

  # Filter the data for the specified learner
  matches <- data[[learner_col]] == learnerName
  data <- data[matches, ]

  # If ground truths are factors, convert to numeric based on levels
  if (is.factor(data[[ground_truths_col]])) {
    levels_vec <- levels(data[[ground_truths_col]])
    data[[ground_truths_col]] <- as.numeric(data[[ground_truths_col]] == levels_vec[2])
  }

  # Convert predicted probabilities to numeric if they aren't already
  data[[predicted_probs_col]] <- as.numeric(as.character(data[[predicted_probs_col]]))

  # Create bins
  data$bin <- cut(data[[predicted_probs_col]],
                  breaks = seq(0, 1, length.out = n_bins + 1),
                  include.lowest = TRUE)

  # Calculate the average predicted probability and observed proportion for each group and bin
  calibration_data <- data %>%
    group_by(.data[[group_col]], bin) %>%
    summarise(
      avg_predicted_prob = mean(.data[[predicted_probs_col]], na.rm = TRUE),
      observed_prob = mean(.data[[ground_truths_col]], na.rm = TRUE),
      .groups = "drop"
    )

  # Create the calibration plot with one curve per group
  ggplot(calibration_data, aes(x = avg_predicted_prob, y = observed_prob, color = .data[[group_col]])) +
    geom_point(size = 3) +
    geom_line(aes(group = .data[[group_col]]), linetype = "solid") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    labs(
      x = "Average Predicted Probability",
      y = "Observed Proportion",
      color = "Group",
      title = paste("Calibration Plot by Group for Learner:", learnerName)
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_fixed() +
    theme_minimal()
}
