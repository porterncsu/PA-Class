# List of packages to install
packages <- c(
  "abind", "crosstalk", "foreach", "highr", "lme4", "pillar", "readr", "snakecase", "viridis",
  "askpass", "curl", "Formula", "hms", "lubridate", "pkgconfig", "readxl", "SparseM", "viridisLite",
  "backports", "data.table", "fs", "htmltools", "magrittr", "plyr", "recipes", "sparsevctrs", "visdat",
  "base64enc", "DBI", "furrr", "htmlwidgets", "MatrixModels", "polynom", "reformulas", "SQUAREM", "vroom",
  "bit", "dbplyr", "future", "httr", "memoise", "prettyunits", "rematch", "stringi", "warp",
  "bit64", "Deriv", "future.apply", "ids", "microbenchmark", "processx", "rematch2", "stringr", "wesanderson",
  "blob", "diagram", "generics", "infer", "mime", "prodlim", "repr", "sys", "withr",
  "broom", "dials", "ggplot2", "ipred", "minqa", "progress", "reshape2", "systemfonts", "workflows",
  "bslib", "DiceDesign", "ggpubr", "isoband", "mnormt", "progressr", "rlang", "tailor", "workflowsets",
  "cachem", "digest", "ggrepel", "iterators", "modeldata", "promises", "rmarkdown", "tibble", "xfun",
  "callr", "discrim", "ggsci", "janitor", "modelenv", "ps", "rsample", "tidymodels", "xgboost",
  "car", "doBy", "ggsignif", "jquerylib", "modelr", "psych", "rstatix", "tidyr", "xml2",
  "carData", "doParallel", "glmnet", "jsonlite", "naivebayes", "purrr", "rstudioapi", "tidyselect", "yaml",
  "cellranger", "dplyr", "globals", "kernlab", "naniar", "quantreg", "rvest", "timechange", "yardstick",
  "cli", "DT", "glue", "knitr", "nloptr", "R6", "S7", "timeDate",
  "clipr", "dtplyr", "gower", "labeling", "norm", "ranger", "sass", "tinytex",
  "clock", "evaluate", "GPArotation", "later", "numDeriv", "rappdirs", "scales", "tune",
  "conflicted", "farver", "GPfit", "lava", "openssl", "rbibutils", "selectr", "tzdb",
  "corrplot", "fastDummies", "gridExtra", "lazyeval", "parallelly", "RColorBrewer", "sfd", "UpSetR",
  "cowplot", "fastmap", "gtable", "lhs", "parsnip", "Rcpp", "shape", "utf8",
  "cpp11", "fontawesome", "hardhat", "lifecycle", "patchwork", "RcppEigen", "skimr", "uuid",
  "crayon", "forcats", "haven", "listenv", "pbkrtest", "Rdpack", "slider", "vctrs"
)

# Function to install packages that aren't already installed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Install all packages
cat("Installing packages...\n")
for (pkg in packages) {
  cat("Checking:", pkg, "\n")
  install_if_missing(pkg)
}

cat("\nInstallation complete!\n")