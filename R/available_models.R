Mods <- data.frame(
  "modelingApproach" = c(
    "Logistic Regression",
    "Least Absolute Shrinkage and Selection Operator",
    "Random Forest",
    "XgBoost",
    "Naive Bayes",
    "SVM Poly",
    "SVM RBF"
  ),
  "modelingApproachName" = c(
    "glm",
    "lasso",
    "random_forest",
    "xgboost",
    "naive_bayes",
    "svm_poly",
    "svm_rbf"
  )
)
saveRDS(Mods, here::here("data-r-objects/inputs", "Mods.rds"))
