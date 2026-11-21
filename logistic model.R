
# ============================================================
# NBA All-Star Prediction: Logistic Regression
# ============================================================

library(pROC)
library(PRROC)

# 1. Import and prepare data

set.seed(1)

data <- read.csv(
  "NBA_Processed_Data.csv",
  stringsAsFactors = TRUE
)

# Separate identifiers from model predictors
player_id <- data[, c("Player", "Season")]
model_data <- data[, !names(data) %in% c("Player", "Season")]

# Set response reference category
model_data$AllStar <- factor(
  model_data$AllStar,
  levels = c("No", "Yes")
)

# 2. Training and test split (80/20)

train_idx <- sample(
  seq_len(nrow(model_data)),
  floor(0.8 * nrow(model_data))
)

train_data <- model_data[train_idx, ]
test_data <- model_data[-train_idx, ]

# Check response proportions
prop.table(table(train_data$AllStar))
prop.table(table(test_data$AllStar))

# 3. Fit logistic regression

logistic_model <- glm(
  AllStar ~ .,
  data = train_data,
  family = binomial
)

summary(logistic_model)

# 4. Test-set predictions and confusion matrix

test_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

test_pred <- factor(
  ifelse(test_prob >= 0.5, "Yes", "No"),
  levels = c("No", "Yes")
)

conf_matrix <- table(
  Actual = test_data$AllStar,
  Predicted = test_pred
)

# Counts and percentages of entire test set
print(conf_matrix)
print(round(100 * prop.table(conf_matrix), 2))

# Classification metrics
TP <- conf_matrix["Yes", "Yes"]
TN <- conf_matrix["No", "No"]
FP <- conf_matrix["No", "Yes"]
FN <- conf_matrix["Yes", "No"]

accuracy <- (TP + TN) / sum(conf_matrix)
precision <- safe_divide(TP, TP + FP)
sensitivity <- safe_divide(TP, TP + FN)
specificity <- safe_divide(TN, TN + FP)

classification_metrics <- c(
  Accuracy = accuracy,
  Precision = precision,
  Sensitivity = sensitivity,
  Specificity = specificity
)

print(classification_metrics)

# 5. ROC curve and ROC-AUC

roc_model <- roc(
  response = test_data$AllStar,
  predictor = test_prob,
  levels = c("No", "Yes"),
  direction = "<",
  quiet = TRUE
)

plot(
  roc_model,
  main = "ROC Curve: Logistic Regression",
  col = "blue",
  lwd = 2
)

# 6. Precision-Recall curve and PR-AUC

positive_scores <- test_prob[test_data$AllStar == "Yes"]
negative_scores <- test_prob[test_data$AllStar == "No"]

pr_model <- pr.curve(
  scores.class0 = positive_scores,
  scores.class1 = negative_scores,
  curve = TRUE
)

plot(
  pr_model,
  main = "Sensitivity vs. Precision",
  xlab = "Sensitivity (Recall)",
  ylab = "Precision",
  color = "blue"
)

# 7. Stratified 10-fold cross-validation

K <- 10
folds <- integer(nrow(train_data))

# Assign folds within each response category
for (class in levels(train_data$AllStar)) {
  idx <- which(train_data$AllStar == class)
  folds[idx] <- sample(rep(1:K, length.out = length(idx)))
}

# Log-loss function
log_loss <- function(actual, predicted) {
  y <- as.integer(actual == "Yes")
  p <- pmin(pmax(predicted, 1e-15), 1 - 1e-15)
  
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

cv_log_loss <- numeric(K)
baseline_log_loss <- numeric(K)

for (k in 1:K) {
  
  # Split training data into folds
  fold_train <- train_data[folds != k, ]
  fold_valid <- train_data[folds == k, ]
  
  # Fit logistic regression
  fold_model <- glm(
    AllStar ~ .,
    data = fold_train,
    family = binomial
  )
  
  # Predict on validation fold
  fold_prob <- predict(
    fold_model,
    newdata = fold_valid,
    type = "response"
  )
  
  # Calculate validation log-loss
  cv_log_loss[k] <- log_loss(
    fold_valid$AllStar,
    fold_prob
  )
  
  # Baseline: predict training-fold All-Star prevalence
  baseline_prob <- mean(fold_train$AllStar == "Yes")
  
  baseline_log_loss[k] <- log_loss(
    fold_valid$AllStar,
    rep(baseline_prob, nrow(fold_valid))
  )
}

# Cross-validation summary
cv_results <- data.frame(
  Fold = 1:K,
  Logistic_LogLoss = cv_log_loss,
  Baseline_LogLoss = baseline_log_loss
)

print(cv_results)

cat("Mean CV Log-Loss:", mean(cv_log_loss), "\n")
cat("SD CV Log-Loss:", sd(cv_log_loss), "\n")
cat("Baseline CV Log-Loss:", mean(baseline_log_loss), "\n")

# 8. Cross-validation plot

plot(
  1:K,
  cv_log_loss,
  type = "b",
  pch = 19,
  xlab = "Fold",
  ylab = "Validation Log-Loss",
  main = "10-Fold Cross-Validation"
)

abline(
  h = mean(cv_log_loss),
  col = "red",
  lty = 2
)

## 9. Subset Selection

# Backward stepwise selection
glm.backward <- step(
  logistic_model,
  direction = "backward",
  trace = 0
)

# Selected variables
formula(glm.backward)

# Model summary
summary(glm.backward)

# Compare AIC
AIC(logistic_model, glm.backward)
