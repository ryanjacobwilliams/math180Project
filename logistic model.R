# CSV import
data <- read.csv(
  "C:/Users/User/OneDrive/Desktop/UC Merced/Senior Year/Fall 2026/Math 180/project/NBA_Processed_Data.csv",
  stringsAsFactors = TRUE
)

set.seed(1)

# Create ID to identify observations
id <- data[, c("Player", "Season")]

# Remove identifying variables from predictor data
df <- data[, !names(data) %in% c("Player", "Season")]

# Ensure No is the reference category
df$AllStar <- factor(df$AllStar, levels = c("No", "Yes"))

# Split set (80/20)
train <- sample(seq_len(nrow(df)), floor(0.8 * nrow(df)))

# Create training and test sets
df.train <- df[train, ]
df.test <- df[-train, ]

# Verify proportions
prop.table(table(df.train$AllStar))
prop.table(table(df.test$AllStar))

# Logistic regression model
glm.linear <- glm(
  AllStar ~ .,
  data = df.train,
  family = binomial
)
  
summary(glm.linear)

# Generate predicted probabilities
df.train$prob <- predict(
  glm.linear,
  newdata = df.train,
  type = "response"
)

# Test set predicted probabilities
df.test$prob <- predict(
  glm.linear,
  newdata = df.test,
  type = "response"
)

# Convert probabilities to classifications
df.test$predicted <- factor(
  ifelse(df.test$prob >= 0.5, "Yes", "No"),
  levels = c("No", "Yes")
)

# Confusion matrix
conf.matrix <- table(
  Actual = df.test$AllStar,
  Predicted = df.test$predicted
)

conf.matrix

round(100 * conf.matrix / sum(conf.matrix), 2)

library(pROC)

roc.model <- roc(
  response = df.test$AllStar,
  predictor = df.test$prob,
  levels = c("No", "Yes"),
  direction = "<"
)

plot(
  roc.model,
  main = "ROC Curve - Logistic Regression",
  col = "blue",
  lwd = 2
)

auc(roc.model)

library(PRROC)

# Predicted probabilities
scores.pos <- df.test$prob[df.test$AllStar == "Yes"]
scores.neg <- df.test$prob[df.test$AllStar == "No"]

# Precision-Recall curve
pr.model <- pr.curve(
  scores.class0 = scores.pos,
  scores.class1 = scores.neg,
  curve = TRUE
)

# Plot
plot(
  pr.model,
  main = "Sensitivity vs. Precision",
  xlab = "Sensitivity (Recall)",
  ylab = "Precision",
  color = "blue"
)

# Area under the curve
pr.model$auc.integral

# Parameter for K-fold validation
K <- 10

# Create stratified folds
folds <- integer(nrow(df.train))

for (class in levels(df.train$AllStar)) {
  idx <- which(df.train$AllStar == class)
  folds[idx] <- sample(rep(1:K, length.out = length(idx)))
}

cv.error <- numeric(K)

for (k in 1:K) {
  
  # Divide into training and validation folds
  train.fold <- df.train[folds != k, ]
  valid.fold <- df.train[folds == k, ]
  
  # Fit logistic regression
  model <- glm(
    AllStar ~ . - prob,
    data = train.fold,
    family = binomial
  )
  
  # Predict validation probabilities
  p <- predict(
    model,
    newdata = valid.fold,
    type = "response"
  )
  
  # Actual outcomes (1 = Yes, 0 = No)
  y <- as.integer(valid.fold$AllStar == "Yes")
  
  # Avoid log(0)
  p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  
  # Calculate log-loss
  cv.error[k] <- -mean(
    y * log(p) + (1 - y) * log(1 - p)
  )
}
