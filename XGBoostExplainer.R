# Explaining XGBoost results ----
library(xgboost)
library(xgboostExplainer)

# Make matrices for training and test data
training_xgb <- xgb.DMatrix(training %>%
                              select(-dates, -tenyear) %>%
                              as.matrix(), label = training %>%
                              pull(tenyear))

test_xgb <- xgb.DMatrix(future %>%
                          select(-dates, -tenyear) %>%
                          as.matrix())

# Train XGBoost model using the same hyperparameters as caret
xgb_explain <- xgboost(data = xgb.DMatrix(training %>%
                                            select(-dates, -tenyear) %>%
                                            as.matrix(),
                                          label = training %>% pull(tenyear)), 
                       nround = 150,
                       max_depth = 1, 
                       eta = 0.3,
                       gamma = 0,
                       colsample_bytree = 0.8,
                       min_child_weight = 1,
                       subsample = 0.75)

# Make explainer object
explainer <-  buildExplainer(xgb_explain,
                             training_xgb,
                             base_score = 0.5, 
                             trees = NULL,
                             type = "regression"
)

# Plot waterfall chart
showWaterfall(xgb_explain, explainer, test_xgb, future %>%
                select(-dates, -tenyear) %>%
                as.matrix(),
				idx = 1)