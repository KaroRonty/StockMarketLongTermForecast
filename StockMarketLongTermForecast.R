library(tidyverse)
library(corrplot)
library(GGally)
library(caret)
library(doParallel)
library(parallel)
library(patchwork)

# Register parallelization using all cores
registerDoParallel(cores = detectCores())

# Load data from different sources and combine ----

# Get the needed data from external script
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Load unemployment data from BLS, format dates
bls_data <- read_xlsx("bls_data.xlsx",
                      sheet = 1,
                      skip = 10) %>%
  gather(month, UnEmp, Jan:Dec) %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(Year, month) %>%
  mutate(months = sprintf("%02d", match(month, month.abb)),
         dates = paste(Year, months, sep = "-")) %>%
  select(dates, UnEmp)

# Join the data with data from Shiller & Goyal by year and month
# Calculate more needed variables, keep only months where all data is available
combined_data <- full_data %>%
  full_join(bls_data, by = "dates") %>%
  mutate("PE" = P / E,
         "PB" = 1 / as.numeric(bm),
         "PD" = P / D,
         "TR_CAPE" = as.numeric(`TR CAPE`),
         "Rate_GS10" = `Rate GS10`) %>%
  na.omit()

# Keep only date column and the columns used in the model
data <- combined_data %>% select(-P:-Fraction,
                                 -Price:-Earnings,
                                 -diff,
                                 -bm,
                                 -index:-tenyear_real,
                                 -div_percent,
                                 tenyear,
                                 -`TR CAPE`,
                                 -`Rate GS10`) %>%
  na.omit() 

# Split into training and test sets ----
train_test_split <- 0.7

training <- data %>% 
  slice(1:I(nrow(data) * train_test_split))

test <- data %>% 
  slice(I(nrow(data) * train_test_split + 1):I(nrow(data) + 1))

# Exploratory data analysis ----

# Examine correlations excluding the date column
corrplot(cor(training[, -1]), method = "square", order = "hclust")

# Examine pair plots excluding the date column
ggpairs(training[, -1])

# Modelling ----

# Make cross validation object for caret
cv <- trainControl(method = "timeslice",
                   initialWindow = 65,
                   horizon = 29,
                   skip = 65 + 29 - 1,
                   fixedWindow = TRUE,
                   savePredictions = "all", # FIXME
                   allowParallel = TRUE)

# Train baseline model using glmnet
glmnet <- train(training %>% select(-dates, -tenyear) %>% as.matrix(),
                training %>% pull(tenyear),
                method = "glmnet",
                trControl = cv)

# Train rest of the models
xgb <- train(training %>% select(-dates, -tenyear) %>% as.matrix(),
             training %>% pull(tenyear),
             method = "xgbTree",
             trControl = cv)

knn <- train(training %>% select(-dates, -tenyear) %>% as.matrix(),
             training %>% pull(tenyear),
             method = "knn",
             trControl = cv)

mars <- train(training %>% select(-dates, -tenyear) %>% as.matrix(),
              training %>% pull(tenyear),
              method = "earth",
              trControl = cv)

svm <- train(training %>% select(-dates, -tenyear) %>% as.matrix(),
             training %>% pull(tenyear),
             method = "svmLinear",
             trControl = cv)

# Evaluation ----

# Make a tibble for storing the results
models <- tibble(name = c("glmnet", "xgb", "knn", "mars", "svm"),
                 model = NA,
                 rmse = NA,
                 rsq_cv = NA,
                 mae = NA,
                 actual = NA,
                 pred = NA,
                 dates_train = NA)

# Loop the models, predictions and accuracy measures into the tibble
for(i in 1:nrow(models)){
  models$model[i] <- get(models$name[i]) %>% list()
  models$rmse[i] <- get(models$name[i])$resample$RMSE %>% mean(na.rm = TRUE)
  models$rsq_cv[i] <- get(models$name[i])$resample$Rsquared %>% mean(na.rm = TRUE)
  models$mae[i] <- get(models$name[i])$resample$MAE %>% mean(na.rm = TRUE)
  models$actual[i] <- training %>% pull(tenyear) %>% list()
  models$pred[i] <- predict(get(models$name[i]),
                            training %>%
                              select(-dates, -tenyear) %>%
                              as.matrix()) %>%
    as.vector() %>% 
    list()
  models$dates_train[i] <- training %>% pull(dates) %>% list()
}

models_test <- tibble(name = models$name,
                      actual_test = NA,
                      pred_test = NA,
                      rsq_test = NA,
                      mae_test = NA,
                      dates_test = NA)

# Loop the models, predictions and accuracy measures into the tibble
for(i in 1:nrow(models_test)){
  models_test$actual_test[i] <- test %>% pull(tenyear) %>% list()
  models_test$pred_test[i] <- predict(get(models_test$name[i]),
                                      test %>%
                                        select(-dates, -tenyear) %>%
                                        as.matrix()) %>%
    as.vector() %>% 
    list()
  models_test$rsq_test[i] <- cor(models_test$actual_test[[i]],
                                 models_test$pred_test[[i]])^2
  models_test$mae_test[i] <- mean(abs(models_test$pred_test[[i]] -
                                        models_test$actual_test[[i]]))
  models_test$dates_test[i] <- test %>% pull(dates) %>% list()
}

# Make feature importance plots
for(i in 1:nrow(models)){
  # Calculate feature importances for the model
  feature_importance <- varImp(get(models$name[i]))$importance
  
  # Convert type while keeping names and arrange
  feature_importance <- feature_importance %>% 
    mutate(Variable = row.names(.),
           Importance = as.numeric(Overall)) %>%
    select(-Overall) %>%
    mutate(Importance = (Importance - min(Importance)) / 
             max(Importance) - min(Importance)) 
  
  # Produce plot
  p <- feature_importance %>% 
    ggplot(aes(x = Variable,
               y = Importance)) +
    geom_col() +
    coord_flip() +
    ggtitle(models$name[i]) +
    theme_light()
  
  assign(paste0("p", i), p)
}

# Plot feature importances using patchwork
p1 + p2 + p3 + p4 + p5