library(tidyverse)
library(corrplot)
library(GGally)
library(caret)
library(doParallel)
library(parallel)
library(patchwork)
library(scales)

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
xgboost <- train(training %>% select(-dates, -tenyear) %>% as.matrix(),
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

# Make a tibble for storing training set results
models <- tibble(name = c("glmnet", "xgboost", "knn", "mars", "svm"),
                 model = NA,
                 actual = NA,
                 pred = NA,
                 rsq_cv = NA,
                 mae_cv = NA,
                 dates_train = NA)

# Loop the models, predictions and accuracy measures into the tibble
for(i in 1:nrow(models)){
  models$model[i] <- get(models$name[i]) %>% list()
  models$actual[i] <- training %>% pull(tenyear) %>% list()
  models$pred[i] <- predict(get(models$name[i]),
                            training %>%
                              select(-dates, -tenyear) %>%
                              as.matrix()) %>%
    as.vector() %>% 
    list()
  models$rsq_cv[i] <- get(models$name[i])$resample$Rsquared %>% mean(na.rm = TRUE)
  models$mae_cv[i] <- get(models$name[i])$resample$MAE %>% mean(na.rm = TRUE)
  models$dates_train[i] <- training %>% pull(dates) %>% list()
}

# Make a tibble for storing test set results
models_test <- tibble(name = models$name,
                      actual_test = NA,
                      pred_test = NA,
                      rsq_test = NA,
                      mae_test = NA,
                      pred_future = NA,
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

# Put training and test set data into tibble for plotting
to_plot <- models %>% 
  select(name, actual, pred, dates_train) %>% 
  unnest() %>% 
  mutate(dates = as.Date(paste0(dates_train, "-01")),
         source = "train") %>% 
  select(-dates_train)

to_plot <- models_test %>% 
  select(name, actual = actual_test, pred = pred_test, dates_test) %>% 
  unnest() %>% 
  mutate(dates = as.Date(paste0(dates_test, "-01")),
         source = "test") %>%
  select(-dates_test) %>% 
  rbind(to_plot, .)

# Add ensemble model predictions
to_plot <- to_plot %>% 
  pivot_wider(names_from = name,
              values_from = pred) %>% 
  mutate(name = "ensemble",
         pred = (glmnet + knn + mars + svm + xgboost) / 5) %>% 
  select(name, actual, pred, dates, source) %>% 
  rbind(to_plot)

# Find date where test set begins to be used in plotting
split_date <- to_plot$dates[first(which(to_plot$source == "test"))]

# Plot actuals and predictions for each model
to_plot %>% 
  mutate(actual = actual - 1,
         pred = pred - 1) %>% 
  ggplot(aes(x = dates)) +
  geom_line(aes(y = actual)) +
  geom_line(aes(y = pred), color = "#00BFC4") +
  geom_vline(xintercept = split_date,
             color = "red", alpha = 0.5, size = 1, linetype = "dashed") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("1950-01-01"),
                                             to = max(x), by = "10 years"),
               date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(rows = vars(name)) +
  ggtitle("Predicting future 10-year returns for the S&P 500",
          subtitle = "Actuals vs predictions (blue) for different models") +
  xlab("Date") +
  ylab("Yearly average return (CAGR) for the next 10 years") +
  labs(caption =
         "Source: Shiller, Goyal, U.S. Bureau of Labor Statistics \n
Blog post at: databasedinvesting.blogspot.com") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, lineheight = 0.5))

# Find out emsemble model accuracy on test set
ensemble_accuracy <- to_plot %>% 
  filter(name == "ensemble",
         source == "test") %>% 
  summarise(rsq_test = cor(actual, pred)^2,
            mae_test = mean(abs(pred - actual)))

# Find out MAE of naive model
naive_mae <- to_plot %>% 
  filter(name == "ensemble") %>% # Does not matter which model
  pivot_wider(names_from = source,
              values_from = c(actual, pred)) %>% 
  # Compare past historical (training set) return to test set return
  summarise(mae_test = mean(abs(mean(actual_train, na.rm = TRUE) -
                                  actual_test), na.rm = TRUE))

# Make future data using current values
future <- full_data %>%
  full_join(bls_data, by = "dates") %>%
  mutate("PE" = P / E,
         "PB" = 1 / as.numeric(bm),
         "PD" = P / D,
         "TR_CAPE" = as.numeric(`TR CAPE`),
         "Rate_GS10" = `Rate GS10`) %>% 
  tail(1) %>% 
  select(-P:-Fraction,
         -Price:-Earnings,
         -diff,
         -bm,
         -index:-tenyear_real,
         -div_percent,
         tenyear,
         -`TR CAPE`,
         -`Rate GS10`) %>% 
  mutate(infl = 0.021,
         UnEmp = 3.5,
         PE = 24.27,
         PB = 3.66,
         PD = 1 / 0.0177)

# Loop future predictions to models_test tibble
for(i in 1:nrow(models_test)){
  models_test$pred_future[i] <- predict(get(models_test$name[i]),
                                        future %>%
                                          select(-dates, -tenyear) %>%
                                          as.matrix())
}

# Calculate ensemble prediction
ensemble_pred <- models_test$pred_future %>% mean()

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

# Print all details
print(models)
print(models_test)
print(naive_mae)
print(ensemble_accuracy)
print(ensemble_pred)