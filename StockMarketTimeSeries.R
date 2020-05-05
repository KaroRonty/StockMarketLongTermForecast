library(tidyverse)
library(scales)
library(fable)
library(tsibble)
library(feasts)
library(fable.prophet)
library(ggforce)
library(imputeTS)
library(kableExtra)

# Load data from different sources and combine ----------------------------

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
  mutate(PE = P / E,
         PB = 1 / as.numeric(bm),
         PD = P / D,
         TR_CAPE = as.numeric(`TR CAPE`),
         Rate_GS10 = `Rate GS10`,
         # TODO remove to compute accuracies correctly
         tenyear = tenyear - 1)

# Keep only date column and the columns used in the model
data <- combined_data %>% 
  na.omit() %>% 
  select(dates, CAPE, infl, UnEmp, PE, PB, PD, TR_CAPE, Rate_GS10, tenyear)

# Make a tsibble for modeling
to_model <- data %>% 
  mutate(dates = yearmonth(paste0(dates, "-01"))) %>% 
  as_tibble() %>% 
  as_tsibble()

# Split into training and test sets ---------------------------------------

# Set 65/35 split
train_test_split <- 0.65

training <- to_model %>% 
  slice(1:I(nrow(to_model) * train_test_split))

test <- to_model %>% 
  slice(I(nrow(to_model) * train_test_split + 1):I(nrow(to_model) + 1))

# Get the split date for the bias-free test set
split_date <- min(test$dates) + 120


# Train models ------------------------------------------------------------

# Different formulas for arima and rest of the models
f <- as.formula("tenyear ~
                CAPE + infl + UnEmp + PE + PB + PD + TR_CAPE + Rate_GS10")

f_mc <- as.formula("tenyear ~ CAPE + infl + UnEmp + PB + PD + Rate_GS10")

# Train six models plus make a combination
models <- training %>% 
  model(ARIMA = ARIMA(f, stepwise = FALSE),
        nnetar = NNETAR(f_mc),
        VAR = VAR(f_mc),
        Prophet = prophet(f_mc),
        ETS = ETS(tenyear),
        TSLM = TSLM(tenyear ~
                      CAPE + infl + UnEmp + PB + PD + Rate_GS10 +
                      trend() + season())) %>% 
  mutate(Combination = (ARIMA + nnetar + VAR + Prophet + TSLM) / 5)


# Plot the models ---------------------------------------------------------

# Make forecasts
fcasts <- models %>% forecast(test)

# Calculate accuracies for the test set
# rbind training set to get MASE calculations
accuracies <- fcasts %>% accuracy(rbind(training,
                                        test %>% filter(dates >= split_date)))

# Calculate R-squareds, NA if near-zero variance
accuracies <- accuracies %>% 
  inner_join(fcasts %>% 
               as_tibble() %>% 
               group_by(.model) %>% 
               summarise(rsq = ifelse(sd(tenyear) > 0.01,
                                      cor(tenyear, test$tenyear)^2,
                                      NA)))

# Plot six models with actuals and forecasts
training %>% 
  autoplot(tenyear) +
  autolayer(fcasts %>% filter(.model != "Combination")) +
  autolayer(test, tenyear) +
  autolayer(fitted(models %>% select(-Combination)), .fitted) +
  facet_wrap(~ .model, ncol = 2) +
  geom_vline(xintercept = min(test$dates),
             color = "darkgray", alpha = 0.5, size = 1, linetype = "dashed") +
  geom_vline(xintercept = split_date,
             color = "red", alpha = 0.5, size = 1, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent, breaks = c((-1:5) / 10)) +
  scale_x_date(breaks = seq.Date(as.Date("1950-01-01"),
                                 as.Date("2010-01-01"), "10 years"),
               labels = seq(1950, 2010, 10)) +
  ggtitle("Forecasting future 10-year returns for the S&P 500",
          subtitle = 
            paste("Actuals vs forecasts for different time series models",
                  " \nLook-ahead bias-free forecasts after",
                  "the vertical red line")) +
  xlab("Date") +
  ylab("Yearly average return (CAGR) for the next 10 years") +
  labs(caption =
         "Source: Shiller, Goyal, U.S. Bureau of Labor Statistics \n
Blog post at: databasedinvesting.blogspot.com") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, lineheight = 0.5))


# Future forecasts --------------------------------------------------------

# Keep only the combination model forecasts for plotting
combination_fcasts <- fcasts %>%
  as_tibble() %>% 
  filter(.model == "Combination")

# Combine actuals, fitted values and forecasts
combination_to_plot <- models %>%
  select(Combination) %>% 
  augment() %>% 
  rbind(cbind(combination_fcasts %>% rename(.fitted = tenyear),
              test %>% select(tenyear))) %>% 
  select(dates, tenyear, .fitted) %>% 
  as_tibble()

# Keep only certain columns and format dates
future_data <- combined_data %>% 
  select(dates, CAPE, infl, UnEmp, PE, PB, PD, TR_CAPE, Rate_GS10, tenyear) %>% 
  mutate(dates = as.Date(paste0(dates, "-01")))

# Make future data
future_data <- future_data %>% 
  filter(dates >= "1948-01-01",
         dates <= "2020-04-01") %>% 
  add_row(dates = as.Date("2020-05-01"),
          CAPE = 26.5, # multpl
          infl = NA, # FIXME Estimated linearly instead
          UnEmp = 16.1, # Expert estimate
          PE = 20.3, # multpl
          PB = 3.5 / 3.1 * 4.54, # Estimated from multpl data
          PD = 1 / 0.021, # multpl
          TR_CAPE = 28.25 / 25.88 * 26.5, # Estimated from multpl data
          Rate_GS10 = 0.66) # FRED GS10

# Impute missing values
future_data <- future_data %>% 
  # Don't perform imputation on dates or the future ten year returns
  select(-dates, -tenyear) %>%
  na_interpolation("spline") %>% 
  cbind(dates = future_data$dates, .) %>% 
  cbind(tenyear = future_data$tenyear)

# Keep only future values
future_data <- future_data %>%
  filter(is.na(tenyear)) %>% 
  mutate(dates = yearmonth(dates)) %>%
  as_tsibble()

# Train full models
models_full <- to_model %>% 
  model(ARIMA = ARIMA(f, stepwise = FALSE),
        nnetar = NNETAR(f_mc),
        VAR = VAR(f_mc),
        Prophet = prophet(f_mc),
        ETS = ETS(tenyear),
        TSLM = TSLM(tenyear ~
                      CAPE + infl + UnEmp + PB + PD + Rate_GS10 +
                      trend() + season())) %>% 
  mutate(Combination = (ARIMA + nnetar + VAR + Prophet + TSLM) / 5)

# Forecast using future data
fcasts_future <- models_full %>% forecast(future_data)

# Combine actuals, fitted values and forecasts
combination_to_plot <- combination_to_plot %>% 
  rbind(fcasts_future %>% 
          as_tibble() %>% 
          filter(.model == "Combination") %>% 
          select(dates, .fitted = tenyear) %>% 
          mutate(tenyear = NA))

# Plot combination model with actuals, fitted and (future) forecasts
ggplot(combination_to_plot, aes(x = dates, y = tenyear)) +
  geom_line() +
  geom_line(aes(y = .fitted), color = "#00BFC4") +
  geom_vline(xintercept = min(test$dates),
             color = "gray", alpha = 0.5, size = 1, linetype = "dashed") +
  geom_vline(xintercept = split_date,
             color = "red", alpha = 0.5, size = 1, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent, breaks = c((-1:4) / 20)) +
  scale_x_date(breaks = seq.Date(as.Date("1950-01-01"),
                                 as.Date("2020-01-01"), "5 years"),
               labels = seq(1950, 2020, 5)) +
  facet_zoom(xlim = as.numeric(c(split_date, as.Date("2020-05-01")))) +
  ggtitle("Forecasting future 10-year returns for the S&P 500",
          subtitle =
            paste("Look-ahead bias-free forecasts after the vertical red line",
                  "\nA linear combination of ARIMA, nnetar, Prophet, TSLM and",
                  "Vector Autoregression models")) +
  xlab("Date") +
  ylab("Yearly average return (CAGR) for the next 10 years") +
  labs(caption =
         "Source: Shiller, Goyal, U.S. Bureau of Labor Statistics \n
Blog post at: databasedinvesting.blogspot.com") +
  theme_light() +
  theme(panel.border = element_blank(),
        plot.caption = element_text(hjust = 0, lineheight = 0.5))

# Function for formatting percentages in table
percent <- function(x){
  paste0(format(x * 100, nsmall = 2, digits = 2), "%") 
}


# Make tables of accuracies and forecasts ---------------------------------

# Accuracy metrics
accuracies %>% 
  as_tibble() %>% 
  select(-.type, -ACF1) %>% 
  arrange(MAPE) %>% 
  mutate_at(vars(RMSE, MAE, MAPE, MASE, rsq),
            function(x) formatC(x, digits = 3)) %>% 
  rename(Model = .model, `R-squared` = rsq) %>% 
  kable() %>% 
  kable_styling()

# Forecasts for the next ten years
fcasts_future %>% 
  as_tibble() %>% 
  group_by(.model) %>% 
  filter(dates == "2020-05-01") %>% 
  arrange(tenyear) %>% 
  mutate(tenyear = percent(tenyear)) %>% 
  select(Model = .model, `10-year CAGR forecast` = tenyear) %>% 
  kable() %>% 
  kable_styling()