# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(fpp2)
library(tseries)

###################################################################################
################################ PART 1 ###########################################
###################################################################################

# Specify the file path
setwd("C:/Code_Hanh/Forecasting/")

# Read the CSV file into a data frame
trip <- read.csv("EU_trip_expenditures.csv")

# Group by 'year' and 'month' and calculate the average of 'avg_exp_ngt' for each group
trip2 <- trip %>%
  group_by(year, month) %>%
  summarise(avg_exp_ngt = mean(avg_exp_ngt, na.rm = TRUE))

# Combine year and month into a Date format
trip2$Date <- as.Date(paste(trip2$year, trip2$month, "01", sep = "-"))

# Create a time series object
ts_trip <- ts(trip2$avg_exp_ngt, frequency = 12, start = c(min(trip2$year), 1))


###################################################################################
############################## Question 1 #########################################
###################################################################################

# Time series plot
ggplot() +
  geom_line(aes(x = time(ts_trip), y = ts_trip)) +
  ggtitle("Travelling average expenditure per night 2012-2022") +
  xlab("Year") +
  ylab("Average expenditure") +
  theme_bw()

# Seasonal plot
seasonplot(ts_trip, col = rainbow(20), 
           year.labels = TRUE, main = "Seasonal plot: Travelling average expenditure per night 2012-2022", ylab = "Expenditure")

# Seasonal subseries plot
ggsubseriesplot(ts_trip) +
  ylab("Average expenditure") +
  ggtitle("Monthly travelling average expenditure per night 2012-2022") +
  theme_bw()

# (P)ACF plot.
tsdisplay(ts_trip, main = "EU travelling average expenditure per night 2012-2022")

Acf(ts_trip, main = "Autocorrelation Function of EU travelling average expenditure per night 2012-2022")

###################################################################################
############################## Question 2 #########################################
###################################################################################

# Split the data into training and test sets
train <- window(ts_trip, end=c(2017, 3))
test <- window(ts_trip, start=c(2017, 4), end=c(2020, 3))

# Check the sansionary of dataset
adf_result <- adf.test(train)

### This data is santionary => we do not need to transform it

# BoxCox lambda
lambda <- BoxCox.lambda(train)

# Power transformation
autoplot(BoxCox(train,lambda))
autoplot(train)

# Reciprocal transformation
autoplot(1 / train)


###################################################################################
############################## Question 3 #########################################
###################################################################################

# Fit seasonal naive model to the training data
naive_model <- snaive(train,h=length(test))

# Generate forecasts
naive_forecast <- forecast(naive_model)

# Extract residuals
naive_residuals <- residuals(naive_model) #only do on train

# Visualize residuals
checkresiduals(naive_model) #only do on train

# Ploting the result from model
plot(naive_forecast, main = "Seasonal Naive Forecast", xlab = "Time", ylab = "Value")
lines(test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("black", "red"), lty = 1)

# Generate forecasts for the test set
accuracy_results <- accuracy(naive_model, test)[, c(2, 3, 5, 6)]

###################################################################################
############################## Question 4 #########################################
###################################################################################

# Fit data using the STL decomposition
model1 <- stlf(train, method="rwdrift", h=length(test))
forecast_ml1 <- forecast(model1, h = length(test))

model2 <- stlf(train, method="naive", h=length(test))
forecast_ml2 <- forecast(model2, h = length(test))

model3 <- stlf(train, method="ets", h=length(test))
forecast_ml3 <- forecast(model3, h = length(test))

model4 <- stlf(train, method="arima", h=length(test))
forecast_ml4 <- forecast(model4, h = length(test))

# Ploting the result from model
plot(forecast_ml1, main = "STL decomposition rwdrift", xlab = "Time", ylab = "Value")
lines(test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("black", "red"), lty = 1)

plot(forecast_ml2, main = "STL decomposition naive", xlab = "Time", ylab = "Value")
lines(test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("black", "red"), lty = 1)

plot(forecast_ml3, main = "STL decomposition ets", xlab = "Time", ylab = "Value")
lines(test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("black", "red"), lty = 1)

plot(forecast_ml4, main = "STL decomposition arima", xlab = "Time", ylab = "Value")
lines(test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("black", "red"), lty = 1)


# Calculate the accuracy on original data
accuracy_rwdrift2 <- accuracy(model1, test)[, c(2, 3, 5, 6)]
accuracy_naive2 <- accuracy(model2, test)[, c(2, 3, 5, 6)]
accuracy_ets2 <- accuracy(model3, test)[, c(2, 3, 5, 6)]
accuracy_arima2 <- accuracy(model4, test)[, c(2, 3, 5, 6)]

# Prepare the residual diagnostic for all models
res_matrix <- matrix(nrow = 4, ncol = 4)
colnames(res_matrix) <- c("Model", "Statistic", "Parameter", "P.Value")

# Loop through each model
for (i in 1:4) {
  # Residual diagnostics
  res_name <- checkresiduals(get(paste0("model", i)), plot = FALSE)
  
  # Store results in matrix
  res_matrix[i, ] <- c(i, res_name$statistic, res_name$parameter, res_name$p.value)
}


################### Seasonal Adjusted #####################

# Ajust the seasonal component on train and test set
train_adj <- seasadj(mstl(train))
test_adj <- seasadj(mstl(test))

# Fit data using the STL decomposition for seasonal adjusted data
model1_adj <- stlf(train_adj, method="rwdrift", h=length(test))
forecast_ml1_adj <- forecast(model1_adj, h = length(test))

model2_adj <- stlf(train_adj, method="naive", h=length(test))
forecast_ml2_adj <- forecast(model2_adj, h = length(test))

model3_adj <- stlf(train_adj, method="ets", h=length(test))
forecast_ml3_adj <- forecast(model3_adj, h = length(test))

model4_adj <- stlf(train_adj, method="arima", h=length(test))
forecast_ml4_adj <- forecast(model4_adj, h = length(test))

# Ploting the result from model on seasonal adjusted data
par(mfrow=c(2,2))
plot(model1_adj); plot(model2_adj); plot(model3_adj); plot(model4_adj)


# Calculate the accuracy on seasonal adjusted data
accuracy_rwdrift <- accuracy(model1_adj, test_adj)[, c(2, 3, 5, 6)]
accuracy_naive <- accuracy(model2_adj, test_adj)[, c(2, 3, 5, 6)]
accuracy_ets <- accuracy(model3_adj, test_adj)[, c(2, 3, 5, 6)]
accuracy_arima <- accuracy(model4_adj, test_adj)[, c(2, 3, 5, 6)]

# Prepare the residual diagnostic for all model
res_matrix <- matrix(nrow = 4, ncol = 4)
colnames(res_matrix) <- c("Model", "Statistic", "Parameter", "P.Value")

# Loop through each model
for (i in 1:4) {
  # Fit model
  assign(paste0("model", i, "_adj"), stlf(train_adj, method = c("rwdrift", "naive", "ets", "arima")[i], h = length(test)))
  
  # Forecast
  forecast_name <- paste0("forecast_ml", i, "_adj")
  model_name <- paste0("model", i, "_adj")
  assign(forecast_name, forecast(get(model_name), h = length(test)))
  
  # Residual diagnostics
  res_name <- paste0("res_m", i)
  res_name <- checkresiduals(get(model_name), plot = FALSE)
  
  # Store results in matrix
  res_matrix[i, ] <- c(i, res_name$statistic, res_name$parameter, res_name$p.value)
}

# Prepare the residual diagnostic for arima and ets
par(mfrow=c(1,2))
checkresiduals(model3_adj)
checkresiduals(model4_adj)


###################################################################################
############################## Question 5 #########################################
###################################################################################

# Fit the ETS 
ets <- ets(train)
ets_ANN <- ets(train, model="ANN")
ets_AAN <- ets(train, model="AAN")
ets_AAA <- ets(train, model="AAA")
ets_MAM <- ets(train, model="MAM")
ets_MAA <- ets(train, model="MAA")
ets_AAdN <- ets(train, model="AAN",damped=TRUE)

# Generate forecasts for each model
forecast_ets <- forecast(ets, h = length(test))
forecast_ANN <- forecast(ets_ANN, h = length(test))
forecast_AAN <- forecast(ets_AAN, h = length(test))
forecast_AAA <- forecast(ets_AAA, h = length(test))
forecast_MAM <- forecast(ets_MAM, h = length(test))
forecast_MAA <- forecast(ets_MAA, h = length(test))
forecast_AAdN <- forecast(ets_AAdN, h = length(test))

# Ploting the forecast
####
par(mfrow=c(3,2))
plot(forecast_ANN)
plot(forecast_AAN)
plot(forecast_AAA)
plot(forecast_MAM)
plot(forecast_MAA)
plot(forecast_AAdN)

par(mfrow=c(1,1))
plot(forecast_ets)

# Evaluate forecast accuracy
accuracy1 <- accuracy(forecast_ets, test)[, c(2, 3, 5, 6)]
accuracy2 <- accuracy(forecast_ANN, test)[, c(2, 3, 5, 6)]
accuracy3 <- accuracy(forecast_AAN, test)[, c(2, 3, 5, 6)]
accuracy4 <- accuracy(forecast_AAA, test)[, c(2, 3, 5, 6)]
accuracy5 <- accuracy(forecast_MAM, test)[, c(2, 3, 5, 6)]
accuracy6 <- accuracy(forecast_MAA, test)[, c(2, 3, 5, 6)]
accuracy7 <- accuracy(forecast_AAdN, test)[, c(2, 3, 5, 6)]

# Residual Diagnostic

# List of models
models <- c("ets", "ets_ANN", "ets_AAN", "ets_AAA", "ets_MAM", "ets_MAA", "ets_AAdN")

# Initialize a matrix to store the residual diagnostics
n <- length(models)
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

# Loop over each model
for (i in 1:n) {
  # Fit the ETS model
  ets_model <- get(models[i])
  res <- checkresiduals(ets_model, plot = FALSE)
  res_matrix[i,] <- c(i, res$statistic, res$parameter, res$p.value)
}

# Print the residual diagnostics
print(res_matrix)

# Hyperparameter infor
summary(ets)
summary(ets_ANN)
summary(ets_AAN)
summary(ets_AAA)
summary(ets_MAM)
summary(ets_MAA)
summary(ets_AAdN)

# Check MAM residual
checkresiduals(ets_MAM)

# Final result from ETS MAM
plot(forecast_MAM, main = "Forecast from ETS(M,A,M)", xlab = "Time", ylab = "Value")
lines(test, col = "red")

###################################################################################
############################## Question 6 #########################################
###################################################################################

# Auto ARIMA
auto.model <- auto.arima(train, lambda = 0, allowdrift = FALSE, 
                approximation = FALSE, stepwise = FALSE)
summary(auto.model)

# Forecast by auto ARIMA
fa_auto.arima <- forecast(auto.model, h = length(test))

# Final result from auto ARIMA
plot(fa_auto.arima , main = "Forecast from Auto Arima", xlab = "Time", ylab = "Value")
lines(test, col = "red")

# Forecast accuracy
accuracy(fa_auto.arima , test)[, c(2, 3, 5, 6)]

# Check residuals
checkresiduals(fa_auto.arima )
tsdisplay(fa_auto.arima$residuals)

# Manual ARIMA
man1 <- Arima(train, order=c(0,1,1), seasonal=c(0,1,1))
fa_man1 <- forecast(man1, h = length(test))
accuracy(fa_man1, test)[, c(2, 3, 5, 6)]
checkresiduals(fa_man1)

man2 <- Arima(train, order=c(0,1,1), seasonal=c(1,1,1))
fa_man2 <- forecast(man2, h = length(test))
accuracy(fa_man2, test)[, c(2, 3, 5, 6)]
checkresiduals(fa_man2)

man3 <- Arima(train, order=c(0,1,2), seasonal=c(0,1,0))
fa_man3 <- forecast(man3, h = length(test))
accuracy(fa_man3, test)[, c(2, 3, 5, 6)]
checkresiduals(fa_man3)

###################################################################################
############################## Question 8 #########################################
###################################################################################

# Split the data into training and test sets
train2 <- window(ts_trip, start = c(2012, 1), end=c(2020, 3))
test2 <- window(ts_trip, start=c(2020, 4), end=c(2022, 12))

# Fit model and create forecasting
ets_2 <- ets(train2, model="MAM")
forecast_ets2 <- forecast(ets_2, h = 57)

arima_2 <- Arima(train2, order=c(0,1,1), seasonal=c(0,1,1))
forecast_arima2 <- forecast(arima_2, h = 57)

# Plot the result
par(mfrow=c(1,2))
plot(forecast_ets2)
plot(forecast_arima2)

###################################################################################
############################## Question 9 #########################################
###################################################################################

# Split the data into training and test sets
train3 <- window(ts_trip, end=c(2020, 3))
test3 <- window(ts_trip, start=c(2020, 4), end=c(2022, 12))

# Fit model and create forecasting
ets_3 <- ets(train3, model="MAM")
forecast_ets3 <- forecast(ets_3, h = length(test3))

arima_3 <- Arima(train3, order=c(0,1,1), seasonal=c(0,1,1))
forecast_arima3 <- forecast(arima_3, h = length(test3))

# Plot the forecast value and actual value during covid period
par(mfrow=c(1,1))
plot(forecast_ets3, main = "Forecast from ETS(M,A,M)", xlab = "Time", ylab = "Value")
lines(test3, col = "red")

plot(forecast_arima3, main = "Forecast from ARIMA(0,1,1)(0,1,1)[12]", xlab = "Time", ylab = "Value")
lines(test3, col = "red")

# Quick test accuracy on available data
accuracy_arima <- accuracy(forecast_arima3, test3)[, c(2, 3, 5, 6)]

# Difference
differences <- test3 - forecast_arima3$mean
write.csv(differences, file = "C:/Code_Hanh/Forecasting/diff.csv")


###################################################################################
############################## Question 10 ########################################
###################################################################################

# Fit model and create forecasting with tbats
tbats <- tbats(train)
f_tbats <- forecast(tbats, h = length(test))
accuracy_tbats <- accuracy(f_tbats, test)[, c(2, 3, 5, 6)]
checkresiduals(tbats)

# Fit model and create forecasting with NNAR
nnetar <- nnetar(train)
f_nnetar <- forecast(nnetar, h = length(test))
accuracy_nnetar <- accuracy(f_nnetar, test)[, c(2, 3, 5, 6)]
checkresiduals(nnetar)



###################################################################################
################################ PART 2 ###########################################
###################################################################################

# Read the CSV file into a data frame
air <- read.csv("AirTraffic.csv")

# Extract only information about total Domestic Air Travel Passengers
air <- air[, c("Year", "Month", "Dom_Pax")]

# Convert Dom_Pax to numeric value
air$Dom_Pax <- gsub(",", "", air$Dom_Pax)
air$Dom_Pax <- as.numeric(air$Dom_Pax)

# Combine year and month into a Date format
air$Date <- as.Date(paste(air$Year, air$Month, "01", sep = "-"))

# Create a time series object
ts_air_original <- ts(air$Dom_Pax, frequency = 12, start = c(min(air$Year), 1))

# Calendar adjusted 
ts_air <- ts_air_original / monthdays(time(ts_air_original))

###################################################################################
########################### Exploring data ########################################
###################################################################################

# Time series plot
ggplot() +
  geom_line(aes(x = time(ts_air_original), y = ts_air_original)) +
  ggtitle("Monthly Average Domestic Air Travel Passengers: 2003 -2023") +
  xlab("Year") +
  ylab("Passengers") +
  theme_bw()

# Seasonal plot
seasonplot(ts_air, col = rainbow(20), 
           year.labels = TRUE, main = "Seasonal Plot: Monthly Average Domestic Air Travel Passengers: 2003 -2023", ylab = "Passengers")

# Seasonal subseries plot
ggsubseriesplot(ts_air) +
  ylab("Passengers") +
  ggtitle("Seasonal Subseries Plot: Monthly Average Domestic Air Travel Passengers: 2003 -2023") +
  theme_bw()

# (P)ACF plot.
tsdisplay(ts_air, main = "ACF and PACF Plot: Monthly Average Domestic Air Travel Passengers: 2003 -2023")

Acf(ts_air, main = "Autocorrelation Function (ACF) of Monthly Average Domestic Air Travel Passengers: 2003 -2023")

###################################################################################
############################ Transformation #######################################
###################################################################################

# Split the data into training and test sets
air_train <- window(ts_air, end=c(2017, 12))
air_test <- window(ts_air, start=c(2018, 1), end=c(2020, 2))

# Check the sansionary of dataset
adf_result <- adf.test(air_train)

### This data is santionary => No need to transform it

###################################################################################
############################ Seasonal naive  ######################################
###################################################################################

# Fit seasonal naive model to the training data
naive_air <- snaive(air_train,h=length(air_test))

# Generate forecasts
fa_naive <- forecast(naive_air)

# Plot the forecast value and actual value 
plot(fa_naive)
accuracy(fa_naive, air_test)[, c(2, 3, 5, 6)]
checkresiduals(fa_naive)

###################################################################################
############################## STL  ###############################################
###################################################################################

# Fit and forecast
stl_ets <- stlf(air_train, method="ets", h=length(air_test))
fa_stl.ets <- forecast(stl_ets, h = length(air_test))

stl_arima <- stlf(air_train, method="arima", h=length(air_test))
fa_stl.arima <- forecast(stl_arima, h = length(air_test))

# Plot the result
par(mfrow=c(1,2))
plot(fa_stl.ets)
plot(fa_stl.arima)

# Accuracy of all model
accuracy(fa_stl.ets, air_test)[, c(2, 3, 5, 6)]
accuracy(fa_stl.arima, air_test)[, c(2, 3, 5, 6)]

# Check residuals
checkresiduals(stl_ets)
checkresiduals(stl_arima)


###################################################################################
######################## exponential smoothing  ###################################
###################################################################################

# Holt-Winters' seasonal method
hw1 <- hw(air_train,seasonal="additive",h = length(air_test))
forecast_hw1 <- forecast(hw1)
hw2 <- hw(air_train,seasonal="multiplicative",h = length(air_test))
forecast_hw2 <- forecast(hw2)

# Plot forecast Holt-Winters' seasonal
par(mfrow=c(1,2))
plot(forecast_hw1)
plot(forecast_hw2)

# Fit the ETS 
ets.air_auto <- ets(air_train)
ets.air_MAM <- ets(air_train, model="MAM")
ets.air_MAA <- ets(air_train, model="MAA")
ets.air_AAdN <- ets(air_train, model="AAN",damped=TRUE)

# Generate forecasts for ETS
fa_auto <- forecast(ets.air_auto, h = length(air_test))
fa_MAM <- forecast(ets.air_MAM, h = length(air_test))
fa_MAA <- forecast(ets.air_MAA, h = length(air_test))
fa_AAdN <- forecast(ets.air_AAdN, h = length(air_test))

# Plot forecast ETS
par(mfrow=c(2,2))
plot(fa_auto)
plot(fa_MAM)
plot(fa_MAA)
plot(fa_AAdN)

# Accuracy of all model
accuracy(forecast_hw1, air_test)[, c(2, 3, 5, 6)]
accuracy(forecast_hw2, air_test)[, c(2, 3, 5, 6)]
accuracy(fa_auto, air_test)[, c(2, 3, 5, 6)]
accuracy(fa_MAM, air_test)[, c(2, 3, 5, 6)]
accuracy(fa_MAA, air_test)[, c(2, 3, 5, 6)]
accuracy(fa_AAdN, air_test)[, c(2, 3, 5, 6)]

# Check the residuals
# List of models
models <- c("ets.air_auto", "ets.air_MAM", "ets.air_MAA", "ets.air_AAdN")

# Initialize a matrix to store the residual diagnostics
n <- length(models)
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

# Loop over each model
for (i in 1:n) {
  # Fit the ETS model
  ets_model <- get(models[i])
  res <- checkresiduals(ets_model, plot = FALSE)
  res_matrix[i,] <- c(i, res$statistic, res$parameter, res$p.value)
}

checkresiduals(hw1)
checkresiduals(hw2)

###################################################################################
############################### Arima #############################################
###################################################################################

# Auto ARIMA
arima_0 <- auto.arima(air_train, lambda = 0, allowdrift = FALSE, 
                         approximation = FALSE, stepwise = FALSE)
fa0 <- forecast(arima_0, h = length(air_test))
accuracy(fa0, air_test)[, c(2, 3, 5, 6)]
checkresiduals(fa0)

# Manual ARIMA
arima_1 <- Arima(air_train, order=c(0,1,1), seasonal=c(0,1,1))
fa1 <- forecast(arima_1, h = length(air_test))
accuracy(fa1, air_test)[, c(2, 3, 5, 6)]
checkresiduals(fa1)

arima_2 <- Arima(air_train, order=c(0,1,1), seasonal=c(1,1,1))
fa2 <- forecast(arima_2, h = length(air_test))
accuracy(fa2, air_test)[, c(2, 3, 5, 6)]
checkresiduals(fa2)

# Plot model
par(mfrow=c(1,1))
plot(fa0)
par(mfrow=c(1,2))
plot(fa1)
plot(fa2)

###################################################################################
############################### Covid #############################################
###################################################################################

# Split the data into training and test sets
air_train2 <- window(ts_air, end=c(2020, 2))
air_test2 <- window(ts_air, start=c(2020, 3), end=c(2023, 9))

# Fit the model 
arima_covid <- Arima(air_train2, order=c(1,1,0), seasonal=c(0,1,2))
fa_covid <- forecast(arima_covid, h = length(air_test2))

# Plot and compare result
par(mfrow=c(1,1))
plot(fa_covid, main = "Forecast from ARIMA(1,1,0)(0,1,2)[12]", xlab = "Time", ylab = "Value")
lines(air_test2, col = "red")

# check residual on testset
predicted_values_1 <- fa_covid$mean
residuals <- air_test2 - predicted_values_1

ggplot() +
  geom_line(aes(x = time(residuals), y = residuals)) +
  ggtitle("Time Series Plot of Residuals during COVID-19") +
  xlab("Year") +
  ylab("Residuals") +
  theme_bw()

plot(residuals, 
     type = "l",  # "l" for lines plot
     xlab = "Time", 
     ylab = "Residuals", 
     main = "Time Series Plot of Residuals during COVID-19")


