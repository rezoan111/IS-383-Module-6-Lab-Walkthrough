# Problem 2
# I am using the same data from Problem 1.

week <- 1:6
value <- c(18, 13, 16, 11, 17, 14)

p2 <- data.frame(week, value)
p2

# I am finding the forecast for each week
# by using the average of all previous values.

forecast <- c(NA, NA, NA, NA, NA, NA)

for (i in 2:6) {
  forecast[i] <- mean(value[1:(i - 1)])
}

# forecast for week 7
forecast_week7 <- mean(value)

p2$forecast <- forecast
p2
forecast_week7

# I am now calculating the forecast errors
# and the accuracy measures for Problem 2.

p2$error <- p2$value - p2$forecast
p2$abs_error <- abs(p2$error)
p2$sq_error <- p2$error^2
p2$abs_pct_error <- abs(p2$error / p2$value) * 100

# accuracy measures
MAE <- mean(p2$abs_error, na.rm = TRUE)
MSE <- mean(p2$sq_error, na.rm = TRUE)
MAPE <- mean(p2$abs_pct_error, na.rm = TRUE)

round(p2, 4)
MAE
MSE
MAPE

# Final answers for Problem 2

cat("Problem 2 Final Answers\n")
cat("MAE =", round(MAE, 4), "\n")
cat("MSE =", round(MSE, 4), "\n")
cat("MAPE =", round(MAPE, 4), "%\n")
cat("Forecast for Week 7 =", round(forecast_week7, 4), "\n")



# Problem 4
# I am entering the monthly time series data first.

month <- 1:7
value_m <- c(24, 13, 20, 12, 19, 23, 15)

p4 <- data.frame(month, value_m)
p4


# Problem 4(a)
# I am using the most recent value as the forecast for the next month.

forecast_naive <- c(NA, value_m[1:6])

p4$forecast_naive <- forecast_naive
p4$error_naive <- p4$value_m - p4$forecast_naive
p4$sq_error_naive <- p4$error_naive^2

MSE_naive <- mean(p4$sq_error_naive, na.rm = TRUE)
forecast_month8_naive <- value_m[7]

round(p4, 4)
MSE_naive
forecast_month8_naive

# Problem 4(b)
# I am using the average of all available data
# as the forecast for the next month.

forecast_avg <- c(NA, NA, NA, NA, NA, NA, NA)

for (i in 2:7) {
  forecast_avg[i] <- mean(value_m[1:(i - 1)])
}

p4$forecast_avg <- forecast_avg
p4$error_avg <- p4$value_m - p4$forecast_avg
p4$sq_error_avg <- p4$error_avg^2

MSE_avg <- mean(p4$sq_error_avg, na.rm = TRUE)
forecast_month8_avg <- mean(value_m)

round(p4, 4)
MSE_avg
forecast_month8_avg

# Final answers for Problem 4

cat("Problem 4(a)\n")
cat("MSE using most recent value =", round(MSE_naive, 4), "\n")
cat("Forecast for month 8 =", round(forecast_month8_naive, 4), "\n\n")

cat("Problem 4(b)\n")
cat("MSE using average of all previous data =", round(MSE_avg, 4), "\n")
cat("Forecast for month 8 =", round(forecast_month8_avg, 4), "\n\n")

cat("Better method based on MSE = Average of all previous data\n")


# Problem 6
# I am entering the weekly data first.

week6 <- 1:7
value6 <- c(24, 13, 20, 12, 19, 23, 15)

p6 <- data.frame(week6, value6)
p6

# Time series plot for Problem 6
plot(week6, value6, type = "b",
     main = "Problem 6 Time Series Plot",
     xlab = "Week",
     ylab = "Value")


# Problem 6(b)
# I am using a three-week moving average.

forecast_ma3 <- c(NA, NA, NA, NA, NA, NA, NA)

for (i in 4:7) {
  forecast_ma3[i] <- mean(value6[(i - 3):(i - 1)])
}

p6$forecast_ma3 <- forecast_ma3
p6$error_ma3 <- p6$value6 - p6$forecast_ma3
p6$sq_error_ma3 <- p6$error_ma3^2

MSE_ma3 <- mean(p6$sq_error_ma3, na.rm = TRUE)
forecast_week8_ma3 <- mean(value6[5:7])

round(p6, 4)
MSE_ma3
forecast_week8_ma3

# Problem 6(c)
# I am using exponential smoothing with alpha = 0.2.

alpha <- 0.2

forecast_exp <- c(NA, NA, NA, NA, NA, NA, NA)
forecast_exp[2] <- value6[1]

for (i in 3:7) {
  forecast_exp[i] <- alpha * value6[i - 1] + (1 - alpha) * forecast_exp[i - 1]
}

p6$forecast_exp <- forecast_exp
p6$error_exp <- p6$value6 - p6$forecast_exp
p6$sq_error_exp <- p6$error_exp^2

MSE_exp <- mean(p6$sq_error_exp, na.rm = TRUE)
forecast_week8_exp <- alpha * value6[7] + (1 - alpha) * forecast_exp[7]

round(p6, 4)
MSE_exp
forecast_week8_exp


# Final answers for Problem 6

cat("Problem 6(a)\n")
cat("Pattern = Horizontal pattern with random variation\n\n")

cat("Problem 6(b)\n")
cat("Three-week moving average MSE =", round(MSE_ma3, 4), "\n")
cat("Forecast for week 8 =", round(forecast_week8_ma3, 4), "\n\n")

cat("Problem 6(c)\n")
cat("Exponential smoothing MSE =", round(MSE_exp, 4), "\n")
cat("Forecast for week 8 =", round(forecast_week8_exp, 4), "\n\n")

cat("Better method based on MSE = Three-week moving average\n")


# Problem 8
# I am entering the gasoline sales data from Table 9.1.

week8 <- 1:12
sales8 <- c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22)

p8 <- data.frame(week8, sales8)
p8


# Problem 8(a)
# I am using exponential smoothing with alpha = 0.1.

alpha1 <- 0.1

forecast_a01 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
forecast_a01[2] <- sales8[1]

for (i in 3:12) {
  forecast_a01[i] <- alpha1 * sales8[i - 1] + (1 - alpha1) * forecast_a01[i - 1]
}

p8$forecast_a01 <- forecast_a01
p8$error_a01 <- p8$sales8 - p8$forecast_a01
p8$abs_error_a01 <- abs(p8$error_a01)
p8$sq_error_a01 <- p8$error_a01^2
p8$abs_pct_error_a01 <- abs(p8$error_a01 / p8$sales8) * 100

MAE_a01 <- mean(p8$abs_error_a01, na.rm = TRUE)
MSE_a01 <- mean(p8$sq_error_a01, na.rm = TRUE)
MAPE_a01 <- mean(p8$abs_pct_error_a01, na.rm = TRUE)

forecast_week13_a01 <- alpha1 * sales8[12] + (1 - alpha1) * forecast_a01[12]

round(p8, 4)
MAE_a01
MSE_a01
MAPE_a01
forecast_week13_a01


# Problem 8(b)
# I am using exponential smoothing with alpha = 0.2.

alpha2 <- 0.2

forecast_a02 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
forecast_a02[2] <- sales8[1]

for (i in 3:12) {
  forecast_a02[i] <- alpha2 * sales8[i - 1] + (1 - alpha2) * forecast_a02[i - 1]
}

p8$forecast_a02 <- forecast_a02
p8$error_a02 <- p8$sales8 - p8$forecast_a02
p8$abs_error_a02 <- abs(p8$error_a02)
p8$sq_error_a02 <- p8$error_a02^2
p8$abs_pct_error_a02 <- abs(p8$error_a02 / p8$sales8) * 100

MAE_a02 <- mean(p8$abs_error_a02, na.rm = TRUE)
MSE_a02 <- mean(p8$sq_error_a02, na.rm = TRUE)
MAPE_a02 <- mean(p8$abs_pct_error_a02, na.rm = TRUE)

forecast_week13_a02 <- alpha2 * sales8[12] + (1 - alpha2) * forecast_a02[12]

round(p8, 4)
MAE_a02
MSE_a02
MAPE_a02
forecast_week13_a02

# Final answers for Problem 8

cat("Problem 8(a)\n")
cat("Alpha = 0.1\n")
cat("MAE =", round(MAE_a01, 4), "\n")
cat("MSE =", round(MSE_a01, 4), "\n")
cat("MAPE =", round(MAPE_a01, 4), "\n")
cat("Forecast for week 13 =", round(forecast_week13_a01, 4), "\n\n")

cat("Problem 8(b)\n")
cat("Alpha = 0.2\n")
cat("MAE =", round(MAE_a02, 4), "\n")
cat("MSE =", round(MSE_a02, 4), "\n")
cat("MAPE =", round(MAPE_a02, 4), "\n")
cat("Forecast for week 13 =", round(forecast_week13_a02, 4), "\n\n")

cat("Better alpha based on MSE = 0.2\n")