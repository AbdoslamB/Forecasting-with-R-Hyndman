## Chapter 7

# libraries
library(fpp2)
library(seasonal)
library(xlsx)
library(tseries)

# Question 1
str(pigs)
head(pigs)
autoplot(pigs)

ses_pigs <- ses(pigs, h = 4)
ses_pigs$model

# 95% prediction interval for the first forecast
ses_pigs$upper[1, "95%"]

ses_pigs$lower[1, "95%"]

# calculate 95% prediction interval using formula
s <- sd(ses_pigs$residuals)
ses_pigs$mean[1] + 1.96*s

ses_pigs$mean[1] - 1.96*s

autoplot(ses_pigs) + autolayer(ses_pigs$fitted)

# Question 2
SES <- function(y, alpha, l0){
  y_hat <- l0
  for(index in 1:length(y)){
    y_hat <- alpha*y[index] + (1 - alpha)*y_hat 
  }
  cat("Forecast of next observation by SES function: ",
      as.character(y_hat),
      sep = "\n")
}

alpha <- ses_pigs$model$par[1]
l0 <- ses_pigs$model$par[2]

SES(pigs, alpha = alpha, l0 = l0)

# Question 3
SES <- function(pars = c(alpha, l0), y){
  # change the first argument as vector of alpha and l0, rather than separate alpha and l0 because optim function wants to take a function that requires vector as its first argument as fn argument.
  error <- 0
  SSE <- 0
  alpha <- pars[1]
  l0 <- pars[2]
  y_hat <- l0
  
  for(index in 1:length(y)){
    error <- y[index] - y_hat
    SSE <- SSE + error^2
    
    y_hat <- alpha*y[index] + (1 - alpha)*y_hat 
  }
  
  return(SSE)
}

opt_SES_pigs <- optim(par = c(0.5, pigs[1]), y = pigs, fn = SES)

opt_SES_pigs$par[2]
ses_pigs$model$par[1]
ses_pigs$model$par[2]

# Question 4

SES <- function(init_pars, data){      
  fc_next <- 0
  SSE <- function(pars, data){
    error <- 0
    SSE <- 0
    alpha <- pars[1]
    l0 <- pars[2]
    y_hat <- l0
    
    for(index in 1:length(data))
    {
      error <- data[index] - y_hat
      SSE <- SSE + error^2
      
      y_hat <- alpha*data[index] + (1 - alpha)*y_ha
    }
    fc_next <<- y_hat
    return(SSE)
  }
  
  optim_pars <- optim(par = init_pars, data = data, fn = SSE)
  
  return(list(
    Next_observation_forecast = fc_next,
    alpha = optim_pars$par[1],
    l0 = optim_pars$par[2]
  ))
}


# getting the result to compare 
ses_pigs$mean[1]

ses_pigs$model$par[1]

#0 calculated by ses
ses_pigs$model$par[2]

opt_SES_pigs$par[1]

# Question 5

# (a) Plot the series and discuss the main features of the data.
str(books)
head(books)
autoplot(books)  

# (b) Use the ses() function to forecast each series, and plot the forecasts.
sespaper <- ses(books[,"Paperback"], h=4)
seshard  <- ses(books[,"Hardcover"], h=4)
autoplot(sespaper)

autoplot(seshard)

# (c) Compute the RMSE values for the training data in each case.
sqrt(mean(sespaper$residuals^2))

sqrt(mean(seshard$residuals^2))

# Question 6

# a. Now apply Holt's linear method to the paperback and hardback series and compute four-day forecasts in each case.
holt_paperback <- holt(books[, "Paperback"], h = 4)
holt_hardcover <- holt(books[, "Hardcover"], h = 4)

autoplot(books[, "Paperback"]) +
  autolayer(holt_paperback)

autoplot(books[, "Hardcover"]) +
  autolayer(holt_hardcover)

#(b) Compare the RMSE measures of Holt's method for the two series to those of simple exponential smoothing in the previous question. (Remember that Holt's method is using one more parameter than SES.) Discuss the merits of the two forecasting methods for these data sets.
s_paperback <- sqrt(mean(holt_paperback$residuals^2))
s_hardcover <- sqrt(mean(holt_hardcover$residuals^2))
s_paperback

s_hardcover

# (c) Compare the forecasts for the two series using both methods. Which do you think is best?

# The RMSE value is lower for hardcover sales so I think the hardcover sales are better than the paperback sales.


# (d) Calculate a 95% prediction interval for the first forecast for each series, using the RMSE values and assuming normal errors. Compare your intervals with those produced using ses and holt.
#95% PI paperback sales Holt
holt_paperback$upper[1, "95%"]

holt_paperback$lower[1, "95%"]

#95% PI paperback sales by formula
holt_paperback$mean[1] + 1.96*s_paperback

holt_paperback$mean[1] - 1.96*s_paperback

#95% PI of hardcover sales calculated by holt function
holt_hardcover$upper[1, "95%"]

holt_hardcover$lower[1, "95%"]

# 95% PI of hardcover sales calculated by formula
holt_hardcover$mean[1] + 1.96*s_hardcover

holt_hardcover$mean[1] - 1.96*s_hardcover

# Question 7
str(eggs)
head(eggs)
autoplot(eggs)

# Holt only
holt_eggs <- holt(eggs, h = 100)
autoplot(holt_eggs) +
  autolayer(holt_eggs$fitted)

#holt function with damped option
holt_damped_eggs <- holt(eggs, damped = TRUE, h = 100)
autoplot(holt_damped_eggs) +
  autolayer(holt_damped_eggs$fitted)

# holt function with Box-Cox
holt_BoxCox_eggs <- holt(eggs, 
                         lambda = BoxCox.lambda(eggs), 
                         h = 100)
autoplot(holt_BoxCox_eggs) +
  autolayer(holt_BoxCox_eggs$fitted)

#  Holt function with Box-Cox transformation and damped option
holt_BoxCox_damped_eggs <- holt(
  eggs, 
  damped = TRUE,
  lambda = BoxCox.lambda(eggs),
  h = 100)
autoplot(holt_BoxCox_damped_eggs) +
  autolayer(holt_BoxCox_damped_eggs$fitted)

autoplot(holt_BoxCox_eggs)

accuracy(holt_BoxCox_eggs)


# Question 8

# load the data
retail <- xlsx::read.xlsx("C:/Users/baaba/Downloads/retail.xlsx",
                          sheetIndex = 1,
                          startRow = 2)

ts_retail <- ts(retail[, "A3349873A"],
                frequency = 12,
                start = c(1982, 4))


# (a) Why is multiplicative seasonality necessary for this series?

autoplot(ts_retail)

ets_AAM_retail <- hw(ts_retail,
                     seasonal = "multiplicative")
ets_AAdM_retail <- hw(ts_retail,
                      seasonal = "multiplicative",
                      damped = TRUE)
autoplot(ets_AAM_retail)

autoplot(ets_AAdM_retail)

error_ets_AAM_retail <- tsCV(
  ts_retail, 
  hw, h = 1, seasonal = "multiplicative"
)

# Question 8(c) Compare the RMSE of the one-step forecasts from the two methods. Which do you prefer?
error_ets_AAdM_retail <- tsCV(
  ts_retail, 
  hw, h = 1, seasonal = "multiplicative", damped = TRUE
)
sqrt(mean(error_ets_AAM_retail^2, na.rm = TRUE))

sqrt(mean(error_ets_AAdM_retail^2, na.rm = TRUE))

checkresiduals(ets_AAdM_retail)

# Question 8(d) Now find the test set RMSE, while training the model to the end of 2010. Can you beat the seasonal naïve approach from Exercise 7 in Section 3.7?
ts_retail_train <- window(ts_retail,
                          end = c(2010, 12))
ts_retail_test <- window(ts_retail,
                         start = 2011)

ets_AAdM_retail_train <- hw(ts_retail_train,
                            h = 36,
                            seasonal = "multiplicative",
                            damped = TRUE)
autoplot(ets_AAdM_retail_train)

accuracy(ets_AAdM_retail_train, ts_retail_test)

ets_AAM_retail_train <- hw(ts_retail_train,
                           h = 36,
                           seasonal = "multiplicative")

accuracy(ets_AAM_retail_train, ts_retail_test)

# Question 10

# (a) Plot the data and describe the main features of the series.
str(ukcars)

head(ukcars)
autoplot(ukcars)

# (b) Decompose the series using STL and obtain the seasonally adjusted data.

Sadj_uk_cars <- ukcars %>% stl(s.window = 4, robust = TRUE) %>% seasadj() 
autoplot(Sadj_uk_cars)


# (c) Decompose the series using STL and obtain the seasonally adjusted data.
stlf_ets_AAdN_ukcars <- ukcars %>% stlf(h = 8, etsmodel = "AAN", damped = TRUE)

autoplot(stlf_ets_AAdN_ukcars)

# (d) Forecast the next two years of the series using Holt's linear method applied to the seasonally adjusted data (as before but with damped=FALSE).

stlf_ets_AAN_ukcars <- ukcars %>% stlf(h = 8, etsmodel = "AAN", damped = FALSE)

autoplot(stlf_ets_AAN_ukcars)

# (e) Now use ets() to choose a seasonal model for the data.

ets_ukcars <- ets(ukcars)

summary(ets_ukcars)

autoplot(forecast(ets_ukcars, h = 8))

# (f)  Compare the RMSE of the ETS model with the RMSE of the models you obtained using STL decompositions. Which gives the better in-sample fits?

accuracy(stlf_ets_AAdN_ukcars)

accuracy(stlf_ets_AAN_ukcars)

accuracy(ets_ukcars) 

# (g) Compare the forecasts from the three approaches? Which seems most reasonable?
# the  STL & ETS models are better because there were smaller variation trend after 2001 which make it better in my opinion

#(h) Check the residuals of your preferred model.
checkresiduals(stlf_ets_AAdN_ukcars)

## Question 11

# (a)  Make a time plot of your data and describe the main features of the series.
str(visitors)
head(visitors)
autoplot(visitors)

ggseasonplot(visitors)

# (g)b) Split your data into a training set and a test set comprising the last two years of available data. Forecast the test set using Holt-Winters' multiplicative method.
visitors_train <- subset(visitors, 
                         end = length(visitors) - 24)
visitors_test <- subset(visitors,
                        start = length(visitors) - 23)

hw_mul_visitors_train <- hw(visitors_train,
                            h = 24,
                            seasonal = "multiplicative")


#(c) Why is multiplicative seasonality necessary here?
autoplot(hw_mul_visitors_train)

#(d) Forecast the two-year test set using each of the following methods:

# ETS
fc_ets_visitors_train <- forecast(ets(visitors_train), h = 24)
autoplot(fc_ets_visitors_train)

#additive ETS model
fc_ets_add_BoxCox_visitors_train <- forecast(
  ets(visitors_train, 
      lambda = BoxCox.lambda(visitors_train),
      additive.only = TRUE),
  h = 24
)
autoplot(fc_ets_add_BoxCox_visitors_train)


# seasonal naive method
fc_BoxCox_stl_ets_visitors_train <- visitors_train %>%
  stlm(
    lambda = BoxCox.lambda(visitors_train),
    s.window = 13,
    robust = TRUE,
    method = "ets"
  ) %>%
  forecast(h = 24)

autoplot(fc_BoxCox_stl_ets_visitors_train)

#(e) Which method gives the best forecasts? Does it pass the residual tests?
accuracy(hw_mul_visitors_train, visitors_test)

accuracy(fc_ets_visitors_train, visitors_test)

accuracy(fc_ets_add_BoxCox_visitors_train, visitors_test)

accuracy(fc_BoxCox_stl_ets_visitors_train, visitors_test)

# Question 13

str(ausbeer)
head(ausbeer)
autoplot(ausbeer)

ausbeer_train <- subset(
  ausbeer, end = length(ausbeer) - 12
)
ausbeer_test <- subset(
  ausbeer, start = length(ausbeer) - 11
)

ets_ausbeer_train <- forecast(
  ets(ausbeer_train), h = 12
)

snaive_ausbeer_train <- snaive(ausbeer_train,  h = 12)

stlf_ausbeer_train <- stlf(
  ausbeer_train, 
  h = 12,
  s.window = 5,
  robust = TRUE,
  lambda = BoxCox.lambda(ausbeer_train))

# the accuracy
accuracy(ets_ausbeer_train, ausbeer_test)

accuracy(snaive_ausbeer_train, ausbeer_test)

accuracy(stlf_ausbeer_train, ausbeer_test)






