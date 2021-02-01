library(readxl)
library(caret)
library(car)
library(dplyr)
library(tidyverse)
#library(estimatr)


##  a
data <- read_excel(file.choose(), sheet = "Wine")

data2 <- dummyVars("~ .", data = data)

data2
data_dummyd <- data.frame(predict(data2, data))
head(data_dummyd)



data_reg <- lm(data = data_dummyd, formula = Rating ~ .)
summary(data_reg)
#plot(data_reg)
#plot(density(resid(data_reg)))


#data_reg_pred <- step(lm(formula = data_dummyd$Rating ~ 1, data = data_dummyd),
#                      direction = "forward", scope = formula(data_reg))
data_reg_pred <- step(lm(formula = data_reg, data = data_dummyd),
                      direction = "backward")
summary(data_reg_pred)

##  b
plot(data_reg_pred)
plot(density(resid(data_reg_pred)))

data_dummyd$predicted <- predict(data_reg_pred)
data_dummyd$residuals <- residuals(data_reg_pred)

data_dummyd %>% 
  gather(key = "Obs", value = "x", -Rating, -predicted, -residuals, -Obs) %>%  
  ggplot(aes(x = x, y = residuals)) +  
  geom_point(aes(y = residuals), shape = 1) +
  facet_grid(~ Obs, scales = "free_x") + 
  theme_bw()

ncvTest(data_reg_pred)

#if heteroskedasticity
#rob_reg <- lm_robust(formula = data_reg_pred, data_dummyd, se_type = "HC3")
#summary(rob_reg)

##  c
data_point <- data.frame(Price = 39.99,
                         Alcohol = mean(data_dummyd$Alcohol),
                         Residual_Sugar = mean(data_dummyd$Residual_Sugar),
                         Sulphates = mean(data_dummyd$Sulphates),
                         CountryFrance = 1,
                         CountryCanada = 0,
                         CountryItaly = 0,
                         CountryUS = 0,
                         pH = 2.1)
predict_point <- data.frame(predict(data_reg_pred, newdata = data_point))













