library(car)

data <- read_excel(file.choose(), sheet = "Curry")

head(data)
summary(data)
str(data)
plot(data)


## a ##
#data2 <- dummyVars("~.", data = data)
#data_dummyd <- data.frame(predict(data2, data))
data_dummyd <- cbind(data, "CountryUS" = ifelse(data$Country == "US", 1, 0))
data_dummyd <- data_dummyd[ , !(names(data_dummyd) %in% c("Country"))]
head(data_dummyd)


reg <- step(lm(formula = Sales ~., data = data_dummyd),
            direction = "backward")
summary(reg)

## b ## wrong answer
linearHypothesis(reg,
                 c("CountryUS = 0"))


## c ##
mod_data <- cbind(data_dummyd,
                  "US_Ad_Budget" = ifelse(data_dummyd$CountryUS == 1, data_dummyd$Ad_Budget, 0),
                  "US_Price" = ifelse(data_dummyd$CountryUS == 1, data_dummyd$Price, 0),
                  "US_Distance" = ifelse(data_dummyd$CountryUS == 1, data_dummyd$Distance, 0)
                  )
head(mod_data)
plot(mod_data)

mod_reg <- lm(data = mod_data, Sales ~ Ad_Budget + Price + Distance + CountryUS + US_Ad_Budget + US_Price + US_Distance)
summary(mod_reg)

linearHypothesis(mod_reg, c("CountryUS = 0", "US_Ad_Budget = 0", "US_Price = 0", "US_Distance = 0"))

