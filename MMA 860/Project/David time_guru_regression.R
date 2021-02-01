library(car)
library(ggplot2)
library(scales)
library(estimatr)
library(ISLR)

#mobile_activity <- read.csv("~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 3/mobile_activity.csv")
#web_activity <- read.csv("~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 3/web_activity.csv")


mobile_activity$mobile <- 1
mobile_activity[which(is.na(mobile_activity$l_country)), "l_country"] <- "Other"
web_activity$mobile <- 0
web_activity[which(is.na(web_activity$l_country)), "l_country"] <- "Other"
all_activity <- rbind(mobile_activity, web_activity)


# purchase or not

# mobile_reg <- lm(data = mobile_activity,
#                 formula = p_purchase ~ t_PM + t_workhr + r_response_time
#                 )
# ncvTest(mobile_reg)
# mobile_reg_rob <- lm_robust(formula = p_purchase ~ t_PM + t_workhr + r_response_time, mobile_activity, se_type = "HC3")
# summary(mobile_reg_rob)
# linearHypothesis(mobile_reg_rob,
#                 c(
#                   "t_PM = 0",
#                   "t_workhr = 0",
#                   "r_response_time = 0"
#                 )
# )

mobile_reg <- glm(data = mobile_activity,
                      p_purchase ~ t_PM + t_workhr + r_response_time,
                      family = binomial
)
summary(mobile_reg)
plot(mobile_reg)


# web_reg <- lm(data = web_activity,
#               formula = p_purchase ~ t_PM + t_workhr + r_response_time
# )
# ncvTest(web_reg)
# web_reg_rob <- lm_robust(formula = p_purchase ~ t_PM + t_workhr + r_response_time, web_activity, se_type = "HC3")
# summary(web_reg_rob)
# linearHypothesis(web_reg_rob,
#                  c(
#                    "t_PM = 0",
#                    "t_workhr = 0",
#                    "r_response_time = 0"
#                    )
#                  )

web_reg <- glm(data = web_activity,
                  p_purchase ~ t_PM + t_workhr + r_response_time,
                  family = binomial
)
summary(web_reg)



# all_reg <- lm(data = all_activity,
#               formula = p_purchase ~ t_PM + t_workhr + r_response_time + mobile
# )
# summary(all_reg)
# ncvTest(all_reg)
# all_reg_rob <- lm_robust(formula = p_purchase ~ t_PM + t_workhr + r_response_time + mobile, all_activity, se_type = "HC3")
# summary(all_reg_rob)
# linearHypothesis(all_reg_rob,
#                  c(
#                    "t_PM = 0",
#                    "t_workhr = 0",
#                    "r_response_time = 0"
#                  )
# )
all_reg <- glm(data = all_activity,
               p_purchase ~ t_PM + t_workhr + r_response_time + mobile,
               family = binomial
)
summary(all_reg)



# purchase price
mobile_purchased <- mobile_activity[which(mobile_activity$p_purchase == 1),]

mobile_reg_price <- lm(mobile_purchased,
                       formula = p_price ~ t_PM + t_workhr + r_response_time
                       )
summary(mobile_reg_price)
ncvTest(mobile_reg_price)
mobile_reg_price_rob <- lm_robust(formula = p_purchase ~ t_PM + t_workhr + r_response_time, mobile_purchased, se_type = "HC3")
summary(mobile_reg_price_rob)
linearHypothesis(mobile_reg_price_rob,
                 c(
                   "t_PM = 0",
                   "t_workhr = 0",
                   "r_response_time = 0"
                 )
)


web_purchased <- web_activity[which(web_activity$p_purchase == 1),]

web_reg_price <- lm(web_purchased,
                       formula = p_price ~ t_PM + t_workhr + r_response_time
)
summary(web_reg_price)
ncvTest(web_reg_price)
web_reg_price_rob <- lm_robust(formula = p_purchase ~ t_PM + t_workhr + r_response_time, web_purchased, se_type = "HC3")
summary(web_reg_price_rob)
linearHypothesis(web_reg_price_rob,
                 c(
                   "t_PM = 0",
                   "t_workhr = 0",
                   "r_response_time = 0"
                   )
                 )



all_purchased <- all_activity[which(all_activity$p_purchase == 1),]

all_reg_price <- lm(all_purchased,
                       formula = p_price ~ t_PM + t_workhr + r_response_time + mobile
)
summary(all_reg_price)
ncvTest(all_reg_price)
all_reg_price_rob <- lm_robust(formula = p_purchase ~ t_PM + t_workhr + r_response_time + mobile, all_purchased, se_type = "HC3")
summary(all_reg_price_rob)
linearHypothesis(all_reg_price_rob,
                 c(
                   "t_PM = 0",
                   "t_workhr = 0",
                   "r_response_time = 0"
                 )
)


# location
mobile_country <- data.frame(country = unique(mobile_activity$l_country))
for(i in mobile_country$country){
  mobile_country[which(mobile_country$country == i), "purchase"] <- nrow(mobile_activity[which(mobile_activity$p_purchase == 1 & mobile_activity$l_country == i),]) 
  mobile_country[which(mobile_country$country == i), "total"] <- nrow(mobile_activity[which(mobile_activity$l_country == i),])
  mobile_country[which(mobile_country$country == i), "percent"] = mobile_country[which(mobile_country$country == i), "purchase"] / mobile_country[which(mobile_country$country == i), "total"]
}

ggplot(data = mobile_country,
       aes(
         x = country, y = percent, label = paste(purchase,"/",total)
         )
       ) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Country", y = "Percentage of Purchase (Mobile)") +
  geom_text(size = 4, angle = -90) +
  scale_y_continuous(labels=scales::percent) +
  coord_cartesian(ylim=c(0.08, 0.22)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        plot.margin = unit(c(10,23,10,10),"points")
  )

web_country <- data.frame(country = unique(web_activity$l_country))
for(i in web_country$country){
  web_country[which(web_country$country == i), "purchase"] <- nrow(web_activity[which(web_activity$p_purchase == 1 & web_activity$l_country == i),]) 
  web_country[which(web_country$country == i), "total"] <- nrow(web_activity[which(web_activity$l_country == i),])
  web_country[which(web_country$country == i), "percent"] = web_country[which(web_country$country == i), "purchase"] / web_country[which(web_country$country == i), "total"]
}

ggplot(data = web_country,
       aes(
         x = country, y = percent, label = paste(purchase,"/",total)
       )
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Country", y = "Percentage of Purchase (Website)") +
  geom_text(size = 4, angle = -90) +
  scale_y_continuous(labels=scales::percent) +
  coord_cartesian(ylim=c(0.16, 0.62)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        plot.margin = unit(c(10,23,10,10),"points")
  )





ggplot(data = mobile_purchased, aes(x = l_country, y = p_price)) +
  geom_boxplot() +
  labs(x = "Country", y = "Purchase Price (Mobile)") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        plot.margin = unit(c(10,23,10,10),"points")
  )

ggplot(data = web_purchased, aes(x = l_country, y = p_price)) +
  geom_boxplot() +
  labs(x = "Country", y = "Purchase Price (Website)") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
        plot.margin = unit(c(10,23,10,10),"points")
  )



