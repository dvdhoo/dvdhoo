library(lubridate)

# reading files
mobile_activity <- read.csv("~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 2/mobile_activity.csv")
web_activity <- read.csv("~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 2/web_activity.csv")

str(mobile_activity)
str(web_activity)


# timezone
mobile_activity$t_time1 <- ymd_hms(mobile_activity$t_time)
tz(mobile_activity$t_time1) <- "America/Toronto"

for (i in c(1:length(mobile_activity[,1]))) {
  if (!is.na(mobile_activity[i, "t_timezone"])) {
    mobile_activity[i, "t_time2"] <- with_tz(mobile_activity[i, "t_time1"], mobile_activity[i, "t_timezone"])
  }
}

web_activity$t_time1 <- parse_date_time(web_activity$t_time, c("%d/%b/%Y %H:%M:%S:%OS"), exact = T, tz = "America/Toronto")

for (i in c(1:length(web_activity[,1]))) {
  if (!is.na(web_activity[i, "t_timezone"])) {
    web_activity[i, "t_time2"] <- with_tz(web_activity[i, "t_time1"], web_activity[i, "t_timezone"])
  }
}

# AM/PM
mobile_activity$t_PM <- ifelse(pm(mobile_activity$t_time2), 1, 0)
web_activity$t_PM <- ifelse(pm(web_activity$t_time2), 1, 0)

# work hour
mobile_activity$t_workhr <- ifelse(hour(mobile_activity$t_time2) >= 9 & hour(mobile_activity$t_time2) < 17, 1, 0)
web_activity$t_workhr <- ifelse(hour(web_activity$t_time2) >= 9 & hour(web_activity$t_time2) < 17, 1, 0)


# purchase indicator
mobile_activity$p_purchase <- ifelse(is.na(mobile_activity$p_price), 0, 1)
web_activity$p_purchase <- ifelse(is.na(web_activity$p_price), 0, 1)



# write file
write.csv(mobile_activity, na = "NA", file = "/Users/dvdhoo/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 3/mobile_activity.csv", row.names = F)
write.csv(web_activity, na = "NA", file = "/Users/dvdhoo/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 3/web_activity.csv", row.names = F)


