library(tidyverse)

#reading files
mobile_activity <- read.csv("~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 1/mobile_activity.csv")
web_activity <- read.csv("~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 1/web_activity.csv")

# obtain location data
library(rgeolocate)

ip_address <- data.frame(l_ip = unique(c(mobile_activity[, "l_ip"], web_activity[, "l_ip"]), incomparables = FALSE))

ip_address_lookup <- ip_api(ip_addresses = ip_address[, "l_ip"],
                    as_data_frame = TRUE,
                    delay = TRUE
                    )

ip_address2 <- data.frame(l_ip = ip_address[,"l_ip"], ip_address_lookup)

temp <- ip_info(ip_addresses = c("96.246.153.14", "97.16.177.171", "97.253.173.38", "97.92.123.199", "98.48.149.168"), token = NULL)
temp2 <- data.frame(l_ip = c("96.246.153.14", "97.16.177.171", "97.253.173.38", "97.92.123.199", "98.48.149.168"), temp)

temp3 <- ip_info(ip_addresses = c("224.236.136.96", "236.130.142.92", "252.136.1.140", "10.2.4.92"))

temp4 <- merge(ip_address2, temp2, by = "l_ip", all = TRUE)
temp4$city_name <- ifelse(is.na(temp4$city_name), temp4$city, temp4$city_name)
temp4$country_name <- ifelse(is.na(temp4$country_name), temp4$country, temp4$country_name)
temp4$region_name <- ifelse(is.na(temp4$region_name), temp4$region, temp4$region_name)

temp4$timezone <- ifelse(is.na(temp4$timezone.x), temp4$timezone.y, temp4$timezone.x)

# complete IP address table
ip_complete <- data.frame(ip = temp4$l_ip, country = temp4$country_name, region = temp4$region_name, city = temp4$city_name, timezone = temp4$timezone)
ip_complete$country <- ifelse(ip_complete$country == "US", "United States", ip_complete$country)

write.csv(file = "~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 2/ip_addresses.csv", ip_complete, row.names = F)


# complete data
mobile_activity2 <- merge(mobile_activity, ip_complete, by.x = "l_ip", by.y = "ip")
mobile_activity2$l_country <- ifelse(mobile_activity2$l_country == "", mobile_activity2$country, mobile_activity2$l_country)
mobile_activity2$l_region <- ifelse(mobile_activity2$l_region == "", mobile_activity2$region, mobile_activity2$l_region)
mobile_activity2$l_city <- ifelse(mobile_activity2$l_city == "", mobile_activity2$city, mobile_activity2$l_city)

mobile_activity2$l_country <- ifelse(mobile_activity2$l_country == "", NA, mobile_activity2$l_country)
mobile_activity2$l_region <- ifelse(mobile_activity2$l_region == "", NA, mobile_activity2$l_region)
mobile_activity2$l_city <- ifelse(mobile_activity2$l_city == "", NA, mobile_activity2$l_city)

mobile_activity2 <- select(mobile_activity2, -c("city", "country", "region"))
colnames(mobile_activity2)[names(mobile_activity2) == "timezone"] <- c("t_timezone")
mobile_activity2 <- mobile_activity2[c("user", "t_time", "t_timezone", "p_cost", "p_price", "r_bytes_in", "r_bytes_out", "r_response_time", "e_error_code", "l_ip", "l_country", "l_region", "l_city")]

web_activity2 <- merge(web_activity, ip_complete, by.x = "l_ip", by.y = "ip")
web_activity2$l_country <- ifelse(web_activity2$l_country == "", web_activity2$country, web_activity2$l_country)
web_activity2$l_region <- ifelse(web_activity2$l_region == "", web_activity2$region, web_activity2$l_region)
web_activity2$l_city <- ifelse(web_activity2$l_city == "", web_activity2$city, web_activity2$l_city)

web_activity2$l_country <- ifelse(web_activity2$l_country == "", NA, web_activity2$l_country)
web_activity2$l_region <- ifelse(web_activity2$l_region == "", NA, web_activity2$l_region)
web_activity2$l_city <- ifelse(web_activity2$l_city == "", NA, web_activity2$l_city)

web_activity2 <- select(web_activity2, -c("city", "country", "region"))
colnames(web_activity2)[names(web_activity2) == "timezone"] <- c("t_timezone")
web_activity2 <- web_activity2[c("user", "t_time", "t_timezone",  "p_cost", "p_price", "r_bytes_in", "r_bytes_out", "r_response_time", "e_error_code", "l_ip", "l_country", "l_region", "l_city")]


# write files
write.csv("~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 2/mobile_activity.csv", x = mobile_activity2, na = "NA", row.names = FALSE)
write.csv("~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 2/web_activity.csv", x= web_activity2, na = "NA", row.names = FALSE)



