#WEB ACTIVITY
#Time Data:             timestamp
#Price:                 cost; price
#Response Time:         request_bytes; response_bytes; response_time_microseconds
#Disconnection/Errors:  4xx_status
#Location:              src_ip; Country; City; Region

#MOBILE ACTIVITY
#Time Data:             _time
#Price:                 cost; price
#mobile:                carrier; device
#Response Time:         requestLength; responseLength; latency (microseconds)
#Disconnection/Errors:  4xx_error
#Location:              remoteIP

#ip_location
#Location:              remoteIP; City; Country; Region

webdata <- read.csv("~/Queen's University/GROUP-Team Alfred - General/MMA 860/Team Project/Final Dataset For Project/Older Versions/all_web_activity_v1.csv")
mdata <- read.csv("~/Queen's University/GROUP-Team Alfred - General/MMA 860/Team Project/Final Dataset For Project/Older Versions/all_mobile_activity_v1.csv")
ipdata <- read.csv("~/Queen's University/GROUP-Team Alfred - General/MMA 860/Team Project/Final Dataset For Project/Older Versions/ip_locations.csv")

mipdata <- merge(mdata, ipdata, by = "remoteIP")


library(tidyverse)
web_activity <- select(webdata,
                       user = User_ID,
                       t_time = timestamp,
                       p_cost = cost, p_price = price,
                       r_bytes_in = request_bytes, r_bytes_out = response_bytes, r_response_time = response_time_microseconds,
                       e_error_code = X4xx_status,
                       l_ip = src_ip, l_country = Country, l_region = Region, l_city = City
                       )

mobile_activity <- select(mipdata,
                          user = user,
                          t_time = X_time,
                          p_cost = cost, p_price = price,
                          r_bytes_in = requestLength, r_bytes_out = responseLength, r_response_time = latency,
                          e_error_code = X4xx_error,
                          l_ip = remoteIP, l_country = Country, l_region = Region, l_city = City
                          )

write.csv(web_activity, "~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 1/web_activity.csv", na = "", row.names = F)
write.csv(mobile_activity, "~/OneDrive - Queen's University/MMA/MMA 860/Team Project/Step 1/mobile_activity.csv", na = "", row.names = F)


