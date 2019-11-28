uber_data <- read.csv("Uber Request Data.csv",header = T,stringsAsFactors = F)

# Converting the data set to lowercase
uber_data <- sapply(uber_data, tolower)
#Storing it back as data frame
uber_data <- data.frame(uber_data, stringsAsFactors = F)

# Loading required packages 
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
# 1. Data Cleaning
# This helps excluding any duplicate rows and keeps distinct rows
uber_data <- unique(uber_data)

sum(duplicated(uber_data))

# .Checking for blank or Missing values

sum(is.na(uber_data)) # NA is normal in thise case since there cancelled trips either by cutomer or driver

sapply(uber_data, function(x) length(which(x == ""))) # checking for blank "" values; there are none

#  Checking individual columns

str(uber_data$Request.id) # can't see any problems

str(uber_data$Pickup.point)
# Convert uber_data$Pickup.point into a factor
uber_data$Pickup.point <- factor(uber_data$Pickup.point)
str(uber_data$Pickup.point)

str(uber_data$Driver.id)
#Convert uber_data$Driver.id into a factor
uber_data$Driver.id <- factor(uber_data$Driver.id)
str(uber_data$Driver.id)

str(uber_data$Status)
#Convert uber_data$Status into a factor
uber_data$Status <- factor(uber_data$Status)
str(uber_data$Status)

str(uber_data$Request.timestamp)

# Correcting the date format for Request.timestamp
uber_data$Request.timestamp <- parse_date_time(x = uber_data$Request.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"))
str(uber_data$Request.timestamp)

# Correcting the date format for Drop.timestamp
uber_data$Drop.timestamp <- parse_date_time(x = uber_data$Drop.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"))
str(uber_data$Drop.timestamp)
#Extracting day and time from Request.timestamp
uber_data$day <- weekdays(uber_data$Request.timestamp) # day common for request and drop

#splitting date & time from Request.timstamp and Drop.timestamp respectively
uber_data$R_D.Date <- as.Date(uber_data$Request.timestamp)# again date common for request 

uber_data$Req.T <- format(uber_data$Request.timestamp,"%H:%M:%S")

uber_data$Drp.T <-format(uber_data$Drop.timestamp,"%H:%M:%S")

# Figuring out the hours from request and drop timestamp respectively
uber_data$Req.hrs <- as.factor(substring(uber_data$Req.T,0,2))
uber_data$drop.hrs <- as.factor(substring(uber_data$Drp.T,0,2))
# Filtering the data for analysis
uber_data <- uber_data[,c(-5,-6)]

# To find the what is the most pressing problem pickup point airport
cancel_apt <- ggplot(filter(uber_data, uber_data$Pickup.point == "airport" & uber_data$Status == "cancelled"),
       aes(x = Req.hrs)) + geom_bar( position = position_dodge(2),fill = "blue") + labs(title ="cancellation at airpot")

nocar_apt<- ggplot(filter(uber_data, uber_data$Pickup.point == "airport" & uber_data$Status == "no cars available"),
                   aes(x = Req.hrs)) + geom_bar( position = position_dodge(2),fill = "red") 



# To find the demand and supply from airport Note: Status =='trip completed' is considered as supply
demand_supply_airport <- ggplot(filter(uber_data, uber_data$Pickup.point == "airport"),
                 aes(x = Req.hrs, fill = Status)) + geom_bar( position = position_dodge(2)) +
                 labs(title ="Demand and Supply at Airport")+
                 theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)





# To find the demand and supply from cityt Note: Status =='trip completed' is considered as supply
demand_supply_city <- ggplot(filter(uber_data, uber_data$Pickup.point == "city"),
                      aes(x = Req.hrs, fill = Status)) + geom_bar( position = position_dodge(2))+
                      labs(title ="Demand and Supply at City")+
                      theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
  



# Over all trend of demand and supply from city and airport
grid.arrange(demand_supply_airport , demand_supply_city) 


#1. Most pressing problems from Uber:
# You can see in "Demand  and Supply at Airport" plot from Airport to City 'no cars available' is most pressing cause. 
# Whereas in the "Demand and Supply at City" plot you can see from City to Airport 'cancelled' is most pressing cause.
#2. Time slots where the gap is highest:
#You can see in "Demand  and Supply at Airport" plot from Airport to City during evening between 5-10pm demand and supply gap is the most.
# In the "Demand and Supply at City" plot you can see from City to Airportduring morning between 4-8am demand and supply gap is the most.
#3.Requests of airport-city has the most severe gap in the identified time slots as you can see when we compare both by 
#grid.arrange(demand_supply_airport , demand_supply_city) plot.

# Exporting clean data frame for operations in tableau
write.csv(uber_data, "Uber Request Clean Data.csv", na = "")
