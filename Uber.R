##############################################################################################################
#
#                                                  Uber Case Study
#
##############################################################################################################


master_data <- read.csv("Uber request data.csv", stringsAsFactors = F)
summary(master_data)

# Question 1: Make a grouped bar chart depicting the hour-wise trip request made at city and airport respectively. 
#             You can aggregate the data for all 5 days on the same axis of 24 hours. 
#             Each bar should correspond to an hour and pick-up point (city / airport) should be displayed in two colours?
library(plyr)
library(ggplot2)
library(scales)
#Store hourly data in a separate vector
hourly_data <- as.POSIXct(master_data$Request.time, format="%H")
master_data[, c("hourly.data")] <- as.numeric(format(hourly_data, "%H"))
ggplot(data = master_data, aes(x=master_data$hourly.data, fill=Pickup.point )) + geom_bar(position = "dodge", width = 1) + labs(x= "Hour of the Day", y="Number of trips")
    
            
#Question 2: In the bar chart (question 1), you’ll be able to see 5 major time blocks based on the frequency of requests 
#            made at the city and airport. You have to now divide the request-time into 5 time-slots described below. 
#            Make an additional column “Time_Slot” which takes these 5 categorical values depending on the request time
Time_Slot <- c();
for (x in master_data$hourly.data) {
  if(0<= x && x <= 5){
    Time_Slot <- append(Time_Slot, "Pre_Morning")
  }
  else if(6<= x && x <= 10){
    Time_Slot <- append(Time_Slot, "Morning_Rush")
  }
  else if(11<= x && x <= 16){
    Time_Slot <- append(Time_Slot, "Day_Time")
  }
  else if(17<= x && x <= 20){
    Time_Slot <- append(Time_Slot, "Evening_Rush")
  }
  else if(21<= x && x <= 23){
    Time_Slot <- append(Time_Slot, "Late_Night")
  }
}
master_data[, c("Time_Slot")] <- Time_Slot

#Question 3: Make a stacked bar chart where each bar represents a time slot and the y-axis shows the frequency of requests. 
#            Different proportions of bars should represent the completed, cancelled and no cars available out of the 
#            total customer requests.
ggplot(data = master_data, aes(x=Time_Slot, fill = Status)) + geom_bar()

#counts of the number of trips made during the time slots
trips_completed <-master_data[master_data$Status=="Trip Completed",]
ggplot(data = trips_completed, aes(x=Time_Slot, fill = Status)) + geom_bar()
aggregate(Status~Time_Slot, trips_completed, length)

#Question 4: Visually identify the 2 most pressing problems for Uber, out of the 15 possible scenarios
#
# ANSWER: Two most pressing are problems are as follows
#         1) Large number of cancellations in the morning rush hours  (between 6am to 10 am)
#         2) Not enough cars availabe during evening rush hours (between 17:00 to 20:00 hours)

#Question 5: For the time slot when problem 1 exists, plot a stacked bar chart to find out if the 
#           problem is more severe for pick-up requests made at the airport or the city. 
#           As a next step, you have to determine the number of times this issue exists in that time slot. 
#           Also find the percentage breakup for the total number of issues in this time slot based on the 
#           pick-up point?

ggplot(master_data[which(master_data$Time_Slot=="Morning_Rush"),], aes(x=Pickup.point , fill = Status)) + geom_bar() 
#ANSWER : As can be seen, the number of cancellations occur more in the city. 



#Number of times it cancellations occur during the morning rush hour slot
cancelled_trips <- master_data[master_data$Status=="Cancelled" & master_data$Time_Slot=="Morning_Rush" ,]
cancelled_trips_morning_hours <- count(cancelled_trips$Status) 
#ANSWER: 707 cancellations


#Percentage of cancellations based on pick up point during morning rush hour
#calculate percentage:
p<- ggplot(master_data[which(master_data$Time_Slot=="Morning_Rush"),], aes(x = Pickup.point, fill = Status)) +
    geom_bar()
ggplot_build(p)$data[[1]]

freq = ggplot_build(p)$data[[1]]
freq$y_pos = (freq$ymin + freq$ymax) / 2

p + annotate(x=freq$x, y=freq$y_pos, label=round((freq$count)/(sum(freq$count)),2), geom="text", size=3)

#ggplot(master_data[which(master_data$Time_Slot=="Morning_Rush"),], aes(x = Pickup.point, fill = Status)) +
#  geom_bar(aes(y = (..count..)/sum(..count..))) +
#  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -7.55) +
#  labs(title = "City vs. Airport Cancellations ", y = "Percent", x = "Pickup point")
#ANSWER : 96.5% of cancellations during the morning rush hours occur in the city between 6 to 10 am
#         Only 3.5% cancellations occur from the airport during the same time period.

#Question 6: Now let’s find out the gap between supply and demand. For this case, 
#            the demand is the number of trip requests made at the city, whereas the supply 
#            is the number of trips completed from city to the airport?

demand <- length(master_data[master_data$Pickup.point=="City" & master_data$Time_Slot=="Morning_Rush",]$Request.id)
supply <- length(master_data[master_data$Pickup.point=="City" & master_data$Time_Slot=="Morning_Rush" & master_data$Status=="Trip Completed",]$Request.id)

# ANSWER: Gap between demand (1457) and supply(434) is 1023

#Question 7: What do you think is the reason for this issue for the supply-demand gap? 
#           (Write the answer in less than 100 words).?
# ANSWER:   Please refer word document attached

#Question 8: 4.	What is your recommendation to Uber
# ANSWER:   Please refer word document attached

##############################
#
#        PROBLEM 2
#
###############################

#Question 1:  	For the time slot when problem 2 exists, plot the stacked bar chart to find out 
#               if the issue is for pick-up request made at the airport or the city. 
#               Just like problem 1:
ggplot(master_data[which(master_data$Time_Slot=="Evening_Rush"),], aes(x=Pickup.point , fill = Status)) + geom_bar() 
#ANSWER : As can be seen, unavailability largely occurs at the airport during evening rush hours

#calculate percentage:
q<- ggplot(master_data[which(master_data$Time_Slot=="Evening_Rush"),], aes(x = Pickup.point, fill = Status)) +
  geom_bar()
ggplot_build(q)$data[[1]]

freq = ggplot_build(q)$data[[1]]
freq$y_pos = (freq$ymin + freq$ymax) / 2
q + annotate(x=freq$x, y=freq$y_pos, label=round((freq$count)/(sum(freq$count)),2), geom="text", size=3)

#Question 2: Now let’s find out the gap between supply and demand. For this case, 
#            the demand is the number of trip requests made at the city, whereas the supply 
#            is the number of trips completed from city to the airport?
demand_evening <- length(master_data[master_data$Pickup.point=="Airport" & master_data$Time_Slot=="Evening_Rush",]$Request.id)
supply_evening <- length(master_data[master_data$Pickup.point=="Airport" & master_data$Time_Slot=="Evening_Rush" & master_data$Status=="Trip Completed",]$Request.id)
#Answer : demand : 1514; supply : 329
