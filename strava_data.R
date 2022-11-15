library(data.table)
library(stringr)
library(zoo)
library(dplyr)
library(ggplot2)
library(scales)

activities.dt <- fread("C:/Users/kyler/Downloads/activities.csv")
colnames(activities.dt) <- str_replace_all(colnames(activities.dt),
                                                   "\\s", "_")
runs.dt <- activities.dt[(Activity_Type == "Run")]
runs.dt[,distance_miles := as.numeric(Distance_A)/1.60934]
runs.dt$Calories <- as.numeric(runs.dt$Calories)
runs.dt$Elevation_Gain <- as.numeric(runs.dt$Elevation_Gain)

basic.run.dt <- runs.dt[distance_miles >= 10]
long.run.dt <- runs.dt[distance_miles >= 15]
too.far.dt <- runs.dt[distance_miles >= 20]

runs.dt[,Average_Distance := cummean(distance_miles)]
runs.dt[,Average_Climbing := cummean(ifelse(is.na(Elevation_Gain), 0, Elevation_Gain))]
runs.dt[,Date_Split := str_split(Activity_Date,"[\\s\\,]")]
runs.dt[,c("Month", "Date", "Null", 'Year', "AlsoNull", "Time", "Time_Of_Day") := transpose(Date_Split)]
runs.dt$Month <- match(runs.dt$Month, month.abb)
runs.dt[, Full_Date := paste(Month, Date, Year, sep="/")]
runs.dt$Full_Date <- as.Date(runs.dt$Full_Date, format = "%m/%d/%Y")


plot.dt <- runs.dt[,.(Full_Date, Average_Distance, Average_Climbing)]

ggplot(plot.dt, aes(x= Full_Date))+
  geom_line(aes(y=Average_Distance))+
  xlab("Time")+
  ylab("Distance (Miles)")+
  ggtitle("Kyle's Average Run Distance")+
  scale_x_date(labels = date_format('%Y-%m'), date_breaks = "3 months")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


plot.dt <- runs.dt[,.(Calories, distance_miles, Elevation_Gain)]

ggplot(plot.dt)+
  geom_point(aes(Calories, distance_miles))+
  ylab("Distance (Miles)")

# plot.dt <- runs.dt[,.(Average_Distance, Average_Climbing)]
# 
# ggplot(plot.dt)+
#   geom_point(aes(Average_Distance, Average_Climbing))+
#   ylab("Distance (Miles)")


