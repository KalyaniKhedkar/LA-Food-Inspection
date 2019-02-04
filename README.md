# LA-Food-Inspection

-----------------------------
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
 
#Joing the data on basis of serial number   
all_data<-inspections %>% 
          inner_join(violations,by = 'serial_number')
str(all_data)

##*** 
#Sorting the data in a descending way to find the greatest violations
top_codes<-sort(table(all_data$violation_code),decreasing = TRUE)[1:10]
str(top_codes)
df<- data.frame(top_codes)
 colnames(df)<- c('code_name','code_count')

# one step process for above 3 line code
dff<-sort(table(all_data$violation_code),decreasing = TRUE)[1:10]

#Barplot of the top 10 codes  have been distributed over the frequency
barplot(top_codes)

#using ggplot library from tidyverse

#Horizontal plot
ggplot(data=df, aes(x=code_name,y=code_count)) +
  geom_bar(stat="identity") + 
  coord_flip() + ##converts it into Horizontal plot
  ggtitle("Top Codes for Violations are as shown")

#Vertical plot
ggplot(data=df, aes(x=code_name, y=code_count)) + geom_bar(stat="identity")

library(lubridate)

#FIding the date month and year of the inspections
all_data <- all_data %>%
  mutate(activity_date = ymd(activity_date),
         activity_month = ceiling_date(activity_date, "month"),
         activity_year=round_date(activity_date,"year"))

#visualisation of data over the year
all_data %>%
        group_by(activity_year) %>%
         summarise(number_violations=n()) %>%
          ggplot( aes(activity_year,number_violations))+
           geom_line()

#visualisation of violation data over 2 month interval
all_data %>%
  group_by(activity_month) %>%
  summarise(number_violations=n()) %>%
  ggplot( aes(activity_month,number_violations))+
  geom_line()+
  scale_x_date(date_breaks="2 months",date_labels = "%b-%y")+
  labs(title="Violation distribution over with 2 month interval over time")

#***

#determining the month of the activity
all_data<-all_data %>% 
  mutate(month = month(activity_month, TRUE))
str(all_data)
all_data$month

#determining monthly violations
monthly_violations1 <- all_data %>%
  filter(violation_code %in% df$code_name) %>%
  group_by(violation_code,month) %>%
  summarise(num_violations=n())

options(repr.plot.width = 10, repr.plot.height = 6)

#In detail graphicalpresentation of violation trends in LA county.
#where light shade represents high amount and dark represents low number

ggplot(monthly_violations1,aes(month,violation_code,fill=num_violations))+
  geom_tile()+ 
  labs(y = "Violation Code",
       x = "Month of Violation",
       title = "Monthly trends of the top ten health code violations")

View(violations)




