
#1.	Install the package "hflights" and load the library 
library(hflights)
library(dplyr)
data(hflights)
head(hflights)
#2.	Convert to a local data frame (use command tbl_df)
flights <- tbl_df(hflights)
#3.	Convert to a normal data frame to see all of the columns
data.frame(head(flights))
#4.	base R approach & dplyr approach to view all flights on January 1st
# base R approach to view all flights on January 1

flights[flights$Month==1 & flights$DayofMonth==1, ] 

# dplyr approach
# note: you can use comma or ampersand to represent AND condition
filter(flights, Month==1, DayofMonth==1)

#5.	Select those flights where UniqueCarrier is "UA" or "AA"
filter(flights, UniqueCarrier %in% c("AA", "UA"))

#6.	Pick columns by name - DepTime, ArrTime, FlightNum using base r and dplyr
#base r approach
flights[, c("DepTime", "ArrTime", "FlightNum")]

# dplyr approach
select(flights, DepTime, ArrTime, FlightNum)
#	select UniqueCarrier and DepDelay columns and filter for delays over 60 minutes in dplyr
filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)
#or
# chaining method
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)
#	use mutate to create a new variable Speed (in mph) and  display distance ,airtime and speed columns
# dplyr approach (prints the new variable but does not store it)
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)

#9.	store the data into variable "flights"
flights <- flights %>% mutate(Speed = Distance/AirTime*60)
#	create a dataset grouped by Dest, and then summarise each group by taking the mean of ArrDelay
# dplyr approach: create a table grouped by Dest, and then summarise #each group by taking the mean of ArrDelay
flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))


#for each day of the year, count the total number of flights and sort in descending order

flights%>%group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))
          
          
#  12.	for each destination, count the total number of flights and the number of distinct planes that flew there
flights %>%group_by(Dest) %>%
            summarise(flight_count = n(), plane_count = n_distinct(TailNum))
      #13.	 for each destination, show the number of cancelled and not cancelled flights
          flights %>%
            group_by(Dest) %>%
            select(Cancelled) %>%
            table() %>%
            head()
          