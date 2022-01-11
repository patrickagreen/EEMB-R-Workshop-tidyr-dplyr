#basic R and tidyverse indexing etc....
#created PAG 11 Jan 2021

#load tidyverse
#install.packages("tidyverse")
library(tidyverse)

#set working directory to wherever you have your data
setwd("C:/Users/Patrick/Dropbox (Personal)/Work/Classes and Teaching/UCSB_R_Seminar/EEMB-R-Workshop-tidyr-dplyr/")
basic.mongooses <- read.csv("mongooses.csv")
mongooses <- as_tibble(read.csv("mongooses.csv"))

#check out how R views these differently
basic.mongooses
mongooses  

str(basic.mongooses)
str(mongooses)
str(as.data.frame(mongooses))#important for some functions like nlme that don't recognize tibbles

#what kind of stuff do we want to do w/ datasets?

#indexing--finding specific rows & columns
#find first row, fifth column
basic.mongooses[1,5]
#find all data in 47th row
basic.mongooses[47,]
#find all data in "n_adults" column
basic.mongooses[,5] #using indexing
basic.mongooses$n.adults #using $ operator
#subsetting
#find all rows where n_adults > 10
basic.mongooses[basic.mongooses$n.adults>10,]
#find only the focal groups with n_adults>10 and mean_age>500
basic.mongooses$focal[basic.mongooses$n.adults>10 & basic.mongooses$mean.age>1500]

#how do we do similar stuff in tidyr?
#piping!
#finding specific rows and columns using filter and select
#try finding rows where n_adults > 10
mongooses%>%filter(n.adults>10) #how is this different from line 33 above?
#find only the focal groups with n_adults>10 and n_males>5
mongooses%>%filter(n.adults>10, mean.age>1500)%>%select(focal) #WOO THAT IS EASIER than line 35

#let's compare focal groups to rival groups
#load the rival group data
rival.mongooses <- as_tibble(read.csv("rival.mongooses.csv"))
#inspect datasets
rival.mongooses
mongooses
#how do we differentiate focal from rival data? rename some variables
names(rival.mongooses)[5:6] <- c("n.rival.adults", "mean.rival.age")

#how do we combine the datasets?
#basic R
merge(mongooses, rival.mongooses, by = c("igi.event", "focal", "rival", "daten"))
#tidyr
left_join(mongooses, rival.mongooses, by = c("igi.event", "focal", "rival", "daten"))
#make sure you assign this to a new dataframe!
new.mongooses <- left_join(mongooses, rival.mongooses, by = c("igi.event", "focal", "rival", "daten"))

#a better way to organize data? tidy data
#as "long" as possible--each observation in its own row
mongooses$type <- "focal"
rival.mongooses$type <- "rival"
names(rival.mongooses)[5:6] <- c("n.adults", "mean.age")

tidy.mongooses <- bind_rows(mongooses, rival.mongooses)%>%arrange(igi.event)

#plotting is easier using tidy data
ggplot(data = tidy.mongooses, aes(x = n.adults, y = mean.age, col = type))+
  geom_point()+
  geom_smooth(method="lm")

#how would we plot with untidy data?
ggplot(data = new.mongooses, aes(x=n.adults, y = mean.age))+
  geom_point(col = "blue")+
  geom_smooth(method = "lm", col = "blue")+
  geom_point(aes(x=n.rival.adults, y = mean.rival.age))+
  geom_point(col = "red")+
  geom_smooth(method = "lm", col = "red")
#...it doesn't work very well

#we can also make new variables using tidy data
#what about relative n.adults = focal - rival
#I think this is not the easiest/best way, but it works for me
tidy.mongooses%>%group_by(igi.event, daten, focal, rival)%>%summarise(rel.n.adults = n.adults[type=="focal"]-n.adults[type=="rival"])

#(we can also do this using untidy data)
new.mongooses%>%mutate(rel.n.adults = n.adults - n.rival.adults)

#how do we summarise data? 
#for example, what's the highest mean age value for each focal group?
#base R
aggregate(x = new.mongooses$mean.age, by = list(new.mongooses$focal), FUN=max)
#tidyR
tidy.mongooses%>%filter(type=="focal")%>%group_by(focal)%>%summarise(oldest.mean.age = max(mean.age))