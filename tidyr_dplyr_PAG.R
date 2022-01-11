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

#what kind of stuff do we want to do w/ datasets?

#indexing--finding specific rows & columns
#find first row, fifth column
basic.mongooses[1,5]
#find all data in 47th row
basic.mongooses[47,]
#find all data in "n_adults" column
basic.mongooses[,5] #using indexing
basic.mongooses$n.adults #using $ operator
#find all rows where n_adults > 10
basic.mongooses[basic.mongooses$n.adults>10,]
#find only the focal groups with n_adults>10 and mean_age>500
basic.mongooses$focal[basic.mongooses$n.adults>10 & basic.mongooses$mean.age>500]

#how do we do similar stuff in tidyr?
#finding specific rows and columns using filter and select
#try finding rows where n_adults > 10
mongooses%>%filter(n.adults>10) #how is this different from line 28 above?
#find only the focal groups with n_adults>10 and n_males>5
mongooses%>%filter(n.adults>10, mean.age>500)%>%select(focal) #WOO THAT IS EASIER

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
merge(mongooses, rival.mongooses, by = "igi.event")
#tidyr, w/ some improvements on the above
left_join(mongooses, rival.mongooses, by = c("igi.event", "focal", "rival", "daten"))
#make sure you assign this to a new dataframe!
new.mongooses <- left_join(mongooses, rival.mongooses, by = c("igi.event", "focal", "rival", "daten"))

#how do we summarise some data? 
#for example, what's the highest mean age value for each focal group?
#base R
aggregate(x = new.mongooses$mean.age, by = list(new.mongooses$focal), FUN=max)
#tidyR
new.mongooses%>%group_by(focal)%>%summarise(oldest.mean.age = max(mean.age))

#how do we make a new variable?
#what's the relative (focal - rival) n_adults for each fight?
#base R
new.mongooses$rel.n.adults <- new.mongooses$n.adults - new.mongooses$n.rival.adults
#tidyR -- NOTE YOU ALWAYS HAVE TO ASSIGN TO THE DATAFRAME AGAIN FOR THIS TO SAVE YOUR NEW COLUMN!
new.mongooses <- new.mongooses%>%mutate(rel.n.adults = n.adults - n.rival.adults)
