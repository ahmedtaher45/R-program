#Setting work directory
setwd("C:/Users/ahmad/OneDrive/سطح المكتب/Task")
getwd()

#Reading Dataset
G2 <- read.csv('G2_anthropometry.csv',na.strings = c(''))

#viewing Dataset
View(G2)

#replacing cm in gender with m
G2$gender[G2$gender=="cm"]="M"

#filtering dataset according to female members
femaleFilter <- G2[G2$gender=="F",]

#To filter according to height we need to remove cm from height
G2$height <- gsub("cm",'',G2$height)

#filtering dataset according to height and foot_length
heightfilter <- G2[G2$height>=140 & G2$foot_length>=190 ,]

#filtering dataset according to female members without printing age and fppt_length columns
femaleFilterNoAge <- G2[G2$gender=="F",-c(1,3)]

#Recoding gender column
G2$gender2[G2$gender == 'F'] = '1'
G2$gender2[G2$gender == 'cm'] = '2'

# doing some analysis of age column and making factor
G2$AgeRanges[G2$age<8]="young"
G2$AgeRanges[G2$age>=8 & G2$age<13]="midAged"
G2$AgeRanges[G2$age>=13]="Old"
G2$FactorAge <- as.factor(G2$AgeRanges)
levels(G2$FactorAge)= c("young","midAged","Old")

#Recoding of code
G2$AgeRanges2[G2$AgeRanges == "young"] = '1'
G2$AgeRanges2[G2$AgeRanges == "midAged"] = '2'
G2$AgeRanges2[G2$AgeRanges == "Old"] = '3'

#Doing some Sorting
SortG2 <- G2[order(G2$age,G2$height),]

#getting missing values in foot_length
missingValues<- G2[is.na(G2$foot_length),]

#Drop missing values rows 
G2_new <- G2[!is.na(G2$foot_length),]

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#To solve missing values problem in scatter plot
#we use a removed missing values virgin of dataset (G2_new) 
draw1 <- ggplot(G2_new, aes(x = foot_length, y= age))

#Drawing scatter chart
draw1 + geom_point(aes(color=gender)) + stat_smooth(se=FALSE)

#Drawing histogram chart of foot lengths
draw_hist <- ggplot(G2, aes(foot_length))
draw_hist + geom_histogram(binwidth = 6,color="orange",fill="purple")+ggtitle("the length of people's foot")+labs(x="foot length", y="Numbers")

#Drawing bar Chart 
draw_bar <- ggplot(G2 ,aes(x=age,fill =gender))
draw_bar + geom_bar() + ggtitle("gender to age") + theme_light() + facet_wrap(~ AgeRanges)










