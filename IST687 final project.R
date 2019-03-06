library('Lahman')
library('dplyr')
library('ggplot2')
library('sqldf')

#Get 30 greatest salaries
playerSalary <- head(Salaries[order(-Salaries$salary),],30)

#Merge with player info to get name
playerSalary <- merge(playerSalary,Master, by="playerID")

#Remove NA columns
playerSalary <- playerSalary[,colSums(is.na(playerSalary))<nrow(playerSalary)]

#Create dataframe for 5 top paid players
kershaw <- sqldf("Select * from Salaries where playerID = 'kershcl01'")
arod <- sqldf("Select * from Salaries where playerID = 'rodrial01'")
greinke <- sqldf("Select * from Salaries where playerID = 'greinza01'")
price <- sqldf("Select * from Salaries where playerID = 'priceda01'")
cabrera <- sqldf("Select * from Salaries where playerID = 'cabremi01'")

#Plot salaries
ggplot(kershaw,aes(x=yearID,y=salary)) + geom_line()
ggplot(arod,aes(x=yearID,y=salary)) + geom_line()
ggplot(greinke,aes(x=yearID,y=salary)) + geom_line()
ggplot(price,aes(x=yearID,y=salary)) + geom_line()
ggplot(cabrera,aes(x=yearID,y=salary)) + geom_line()
