library('Lahman')
library('dplyr')
library('ggplot2')
library('sqldf')
library('reshape2')

#Get 30 greatest salaries
playerSalary <- head(Salaries[order(-Salaries$salary),],30)

#Merge with player info to get name
playerSalary <- merge(playerSalary,Master, by="playerID")

#Remove NA columns
playerSalary <- playerSalary[,colSums(is.na(playerSalary))<nrow(playerSalary)]

#Create dataframe for top 5 paid players
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

#Get team records
teamRecs <- sqldf("Select yearID,teamID,attendance,W from Teams")

#Merge with team record and attendance
kershaw <- merge(kershaw,teamRecs,by=c("teamID","yearID"))
arod <- merge(arod,teamRecs,by=c("teamID","yearID"))
greinke <- merge(greinke,teamRecs,by=c("teamID","yearID"))
price <- merge(price,teamRecs,by=c("teamID","yearID"))
cabrera <- merge(cabrera,teamRecs,by=c("teamID","yearID"))

#Plot salary and team wins
library('latticeExtra')
obj1 <- xyplot(arod$salary ~ arod$yearID, arod)
obj2 <- xyplot(arod$W ~ arod$yearID, arod)
doubleYScale(obj1,obj2,add.ylab2 = TRUE)
