library('Lahman')
library('dplyr')
library('ggplot2')
library('sqldf')
library('reshape2')
library('gridExtra')

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
arodPlot <- ggplot(arod,aes(x=yearID,y=salary)) + geom_line()
ggplot(greinke,aes(x=yearID,y=salary)) + geom_line()
ggplot(price,aes(x=yearID,y=salary)) + geom_line()
ggplot(cabrera,aes(x=yearID,y=salary)) + geom_line()

#Get team records
teamRecs <- sqldf("Select yearID,teamID,attendance,W from Teams")

#Merge with team record and attendance and sort by year
kershaw <- merge(kershaw,teamRecs,by=c("teamID","yearID"))
kershaw <- kershaw[order(kershaw$yearID),]

arod <- merge(arod,teamRecs,by=c("teamID","yearID"))
arod <- arod[order(arod$yearID),]

greinke <- merge(greinke,teamRecs,by=c("teamID","yearID"))
greinke <- greinke[order(greinke$yearID),]

price <- merge(price,teamRecs,by=c("teamID","yearID"))
price <- price[order(price$yearID),]

cabrera <- merge(cabrera,teamRecs,by=c("teamID","yearID"))
cabrera <- cabrera[order(cabrera$yearID),]

#Plot salary and team wins
library('latticeExtra')
obj1 <- xyplot(kershaw$salary ~ kershaw$yearID, kershaw, xlab = "Year", ylab = "Salary", type = 'l')
obj2 <- xyplot(kershaw$W ~ kershaw$yearID, kershaw, xlab = "Year", ylab = "Wins", type = 'l')
kershawPlot <- doubleYScale(obj1, obj2, text = c("Salary","Wins"), add.ylab2 = TRUE, title = "Kershaw")

obj3 <- xyplot(arod$salary ~ arod$yearID, arod, xlab = "Year", ylab = "Salary", type = 'l')
obj4 <- xyplot(arod$W ~ arod$yearID, arod, xlab = "Year", ylab = "Wins", type = 'l')
arodPlot <- doubleYScale(obj3, obj4, text = c("Salary","Wins"), add.ylab2 = TRUE, title = "Rodriguez")

obj5 <- xyplot(greinke$salary ~ greinke$yearID, greinke, xlab = "Year", ylab = "Salary", type = 'l')
obj6 <- xyplot(greinke$W ~ greinke$yearID, greinke, xlab = "Year", ylab = "Wins", type = 'l')
greinkePlot <- doubleYScale(obj5, obj6, text = c("Salary","Wins"), add.ylab2 = TRUE, title = "Greinke")

obj7 <- xyplot(price$salary ~ price$yearID, price, xlab = "Year", ylab = "Salary", type = 'l')
obj8 <- xyplot(price$W ~ price$yearID, price, xlab = "Year", ylab = "Wins", type = 'l')
pricePlot <- doubleYScale(obj7, obj8, text = c("Salary","Wins"), add.ylab2 = TRUE, title = "Price")

obj9 <- xyplot(cabrera$salary ~ cabrera$yearID, cabrera, xlab = "Year", ylab = "Salary", type = 'l')
obj10 <- xyplot(cabrera$W ~ cabrera$yearID, cabrera, xlab = "Year", ylab = "Wins", type = 'l')
cabreraPlot <- doubleYScale(obj9, obj10, text = c("Salary","Wins"), add.ylab2 = TRUE,title = "Cabrera")

#Combine all plots in one
grid.arrange(kershawPlot,arodPlot,greinkePlot,pricePlot,cabreraPlot, nrow = 2)

#Create new dataframe with salary and batting stats
allSalary <- merge(Salaries, Batting, by = c("playerID", "yearID", "lgID", "teamID"))
allSalary <- allSalary[order(allSalary$yearID),]

#Run Random Forest on salary
allSalary.rf <- randomForest(salary ~ teamID + G + AB + R + H + HR + RBI, allSalary,ntree = 500)
importance(allSalary.rf)

#At first glance, the teamID being the most important seemed strange. But after some thought, it made sense. In baseball
#there is no salary cap so teams like the Yankees, Red Sox, and Cardinals can throw large sums of money at players.

