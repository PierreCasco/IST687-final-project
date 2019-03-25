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
library('lattice')

#Function to plot wins
doublePlot <- function(s,y,w,df,n){
  obj1 <- xyplot(round(s/1000000,digits = 1) ~ y, df, main = n,
                 xlab = "Year", ylab = "Salary (Millions)", type = 'l')
  obj2 <- xyplot(w ~ y, df, xlab = "Year", ylab = "Wins", type = 'l')
  dPlot <- doubleYScale(obj1,obj2, add.ylab2 = TRUE)
  return(dPlot)
}

#Function to plot attendance
doublePlot.a <- function(s,y,w,df,n){
  obj1 <- xyplot(round(s/1000000,digits = 1) ~ y, df, main = n,
                 xlab = "Year", ylab = "Salary (Millions)", type = 'l')
  obj2 <- xyplot(w/1000000 ~ y, df, xlab = "Year", ylab = "Attendance (Millions)", type = 'l')
  dPlot <- doubleYScale(obj1,obj2, add.ylab2 = TRUE)
  return(dPlot)
}

#Plot salary and wins
kershawPlot <- doublePlot(kershaw$salary, kershaw$yearID, kershaw$W, kershaw, "Kershaw")
arodPlot <- doublePlot(arod$salary, arod$yearID, arod$W, arod, "Rodriguez")
greinkePlot <- doublePlot(greinke$salary,greinke$yearID,greinke$W,greinke, "Greinke")
pricePlot <- doublePlot(price$salary,price$yearID,price$W, price, "Price")
cabreraPlot <- doublePlot(cabrera$salary,cabrera$yearID,cabrera$W, cabrera, "Cabrera")

#Combine all salary/wins plots in one
grid.arrange(kershawPlot, arodPlot, greinkePlot, pricePlot, cabreraPlot, 
             nrow = 3, top = "Salary and Team Wins")

#Plot salary and team attendance
kershaw.att <- doublePlot.a(kershaw$salary, kershaw$yearID, kershaw$attendance, kershaw, "Kershaw")
arod.att <- doublePlot.a(arod$salary, arod$yearID, arod$attendance, arod, "Rodriguez")
greinke.att <- doublePlot.a(greinke$salary, greinke$yearID, greinke$attendance, greinke, "Greinke")
price.att <- doublePlot.a(price$salary, price$yearID, price$attendance, price, "Price")
cabrera.att <- doublePlot.a(cabrera$salary, cabrera$yearID, cabrera$attendance, cabrera, "Cabrera")

#Combine all salary/attendance plots in one
grid.arrange(kershaw.att, arod.att, greinke.att, price.att, cabrera.att, 
             nrow = 3, top = "Salary and Team Attendance (Year Total)")

#Create new dataframe with salary and batting stats
allSalary <- merge(Salaries, Batting, by = c("playerID", "yearID", "lgID", "teamID"))
allSalary <- allSalary[order(allSalary$yearID),]

#Run Random Forest on salary
allSalary.rf <- randomForest(salary ~ teamID + G + AB + R + H + HR + RBI, allSalary,ntree = 500)
importance(allSalary.rf)

#At first glance, the teamID being the most important seemed strange. But after some thought, it made sense. In baseball
#there is no salary cap so teams like the Yankees, Red Sox, and Cardinals can throw large sums of money at players.


#Run decision tree to predict World Series Winners
#Create data set using all World Series winners
WSWinners <- sqldf("Select * from Teams where WSWin = 'Y'")

#Create data set with modern info
modernWSWinners <- sqldf("Select * from Teams where WSWin = 'Y' and yearID > '1970'")

#Read in 2018 stats info
library('readxl')
library('knitr')
b <- ('2018 batting.xlsx')

#Create data frame for batting and rename columns
batting <- read_xlsx(b)
batting <- as.data.frame(batting)
names(batting)[10] <- "X2B"
names(batting)[11] <- "X3B"

#Pitching data
p <- ('2018 pitching.xlsx')
pitching <- read_xlsx(p)
pitching <- as.data.frame(pitching)

#Get Win/Loss data and merge with batting data frame
record <-  as.data.frame(pitching$Tm,pitching$W,pitching$L)
record$Tm <- pitching$Tm
record$W <- pitching$W
record$L <- pitching$L
batting <- merge(batting, record, by = "Tm") 

#Create decision tree for batting info, all data
library(rpart)
fit <- rpart(franchID ~ R + H + X2B + X3B + HR + AB + 
               SB + W + L, data = WSWinners, method = "class")
dt <- predict(fit, batting, type = "class")
summary(dt)

#Create decision tree for batting info, modern data
modernFit <- rpart(franchID ~ R + H + X2B + X3B + HR + AB + 
                     SB + W + L, data = modernWSWinners, method = "anova")
mdt <- predict(modernFit, batting, type = "class")
summary(mdt)

#Random Forest with modern data
a <- droplevels(modernWSWinners)

a.rf <- randomForest(franchID ~ R + H + X2B + X3B + HR + AB + 
               SB, a, ntree = 500)
importance(a.rf)
a.p <- predict(a.rf,batting,type = "class")

