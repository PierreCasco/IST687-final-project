library('Lahman')

#Get 30 greatest salaries
playerSalary <- head(Salaries[order(-Salaries$salary),],30)

#Merge with player info to get name
playerSalary <- merge(playerSalary,Master, by="playerID")

