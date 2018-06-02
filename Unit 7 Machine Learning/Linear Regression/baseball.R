baseball <- read.csv("baseball.csv")
str(baseball)
moneyball <- subset(baseball, Year < 2002)
str(moneyball)
moneyball$rd <- moneyball$RS - moneyball$RA
str(moneyball)
plot(moneyball$rd,moneyball$W)

WinsReg <- lm(W ~ rd, data = moneyball)
summary(WinsReg)
  
RunsReg <- lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsReg)

teamRank <- c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank,wins2012)
cor(teamRank,wins2013)
