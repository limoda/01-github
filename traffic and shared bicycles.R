#数据导入#
read.csv("traf.csv")
traf <- read.csv("traf.csv")
traffic <- traf[,3:12]

#OLS回归分析#
lm1 <- lm(bs~pa+pc+busdensity+bus+taxi+rushhour+speed+jam+time+stopcover,data = traf)
print(lm1)
summary(lm1)
library(mvstats)
coef.sd(lm1)

#相关分析#
library(corrplot)
options(digits = 2)
cor1 <- cor(traffic)
print(cor1)
corrplot(cor1,"shade")

#多重共线性检验#
library(car)
vif1 <- vif(lm1)
print(vif1)

#岭回归#
library(MASS)
plot(lm.ridge(bs~pa+pc+busdensity+bus+taxi+rushhour+speed+jam+time+stopcover,data = traf,lambda=seq(0,0.9,0.001)))
select(lm.ridge(bs~pa+pc+busdensity+bus+taxi+rushhour+speed+jam+time+stopcover,data = traf,lambda=seq(0,0.9,0.001)))
rlm <- lm.ridge(bs~pa+pc+busdensity+bus+taxi+rushhour+speed+jam+time+stopcover,data = traf,lambda=seq(0,0.9,0.001),lambda=0.27)
print(rlm)

#因子分析#
fa <- factanal(traffic,factors = 4,rotation = "none")
print(fa)
fa2 <- factanal(traffic,factors = 4,rotation = "varimax")
print(fa2)
fa3 <- factanal(traffic,factors=4,scores = "regression")
print(fa3$scores)
library(mvstats)
rank1 <- factanal.rank(fa3,plot = T)
print(rank1$Ri)
rank2 <- rank1$Ri[,1]
plot(rank2,traf$bs)
cor(rank2,traf$bs)
cor(rank2[-11],traf$bs[-11])

