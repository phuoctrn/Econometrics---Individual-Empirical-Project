library(car)
library(dplyr)
library(fpp3)
library(tidyr)
library(pastecs)
library(VIM)
library(FactoMineR)
library(missMDA)
library(naniar)
library(strucchange)



#Import data
data <- read.csv("/Users/phuoctran/Desktop/BE ASM2/ass2_data_V2.csv")
data[,'TOTPOP'] <- data[,'TOTPOP'] %>% as.numeric()


summary(data)
## Replacing NA value with latest value and subset required country group and year.

A2013_notcleaned <- data %>% filter(GROUP == "A" & YEAR == "2013")
summarynotcleaned <- format(stat.desc(A2013_notcleaned), scientific = FALSE,save = TRUE)
write.csv(summarynotcleaned,"/Users/phuoctran/Desktop/BE ASM2/summaryA2013_notcleaned.csv")

res<-summary(aggr(A2013_notcleaned, sortVar=TRUE))$combinations
matrixplot(A2013_notcleaned, sortby = 2)
gg_miss_upset(A2013_notcleaned)

A2013 <- data %>% group_by(COUNTRY) %>%
  fill(names(.),.direction = c("updown")) %>% ungroup() %>%
  filter(GROUP == "A" & YEAR == "2013")


plot(log(A2013$PCHE),A2013$DEPRATIO)
lines(lowess(log(A2013$PCHE),A2013$DEPRATIO), col="blue")

plot(log(A2013$PCHE),A2013$DEPRATIO)
lines(lowess(log(A2013$PCHE),A2013$DEPRATIO), col="blue")



write.csv(A2013, "/Users/phuoctran/Desktop/BE ASM2/ A2013.csv" )
A2013 <- A2013 %>% mutate("Log(GDPPC)" = log(GDPPC))
A2013 <- A2013 %>% mutate("Log(CBR)" = log(CBR))



A2013 <- A2013 %>% mutate("Log(PCHE)" = log(PCHE))
#correlation metrix
cormetrix <- cor(A2013[,c('Log(PCHE)','Log(CBR)','URBAN','DEPRATIO','Log(GDPPC)','NETODA')])
write.csv(cormetrix,"/Users/phuoctran/Desktop/BE ASM2/correlation_matrix.csv")


par(mfrow=c(2,2))
hist(A2013_notcleaned$PCHE)
hist(A2013$PCHE)
boxplot(A2013_notcleaned$PCHE,horizontal = TRUE)
boxplot(A2013$PCHE, horizontal = TRUE)

PCHE_NA <- format(stat.desc(A2013_notcleaned$PCHE), scientific = FALSE)
PCHE_no_NA <- format(stat.desc(A2013$PCHE), scientific = FALSE,save = TRUE)
write.csv(PCHE_NA,"/Users/phuoctran/Desktop/BE ASM2/PCHE with NA.csv")
write.csv(PCHE_no_NA,"/Users/phuoctran/Desktop/BE ASM2/PCHE with no NA.csv")

DEPRATIO_NA <- format(stat.desc(A2013_notcleaned$DEPRATIO), scientific = FALSE)
DEPRATIO_no_NA <- format(stat.desc(A2013$DEPRATIO), scientific = FALSE)
write.csv(DEPRATIO_NA,"/Users/phuoctran/Desktop/BE ASM2/DEPRATIO with NA.csv")
write.csv(DEPRATIO_no_NA,"/Users/phuoctran/Desktop/BE ASM2/DEPRATIO with no NA.csv")

  
summary <- format(stat.desc(A2013), scientific = FALSE,save = TRUE)
write.csv(summary,"/Users/phuoctran/Desktop/BE ASM2/summaryA2013.csv")
summary(A2013)



#Log transformation
## Historgram distribution before and after taking natural log
par(mfrow=c(2,2))
hist(A2013$PCHE, main = "PCHE before log transformation", xlab = "current US$")
hist(log(A2013$PCHE), main = "PCHE after log transofmation", xlab = "log(PCHE)")
hist(A2013$GDPPC, main = "GDPPC before log transformation", xlab = "current US$")
hist(log(A2013$GDPPC), main = "GDPPC after log transformation", xlab = "log(GDPPC)")



hist(A2013$CBR, main = "CBR before log transformation", xlab = "per 1000 people")
hist(log(A2013$CBR), main = "CBR after log transformation", xlab = "log(CBR)")

##Explanatory vs Dependent Log
par(mfrow=c(2,2))
plot(A2013$PCHE,A2013$CBR,pch=20, xlab = "PCHE (current US$)", ylab = "CBR (per 1000 people)", main = "PCHE vs CBR before Log transformation")
lines(lowess(A2013$PCHE,A2013$CBR), col="blue")
plot(log(A2013$PCHE),log(A2013$CBR),pch=20, xlab = "Log(PCHE)", ylab="Log(CBR)", main = "PCHE vs CBR after Log transformation")
lines(lowess(log(A2013$PCHE),log(A2013$CBR)), col="blue")

plot(A2013$PCHE,A2013$GDPPC,pch=20, xlab = "PCHE (current US$)", ylab = "GDPPC (current US$)", main = "PCHE vs GDPPC before Log transformation")
lines(lowess(A2013$PCHE,A2013$GDPPC), col="red")
plot(log(A2013$PCHE),log(A2013$GDPPC),pch=20, xlab= "log(PCHE)", ylab="PCHE",main = "PCHE vs GDPPC after Log transformation" )
lines(lowess(log(A2013$PCHE),log(A2013$GDPPC)), col="red")

nolog <- lm(data = A2013, PCHE ~ CBR+GDPPC)
log <- lm(data = A2013, log(PCHE)~ log(CBR) + log(GDPPC))

par(mfrow=c(1,2))
plot(nolog$fitted.values,nolog$residuals, main = "PCHE ~ CBR + GDPPC model's residuals vs fitted values plot", xlab = "Fitted values", ylab = "Residuals")
lines(lowess(nolog$fitted.values,nolog$residuals), col="red")
plot(log$fitted.values,log$residuals, main = "log(PCHE) ~ log(CBR) + log(GDPPC) model's residuals vs fitted values plot", xlab = "Fitted values", ylab = "Residuals")
lines(lowess(log$fitted.values,log$residuals), col="red")

bptest(nolog)
bptest(log)

par(mfrow=c(1,2))
boxplot(A2013$GDPPC,horizontal = TRUE, xlab = "Current US$", main = "GDPPC before log transform")
boxplot(log(A2013$GDPPC),horizontal = TRUE, xlab = "log(GDPPC)", main = "GDPPC after log transform")



#descriptive statisitcs
par(mfrow=c(2,3))
boxplot(A2013$PCHE,horizontal = T, main = "PCHE", xlab = "current US$")
boxplot(A2013$CBR,horizontal = T, main = "CBR",xlab = "per 1000 people")
boxplot(A2013$URBAN,horizontal = T,main = "URBAN", xlab = "% of total population")
boxplot(A2013$DEPRATIO,horizontal = T,main = "DEPRATIO", xlab = "% of total population")
boxplot(A2013$GDPPC,horizontal = T,main = "GDPPC" ,xlab = "current US$")
boxplot(A2013$NETODA,horizontal = T, main = "NETODA", xlab = "current US$" )

#Model 1
m1 <- lm(data = A2013, log(PCHE) ~ 
           log(CBR) + URBAN + DEPRATIO +log(GDPPC) + NETODA )

summary(m1)



#MLR1
plot(m1$fitted.values,m1$residuals,main = "Residuals vs Fitted values plot of model 1",xlab = "fitted values",ylab = "residuals")
lines(lowess(m1$fitted.values,m1$residuals), col="blue")



#MLR3 no perfect collinearity
vif(m1)
vif_values <- vif(m1)
barplot(vif_values, ylim=c(0,15), main = 'VIF values', horiz = FALSE)
abline(h=5, lwd = 3, lty = 2, col = "blue")
abline(h=10, lwd = 3, lty = 2, col = "red")



#Model 2
#Calculate DepRatio mediano

##Generate HDP Dummy Variable
HDP <- ifelse(A2013$DEPRATIO > median(A2013$DEPRATIO),1,0)

A2013 <- A2013 %>% mutate("HDP" = HDP)


m2 <- lm(data = A2013,log(PCHE) ~ 
           log(CBR) + URBAN + HDP +log(GDPPC) + NETODA )
summary(m2)

write.csv(tidy(summary(m2)),"/Users/phuoctran/Desktop/BE ASM2/m2model.csv")

#Adjusted R-squared:  0.8356 F-statistic: 37.59 on 5 and 31 DF

#Model 3



m3 <- lm(data = A2013,log(PCHE) ~ 
           log(CBR) + URBAN + HDP + 
           log(GDPPC) + NETODA + log(GDPPC)*HDP)

summary(m3)
write.csv(tidy(summary(m3)),"/Users/phuoctran/Desktop/BE ASM2/m3model.csv")

#Adjusted R-squared:  0.8246  F-statistic: 25.17 on 7 and 29 DF

#Model 4

plot(A2013$DEPRATIO,log(A2013$PCHE),main = "Scatterplot log(PCHE) vs DEPRATIO",xlab = "DEPRATIO", ylab ="log(PCHE)")
lines(lowess(A2013$DEPRATIO,log(A2013$PCHE)), col="blue")



m4 <- lm(data = A2013, log(PCHE) ~ log(CBR) + URBAN + log(GDPPC) + DEPRATIO + I(DEPRATIO^2) + NETODA)
summary(m4)

write.csv(tidy(m4),"/Users/phuoctran/Desktop/BE ASM2/summarym4.csv")



# Best model

#multicollinearity test
vif_values_m1 <- vif(m1)
vif_values_m2 <- vif(m2)
vif_values_m3 <- vif(m3)
vif_values_m4 <- vif(m4)

par(mfrow=c(2,2))
barplot(vif_values_m1, ylim=c(0,15), main = 'Model 1 VIF values', horiz = FALSE)
abline(h=5, lwd = 3, lty = 2, col = "blue")
abline(h=10, lwd = 3, lty = 2, col = "red")
barplot(vif_values_m2, ylim=c(0,15), main = 'Model 2 VIF values', horiz = FALSE)
abline(h=5, lwd = 3, lty = 2, col = "blue")
abline(h=10, lwd = 3, lty = 2, col = "red")
barplot(vif_values_m3, ylim=c(0,15), main = 'Model 3 VIF values', horiz = FALSE)
abline(h=5, lwd = 3, lty = 2, col = "blue")
abline(h=10, lwd = 3, lty = 2, col = "red")
barplot(vif_values_m4, ylim=c(0,15), main = 'Model 4 VIF values', horiz = FALSE)
abline(h=5, lwd = 3, lty = 2, col = "blue")
abline(h=10, lwd = 3, lty = 2, col = "red")

#Homoskedastic BP test
bptest(m1)
bptest(m2)
bptest(m3)
bptest(m4)






