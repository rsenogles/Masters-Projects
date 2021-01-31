# IMPORTING AND TIDYING DATA
load("project_data.rdata")
head(lse)
summary(lse)
str(lse)

# MULTICOLLINEARITY CHECK
cor(lse[,-(1:4)])

# FINAL COLLINEARITY CHECK
cor(lse[,-c(1,2,3,4,7,8,11,12,13,17,23,24,26,27,30)])

# REMOVE 7 VARIABLES: NMC,SMT,VOD,MIN,CAN,GVC,SDR
newlse = subset(lse, select = -c(NMC, SMT, VOD, MIN, CNA, GVC, SDR, RB, EXPN, RR, BATS))

# CHECK FOR TRANSFORMATIONS
for(i in names(newlse)[6:ncol(newlse)]){
  plot(BA~get(i), data=newlse)
  readline()
}

# PERFORM TRANSFORMATIONS
# for STJ
newlse["logSTJ"] <- log(newlse$STJ+2)

plot(newlse$BA ~ newlse$STJ)
plot(newlse$BA ~ newlse$logSTJ)


# for DLG
newlse["logDLG"] <- log(newlse$DLG+2)

plot(newlse$BA ~ newlse$DLG)
plot(newlse$BA ~ newlse$logDLG)


# for RMV
newlse["logRMV"] <- log(newlse$RMV+2)

plot(newlse$BA ~ newlse$RMV)
plot(newlse$BA ~ newlse$logRMV)


# for AHT
newlse["logAHT"] <- log(newlse$AHT+2)

plot(newlse$BA ~ newlse$AHT)
plot(newlse$BA ~ newlse$logAHT)

# REMOVE ORIGINAL VARIABLES FROM DATASET - ONLY KEEP TRANSFORMATIONS
newlse = subset(newlse, select = -c(STJ, DLG, RMV, AHT))


# Question 1ai
x <- data.frame(cor(newlse$BA, newlse[6:21]))
tidy_cor <- pivot_longer(x, cols = "PRU":"logAHT", names_to = "Company", values_to = "Correlation")
ggplot(tidy_cor) + geom_col(aes(x=Company, y=Correlation)) + theme(axis.text.x = element_text(size = 8))

# Question 1aii
abs_cor <- tidy_cor
abs_cor$Correlation <- abs(abs_cor$Correlation)
ordered_cor <- abs_cor[order(-abs_cor$Correlation),]

library(DT)
datatable(ordered_cor)


# Question 1bi
model1 <- lm(BA ~ logSTJ + EZJ + logRMV + logAHT + logDLG, data=newlse)
anova(model1)
summary(model1)


# Question 1d

install.packages("leaps")
library(leaps)
str(newlse)

# LEAPS AND BOUNDS SELECTION
leaps1 <- leaps(x=newlse[,6:21], y=newlse[,5], nbest=5, method="adjr2", names=colnames(newlse[6:21]))
leaps2 <- data.frame(Size=leaps1$size, Adjr2=round(leaps1$adjr2, 3), leaps1$which, row.names=NULL)
plot(leaps2$Size~leaps2$Adjr2, xlab="AdjR2 Value", ylab="Model Size", main="Comparison of Model Size & AdjR2 Value")

# BACKWARDS SELECTION
dropmodel <- lm(BA ~ PRU + CCH + CPG + CCL + TUI + LLOY + SSE + SPX + TSCO + RTO + ANTO + logSTJ + logDLG + logRMV + logAHT, data=newlse)
drop1(dropmodel, test="F", scope = ~ PRU + CCH + CPG + CCL + TUI + LLOY + SSE + SPX + TSCO + RTO + ANTO + logSTJ + logDLG + logRMV + logAHT)
# remove EZJ only

# FORWARDS SELECTION
addmodel <- lm(BA ~ logSTJ + EZJ + TSCO + PRU + logDLG + CPG + RTO + logAHT + logRMV + LLOY + SPX + CCH + CCL + SSE + ANTO + TUI, data=newlse)
add1(addmodel, test="F", scope = ~ PRU + CCH + CPG + CCL + TUI + LLOY + SSE + EZJ + SPX + TSCO + RTO + ANTO + logSTJ + logDLG + logRMV + logAHT)
# kept all variables

# STEPWISE SELECTION
stepmodel <- lm(BA ~ 1, data=newlse)
step(stepmodel, scope = ~ PRU + CCH + CPG + CCL + TUI + LLOY + SSE + EZJ + SPX + TSCO + RTO + ANTO + logSTJ + logDLG + logRMV + logAHT, direction="both")
# removed SSE only


# PRODUCE MODELS FOR FORWARDS, BACKWARDS, STEPWISE, LEAPS1 AND LEAPS2 METHODS
forwards_model <- lm(BA ~ PRU + CCH + CPG + CCL + TUI + LLOY + SSE + EZJ + SPX + TSCO + RTO + ANTO + logSTJ + logDLG + logRMV + logAHT, data=newlse)
backwards_model <- lm(BA ~ PRU + CCH + CPG + CCL + TUI + LLOY + SSE + SPX + TSCO + RTO + ANTO + logSTJ + logDLG + logRMV + logAHT, data=newlse)
stepwise_model <- lm(BA ~ PRU + CCH + CPG + CCL + TUI + LLOY + EZJ + SPX + TSCO + RTO + ANTO + logSTJ + logDLG + logRMV + logAHT, data=newlse)
leaps1_model <- lm(BA ~ PRU + CCH + CPG + CCL + LLOY + SSE + SPX + TSCO + RTO + logSTJ + logDLG + logRMV + logAHT, data=newlse)
leaps2_model <- lm(BA ~ PRU + CPG + LLOY + RTO + logSTJ + logDLG + logRMV + logAHT, data=newlse)

# CALCULATE PRESS VALUES FOR ALL MODELS
press1 <- sum((resid(forwards_model)/ (1 - hatvalues(forwards_model)))^2)
press2 <- sum((resid(backwards_model)/ (1 - hatvalues(backwards_model)))^2)
press3 <- sum((resid(stepwise_model)/ (1 - hatvalues(stepwise_model)))^2)
press4 <- sum((resid(leaps1_model)/ (1 - hatvalues(leaps1_model)))^2)
press5 <- sum((resid(leaps2_model)/ (1 - hatvalues(leaps2_model)))^2)

# OBTAIN R-SQ VALUES FOR ALL MODELS
summary(forwards_model)
summary(backwards_model)
summary(stepwise_model)
summary(leaps1_model)
summary(leaps2_model)

# CALCULATE Cp STATISTIC FOR ALL MODELS
cp1 <- ((sum(resid(forwards_model)^2)/ (sum(resid(forwards_model)^2)/(1007-16-1))) + 2*(17 + 1) - 1007)
cp2 <- ((sum(resid(backwards_model)^2)/ (sum(resid(backwards_model)^2)/(1007-16-1))) + 2*(16 + 1) - 1007)
cp3 <- ((sum(resid(stepwise_model)^2)/ (sum(resid(stepwise_model)^2)/(1007-16-1))) + 2*(16 + 1) - 1007)
cp4 <- ((sum(resid(leaps1_model)^2)/ (sum(resid(leaps1_model)^2)/(1007-16-1))) + 2*(14 + 1) - 1007)
cp5 <- ((sum(resid(leaps2_model)^2)/ (sum(resid(leaps2_model)^2)/(1007-16-1))) + 2*(9 + 1) - 1007)


# RE-SAVING LEAPS1 AS THE FINAL MODEL
finalmodel <- lm(BA ~ PRU + CCH + CPG + CCL + LLOY + SSE + SPX + TSCO + RTO + logSTJ + logDLG + logRMV + logAHT, data=newlse)


# Question 1e

# PRODUCING RESIDUAL PLOTS FOR ASSUMPTIONS CHECK
plot(finalmodel, which=2, main = "Q-Q Plot for Testing Normality")
plot(finalmodel, which=1, main = "Residual vs Fitted Plot for Constant Variance")



# Question 2a

predict_BAE <- function(lse, newdata){
  lse$STJlog <- log(lse$STJ+2)
  lse$DLGlog <- log(lse$DLG+2)
  lse$RMVlog <- log(lse$RMV+2)
  lse$AHTlog <- log(lse$AHT+2)
  newdata$STJlog <- log(newdata$STJ+2)
  newdata$DLGlog <- log(newdata$DLG+2)
  newdata$RMVlog <- log(newdata$RMV+2)
  newdata$AHTlog <- log(newdata$AHT+2)
  finalmodel <- lm(BA ~ PRU + CCH + CPG + CCL + LLOY + SSE + SPX + TSCO + RTO + STJ + DLG + RMV + AHT, data=lse)
  predictions <- predict(model1, newdata = newdata)
  return(predictions)
}


# Question 2b

# RE-PERFORMING MULTICOLLINEARITY CHECK - REMOVING DIFFERENT VARIABLES
cor(lse[,-c(1,2,3,4,17,22,26)])
implse = subset(lse, select = -c(RR, SSE, NMC, MIN, SMT))

# RE-PERFORMING TRANSFORMATIONS
implse["logSTJ"] <- log(implse$STJ+2)
implse["logDLG"] <- log(implse$DLG+2)
implse["logRMV"] <- log(implse$RMV+2)
implse["logAHT"] <- log(implse$AHT+2)
implse = subset(implse, select = -c(STJ, DLG, RMV, AHT))

# STEPWISE SELECTION
stepmodel <- lm(BA ~ 1, data=implse)
step(stepmodel, scope = ~ RB + PRU + CCH + EXPN + VOD + GVC + CPG + CCL + TUI + LLOY + SDR + EZJ + BATS + SPX + CNA + RTO + ANTO + logSTJ + logDLG + logRMV + logAHT, direction="both")

# NEW PREDICTION FUNCTION

predict_BAE <- function(lse, newdata){
  lse$STJlog <- log(lse$STJ+2)
  lse$DLGlog <- log(lse$DLG+2)
  lse$RMVlog <- log(lse$RMV+2)
  lse$AHTlog <- log(lse$AHT+2)
  newdata$STJlog <- log(newdata$STJ+2)
  newdata$DLGlog <- log(newdata$DLG+2)
  newdata$RMVlog <- log(newdata$RMV+2)
  newdata$AHTlog <- log(newdata$AHT+2)
  finalmodel <- lm(BA ~ CNA + EZJ + RTO + GVC + CPG + STJ + AHT + ANTO + SDR + SPX + RB + VOD + PRU + DLG + LLOY + RMV + CCH + CCL + EXPN, data=lse)
  predictions <- predict(model1, newdata = newdata)
  return(predictions)
}






