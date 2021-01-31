# load in data
data <- read.csv("ExpDesign.csv")
cows <- subset(data, Student != 202060636 | Student %in% c(NA))
summary(cows)

# change treatment, day and animal to factors
cows$Treatment <-as.factor(cows$Treatment)
cows$Day <-as.factor(cows$Day)
cows$AnimalID <-as.factor(cows$AnimalID)

# re-check the data
str(cows)
summary(cows)

# visualise the data
boxplot(cows$FECs ~ cows$Treatment, xlab="Treatment", 
        ylab="FECs (Eggs per g Faeces)", 
        main = "Comparison of FECs Between Treatments")
boxplot(cows$FECs ~ cows$Day, xlab = "Day", 
        ylab = "FECS (Eggs per g Faeces)", 
        main = "Comparison of FECs Over Time")

# caclulating median values
summary(cows$FECs[cows$Treatment=="DOR"])
summary(cows$FECs[cows$Treatment=="FBZ"])
summary(cows$FECs[cows$Treatment=="IVM"])
summary(cows$FECs[cows$Day=="0"])
summary(cows$FECs[cows$Day=="14"])
summary(cows$FECs[cows$Day=="21"])

# producing the fixed linear model
model1 <- lm(FECs ~ Treatment + AnimalID + Day + Treatment:Day, data=cows)
summary(model1)
anova(model1)

# check assumptions of fixed linear model
par(mfrow=c(2,2))
plot(model1)

# produce a random effect linear model with animalID as the random effect
model2 <- aov(FECs ~ Treatment + Day + Treatment:Day + Error(AnimalID), data=cows)
summary(model2)

# since this suggests difference, will produce interaction plots
par(mfrow=c(1,1))
with(cows, interaction.plot(Day, Treatment, FECs))

# produce the lme model with animal ID as a random effect
install.packages("lme4")
library(lme4)
model3 <- lmer(FECs ~ Treatment + Day + Treatment:Day + (1|AnimalID), data=cows) 
summary(model3)
anova(model3)

