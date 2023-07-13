mydata <- read.csv(file.choose(), header=TRUE)
str(mydata)

max(mydata$salary)
min(mydata$salary)
max(mydata$experience)
min(mydata$experience)
mydata.men <- subset(mydata, mydata$gender == "male")
mydata.women <- subset(mydata, mydata$gender == "female")
max(mydata.men$salary)
min(mydata.men$salary)
max(mydata.men$experience)
min(mydata.men$experience)

max(mydata.women$salary)
min(mydata.women$salary)
max(mydata.women$experience)
min(mydata.women$experience)

mean(mydata$salary)
mean(mydata$experience)
mean(mydata.men$salary)
mean(mydata.men$experience)
mean(mydata.women$salary)
mean(mydata.women$experience)

plot(salary ~ experience, data=mydata, col=c("black", "red")[as.numeric(mydata$gender)], xlab = "experience (year)", ylab = "Salary (dollar)",pch = 16, main = 'Salary vs Experience')
abline(lm(salary ~ experience, data=mydata.men), lty=1, col = 'red')
abline(lm(salary ~ experience, data=mydata.women), lty=1)
legend("topleft",legend=c("men", "women"),col=c("red", "black"), pch =16:16)

z.full <- lm(salary ~ gender + experience + experience*gender, data=mydata)
summary(z.full)
predict.full <- predict(z.full)
resid.full <- resid(z.full)
plot(resid.full ~ predict.full, pch=16)
abline(0,0, lty=2)

hist(resid.full)
qqnorm(resid.full, ylab= "standardized residuals", xlab = "Normal scores")
qqline(resid.full)

shapiro.test(resid.full) #Shapiro-Wilk test

anova(z.full)
SSE.full <- sum(( mydata$salary - predict.full)^2)
SSE.full
SSreg.full <- sum((predict.full - mean( mydata$salary))^2)
SSreg.full
SSY.full <- sum((mydata$salary - mean(mydata$salary))^2)
SSY.full

(SSreg.full/3)/(SSE.full/44)

z.experience.only <- lm(salary ~ experience, mydata)
anova(z.full, z.experience.only)
pf(33.473, 2, nrow(mydata)-3-1, lower.tail=FALSE)
nrow(mydata)-3-1

z.gender.only <- lm(salary ~ gender, data=mydata)
anova(z.full, z.gender.only)

predict.gender.only <-predict(z.gender.only)
SSreg.gender.only<-sum((predict.gender.only - mean( mydata$salary))^2)
SSreg.gender.only
SSreg.full
SSE.full
SSE.gender.only<-sum((mydata$salary - predict.gender.only)^2)
((930124188 - 681263112)/2)/(125439510/44)


qf(0.95, 2, nrow(mydata)-3-1)

z.same.slope <- lm(salary ~ experience + gender, data=mydata)
predict.same.slope <- predict(z.same.slope)
resid.same.slope <- resid(z.same.slope)
plot(resid.same.slope ~ predict.same.slope, pch=16)
abline(0,0, lty=2)
hist(resid.same.slope)
qqnorm(resid.same.slope, ylab= "standardized residuals", xlab = "Normal scores")
qqline(resid.same.slope)
shapiro.test(resid.same.slope)
anova(z.full,z.same.slope)
qf(0.95, 1, nrow(mydata)-3-1)

summary(z.same.slope)

30230.08 + 4708.07

SSE.same.slope <- sum(( mydata$salary - predict.same.slope)^2)
SSE.same.slope
SSreg.same.slope <- sum((predict.same.slope - mean( mydata$salary))^2)
SSreg.same.slope
SSY.same.slope <- sum((mydata$salary - mean(mydata$salary))^2)
SSY.same.slope
R2<- SSreg.same.slope/SSY.same.slope
R2
anova(z.same.slope)
sqrt(SSE.same.slope/45)

plot(salary ~ experience, data=mydata, col=c("black", "red")[as.numeric(mydata$gender)], xlab = "experience (year)", ylab = "Salary (dollar)",ylim = c(25000,50000), main = "Salary vs Experience",pch=16)
xnew <- seq(min(mydata$experience), max(mydata$experience), length.out = 100)
ynew.pred.men <- data.frame(predict(z.same.slope, newdata = data.frame(experience = xnew, gender='male'), interval = "prediction", level = 0.95))
lines(ynew.pred.men$fit ~ xnew, lty = 1,col="red")
lines(ynew.pred.men$lwr ~ xnew, lty = 2,col="red")
lines(ynew.pred.men$upr ~ xnew, lty = 2, col='red')
ynew.pred.women <- data.frame(predict(z.same.slope, newdata = data.frame(experience = xnew, gender='female'), interval = "prediction", level = 0.95))
lines(ynew.pred.women$fit ~ xnew, lty = 1,col="black")
lines(ynew.pred.women$lwr ~ xnew, lty = 2,col="black")
lines(ynew.pred.women$upr ~ xnew, lty = 2, col='black')
legend("topleft", cex = 0.65,legend = c("male", "fit line(male)", "prediction interval(male)","female","fit line(female)",  "prediction interval(female)"), col = c(2,2,2,1,1,1), pch = c(16,26,26,16,26,26), lty = c(0,1,2,0,1,2), ncol = 2)
summary(mydata)
