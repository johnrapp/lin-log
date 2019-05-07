sleep <- read.delim("sleep.txt")
sleep$Danger <- factor(sleep$Danger, levels = c(1, 2, 3),
                       labels = c("low", "medium", "high"))

# a.) Frequency table
table(sleep$Danger)
relevel(sleep$Danger,"medium")
table(sleep$Danger)
#b.) Daily sleep for the different danger categories (Mean+Boxplot)
aggregate(TotalSleep ~ Danger, data = sleep, FUN = "mean")
plot(sleep$TotalSleep ~ sleep$Danger, xlab = "Danger (from others) category",
     ylab = "Daily amount of sleep (hours)",
     main = ("Sleep and danger")) 


#c.) Linear Regression Model. Low danger is basecase with expected value B0. 
#E[TotalSleep,i]=β0+β1*(Dangermedium,i)+β2*(Dangerhigh,i)+εi
DangerVsSleepmodel <- lm(TotalSleep ~ Danger, data = sleep)
summary(DangerVsSleepmodel)
#We obtain the following estimates
#B0=13.3824
#B1=-2.2324
#B2=-6.6449 
#Thus acoording to model average sleep for medium danger 
#animals are 13.3824-2.2324=11.15 h and for high 6.7375 h
#Averages are the same as in b.)

#d.) ANOVA table generated below shows that the hypothesis H0:B1=B2=0 
#can be discarded since the P-value of the F-test is less than 0.001
#This means that either B1 and B2 is singnificantly nonzero and that the 
#Danger variable is useful
anova(DangerVsSleepmodel)

#e.) # 95% Confidence interval for the betas is generated below
 ##     low        high
##B0  11.481046   15.283660
##B1  -4.717423   0.252717
##B2  -9.375398   -3.914308
confint(DangerVsSleepmodel)

#f.) Sleep-Weight relationship
# lin-lin plot, cannot be described by a function y=kx+m
plot(sleep$TotalSleep ~ sleep$BodyWt, xlab = "Bodyweight (kg)",
     ylab = "Daily amount of sleep (hours)",
     main = ("Lin-lin plot of Sleep and bodyweight"))
#lin-log plot expected value can maybe be described by a function y=kx+m
#This works better since the weight increases proportionally much much more 
#(k>>1) than sleep in the lin-lin model.Log-lin fixes this problem a bit
sleep$lnOfBodyWt=log(sleep$BodyWt)
plot(sleep$TotalSleep ~ sleep$lnOfBodyWt, xlab = "ln(Bodyweight (kg))",
     ylab = "Daily amount of sleep (hours)",
     main = ("Log-lin plot of Sleep and bodyweight"))

#g.) Model with danger+weight with no interactions is plotted below
WeightSleepmodel <- lm(TotalSleep ~ lnOfBodyWt, data = sleep)
DangerWeightSleepmodel <- lm(TotalSleep ~ Danger+lnOfBodyWt, data = sleep)
anova(WeightSleepmodel,DangerWeightSleepmodel)
anova(DangerVsSleepmodel,DangerWeightSleepmodel)
##The partial F-test results seen in the Anova table above says
#that we can discard the hypothesis that either the danger or weight variable 
#is superfluous if we have the other 
#(thus the model where both is used better)
#Lets se if interactions between the two variales improve our model
DangerWeightSleepmodelwithI<- lm(TotalSleep ~ Danger*lnOfBodyWt, data = sleep)
anova(DangerWeightSleepmodel,DangerWeightSleepmodelwithI)
##The partial F-test shows in this case that the hypothesis that
#the interactionbetas are=0 cannot be discarded. 
#Since the interactions does not significantly contribute we remove them 
#from our model
with(sleep, plot(TotalSleep ~ lnOfBodyWt, main = ("Log-lin plot of Sleep and bodyweight"),ylim = c(0, 30), xlab = "ln(Bodyweight (kg))",
                 ylab = "Daily amount of sleep (hours)"))
with(subset(sleep, Danger == "medium"), 
     points(lnOfBodyWt, TotalSleep, col = "red", pch = 19))
with(subset(sleep, Danger == "high"),
     points(lnOfBodyWt, TotalSleep, col = "blue", pch = 19))
with(subset(sleep, Danger == "low"),
     points(lnOfBodyWt, TotalSleep, col = "black", pch = 19))
with(DangerWeightSleepmodel, abline(a = coefficients[1], b = coefficients["lnOfBodyWt"],
                      col = "black", lwd = 2))
with(DangerWeightSleepmodel, abline(a = coefficients[1] + coefficients[2],
                      b = coefficients["lnOfBodyWt"], col = "red", lwd = 2))
with(DangerWeightSleepmodel, abline(a = coefficients[1] + coefficients[3],
                      b = coefficients["lnOfBodyWt"], col = "blue", lwd = 2))
legend(-6, 30, c("low danger: beta0+beta3*x", "medium danger: beta0+beta1+beta3*x",
                "high danger: beta0+beta2+beta3*x"),cex=0.8,
       col = c("black", "red", "blue"), lty = 1, lwd = 2)


#h.)  95% Confidence interval for the betas in this new model is generated below
##These intervals are almost all greater than those for the previous model
##Perhaps the fact that the ln(bodyweight) almost is a quadratic fucntion of the 
#danger level plays a role in this
##     low        high
##B0  12.473654 15.8978733
##B1  -5.399272 -0.9464831
##B2  -7.933106 -3.0112907
##B3  -1.003242 -0.3499164

## Can be compared to the Betas for the model without 
#a weight variable 
##     low        high
##B0  11.481046   15.283660
##B1  -4.717423   0.252717
##B2  -9.375398   -3.914308
confint(DangerWeightSleepmodel)
with(sleep, plot(lnOfBodyWt~Danger))

##i.) Residual analysis
#Most plots generated look very good. Only the last plot (residuals~danger) is a bit
#worrying where the larger variance and the mean>0 of the low danger 
#category does not quite follow our assumptions. 
e <- DangerWeightSleepmodel$residuals
hist(e )
qqnorm(e )
qqline(e )
plot(e ~ sleep$lnOfBodyWt)
plot(e ~ sleep$Danger)

# j.)95% Prediction interval for a 30 kg(ln(30)=3.401197) medium danger species
##Result: E[Sleep]=11.39343 9.469113 13.31775
x0 <- data.frame(Danger = "medium", lnOfBodyWt=3.401197)
y0pi <- predict(DangerWeightSleepmodel, x0, interval = "prediction")
y0pi

# k.)95% Confidence interval for a 62 kg(ln(62)=4.127134) low danger human
##Result: E[Sleep]=8.2 (lwr:6.2 upr:10.2)
x00 <- data.frame(Danger = "low", lnOfBodyWt=4.127134)
y0ci <- predict(DangerWeightSleepmodel, x00, interval = "confidence")
y0ci

#l.) Estimated mean of difference in sleep: 5.5 h
#Confidence interval lwr 3.0 upr 8.0


Variancematrix=summary(DangerWeightSleepmodel)$cov.unscaled
xMarmot<- matrix(c(1, 0, 0,1.386294), nrow = 1)
xVervet<- matrix(c(1, 0, 1,1.386294), nrow = 1)
Betas<-matrix(c(14.1858, -3.1729,-5.4722,-0.6766 ), nrow = 4)
EstimatedSleepDifference=xMarmot%*%Betas-xVervet%*%Betas
DiffxMxV=xMarmot-xVervet #Row vector
TDiffxMxV=t(DiffxMxV) #Column vector
t=qt( 0.975, 53 ) #t-value 95 %, 53 df
s<-3.428 #Residual standard error
DiffStDev=s*sqrt(DiffxMxV%*%Variancematrix%*%TDiffxMxV)
lowerend=EstimatedSleepDifference-t*DiffStDev
upperend=EstimatedSleepDifference+t*DiffStDev
lowerend
upperend

#m.)
# E=1.854452
#lower 0.9591152
#upper 2.74979
xHuman<- matrix(c(1, 0, 0,4.127134), nrow = 1)
EstimatedSleepDifference1=xMarmot%*%Betas-xHuman%*%Betas
DiffxMxH=xMarmot-xHuman
TDiffxMxH=t(DiffxMxH)
DiffStDev1=s*sqrt(DiffxMxH%*%Variancematrix%*%TDiffxMxH)
lowerend1=EstimatedSleepDifference1-t*DiffStDev1
upperend1=EstimatedSleepDifference1+t*DiffStDev1
lowerend1
upperend1
EstimatedSleepDifference1

#n.) calculated below
#Theoretical -3.617748
#Lower -6.42339
#Upper -0.8121055

EstimatedSleepDifference2=xVervet%*%Betas-xHuman%*%Betas
DiffxVxH=xVervet-xHuman
TDiffxVxH=t(DiffxVxH)
DiffStdev2=s*sqrt(DiffxVxH%*%Variancematrix%*%TDiffxVxH)
lowerend2=EstimatedSleepDifference2-t*DiffStdev2
upperend2=EstimatedSleepDifference2+t*DiffStdev2
lowerend2
upperend2
EstimatedSleepDifference2


