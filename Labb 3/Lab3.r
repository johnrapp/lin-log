cdi <- read.delim("CDI.txt")
cdi$region <- factor(cdi$region, levels = c(1, 2, 3, 4),
                     labels = c("Northeast", "Midwest", "South", "West"))
cdi$phys1000 <- 1000 * cdi$phys / cdi$popul
cdi$crm1000 <- 1000 * cdi$crimes / cdi$popul


##--------A-----------##
plot(phys1000 ~ percapitaincome, data = cdi, main ="Phys vs percapitaincome")
plot(phys1000 ~ crm1000, data = cdi, main ="Phys vs crm1000")
plot(phys1000 ~ pop65plus, data = cdi, main ="Phys vs pop65plus")

plot(log(phys1000) ~ percapitaincome, data = cdi, main ="log(Phys) vs percapitaincome")
plot(log(phys1000) ~ crm1000, data = cdi, main ="log(Phys) vs crm1000")
plot(log(phys1000) ~ pop65plus, data = cdi, main ="log(Phys) vs pop65plus")

##------B--------#

model1 <- lm(log(phys1000) ~ percapitaincome, data = cdi)
model2 <- lm(log(phys1000) ~ crm1000, data = cdi)
model3 <- lm(log(phys1000) ~ pop65plus, data = cdi)
model4 <- lm(log(phys1000) ~ percapitaincome + crm1000, data = cdi)
model5 <- lm(log(phys1000) ~ percapitaincome + pop65plus, data = cdi)
model6 <- lm(log(phys1000) ~ crm1000 + pop65plus, data = cdi)
model7 <- lm(log(phys1000) ~ percapitaincome + crm1000 + pop65plus, data = cdi)
model8 <- lm(log(phys1000) ~ 1, data = cdi)

model1Radj <- summary(model1)$adj.r.squared
model2Radj <- summary(model2)$adj.r.squared
model3Radj <- summary(model3)$adj.r.squared
model4Radj <- summary(model4)$adj.r.squared
model5Radj <- summary(model5)$adj.r.squared
model6Radj <- summary(model6)$adj.r.squared
model7Radj <- summary(model7)$adj.r.squared
model8Radj <- summary(model8)$adj.r.squared

model1BIC <- AIC(model1, k = log(440))
model2BIC <- AIC(model2, k = log(440))
model3BIC <- AIC(model3, k = log(440))
model4BIC <- AIC(model4, k = log(440))
model5BIC <- AIC(model5, k = log(440))
model6BIC <- AIC(model6, k = log(440))
model7BIC <- AIC(model7, k = log(440))
model8BIC <- AIC(model8, k = log(440))

##------C--------##

> plot(influence(model7)$hat) #ja någon sticker ut! -> vi letar efter den

plot(influence(model7)$hat ~cdi$percapitaincome)
plot(influence(model7)$hat ~cdi$crm1000) #Kings seem to have a very high crm1000
plot(influence(model7)$hat ~cdi$pop65plus)

## conslusion -> Kings (NY) is the outlier and therefore has too much leverage

##-----D-------##

studRes <- rstudent(model7)
plot(rstudent(model7) ~ cdi$percapitaincome)
plot(rstudent(model7) ~ cdi$crm1000)
plot(rstudent(model7) ~ cdi$pop65plus)

pred <- predict(model7)
plot(studRes ~ pred)
abline(h=c(-2,2))

##------E--------##

Iout <- which(cdi$crm1000 > 250 )  # get the outlier
Iout2 <- which(studRes > 4 )  # get the outlier

model7i <- lm(log(phys1000) ~ percapitaincome + crm1000 + pop65plus, data = cdi[-c(Iout,Iout2),])
studResi <- rstudent(model7i)

predi <- predict(model7i)
plot(studResi ~ predi)
abline(h=c(-2,2))


r_stud <- rstudent(model7)

plot(r_stud, ylim = c(-7, 7), xlab = "i", ylab = "r*_i",
     main = "stud. residuals, +/- 2")
points(Iout, r_stud[Iout], col = "red", pch = 19)
points(Iinf, r_stud[Iinf], col = "green", pch = 19)
abline(h = 0)
abline(h = c(-2, 2), col = "red")

##------F-----##
D <- cooks.distance(model7)
Di <- cooks.distance(model7i)
plot(D ~ cdi$percapitaincome)
plot(Di ~ cdi[-c(Iout,Iout2),]$percapitaincome)
plot(D ~ cdi$crm1000)
plot(Di ~ cdi[-c(Iout,Iout2),]$crm1000)
plot(D ~ cdi$pop65plus)
plot(Di ~ cdi[-c(Iout,Iout2),]$pop65plus)

