#Project 3
library("ggplot2")
library("GGally")


titanic <- read.csv("titanic.csv")

pred.counties <- subset(cdi, select = c(county, region, hicrm))

subtit <- subset(titanic, select = c(Survived, Pclass, Sex, Age, Siblings.Spouses.Aboard, Parents.Children.Aboard, Fare))

View(subtit)

with(titanic, {
  plot(Survived ~ Fare)
  lines(ksmooth(Fare, Survived, bandwidth = 40))
})

ggpairs(subtit)

plot.binary.model(survived ~ wind, dataset$big.fire, dataset$wind, "wind", c(0, 10), 10)
model.temp <- plot.binary.model(big.fire ~ temp, dataset$big.fire, dataset$temp, "temp", c(0, 35), 10)
model.RH <- plot.binary.model(big.fire ~ RH, dataset$big.fire, dataset$RH, "RH", c(15, 110), 10)