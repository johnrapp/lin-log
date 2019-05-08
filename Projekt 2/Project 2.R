library(dplyr)
library(ggplot2)
library(tidyverse)

# Proj2


cdi <- read.delim("CDI.txt")
cdi$region <- factor(cdi$region, levels = c(1, 2, 3, 4),
                     labels = c("Northeast", "Midwest", "South", "West"))
cdi$crm1000 <- 1000 * cdi$crimes / cdi$popul


summary(cdi$crm1000)
median = 52.429

cdi <- cbind(cdi, hicrm = ifelse(cdi$crm1000 < median, 0, 1))
with(cdi, {
   plot(hicrm ~ higrads)
   lines(ksmooth(higrads, hicrm, bandwidth = 20))
})

model <- glm(hicrm ~ higrads, data = cdi, family = "binomial")
summary(model)

x0 <- data.frame(higrads = seq(0, 100, 1))
predx <- cbind(x0, prob = predict(model, x0, type = "response"))
with(predx, lines(higrads, prob, col = "blue"))

# calculate conf.int for the linear part x*beta:
standard_error <- 1.96
xb <- predict(model, x0, se.fit = TRUE)
ci.xb <- data.frame(lwr = xb$fit - standard_error * xb$se.fit,
                    upr = xb$fit + standard_error * xb$se.fit)

# transform to CI for the odds:
ci.odds <- exp(ci.xb)

# and finally CI for the probabilities and add to the plot:
predx <- cbind(predx, ci.odds / (1 + ci.odds))
with(predx, {
  lines(higrads, lwr, lty = 2, col = "red")
  lines(higrads, upr, lty = 2, col = "red")
})

