library(ggplot2)
library(knitr)
library(dplyr)
library(purrr)
library(pscl)
library(GGally)

plot.correlation.model <- function(formula, dataset, variable.name, range) {
  
  model <- lm(formula, data = dataset)
  
  plot(formula, data = dataset)
  
  x0 <- seq(range[1], range[2], 0.1)
  predx <- setNames(data.frame(x = x0), variable.name)
  predx <- cbind(predx, conf = predict(model, predx, interval = "confidence"))
  predx <- cbind(predx, pred = predict(model, predx, interval = "prediction"))
  with(predx, {
    lines(predx[, variable.name], conf.fit, lty = 1, col = "blue", lwd = 2)
  })
  with(predx, {
    lines(predx[, variable.name], conf.lwr, lty = 2, col = "red", lwd = 2)
    lines(predx[, variable.name], conf.upr, lty = 2, col = "red", lwd = 2)
  })
  # add prediction interval lines
  with(predx, {
    lines(predx[, variable.name], pred.lwr, lty = 3, col = "blue", lwd = 2)
    lines(predx[, variable.name], pred.upr, lty = 3, col = "blue", lwd = 2)
  })
  
  return (model)
}

to.prob <- function(odds) {
  return (odds / (1 + odds))
}

plot.binary.model <- function(formula, dataset, x, y, variable.name, range, bandwidth) {
  plot(formula, data = dataset)
  lines(ksmooth(y, x, bandwidth = bandwidth))
  
  model <- glm(formula, data = dataset, family = "binomial")
  
  x0 <- seq(range[1], range[2], 1)
  predx <- setNames(data.frame(x = x0), variable.name)
  predx <- cbind(predx, prob = predict(model, predx, type = "response"))
  
  with(predx, lines(predx[, variable.name], prob, col = "blue"))
  
  # calculate conf.int for the linear part x*beta:
  standard.error <- 1.96
  xb <- predict(model, predx, se.fit = TRUE)
  ci.xb <- data.frame(lwr = xb$fit - standard.error * xb$se.fit,
                      upr = xb$fit + standard.error * xb$se.fit)
  
  
  # and finally CI for the probabilities and add to the plot:
  predx <- cbind(predx, to.prob(exp(ci.xb)))
  with(predx, {
    lines(predx[, variable.name], lwr, lty = 2, col = "red")
    lines(predx[, variable.name], upr, lty = 2, col = "red")
  })
  
  return (model)
  
}

calc.sense.spec <- function(model) {
  pred <- titanic
  
  pred <- cbind(pred, prob.survived = predict(model, pred, type = "response"))
  pred <- cbind(pred, pred.survived = ifelse(pred$prob.survived > 0.5, 1, 0))
  
  num.survived <- sum(pred$survived)
  num.non.survived <- nrow(pred) - num.survived 
  
  # Sensitivity
  true.postive <- with(pred, ifelse(pred.survived == 1 & survived == 1, 1, 0))
  sensitivity <- sum(true.postive) / num.survived
  sensitivity
  
  # Specificity
  true.negative <- with(pred, ifelse(pred.survived == 0 & survived == 0, 1, 0))
  specificity <- sum(true.negative) / num.non.survived
  specificity
  
  metrics <- data.frame(
    sensitivity,
    specificity
  )

  return(metrics)
}

calc.metrics <- function(model) {
  calc.aic <- AIC(model)
  calc.bic <- AIC(model, k = log(nrow(dataset)))
  sense.spec <- calc.sense.spec(model)
  
  pseudo.R2 <- pR2(model)["r2CU"]
  
  metrics <- data.frame(
    aic = calc.aic,
    bic = calc.bic,
    sensitivity = sense.spec$sensitivity,
    specificity = sense.spec$specificity,
    PseudoR2 = pseudo.R2
  )
  
  return (metrics)
}

calc.metrics.models <- function(models, model.names) {
  models.metrics <- lapply(models, calc.metrics)
  
  compare.models <- do.call(rbind, models.metrics)
  compare.models <- cbind(model.name = model.names, compare.models)
  rownames(compare.models) <- NULL
  
  return (compare.models)  
}

plot.compare.models <- function(compare.models, axis.range) {
  n <- nrow(compare.models)
  x <- 1:n
  with(compare.models, {
    par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for second axis
    
    plot(x, rep(0, n), type = "n", ylim = axis.range, xaxt = "n",
         ylab = "AIC/BIC", xlab = "Model",
         main = "Model comparison")
    axis(1, at = x, labels = model.name)
    lines(x, aic, col = "red", lty = 2, lwd = 2)
    lines(x, bic, col = "red", lty = 1, lwd = 2)
    
    par(new = TRUE)
    plot(x, rep(0, n), type = "n", axes = FALSE, ylim = c(0, 1), xaxt = "n",
         xlab = "", ylab = "")
    lines(x, PseudoR2, col = "blue", lty = 1, lwd = 2)
    best.model <- 3
    axis(side=4, at = pretty(c(0, 1)))
    mtext("Pseudo R^2", side=4, line=3)
    
    legend("topright",legend=c("BIC", "AIC","Pseudo R^2"),
           text.col=c("red","red", "blue"), lty=c(1, 2, 1),col=c("red","red", "blue"))
  })
}

calc.confint.beta <- function(model) {
  return (cbind(beta = summary(model)$coefficients[, "Estimate"], confint(model)))
}

calc.pred.prob <- function(model, x0) {
  pred <- predict(model, x0, se.fit = TRUE, type = "response")
  
  standard.error <- 1.96
  ci.prob <- cbind("2.5 %" = pred$fit - standard.error * pred$se.fit, 
                   "97.5 %" = pred$fit + standard.error * pred$se.fit)
  
  return (cbind(x0, pred$fit * 100, ci.prob * 100))
}