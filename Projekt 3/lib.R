library(ggplot2)
library(knitr)
library(dplyr)
library(purrr)
library(pscl)
library(GGally)
library(gridExtra)

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
    plot(x, rep(0, n), type = "n", axes = FALSE, ylim = c(0, 1.243), xaxt = "n",
         xlab = "", ylab = "")
    lines(x, PseudoR2, col = "blue", lty = 1, lwd = 2)
    lines(x, sensitivity, col = "green", lty = 1, lwd = 2)
    lines(x, specificity, col = "green", lty = 2, lwd = 2)
    best.model <- 3
    axis(side=4, at = pretty(c(0, 1)))
    mtext("Percent", side=4, line=3)
    
    legend("topright", legend=c("BIC", "AIC", "Pseudo R^2", "Specificity", "Sensitivity"),
           text.col=c("red","red", "blue", "green", "green"), lty=c(1, 2, 1, 2, 1),col=c("red","red", "blue", "green", "green"), cex = 2)
  })
}

calc.confint.beta <- function(model) {
  coeff <- summary(model)$coefficients
  return (cbind(beta = coeff[, "Estimate"], confint(model), "P-value" = coeff[, "Pr(>|z|)"]))
}
calc.confint.OR <- function(confint.beta) {
  return (cbind(OR = exp(confint.beta[, "beta"]), exp(confint.beta[, c("2.5 %", "97.5 %")]), "P-value" = confint.beta[, c("P-value")]))
}

calc.pred.prob <- function(model, x0) {
  pred <- predict(model, x0, se.fit = TRUE, type = "response")
  
  standard.error <- 1.96
  ci.prob <- cbind("2.5 %" = pred$fit - standard.error * pred$se.fit, 
                   "97.5 %" = pred$fit + standard.error * pred$se.fit)
  
  return (cbind(x0, pred$fit * 100, ci.prob * 100))
}


qq <- function(model, main, x, y) {
  model.plot <- model
  influence.plot <- influence(model)
  
  xb <- predict(model.plot)
  
  r <- influence.plot$pear.res / sqrt(1 - influence.plot$hat)
  ds <- influence.plot$dev.res / sqrt(1 - influence.plot$hat)
  
  plot(qqnorm(r), main = main, xlab = x, ylab = y)
  qqline(r)
}

QQtable <- function(models) {
   table <- lapply(models, QQ)
}

cook.plotter <- function(model, dataset, m) {
  n <- nrow(dataset)
  data <- dataset
  cook.plot <- cooks.distance(model)
  xb <- predict(model)
  
  with(data, plot(cook.plot ~ xb, ylab = "Cook's distance", main = m))
  abline(h = c(1, 4 / n), col = "red", lty = 2)
}

cooks.plots <- function(models) {
  
  
}

dfbeta.plot <- function(model, dataset, i.a) {
  n <- nrow(dataset)
  xb <- predict(model)
  dfb <- dfbetas(model)
  
  plot(dfb[, "(Intercept)"] ~ xb, ylim = c(-1, 1), main = "DFbeta.Intercept")
  abline(h = c(-1, -2/sqrt(n), 0, 1, 2 / sqrt(n)), col = "red", lty = 3)
  
  # if(pc == 1) {
  #   plot(dfb[, "log.fare"] ~ xb, ylim = c(-1, 1), main = "DFbeta.log.fare")
  #   abline(h = c(-1, -2/sqrt(n), 0, 1, 2 / sqrt(n)), col = "red", lty = 3)
  # } 
  plot(dfb[, "age"] ~ xb, ylim = c(-1, 1), main = "DFbeta.age")
  abline(h = c(-1, -2/sqrt(n), 0, 1, 2 / sqrt(n)), col = "red", lty = 3)
  plot(dfb[, "sexfemale"] ~ xb, ylim = c(-1, 1), main = "DFbeta.sex.female")
  abline(h = c(-1, -2/sqrt(n), 0, 1, 2 / sqrt(n)), col = "red", lty = 3)
  plot(dfb[, "pclass2"] ~ xb, ylim = c(-1, 1), main = "DFbeta.pclass2")
  abline(h = c(-1, -2/sqrt(n), 0, 1, 2 / sqrt(n)), col = "red", lty = 3)
  plot(dfb[, "pclass3"] ~ xb, ylim = c(-1, 1), main = "DFbeta.pclass3")
  abline(h = c(-1, -2/sqrt(n), 0, 1, 2 / sqrt(n)), col = "red", lty = 3)
  if(i.a == 0) {
    plot(dfb[, "siblings.spouses"] ~ xb, ylim = c(-1, 1), main = "DFbeta.siblings.spouses")
    abline(h = c(-1, -2/sqrt(n), 0, 1, 2 / sqrt(n)), col = "red", lty = 3)
  }
  if(i.a == 1) {
    plot(dfb[, "is.alone"] ~ xb, ylim = c(-1, 1), main = "is.alone")
    abline(h = c(-1, -2/sqrt(n), 0, 1, 2 / sqrt(n)), col = "red", lty = 3)
  }
  
}

create.table.png <- function(table, table.name) {
  file.name <- paste("tables/", table.name, ".png", sep = "")
  
  rows <- NROW(table)
  cols <- NCOL(table)
  
  png(filename = file.name, height = 50 * rows, width = 200 * cols, bg = "white")
  grid.table(table)
  dev.off()
}

split.dataset <- function(dataset) {
  ## 75% of the sample size
  smp_size <- floor(0.75 * nrow(dataset))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
  
  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]
  
  return (list(train, test))
}
