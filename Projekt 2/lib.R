## @knitr lib

cdi <- read.delim("CDI.txt")
cdi$region <- factor(cdi$region, levels = c(1, 2, 3, 4),
                     labels = c("Northeast", "Midwest", "South", "West"))
cdi$crm1000 <- 1000 * cdi$crimes / cdi$popul

median = summary(cdi$crm1000)["Median"]

cdi <- cbind(cdi, hicrm = ifelse(cdi$crm1000 < median, 0, 1))