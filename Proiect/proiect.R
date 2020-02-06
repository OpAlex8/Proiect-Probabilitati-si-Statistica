#----------------------------- Problema 1 -------------------------------

plot(rock)

colNames <- colnames(rock)

message(cat("MEANS AND VARIANCES OF ALL PROPERTIES\n"))
colNumber <- 1
for(colName in colNames) {
  currCol <- rock[, colNumber:colNumber]
  message(cat("The mean of", colName, "is", mean(currCol),
              "and the variance of", colName, "is", var(currCol)))
  colNumber <- colNumber + 1
}
message(cat("\n"))

message(cat("QUARTILES OF ALL PROPERTIES\n"))
colNumber <- 1
for(colName in colNames) {
  currCol <- rock[, colNumber:colNumber]
  message(cat("The quartiles of", colName, "are:\n"))
  print(quantile(currCol))
  message("\n")
  colNumber <- colNumber + 1
}

boxplot(rock$area, data = rock, main="Area", staplewex = 1)
boxplot(rock$peri, data = rock, main="Peri", staplewex = 1)
boxplot(rock$shape, data = rock, main="Shape", staplewex = 1)
boxplot(rock$perm, data = rock, main="Perm", staplewex = 1)

boxplot(area~perm, data = , main="Area~Perm", xlab="Area", ylab="Perm", staplewex = 1)
boxplot(peri~perm, data = rock, main="Peri~Perm", xlab="Peri", ylab="Perm", staplewex = 1)
boxplot(shape~perm, data = rock, main="Shape~Perm", xlab="Shape", ylab="Perm", staplewex = 1)

#----------------------------- Problema 2 -------------------------------
attach(rock)
library(tidyverse)
library(ggplot2)
library(stats)

message(cat("LINEAR REGRESSIONS"))


dev.new()
ggplot(rock,aes(x = peri, y = area)) +
  geom_point() +
  stat_smooth(method = lm)

dev.new()
ggplot(rock,aes(x = peri, y = perm)) +
  geom_point() +
  stat_smooth(method = lm)

dev.new()
ggplot(rock,aes(x = shape, y = perm)) +
  geom_point() +
  stat_smooth(method = lm)

#Testare regresii liniare simple
shape_reg <- lm(rock$shape ~ rock$perm)
summary (shape_reg)

peri_reg <- lm (rock$peri ~ rock$perm)
summary (peri_reg)


#Testare regresii liniare multiple
all_reg <- lm (rock$perm ~ rock$peri + rock$area + rock$shape)
summary (all_reg)
reg1 <- lm (rock$perm ~ rock$peri + rock$shape)
summary (reg1)
reg2 <- lm (rock$perm ~ rock$peri + rock$area)
summary (reg2)
reg3 <- lm (rock$perm ~ rock$area + rock$shape)
summary (reg3)


#Comparare intre regresia simpla si cea multipla + coeficientii celor 2

#Regresia simpla
message(cat("SIMPLE LINEAR REGRESSION peri ~ perm PROPERTIES:"))
simple_reg = lm(rock$peri ~ rock$perm)
summary(simple_reg)
simple_reg_coeff = coefficients(simple_reg)
free_coef = round (simple_reg_coeff[1],1)
simple_reg_equation = paste0("y = " ,
                             round(simple_reg_coeff[2],1), " *x + " , 
                             free_coef)

#Regresia multipla
message(cat("MULTIPLE LINEAR REGRESSION perm ~ peri + area PROPERTIES:"))
multiple_reg = lm(rock$perm ~ rock$peri + rock$area)
summary(multiple_reg)
multiple_reg_coeff = coefficients(multiple_reg)
free_coef = round (multiple_reg_coeff[1],1)
multiple_reg_equation = paste0("y = ")
for (i in 2:3){
  multiple_reg_equation = paste0(multiple_reg_equation,
                                 round (multiple_reg_coeff[i],1) , " *x ", i - 1, " + ");
}
multiple_reg_equation = paste0(multiple_reg_equation,
                               free_coef)

#Ecuatiile celor 2 regresii
message(cat(paste0("The simple regression is ",
                   simple_reg_equation))) 
message(cat(paste0("the multiple regression is ",
                   multiple_reg_equation)))

#Adaugare variabila noua
plot(density(perm))
densitate <- ppois(c(100,20),perm)
plot (density(densitate))


#----------------------------- Problema 3 -------------------------------

per <- seq(0, 3, 0.00001)
par(mfrow = c(1, 2))

plot(per, pweibull(per, 0.5), type = "l", main = "Functia de repartitie", 
     xlab = "x", ylab = "P(X <= x)", ylim = c(0, 1))
lines(per, pweibull(per, 1), type = "l", col = "blue")
lines(per, pweibull(per, 2), type = "l", col = "yellow")
lines(per, pweibull(per, 4), type = "l", col = "red")

plot(per, dweibull(per, 0.5), type = "l", main = "Densitatea de probabilitate", 
     xlab="x", ylab = "Rata de esuare", ylim = c(0, 2.5))
lines(per, dweibull(per, 1), type = "l", col = "blue")
lines(per, dweibull(per, 2), type = "l", col = "yellow")
lines(per, dweibull(per, 4), type = "l", col = "red")

