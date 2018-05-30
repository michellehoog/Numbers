# Numbers, Hypotheses & Conclusions
# Tut 16: Multiple Regression
# Colin Tredoux & Kevin Durrheim
# R: Michelle Hoogenhout 

## tut 16 multiple regression ################################

#install pacman package if not yet installed
if(!require(pacman)){install.packages("pacman")}

#load packages
library(pacman)
p_load(dplyr, tidyr, ggplot2, cowplot, psych)


#### R plot for chapter
data(mtcars)
attach(mtcars)
plot(wt, mpg, pch=cyl) #pch specifies different symbol for each level of cyl
abline(lm(mpg~wt, data = mtcars[which(mtcars$cyl == 4),]), col="red") # add regression line for 4 cylinder category 
abline(lm(mpg~wt, data = mtcars[which(mtcars$cyl == 6),]), col="blue") # add regression line for 4 cylinder category 
abline(lm(mpg~wt, data = mtcars[which(mtcars$cyl == 8),]), col="green") # add regression line for 4 cylinder category

#moderation example
set.seed(123)
X <- rnorm(100, 100, 7)
M <- rnorm(100, 20, 3)
Y <- 0.5*X - 0.3*M + 10 + 1.3*X*M + rnorm(100, 0, 3)
Y2 <- 0.5*(X^3) -0.4*Y - 2 + rnorm(100, 0, 3)
Y3 <- 2*log(X) + rnorm(100, 0, 5)
Y4 <- 0.5*X + 10 + rnorm(100, 0, 7)

moddata <- data.frame(X, M, Y, Y2, Y3, Y4)

#centre
moddata$Xc <- c(scale(moddata$X, center=TRUE, scale=FALSE))
moddata$Mc <- c(scale(moddata$M, center=TRUE, scale=FALSE))

#create groups
moddata$mgroup <- cut(moddata$Mc, breaks = 3)

#regression
fitmod <- lm(Y ~ Xc*Mc, data=moddata)
summary(fitmod)

#plot moderation
ggplot(moddata, aes(X, Y, linetype = mgroup)) +
  geom_smooth(se = F, method = "lm", colour = "black") +
  scale_y_continuous(labels = NULL) +
  scale_x_continuous(labels = NULL) +
  theme(axis.title=element_text(size=24, face = "bold"))

#save residual and fitted values
moddata$res <- rstandard(fitmod)
moddata$fit <- scale(fitted(fitmod), scale = T)

#residuals fine
plot(rstandard(fitmod), scale(fitted(fitmod), scale = T))

#regression 2
fitmod2 <- lm(Y2 ~ X, data=moddata)
plot(scale(fitted(fitmod2), scale = T), rstandard(fitmod2))

#save residuals and fitted values
moddata$powerfit <- scale(fitted(fitmod2), scale = T)
moddata$powerres <- rstandard(fitmod2)

#plot linear relationship
ggplot(moddata, aes(X, Y4)) +
  geom_smooth(se = F, method = "lm", colour = "cadetblue") +
  geom_point() +
  ylab("Y") +
  scale_y_continuous(labels = NULL) +
  scale_x_continuous(labels = NULL) +
  theme(axis.title=element_text(size=24, face = "bold"))

#plot nonlinear relationship
ggplot(moddata, aes(X, X^5)) +
  geom_smooth(se = F, method = "lm",  colour = "cadetblue") +
  geom_point() +
  ylab("Y") +
  scale_y_continuous(labels = NULL) +
  scale_x_continuous(labels = NULL) +
  theme(axis.title=element_text(size=24, face = "bold"))

#plot nonlinear relationship 2
x1=c(61,610,1037,2074,3050,4087,5002,6100,7015)
y1=c(0.974206,1.16716,1.19879,1.28192,1.30739,1.32019,1.35494,1.36941,1.37505)
logdata <- data.frame(x1, y1)
logfit = lm(y1 ~ x1)

ggplot(logdata, aes(x1, y1)) +
  geom_smooth(se = F, method = "lm",  colour = "cadetblue") +
  geom_point() +
  labs(y = "Y", x = "X") +
  scale_y_continuous(labels = NULL) +
  scale_x_continuous(labels = NULL) +
  theme(axis.title=element_text(size=24, face = "bold"))

#plot good residuals
ggplot(moddata, aes(res, fit)) +
  geom_smooth(se = F, method = "lm",  colour = "cadetblue") +
  geom_point() +
  labs(y = "Standardised predicted", x = "Standardised residual") +
  theme(axis.title=element_text(size=24, face = "bold"))  

#plot nonlinear residuals
ggplot(moddata, aes(powerres, powerfit)) +
  geom_smooth(se = F, method = "lm",  colour = "cadetblue") +
  geom_point() +
  labs(y = "Standardised predicted", x = "Standardised residual") +
  theme(axis.title=element_text(size=24, face = "bold"))

moddata$resoutlier <- moddata$res  
moddata$resoutlier[6] <- 6.00

#plot good residual w outlier
ggplot(moddata, aes(resoutlier, fit)) +
  geom_smooth(se = F, method = "lm",  colour = "cadetblue") +
  geom_point() +
  labs(y = "Standardised predicted", x = "Standardised residual") +
  theme(axis.title=element_text(size=24, face = "bold"))  

#make heteroscedatic data
y1 <- rnorm(20, 5, 1)
y2 <- rnorm(20, 8, 3)
y3 <- rnorm(20, 10, 5)
y4 <- rnorm(20, 14, 7)
yhetero <- c(y1, y2, y3, y4)
x <- c(1:80) + rnorm(80, 0, 3)
resdata <- data.frame(x, yhetero)

#regression
heterofit <- lm(yhetero ~ x, data = resdata)

#save residuals and fitted values
resdata$fit <- scale(fitted(heterofit), scale = T)
resdata$res <- rstandard(heterofit)

#plot heteroscedatic residuals
ggplot(resdata, aes(res, fit)) +
  geom_smooth(se = F, method = "lm",  colour = "cadetblue") +
  geom_point() +
  labs(y = "Standardised predicted", x = "Standardised residual") +
  theme(axis.title=element_text(size=24, face = "bold"))  
