# Numbers, Hypotheses & Conclusions
# Tut 10: Regression
#Author: Lance Lachenicht
#R:  Michelle Hoogenhout

#install pacman package if not yet installed
if(!require(pacman)){install.packages("pacman")}

#load packages
library(pacman)
p_load(dplyr, tidyr, ggplot2, cowplot, psych, magrittr)


#create random data
set.seed(21)
grp1 <- round(rnorm(n = 10, 20, 3))
grp2 <- round(jitter((grp1 + c(6:10))))
reg <- data.frame(grp1, grp2)

reg$grp1[1] <- 18
reg$grp1[9] <- 19

#example linear regression
regexample <- lm(grp2~grp1, data = reg)
fitval <- fitted(regexample)

#plot1: Health & QoL
ggplot(reg, aes(grp1, grp2)) +
  geom_point(size = 2) +
  labs(x = "Health", y = "Quality of life") +
  theme(axis.title=element_text(size=24, face = "bold"),
        axis.text.x = element_text(size = 20, hjust = 1),
        axis.text.y = element_text(size = 20))+
  scale_x_continuous(breaks = c(15, 21, 27)) +
  geom_abline(intercept = regexample$coefficients[[1]], 
              slope = regexample$coefficients[[2]], colour = "cadetblue", size = 1.2) +
  geom_segment(x = 15, y = 22, xend = 15, yend = fitval[[7]], linetype = 3) +
  geom_segment(x = 16, y = fitval[[4]], xend = 16, yend = 25, linetype = 3) +
  geom_segment(x = 17, y = 25, xend = 17, yend = fitval[[8]], linetype = 3) +
  geom_segment(x = 18, y = fitval[[1]], xend = 18, yend = 28, linetype = 3) +
  geom_segment(x = 19, y = fitval[[9]], xend = 19, yend = 29, linetype = 3) +
  geom_segment(x = 20, y = fitval[[10]], xend = 20, yend = 30, linetype = 3) +
  geom_segment(x = 21, y = 27, xend = 21, yend = fitval[[6]], linetype = 3) +
  geom_segment(x = 22, y = 29, xend = 22, yend = fitval[[2]], linetype = 3) +
  geom_segment(x = 25, y = 33, xend = 25, yend = fitval[[3]], linetype = 3) +
  geom_segment(x = 27, y = fitval[[5]], xend = 27, yend = 37, linetype = 3)


##income examples
age <- c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
income <- c(7, 8, 29, 25, 40, 50, 33, 50, 65, 27)
income2 <- c(6,8,25,29,35,42,48,50,55,60)
incomedata <- data.frame(age, income, income2)

#sort by age
incomedata <- incomedata %>% dplyr::arrange(age)

#regression1
regexample2 <- lm(income~age, data = incomedata)
fitval2 <- fitted(regexample2)
summary(regexample2)

#regression2
regexample3 <- lm(income2~age, data = incomedata)
fitval3 <- fitted(regexample3)
summary(regexample3)

#age plot1
ggplot(incomedata, aes(age, income)) +
  geom_point() +
  geom_smooth(se = F, method = "lm", colour = "cadetblue") +
  labs(x = "Age", y = "Income") +
  theme(axis.title=element_text(size=24, face = "bold"),
        axis.text.x = element_text(size = 20, hjust = 1),
        axis.text.y = element_text(size = 20)) +
  geom_segment(x = age[1], y = income[1], xend = age[1], yend = fitval2[[1]], linetype = 3) +
  geom_segment(x = age[2], y = income[2], xend = age[2], yend = fitval2[[2]], linetype = 3) +
  geom_segment(x = age[3], y = fitval2[[3]], xend = age[3], yend = income[3], linetype = 3) +
  geom_segment(x = age[4], y = income[4], xend = age[4], yend = fitval2[[4]], linetype = 3) +
  geom_segment(x = age[5], y = income[5], xend = age[5], yend = fitval2[[5]], linetype = 3) +
  geom_segment(x = age[6], y = income[6], xend = age[6], yend = fitval2[[6]], linetype = 3) +
  geom_segment(x = age[7], y = fitval2[[7]], xend = age[7], yend = income[7], linetype = 3) +
  geom_segment(x = age[8], y = income[8], xend = age[8], yend = fitval2[[8]], linetype = 3) +
  geom_segment(x = age[9], y = fitval2[[9]], xend = age[9], yend = income[9], linetype = 3) +
  geom_segment(x = age[10], y = income[10], xend = age[10], yend = fitval2[[10]], linetype = 3) 


#age plot2
ggplot(incomedata, aes(age, income2)) +
  geom_point() +
  geom_smooth(se = F, method = "lm", colour = "cadetblue") +
  labs(x = "Age", y = "Income") +
  theme(axis.title=element_text(size=24, face = "bold"),
        axis.text.x = element_text(size = 20, hjust = 1),
        axis.text.y = element_text(size = 20)) +
  geom_segment(x = age[1], y = income2[1], xend = age[1], yend = fitval3[[1]], linetype = 3) +
  geom_segment(x = age[2], y = income2[2], xend = age[2], yend = fitval3[[2]], linetype = 3) +
  geom_segment(x = age[3], y = fitval3[[3]], xend = age[3], yend = income2[3], linetype = 3) +
  geom_segment(x = age[4], y = income2[4], xend = age[4], yend = fitval3[[4]], linetype = 3) +
  geom_segment(x = age[5], y = income2[5], xend = age[5], yend = fitval3[[5]], linetype = 3) +
  geom_segment(x = age[6], y = income2[6], xend = age[6], yend = fitval3[[6]], linetype = 3) +
  geom_segment(x = age[7], y = fitval3[[7]], xend = age[7], yend = income2[7], linetype = 3) +
  geom_segment(x = age[8], y = income2[8], xend = age[8], yend = fitval3[[8]], linetype = 3) +
  geom_segment(x = age[9], y = fitval3[[9]], xend = age[9], yend = income2[9], linetype = 3) +
  geom_segment(x = age[10], y = income2[10], xend = age[10], yend = fitval3[[10]], linetype = 3) 

