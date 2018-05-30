# Numbers, Hypotheses & Conclusions
# Tut 3: Central tendency & variance
# Michelle Hoogenhout & Martin Terreblanche

# This script creates the figures and data displayed in Tut 3. 

#install pacman package if not yet installed
if(!require(pacman)){install.packages("pacman")}

#load packages
library(pacman)
p_load(dplyr, tidyr, ggplot2, cowplot, psych, magrittr)


########## R box central tendency and variation ##############

#create normally distributed variable
#n = 20, mean = 10, sd = 3
set.seed(124356)
happiness <- rnorm(20, 10, 3)
happy <- data.frame(happiness)

#The mean gives the average (arithmetic middle)
mean(happiness)

#A trimmed mean is useful when there are extreme values that bias the mean
#trimmed mean, trimming 2 variables from either end
mean(happiness,trim = 0.2)

#The median is the middle value
median(happiness)

#standard deviation
sd(happiness)

#variance
var(happiness)

#minimum and maximum
range(happiness, na.rm = T)

#The interquartile range gives the interval between the 3rd and 1st quartile
IQR(happiness)

#get a summary of the mean, median and distribution
describe(happiness)

# For nominal (categorical) variables, used the mode
#create nominal variable
language <- c("English", "isiXhosa", "Afrikaans", "Afrikaans", "isiXhosa", "Sesotho", "isiXhosa")

#get group frequency
table(language)

#R does not have an inbuilt function to calculate the mode, so we define one:
#create function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#calculate the mode for language
Mode(language)

#Histograms can visually represent the central tendency and variation in data

#histogram using base R
hist(happiness, cex.lab = 1.8, cex.axis = 1.3, main = "", xlab = "Happiness")

#histogram using ggplot2
ggplot(happy, aes(happiness)) + 
  geom_histogram(bins = 7, center = 8, fill = "white", colour = "black") + 
  labs(x = "Happiness", y = "Frequency") +
  theme(axis.title=element_text(size=24), 
        strip.text.x = element_text(size = 20))

#Note that if you have any missing values, mean will return NA if these
#missing values are not excluded. To illustrate:

#change the first value of happiness to NA
happiness[1] <- NA

mean(happiness) #this doesn't work
mean(happiness, na.rm = T) #this works

## Tutorial graphs ###################################

#fig 3.1

  #create data
  room1 <- rep(3, 20)
  room2 <- c(rep(c(4,2), each = 6), rep(3,8))
  room3 <- c(3,4,3,6,3,1,6,3,4,3,1,3,2,3,3,5,1,2,6,3)
  
  fig31data <- data.frame(room1, room2, room3)
  fig31data <- fig31data %>%
    gather(room, values)
  
  countfig31 <- c(3,8,37,8,1,3)
  
  #graph
  ggplot(fig31data, aes(room, values)) +
    geom_point(size = 3) +
    theme_bw() +
    labs(x = "Room", y = "Values") +
    scale_x_discrete(labels = c("1", "2", "3")) +
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=16))

#fig 3.2
  #data
  scores1 <- c(7:19)
  freq1 <- c(20, 25, 35, 40, 45, 55, 70, 55, 45, 40, 35, 25, 20)

  scores2 <- c(9:17)
  freq2 <- c(12, 64, 66, 68, 70, 68, 66, 64, 12)

  fig32data.1 <- data.frame(scores1, freq1)
  fig32data.2 <- data.frame(scores2, freq2)

  #graph
  fig32a <- ggplot(fig32data.1, aes(as.factor(scores1), freq1)) +
    geom_col(position = "dodge", fill = "lightgrey", colour = "black") +
    labs(x = "SCORES", y = "NUMBER OF STUDENTS", title = "TEST SCORES FOR CLASS 1") + 
    theme_bw()
  
  fig32b <- ggplot(fig32data.2, aes(as.factor(scores2), freq2)) +
    geom_col(position = "dodge", fill = "lightgrey", colour = "black") +
    labs(x = "SCORES", y = "NUMBER OF STUDENTS", title = "TEST SCORES FOR CLASS 2")+ 
    theme_bw() +
    scale_x_discrete(breaks = c(7:19), labels = c(7:19),
                     limits=c("7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
  
  #plot figures a and b together
  plot_grid(fig32a, fig32b)
  
#fig 3.3 orginal (modified)
  monthly.accidents <- c(0:7)
  freq1 <- c(2,3,4,5,4,3,3,2)
  freq2 <- c(1,0,8,10,8,1,0,1)
  
  fig33data.a <- data.frame(monthly.accidents, freq1, freq2)
  
  #graph
  fig33a <- ggplot(fig33data.a, aes(monthly.accidents, freq1)) +
    geom_col(position = "dodge", fill = "lightgrey", colour = "black") +
    labs(x = "ACCIDENTS PER MONTH", y = "FREQUENCY", title = "MINE A") + theme_bw()
  
  fig33b <- ggplot(fig33data.a, aes(monthly.accidents, freq2)) +
    geom_col(position = "dodge", fill = "lightgrey", colour = "black") +
    labs(x = "ACCIDENTS PER MONTH", y = "FREQUENCY", title = "MINE B")+ theme_bw()
  
  #plot figures a and b together
  plot_grid(fig33a, fig33b) 

  #create data
  set.seed(100)
  normal <- data.frame(rnorm (500))
  exp <- data.frame(rexp(500))
  chi <- data.frame(rchisq(500, 3))
  neg.exp <- data.frame(max(exp) - rexp(500))
  
  #bimodal data
  norm1 <- data.frame(rnorm (250,10,3))
  norm2 <- data.frame(rnorm(250,20,3))
  colnames(norm1) <- "dist"
  colnames(norm2) <- "dist"
  bimodal <- rbind(norm1, norm2)
  
  #bind into one data set
  distributions<- cbind(normal, bimodal, exp, chi, neg.exp)
  colnames(distributions) <- c("normal", "bimodal", "pos.skew1", "pos.skew2", "neg.skew")
  
  #remove redundant lists
  rm(normal, exp, neg.exp, norm1, norm2, bimodal)
  
#Fig 3.4 boxplot 
  
  boxplotdata <- distributions
  boxplotdata$normal[1:2] <- c(2.3, 3.3)
  boxplotdata$normal <- boxplotdata$normal + 3
  boxplotdata$group <- rep(1,500)
  
  quantile(boxplotdata$normal,.25) #1st quartile
  quantile(boxplotdata$normal,.75) #3rd quartile
  
  #make boxplot (calibrated for pdf)
  ggplot(boxplotdata, aes(group, normal)) +
    geom_boxplot(outlier.shape = ) +
    annotate("text", x =2.1, y = 3, label = "Median", colour = "black", size = 7) +
    annotate("text", x =2.19, y = 2.39, label = "1st Quartile", colour = "black", size = 7) +
    annotate("text", x =2.22, y = 3.64, label = "3rd Quartile", colour = "black", size = 7) +
    annotate("text", x =2.26, y = 5, label = "Upper whisker", colour = "black", size = 7) +
    annotate("text", x =2.26, y = 1.5, label = "Lower whisker", colour = "black", size = 7) +
    annotate("text", x =2.1, y = 6.3, label = "Outlier", colour = "black", size = 7) +
    annotate("text", x =3.5, y = 3, label = "IQR", colour = "black", size = 7) +
    geom_segment(x = 3.2, y = 2.39, xend = 3.2, yend = 3.64)+
    geom_segment(x = 3.0, y = 2.39, xend = 3.2, yend = 2.39)+
    geom_segment(x = 3.0, y = 3.64, xend = 3.2, yend = 3.64)+
    labs(x = "", y = "") +
    xlim(0,4) + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank())
  
#fig 3.5 boxplots
  #data
  skew <- c(1,1,1,2,1,2,3,5,7,8,9) #skewed
  symm <- c(9,8,7,5,3,2,4,2,1,7,8) #symmetrical
  s_rang <- c(7,2,3,4,3,3,4,3,4,2,2) #small range
  l_rang <- c(0,1,8,4,7,11,7,10,2,1,1) #large range
  
  fig35data <- data.frame(skew, symm, s_rang, l_rang)

  #export data
  write.csv(fig35data, "../output/fig35data.csv", row.names = F)
  
  #rearrange for plot
  fig35data_long <- fig35data %>%
    gather(distribution, values) %>%
    mutate(distribution = factor(distribution, levels = c("skew", "symm", "s_rang", "l_rang"), ordered = T))
  
  #graph showing different distributions
  ggplot(fig35data_long, aes(distribution, values)) +
    stat_boxplot(geom ='errorbar') +
    geom_boxplot(fill = "lightgrey", colour = "black") +
    labs(x = "TYPE OF DISTRIBUTION", y = "") +
    scale_x_discrete(labels = c("Skewed", "Symmetrical", "Small range", "Large range"))+
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=14))
