# Numbers, Hypotheses & Conclusions
# Tut 7: Z-tests & Tut 8: T-tests
# Kevin Durrheim & Gillian Finchilescu
# R: Michelle Hoogenhout


#install pacman package if not yet installed
if(!require(pacman)){install.packages("pacman")}

#load packages
library(pacman)
p_load(dplyr, tidyr, ggplot2, cowplot, psych)


#create normally distributed variable
normvar <- rnorm(100000)
normdat <- data.frame(normvar)

#one-tailed test
ggplot(normdat, aes(normvar)) +
  geom_density(adjust = 4) +
  labs(x = "z-scores", y = "") +
  theme(axis.title=element_text(size=24, face = "bold"),
        axis.text.x = element_text(size = 20, hjust = 1, angle = 30),
        axis.text.y = element_text(size = 20)) +
  scale_x_continuous(breaks = c(0, 1.645, 4.083)) +
  scale_y_continuous(labels = NULL) +
  geom_segment(x = 1.645, y = 0, xend = 1.645, yend = 0.115, linetype = 1)

#two-tailed test
ggplot(normdat, aes(normvar)) +
  geom_density(adjust = 4) +
  labs(x = "z-scores", y = "") +
  theme(axis.title=element_text(size=24, face = "bold"),
        axis.text.x = element_text(size = 20, hjust = 1, angle= 30),
        axis.text.y = element_text(size = 20)) +
  scale_x_continuous(breaks = c(-3.333, -2.575, 0, 2.575)) +
  scale_y_continuous(labels = NULL) +
  geom_segment(x = -2.575, y = 0, xend = -2.575, yend = 0.02, linetype = 1) +
  geom_segment(x = 2.575, y = 0, xend = 2.575, yend = 0.02, linetype = 1) 

## chap 9 t-test #################

sample1 <- rnorm(n = 1000, 50, 3)
sample2 <- rnorm(n = 1000, 60, 3)
ttest <- data.frame(sample1, sample2)

rm(sample1, sample2)

ttest <- ttest %>% gather(sample, value) %>% mutate(sample = factor(sample))

ggplot(ttest, aes(group = sample, value)) +
  geom_density(adjust = 4) +
  labs(x = "", y = "") +
  theme(axis.title=element_text(size=24, face = "bold"),
        axis.text.x = element_text(size = 20, hjust = 1),
        axis.text.y = element_text(size = 20)) +
  scale_x_continuous(breaks = c(50, 60)) +
  scale_y_continuous(labels = NULL) +
  geom_segment(x = 50, y = 0, xend = 50, yend = 0.103, linetype = 3) +
  geom_segment(x = 60, y = 0, xend = 60, yend = 0.101, linetype = 3) +
  annotate("text", x =55, y = 0.112, label = "Mean difference", colour = "black", size = 8) +
  annotate("text", x =68, y = 0.06, label = "Sample 2", colour = "black", size = 7) +
  annotate("text", x =42, y = 0.06, label = "Sample 1", colour = "black", size = 7) +
  geom_rect(aes(xmin=50, xmax=60, ymin=0.107, ymax=0.107), colour = "black") +
  geom_rect(aes(xmin=50, xmax=50, ymin=0.106, ymax=0.107), colour = "black") +
  geom_rect(aes(xmin=60, xmax=60, ymin=0.106, ymax=0.107), colour = "black") 





