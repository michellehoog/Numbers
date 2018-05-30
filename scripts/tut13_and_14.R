# Numbers, Hypotheses & Conclusions
# Tuts 13: One-way ANOVA & 14: Factorial ANOVA
# Michelle Hoogenhout


#install pacman package if not yet installed
if(!require(pacman)){install.packages("pacman")}

#load packages
library(pacman)
p_load(dplyr, tidyr, ggplot2, cowplot, psych, magrittr)


## Tut 14 One-way ANoVA ##############

#dataset with homogenous variance
homo1 <- rnorm(20, 20, 3)
homo2 <- rnorm(20, 25, 5)
homo3 <- rnorm(20, 27, 3)

homo <- data.frame(homo1, homo2, homo3)
homo <- homo %>% gather(group,value)

#dataset with heterogenous variance
hetero1 <- rnorm(20, 27, 15)
hetero2 <- rnorm(20, 25, 10)

hetero <- data.frame(homo1, hetero2, hetero1)
hetero <- hetero %>% gather(group,value)

# boxplot: homogenous variance
ggplot(homo, aes(group, value)) +
  geom_boxplot() +
  labs(x = "Group", y = "") +
  theme(axis.title=element_text(size=30, face = "bold"),
        axis.text = element_text(size = 28, hjust = 1)) +
  scale_x_discrete(labels = c("1", "2", "3"), limit = c("homo3", "homo2", "homo1"))

# boxplot: heterogenous variance
ggplot(hetero, aes(group, value)) +
  geom_boxplot() +
  labs(x = "Group", y = "") +
  theme(axis.title=element_text(size=30, face = "bold"),
        axis.text = element_text(size = 28, hjust = 1)) +
  scale_x_discrete(labels = c("1", "2", "3"))

## Tut 15 Factorial ANoVA ##############

#make data
group <- rep(rep(1:3, each = 2), times = 3)
condition <- rep(1:2, times = 9)
graph <- rep(c("parallel", "disordinal", "ordinal"), each = 6)
values <- c(5,6,4,5,6,7, 5,4,2,6,1,7, 3,4,4,5,6,9)

interactiondata <- data.frame(graph, group, condition, values)
interactiondata <- interactiondata %>%
  mutate(group = factor(group), 
         condition = factor(condition))

#line graphs
p <- ggplot(interactiondata[which(interactiondata$graph == "parallel"),], 
            aes(group, values, group = condition)) +
  geom_point() +
  geom_line() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = NULL) +
  scale_x_discrete(labels = NULL)

o <- ggplot(interactiondata[which(interactiondata$graph == "ordinal"),], 
            aes(group, values, group = condition)) +
  geom_point() +
  geom_line() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = NULL) +
  scale_x_discrete(labels = NULL)

d <- ggplot(interactiondata[which(interactiondata$graph == "disordinal"),], 
            aes(group, values, group = condition)) +
  geom_point() +
  geom_line() +
  labs(x = "", y = "") +
  scale_y_continuous(labels = NULL) +
  scale_x_discrete(labels = NULL)

plot_grid(p,o,d, nrow = 1)
