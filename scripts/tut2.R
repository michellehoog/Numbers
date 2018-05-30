# Numbers, Hypotheses & Conclusions
# Tut 2: Displaying data
# Michelle Hoogenhout

# This script creates the figures and data displayed in Tut 2. 

#install pacman package if not yet installed
if(!require(pacman)){install.packages("pacman")}

#load packages
library(pacman)
p_load(dplyr, tidyr, ggplot2, cowplot)


############### Bar graphs ###########################################

#school enrolment example (year 2013) from WHO Indicators

  #get data, encoding cellss with .. as NA
  education <- read.csv("../data/education4.csv", na.strings = "..")

  #select early, primary, secondary, post-secondary and tertiary enrollment
  school.enroll <- education %>% filter(Series.Code == "UIS.E.0.PU.T" | 
                                          Series.Code == "SE.PRM.ENRL" |
                                         Series.Code == "UIS.E.2" | 
                                          Series.Code == "UIS.E.4" | 
                                         Series.Code == "SE.TER.ENRL")  %>% 
    dplyr::select(Country, enrolment = X2013..YR2013., Series, Series.Code)

  #make Series a factor; rename and reorder levels
  school.enroll$Series <- factor(school.enroll$Series)
  levels(school.enroll$Series) <- c("Early Childhood", "Secondary", "Post-Secondary", "Primary", "Tertiary")
  school.enroll$Series <- factor(school.enroll$Series, levels(school.enroll$Series)[c(1,4,2,3,5)])

  #get data for SA and Brazil in table
  enrol.table <- school.enroll %>% filter(Country == "South Africa" | Country == "Brazil") %>%
    group_by(Country) %>%
    select(Country, Series, enrolment)

  #write to file: Table 2.1
  write.csv(enrol.table, "../output/school_enrolment.csv", row.names = F)

#Fig 2.1
#bar graph: school enrolment: South Africa
# change enrolment scale to 100 000s  
schoolSA <-  ggplot(school.enroll[which(school.enroll$Country == "South Africa"),], 
                    aes(Series, I(enrolment/1000000))) + 
  geom_col(position = "dodge", width  = .80) +
  labs(y = "School enrolment (millions)",  x = "Education level", title = "South Africa") +
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text.x = element_text(size = 14),
        legend.text=element_text(size=12), 
        legend.title=element_text(size=14)) +
  annotate("text", x="Early Childhood", y = 0.4, label = "0.8", colour = "white", size = 5) +
  annotate("text", x="Primary", y = 6.7, label = "7.1", colour = "white", size = 5) +
  annotate("text", x="Secondary", y = 1.7, label = "2.0", colour = "white", size = 5) +
  annotate("text", x="Post-Secondary", y = 0.6, label = "0.3", colour = "black", size = 5) +
  annotate("text", x="Tertiary", y = 0.6, label = "1.0", colour = "white", size = 5)

#Fig 2.2
#bar graph: school enrolment: South Africa & Brazil
# change enrolment scale to 100 000s  
schoolSABR <- ggplot(school.enroll[which(school.enroll$Country == "Brazil" | 
                                           school.enroll$Country == "South Africa"),], 
                     aes(Series, I(enrolment/1000000), fill = Country)) + 
  geom_col(position = "dodge", width  = .80) +
  scale_fill_grey() +
  labs(y = "School enrolment (millions)", x = "Education level") +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text.x = element_text(size = 18),
        legend.text=element_text(size=16), 
        legend.title=element_text(size=18))

#Fig 2.3: calculate percentage enrolled at each level
  
  ##population(millions) in 2013
  brazil.pop <- 200.4
  sa.pop <- 52.98

  #create new dataset with only SA and Brazil
  school.enroll2 <- school.enroll %>%
    filter(Country == "Brazil" | Country == "South Africa") %>%
    mutate(enrolment = enrolment/1000000)

  #calculate percentage enrolment in each country
  school.enroll2$percent.enroll <- ifelse(school.enroll2$Country == "South Africa",
                                       school.enroll2$enrolment/sa.pop*100,
                                       school.enroll2$enrolment/brazil.pop*100)

  #Fig 2.3 new graph with percentage of population enrolled
  ggplot(school.enroll2, aes(Series, percent.enroll, fill = Country)) + 
    geom_col(position = "dodge", width  = .80) +
    scale_fill_grey() +
    labs(y = "Percentage of population enrolled", x = "Education level") +
    theme(axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text.x = element_text(size = 16),
        legend.text=element_text(size=16), 
        legend.title=element_text(size=18))

# Beware that if axes do not start at zero, this can distort the comparison
#Fig 2.4: South Africa, with axis not at zero
  ggplot(school.enroll[which(school.enroll$Country == "South Africa"),], 
         aes(Series, I(enrolment/1000000))) + 
    geom_col(position = "dodge", width  = .80) +
    labs(y = "School enrolment (millions)",  x = "Education level") +
    theme(axis.title=element_text(size=14),
          axis.text.x = element_text(angle = 60, hjust = 1),
          strip.text.x = element_text(size = 14))+
    coord_cartesian(ylim = c(1,7))
  
#Fig 2.5a: horizontal bar graph: school enrolment: South Africa & Brazil
hor <- ggplot(school.enroll[which(school.enroll$Country == "Brazil" | school.enroll$Country == "South Africa"),], 
              aes(Series, I(enrolment/1000000), fill = Country)) + 
  geom_bar(stat = "identity", position = "dodge", width  = .80) +
  coord_flip() +
  scale_fill_grey(guide = F) +
  labs(y = "School enrolment (millions)", x = "Education level", title = "Horizontal bar graph") +
  scale_x_discrete(limits = c("Tertiary", "Post-Secondary", "Secondary", "Primary", "Early Childhood")) +
  theme(axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        strip.text.x = element_text(size = 18))

#fig 2.5b: stacked bar graph: school enrolment: South Africa & Brazil
stacked <- ggplot(school.enroll[which(school.enroll$Country == "Brazil" | school.enroll$Country == "South Africa"),], 
                  aes(Series, I(enrolment/1000000), fill = Country)) + 
  geom_col(width  = .80) +
  scale_fill_grey() +
  labs(y = "School enrolment (millions)", x = "Education level", title = "Stacked bar graph") +
  theme(axis.title=element_text(size=18),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        strip.text.x = element_text(size = 18),
        legend.text=element_text(size=16), 
        legend.title=element_text(size=18))+
  annotate("text", x="Primary", y = 3, label = "7.1", colour = "black", size = 5) +
  annotate("text", x="Primary", y = 15, label = "16.8", colour = "white", size = 5)

#Fig 2.5
plot_grid(hor, stacked, nrow = 1)


########################## Histograms ###################################################

#create random data 
#HIV age at diagnosis
set.seed(22)
hiv1 <- rnorm(80, 22,5)
hiv1[81:100] <- c(1.2, 1, 2.2, 0.5, 0.5, 3, 47.5, 44.0, 48.2, 33.1, 4.4, 39.7, 31.1, 32.5, 38.5, 55.3, 62.1, 51.9, 40.4, 46.9)

#frequency table
hivfreq.table <- as.data.frame(cbind(hist(hiv1, plot = F)$mids, 
                                     hist(hiv1, plot = F)$counts))
#make data frame
hiv.diagnosis <- as.data.frame(hiv1)


#output data
write.csv(t(hiv1), "../output/age.diagnosis.raw.csv", row.names = F) #raw data table in Histogram section
write.csv(hivfreq.table, "../output/age.diagnosis.freq.csv", row.names = F) #Table 2.2

#histogram Fig 2.6
ggplot(hiv.diagnosis, aes(hiv1)) +
  geom_histogram(binwidth = 10, center = 5,fill=I("grey"), col=I("black")) +
  theme_bw() +
  labs(x = "Age at diagnosis", y = "Frequency distribution")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 14))

# In base R
# hist(hiv1, main = "", xlab = "Age at diagnosis", fill = "grey")

#histogram Fig 2.7
ggplot(hiv.diagnosis, aes(hiv1)) +
  geom_histogram(binwidth = 5, center = 2.5,fill=I("grey"), col=I("black")) +
  theme_bw() +
  labs(x = "Age at diagnosis", y = "Frequency distribution")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 14))

############## histogram shapes ##################

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

#Fig 2.8 plots
p1 <- ggplot(distributions, aes(normal)) + 
  geom_histogram(bins = 10, fill=I("grey"), col=I("black")) + 
  labs(x = "", y = "Count", title = "Symmetric, unimodal")

p2 <- ggplot(distributions, aes(pos.skew2)) + 
  geom_histogram(bins = 10, fill=I("grey"), col=I("black")) + 
  labs(x = "", y = "Count", title = "Positive (right) skew, unimodal")

p3 <- ggplot(distributions, aes(neg.skew)) + 
  geom_histogram(bins = 10, fill=I("grey"), col=I("black")) + 
  labs(x = "", y = "Count", title = "Negative (left) skew, unimodal")

p4 <- ggplot(distributions, aes(bimodal)) + 
  geom_histogram(bins = 10, fill=I("grey"), col=I("black")) + 
  labs(x = "", y = "Count", title = "Symmetric, bimodal")

#Fig 2.8
plot_grid(p1, p4, p2, p3, nrow = 2)

#Response time example: Fig 2.9
  #add outlier
  distributions <- distributions %>% arrange(pos.skew2)
  distributions[499,4] <- 27.46192

  #Fig 2.9 
  ggplot(distributions, aes(pos.skew2)) + 
    geom_histogram(bins = 10, fill=I("grey"), col=I("black")) + 
    labs(y = "Count", x = "Response time (s)")

  ######################## cumulative frequency ###########################
  
  #create test performance data
  set.seed(312)
  score <- rnorm(80,65,7)
  test <- data.frame(score)
  
  #get frequency and cumulative frequency table
  test.freq <- as.data.frame(cbind(hist(score, plot = F)$mids, hist(score, plot = F)$counts))
  colnames(test.freq) <- c("Category.midpoint", "Frequency")
  test.freq$cum.frequency <- cumsum(test.freq$Frequency) 
  
  #save to excel: Table 2.3
  write.csv(test.freq, "../output/test.freq.csv", row.names = F)
  
  #Fig 2.10a: histogram 
  histplot <- ggplot(test, aes(score)) +
    geom_histogram(binwidth = 5, center = 52.5, fill= "grey", col="black") +
    labs(x = "Test score (max = 100)", y = "Frequency (count)") +
    theme(axis.title=element_text(size=14),
          strip.text.x = element_text(size = 14))
  
  #Fig 2.10b: cumulative frequency plot 
  cumplot <- ggplot(test.freq, aes(Category.midpoint, cum.frequency)) + 
    geom_point() + 
    geom_line()+
    labs(x = "Test score (max = 100)", y = "Cumulative frequency (count)") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), 
          strip.text.x = element_text(size = 14))
  
  #Fig 2.10: plot histogram and cumulative frequency together
  plot_grid(histplot, cumplot)
  
######## Line graphs ###############

# Fig 2.11: example line graph of generic experiment 
  
  #create data: experiment
  experiment <- ordered(rep(c("Before", "After"), times = 2))
  group <- rep(c("Placebo", "Supplement"), each = 2)
  outcome <- c(5, 8.5, 4.4, 5)
  
  #bind to dataframe
  experiment.data <- data.frame(group, experiment, outcome)
  experiment.data$experiment <- factor(experiment.data$experiment, 
                                       levels(experiment.data$experiment)[c(2,1)])
  #fig 2.11
  ggplot(experiment.data, aes(experiment, outcome, group = group, linetype = group)) + 
    geom_line() +
    geom_point() + 
    labs(x = "", y = "Memory performance (max = 10)", linetype = "") +
    theme(axis.title=element_text(size=14),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))

#life expectancy at birth by country #

#fig 2.12
life.expectancy <- read.csv("../data/lifeexpectancydata.csv") %>%
  rename(country = Country.Name) %>%
  filter(Country.Code == "ZAF" | Country.Code == "BWA" | 
           Country.Code == "UGA" | Country.Code == "KEN") %>%
  tidyr::gather(year, expectancy, starts_with("X"))

  #extract year from name
  life.expectancy$year <-as.numeric(regmatches(life.expectancy$year, 
                                               regexpr("[[:digit:]]+", life.expectancy$year)))

  #make life expectancy numeric
  life.expectancy$expectancy <- as.numeric(life.expectancy$expectancy)

  #remove 2015 (no data)
  life.expectancy <- life.expectancy %>% filter(year != 2015 & country != "Kenya")
  
  #make table
  life.expectancy.table <- life.expectancy %>% 
    select(country, year, expectancy) %>%
    arrange(country, year)
  
  write.csv(life.expectancy.table, "../output/lifeexpetancy_table.csv", row.names = F) #Tables 2.4 & 2.5

  #plot line graph fig 2.12a
  plot1 <- ggplot(life.expectancy, aes(year, expectancy, linetype = country)) + 
    geom_rect(aes(xmin=1973, xmax=1983, ymin=-Inf, ymax=Inf), fill = "lightgrey") + 
    annotate("text", x=1978, y = 46, label = "AIDS \nspreads", colour = "black", size = 5) +
    geom_rect(aes(xmin=2002, xmax=2004, ymin=-Inf, ymax=Inf), fill = "lightgrey") + 
    annotate("text", x=2008, y = 46, label = "Free ARVs", colour = "black", size = 5) +
    geom_line() + 
    labs(x = "Year", y = "Life expectancy at birth (years)", linetype = "Country") +
      theme(legend.text = element_text(size = 16),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 18))
      
  #plot bar graph fig 2.12b
  life.expectancy.small <- life.expectancy %>% 
    filter(year == 1970 | year == 1980 | year == 1990 | year == 2000 | year == 2010)

  plot2 <- ggplot(life.expectancy.small, aes(as.factor(year), expectancy, fill = country)) + 
    geom_col(position = "dodge") +
    scale_fill_grey() +
    labs(x = "Year", y = "Life expectancy at birth (years)", fill = "Country") +
    annotate("text", x="1980", y = 65, label = "AIDS spreads", colour = "black", size = 5) +
    annotate("text", x="2000", y = 65, label = "Free ARVs", colour = "black", size = 5) +
    coord_cartesian(ylim = c(40, 70)) +
    theme(legend.text = element_text(size = 16),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 18))

  #Fig 2.12
  plot_grid(plot2,plot1, nrow = 2)
  
# Fig 2.13 basic line graph
  a <- c(0:8)
  b <- seq(0,16,by = 2)
  mydata <- data.frame(a,b)
  
  ggplot(mydata, aes(a,b)) + 
    geom_point() + 
    geom_line() +
    labs(x = "Independent variable (x)", y = "Dependent variable (y)") +
    geom_segment(aes(x = 0, y = 10, xend = 5, yend = 10), data = mydata, linetype = 2) +
    geom_segment(aes(x = 5, y = 0, xend = 5, yend = 10), data = mydata, linetype = 2)+
    annotate("text", x=6, y = 10, label = "(x = 5, y = 10)", colour = "black", size = 5) +
    theme(axis.title=element_text(size=14),
          strip.text.x = element_text(size = 14))
  
# Activity 2.8 graphs ################  
  
  #sleepless nights
  ##create data
  days <- c(0,1,2,3,4)
  errors <- c(0,2,5,9,20)
  sleepless <- data.frame(days, errors)
  
  ##line plot
  plota <- ggplot(sleepless, aes(days, errors)) + 
    geom_point() + 
    geom_line() +
    labs(x = "Number of days without sleep", y = "Number of errors made")+
    theme(axis.title=element_text(size=14),
          strip.text.x = element_text(size = 14))
  
  #recognition
  ##create data
  gend <- rep(c("Male", "Female"), 2)
  stress <- rep(c("Calm", "Stressed"), each = 2)
  recog <- c(50,70,60,30)
  recogdata <- data.frame(gend, stress, recog)
  
  ##line plot
  plotb <- ggplot(recogdata, aes(stress, recog, group = gend)) + 
    geom_point() + 
    geom_line() +
    labs(x = "Condition", y = "Recognition accuracy (%)")+
    annotate("text", x="Stressed", y = 57, label = "Female", colour = "black") +
    annotate("text", x="Stressed", y = 36, label = "Male", colour = "black")+
    theme(axis.title=element_text(size=14),
          strip.text.x = element_text(size = 14))
  
  ##plot recognition and sleepless together
  plot_grid(plota, plotb, labels = "AUTO")
  
################### R box ###############################
    
  #Bar graph  
  #create mathbook data (Source: World Data Bank, 2012)
  country <- c("Malawi", "Swaziland", "Tanzania", "Zambia")
  pupils.per.book <- c(4.45, 1.00, 3.90, 4.33)
  textbook <- data.frame(country, pupils.per.book)
  
  #using base graphics
    ##basic barplot: R box bargraph
    barplot(textbook$pupils.per.book,
          names.arg = textbook$country) #give names to bars
  
    ##add axis titles and rotate xaxis labels
    barplot(textbook$pupils.per.book,
          names.arg = textbook$country,
          ylab = "Number of learners per textbook",
          las=2) # make axis labels perpendicular to axis
  
    # #using ggplot2
    # 
    #   ##basic graph
    #   ggplot(textbook.data, aes(country, pupils.per.book)) +
    #     geom_col() 
    # 
    #   ##Change axis labels
    #   ggplot(textbook.data, aes(country, pupils.per.book)) +
    #     geom_col() +
    #     labs(x = "Country", y = "Number of learners per textbook")
    

  #Histograms
  #load sleep data
  data(sleep)

  ##basic graph: R box histogram1
  hist(sleep$extra, cex = 2)

  ##get separate histograms for groups 1 and 2
  hist(sleep[which(sleep$group ==1),]$extra, cex = 2)
  hist(sleep[which(sleep$group ==2),]$extra, cex = 2)

# #ggplot2
#   
#   ##basic graph
#   ggplot(sleep, aes(extra)) + 
#     geom_histogram(bins = 7)
#   
#   ##specify two separate plots by group with facet_grid()
#   ggplot(sleep, aes(extra)) + 
#     geom_histogram(bins = 7) + 
#     facet_grid(~group)+
  
#plot in same row: R box histogram 2
  #set parameters to two columns
  par(mfrow = c(1,2))
  #plot histograms
  hist(sleep[which(sleep$group ==1),]$extra, 
       main = "Group 1",
       xlab = "Change in sleep (hours)",
       cex = 2)
  hist(sleep[which(sleep$group ==2),]$extra, 
       main = "Group 2",
       xlab = "Change in sleep (hours)",
       cex = 2)
  #reset to previous settings
  par(mfrow = c(1,1)) 
  
  #export
  png(filename="mygraph.png", type="cairo")
  jpeg(filename="mygraph.jpg", type="cairo")
  pdf(file="mygraph.pdf")
  hist(sleep$extra)
  dev.off()
  
  
  #line graph
    #number of rabbits kept over time
    rabbits <- c(2,4,8,16,32)
    days <- c(0, 30, 60, 90, 120)
    nrrabbits <- data.frame(days, rabbits)
    
    #line graph in base R
    plot(days, rabbits, type = "o")
    
    # #line graph in ggplot2
    # ggplot(nrrabbits, aes(days, rabbits)) +
    #   geom_line() +
    #   geom_point() +
    #   theme(axis.title=element_text(size=14),
    #                     strip.text.x = element_text(size = 14))
    # 
    # ggplot(nrrabbits, aes(x = days, xend = days, y = 0, yend = rabbits)) + 
    #   geom_segment()
    
####### worked example ####################
    
    #get data      
    day <-c(1:14)
    tom1 <- c(0,1,1,2,1,2,5,0,0,1,0,2,4,3)
    tom2 <- c(1,2,4,3,2,6,5,1,1,3,3,2,6,4)

    histtom1 <- hist(tom1)
    histtom2 <- hist(tom2)
    
    tomdata2 <- data.frame(day, tom1, tom2)
    write.csv(t(tomdata2), "../output/tut2_workedexample2.csv")
    
    #means
    mean(tom1); median(tom1)
    mean(tom2); median(tom2)
    
  #make histograms
    ##histogram1
    tomhist1 <- ggplot(tomdata2, aes(tom1)) + 
      geom_histogram(bins = 5, center = 0, binwidth = 1, fill = "white", colour = "black") + 
      labs(x = "Time (hours)", y = "Frequency", title = "Child 1") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            strip.text.x = element_text(size = 12)) +
      scale_x_continuous(breaks = c(0:6))
    
    ##histogram2
    tomhist2 <- ggplot(tomdata2, aes(tom2)) + 
      geom_histogram(bins = 6, center = 0, binwidth = 1, fill = "white", colour = "black") + 
      labs(x = "Time (hours)", y = "Frequency", title = "Child 2") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            strip.text.x = element_text(size = 12)) +
      scale_x_continuous(breaks = c(0:6))
    
    ##plot histograms together: Fig 2.14
    plot_grid(tomhist1, tomhist2)

  #make linegraph
    ##arrange data
    tomdata3 <- tomdata2 %>%
      gather(child, hours, tom1, tom2) %>% 
      mutate(child = factor(child))
  
    ##plot Fig 2.15
    ggplot(tomdata3, aes(day, hours, linetype = child)) + 
      geom_line(size = 1.2) + 
      geom_point() +
      labs(x = "Day", y = "Time (hours)", linetype = "") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            strip.text.x = element_text(size = 12),
            legend.text = element_text(size = 14))+
      scale_x_continuous(breaks = seq(1,14, by = 2)) +
      scale_linetype_discrete(labels = c("Child 1", "Child 2"))
    
  #make bar graph
    ggplot(tomdata3, aes(day, hours, fill = child)) + 
      geom_col(position = "dodge") +
      labs(x = "Day", y = "Time (hours)", fill = "") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            strip.text.x = element_text(size = 12))+
      scale_x_continuous(breaks = seq(1,14, by = 2)) +
      scale_fill_discrete(labels = c("Child 1", "Child 2"))
    
#randomly generated data for exercises ##########################
    
    
  #exercise 1: memory
    ##create data
    exp2 <- rexp(40)
    exp3 <- exp2*100
    
    ##histogram
    hist(exp3)
    
    ##get frequency and cumulative frequency table
    freq1 <- as.data.frame(cbind(hist(exp3, plot = F)$mids, hist(exp3, plot = F)$counts))
    colnames(freq1) <- c("Category.midpoint", "Frequency")
    freq1$cum.frequency <- cumsum(freq1$Frequency)/40*100 
    
    ##cumulative frequency plot
    ggplot(freq1, aes(Category.midpoint, cum.frequency)) + 
      geom_point() + 
      geom_line()
    
    ##export data
    exercise1 <- data.frame(t(exp3)) 
    write.csv(exercise1, "../output/exercise1_data.csv", row.names = F)
    
  #suicide rates
    ##create data
    month <- ordered(c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))
    suicide <- c(12, 10, 11, 5, 7, 3, 6, 5, 9, 14, 10, 12)
    suiciderates <- data.frame(month, suicide)
    
    plot(suiciderates$month, suiciderates$suicide)

    
  #autism
    ##create data
    parent <- rep(c("mother", "father"), each = 7)
    age <- rep(c(20,25,30,35,40,45,50), 2)
    p.autism <- c(.005, .006, .008, .011, .013, .014, .015, .006, .006, .007, .008, .009, .01, .012)
    autismrisk <- data.frame(parent, age, p.autism)
    
    ##export data
    write.csv(autismrisk, "../output/autismrisk.csv", row.names = F)
    write.csv(t(autismrisk), "../output/autismrisk2.csv", row.names = F)
    
    ##plot
    ggplot(autismrisk, aes(age, p.autism, colour = parent)) + geom_line()
   
   
   ##### figures for PPT  ########################################################
   
   #bar graph: school enrolment
   ggplot(school.enroll[which(school.enroll$Country == "South Africa"),], 
          aes(Series, I(enrolment/1000000))) + 
     geom_col(position = "dodge", width  = .80) +
     labs(y = "Enrolment (millions)",  x = "Education level") +
     theme(axis.title=element_text(size=24, face = "bold"),
           axis.text.x = element_text(size = 20, angle = 60, hjust = 1),
           axis.text.y = element_text(size = 20)) +
     annotate("text", x="Early Childhood", y = 0.4, label = "0.8", colour = "white", size = 6) +
     annotate("text", x="Primary", y = 6.7, label = "7.1", colour = "white", size = 6) +
     annotate("text", x="Secondary", y = 1.7, label = "2.0", colour = "white", size = 6) +
     annotate("text", x="Post-Secondary", y = 0.6, label = "0.3", colour = "black", size = 6) +
     annotate("text", x="Tertiary", y = 0.6, label = "1.0", colour = "white", size = 6)
   
   #histogram
   ggplot(hiv.diagnosis, aes(hiv1)) +
     geom_histogram(binwidth = 10, center = 5,fill=I("grey"), col=I("black")) +
     theme_bw() +
     labs(x = "Age at diagnosis", y = "Frequency distribution")  +
     theme(axis.text=element_text(size=20),
           axis.title=element_text(size=24),
           strip.text.x = element_text(size = 20))
   
   #histogram shapes
   p1.1 <- ggplot(distributions, aes(normal)) + 
     geom_histogram(bins = 10, fill=I("grey"), col=I("black")) + 
     labs(x = "", y = "Count", title = "Symmetric, unimodal") +
     theme(axis.text=element_text(size=12),
          plot.title =element_text(size=24),
           axis.title = element_text(size = 20))
   
   p2.1 <- ggplot(distributions, aes(pos.skew2)) + 
     geom_histogram(bins = 10, fill=I("grey"), col=I("black")) + 
     labs(x = "", y = "Count", title = "Positive (left) skew, unimodal") +
     theme(axis.text=element_text(size=12),
           plot.title =element_text(size=24),
           axis.title = element_text(size = 20))
   
   p3.1 <- ggplot(distributions, aes(neg.skew)) + 
     geom_histogram(bins = 10, fill=I("grey"), col=I("black")) + 
     labs(x = "", y = "Count", title = "Negative (right) skew, unimodal") +
     theme(axis.text=element_text(size=12),
           plot.title =element_text(size=24),
           axis.title = element_text(size = 20))
   
   p4.1 <- ggplot(distributions, aes(bimodal)) + 
     geom_histogram(bins = 10, fill=I("grey"), col=I("black")) + 
     labs(x = "", y = "Count", title = "Symmetric, bimodal") +
     theme(axis.text=element_text(size=12),
           plot.title =element_text(size=24),
           axis.title = element_text(size = 20))
   
   plot_grid(p1.1, p4.1, p2.1, p3.1, nrow = 2)
   
   #cumulative frequency plot
   ggplot(test.freq, aes(Category.midpoint, cum.frequency)) + 
     geom_point() + 
     geom_line()+
     labs(x = "Test score (max = 100)", y = "Cumulative frequency") +
     theme(axis.text=element_text(size=20),
           axis.title=element_text(size=24))
   
   #line graph simple
   ggplot(nrrabbits, aes(days, rabbits)) +
     geom_line() +
     geom_point() +
     theme(axis.title=element_text(size=24),
           axis.text = element_text(size = 18)) +
     labs(x = "Number of rabbits", y = "Number of days") +
     xlim(0,130)

   #line graph complex
   ggplot(life.expectancy, aes(year, expectancy, colour = country)) + 
     geom_line() + 
     labs(x = "Year", y = "Life expectancy (years)", colour = "Country")+
     theme(axis.title=element_text(size=24),
           axis.text = element_text(size = 20),
           legend.text = element_text(size = 20),
           legend.title = element_text(size = 24),
           legend.key = element_rect(size = 5),
           legend.key.size = unit(1.5, 'lines'))
   
   ### fig 2.16 reproduced from De Neve article ------------------
   
   #recreate data based on De Neve bar chart      
   gender <- factor(rep(c("Men","Women"), each = 6))
   schoolyears <- factor(rep(c("0", "1-7", "8-9", "10", "11-12", "13+"), times = 2), 
                         ordered = T, levels = c("0", "1-7", "8-9", "10", "11-12", "13+"))   
   probhiv <- c(0.21, 0.22, 0.22, 0.18, 0.13, 0.13, 0.46, 0.45, 0.47, 0.41, 0.35, 0.2)   
   fig2.16data <- data.frame(gender, schoolyears, probhiv)
   
   #create new bar chart
   ggplot(fig2.16data, aes(schoolyears, probhiv, fill = gender)) + 
     geom_col(position = "dodge") + 
     scale_fill_manual(values= c("black","lightgrey")) +
     labs(x = "Years of schooling", y = "Probability HIV+", fill = "Gender")+
     theme(axis.title=element_text(size=24),
           axis.text = element_text(size = 22),
           legend.text = element_text(size = 22),
           legend.title = element_text(size = 24),
           legend.key = element_rect(size = 5),
           legend.key.size = unit(1.5, 'lines'))
  