#########This is a guideline for importing and formating the data as well as the code to generate all the graphs/figures

#To Clear working environment
rm(list=ls())
graphics.off()

# Set the correct root folder
setwd("~/Desktop/R_Folder")

#Load ggplot2,plyr and dplyr packages
library(ggplot2)
library(magrittr)
library(plyr)
library(tidyr)
library(dplyr)
library(ggvis)
library(RColorBrewer)
library(reshape2)

#Make a default theme for optimizeing graph aesthetics
publication_style <- theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
                           axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
                           panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                           panel.grid.minor=element_line(colour=NA),            #Remove grid lines
                           axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
                           axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                           plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30




#Function to convert a factor into a numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#Import tc_wide.csv
my_data <- "tc_wide.csv"
tc <- read.csv(my_data, sep = ",", header = TRUE)

#Make sure you imported the CSV file
summary(tc)

#Look at the headers for the data
head(tc)
tail(tc)

#Calculating the variance
weight.var <- tc%>%
  arrange(day)%>%
  group_by(day)%>%
var(tc$weight, na.rm=TRUE)


#Calculating the average mean through pregnancy
weight_line_graph <-tc%>%
  arrange(day)%>%
  group_by(day)%>%
  summarise(N = length(weight),        
         mean = mean(weight),
           sd = sd(weight),
           se = sd / sqrt(N))%>%
  ggplot(aes(x=day, y=mean,  group=1, colour=parameter)) +
  geom_line(size=2, colour="black") + geom_point(size=3, colour="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.8,  # Width of the error bars
                size=1.5,   # Size of the error bars
                colour="black", #color of the error bars
                position=position_dodge(.9)) +
  theme(legend.position="none") + #remove legend
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Weight (g)") +  # Y axis label  
  ggtitle("Weight Throughout Pregnancy") + scale_y_continuous(expand=c(0,0),limits = c(15, 35)) # Plot title

weight_line_plot_final <- weight_line_graph + publication_style
weight_line_plot_final
  
png("~/Desktop/R_Folder/weight_line_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
weight_line_plot_final
dev.off()
  

#Calculating the weight accounting for number of pups
 ratio <- tc%>%
    arrange(day)%>%
    group_by(day)%>%
    filter(day > 1, pups > 4)%>%
    mutate(pup.weight.ratio = weight/pups)%>%
    summarise(N = length(pup.weight.ratio),        
              mean = mean(pup.weight.ratio),
              sd = sd(pup.weight.ratio),
              se = sd / sqrt(N))%>%
    ggplot(aes(x=day, y=mean,  group=1)) +
    geom_line(size=2, colour="black") + geom_point(size=3, colour="black") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.8, size=1.5, colour="black", position=position_dodge(.9)) +
    theme(legend.position="none") +
    xlab("Day of Gestation") + 
    ylab("Weight/Pup (g)") +  
    ggtitle("Normalized Weight/Pup") + scale_y_continuous(expand=c(0,0),limits = c(0, 4.5)) 
  
ratio_final <- ratio + publication_style

ratio_final

png("~/Desktop/R_Folder/ratio_line_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
ratio_final
dev.off()

#Test for ggvis ratio
ratio.ggvis <- tc%>%
  arrange(day)%>%
  group_by(day)%>%
  filter(day > 1, pups > 4)%>%
  mutate(pup.weight.ratio = weight/pups)%>%
  summarise(N = length(pup.weight.ratio),        
            mean = mean(pup.weight.ratio),
            sd = sd(pup.weight.ratio),
            se = sd / sqrt(N))%>%
  ggvis(x = ~day, y = ~mean)%>%
  layer_points()%>%
  layer_lines()

#Test for ggvis ratio
tc%>%
  arrange(day)%>%
  group_by(day)%>%
  filter(day > 1, pups > 4)%>%
  ggvis(x = ~day, y = ~weight)%>%
  layer_points()%>%
  layer_lines()

#Test for ggvis ratio
tc%>%
  arrange(day)%>%
  group_by(day)%>%
  ggvis(x = ~day, y = ~weight)%>%
  layer_points()%>%
  layer_lines()


#ggvis histogram of number of pups
tc%>%
  arrange(day)%>%
  group_by(day)%>%
  ggvis(~pups) %>% layer_histograms()

#ggvis histogram of number of implantation sites
#still needs work....
tctest%>%
  filter(parameter == "left.horn"|parameter == "right.horn",day == "5"|day == "10"| day == "14"| day == "17")%>%
  arrange(day,parameter)%>%
  ggvis(~value,fill= ~factor(parameter)) %>% 
  group_by(parameter)%>%
  layer_histograms()
 ?layer_histograms  



#Calculating the average number of pups 
tc%>%
  arrange(day)%>%
  group_by(day)%>%
  summarise(N = length(pups),        
            mean = mean(pups),
            sd = sd(pups),
            se = sd / sqrt(N))


##### Generation of BAR GRAPHS
#We want to be able to average the three representative placentas from each individual mouse
# ie- we want to average placentas 1,2 and 3 and make it a new column

tc <- transform(tc, plac.avg = rowMeans(tc[,6:8], #create a new row named "plac.avg" that averages columns 6-8 (placenta.1-placenta.3)
                  na.rm = TRUE)) #ignore NA values
#see if tc works
tc

#Data is formated in 'wide format' which is conducive for us to interpret but we need to make it 
#into 'long format' so that R can understand it better by using the gather' function of the tidyr package

tctest <- tc%>%
  gather(parameter,value,-day) #This reads as "Gather all variables except day,
                               #and put them into a new column named 'variable' and the new value column 'value'."
tctest
str(tctest)

tctest$day <- factor(tctest$day, levels=c("0","5","10","14","17","19")) #Adjust the day column and make it in the correct order

#Parameter needs to be factored uniquely*****
tctest$parameter <- factor(tctest$parameter, levels=unique(tctest$parameter))

str(tctest)

#Determination of mean placental weight
plac.weight_plot <- tctest%>%
  filter(parameter == "plac.avg",day == "10"|day == "14"|day == "17")%>%
  arrange(day)%>%
  group_by(day,parameter)%>%
  summarise(N = length(value),        
            mean = mean(value)*1000, #convert from to g to mg
            sd = sd(value)*1000,     #convert from to g to mg
            se = sd / sqrt(N))%>%
  ggplot(aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25,position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Placenta Weight (mg)") +  # Y axis label
  scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + #removes legend
  ggtitle("") + scale_y_continuous(expand=c(0,0), limits = c(0,110)) # Plot title

plac.weight_plot_final <- plac.weight_plot + publication_style

plac.weight_plot_final 

png("~/Desktop/R_Folder/plac.weight_plot.png", width = 7, height = 5, units = 'in', res = 600)
plac.weight_plot_final 
dev.off()

#Determination of the mean number of pups
pups_plot <- tctest%>%
  filter(parameter == "pups")%>%
  arrange(day)%>%
  group_by(day,parameter)%>%
  summarise(N = length(value),        
            mean = mean(value), 
            sd = sd(value),
            se = sd / sqrt(N))%>%
  ggplot(aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25,position=position_dodge(.9)) +
  xlab("Gestational Age") +  # X axis label
  ylab("Mean Number of Pups") +  # Y axis label
  scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + #removes legend
  ggtitle("Number of Pups") + scale_y_continuous(expand=c(0,0), limits = c(0,10)) # Plot title

pups_plots_final <- pups_plot + publication_style

pups_plots_final

png("~/Desktop/R_Folder/pups_plot.png", width = 7, height = 5, units = 'in', res = 600)
pups_plots_final 
dev.off()

#CREATING A PLOT SIMILAR TO GRAPHPAD PRISM IF YOU COUNT ANIMALS WITH MORE THAN 4 PUPS
weight_boxplot_morethan4 <- tc%>%
  filter( pups > 4)%>%
  gather(parameter,value,-day)%>%
  filter(parameter == "weight")%>%
  arrange(day)%>%
  group_by(day,parameter)%>%
  ggplot(aes(factor(day),value, fill=parameter)) +
  geom_boxplot(colour="black",fill=NA) + 
  geom_point(position='jitter') + 
  xlab("Gestational Age") +  # X axis label
  ylab("Weight (g)") +  # Y axis label
  scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + #removes legend
  ggtitle("Weight Throughout Pregnancy") + scale_y_continuous(expand=c(0,0), limits = c(10,40)) # Plot title
  
weight_boxplot_morethan4_final <- weight_boxplot_morethan4 + publication_style

png("~/Desktop/R_Folder/weight_boxplot_morethan4.png", width = 7, height = 5, units = 'in', res = 600)
weight_boxplot_morethan4_final
dev.off()

#CREATING A WEIGHT PLOT SIMILAR TO GRAPHPAD PRISM 
weight_boxplot <- tctest%>%
  filter(parameter == "weight")%>%
  arrange(day)%>%
  group_by(day,parameter)%>%
  ggplot(aes(factor(day),value, fill=parameter)) +
  geom_boxplot(colour="black",fill=NA) + 
  geom_point(position='jitter') + 
  xlab("Gestational Age") +  # X axis label
  ylab("Weight (g)") +  # Y axis label
  scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + #removes legend
  ggtitle("Weight Throughout Pregnancy") + scale_y_continuous(expand=c(0,0), limits = c(10,40)) # Plot title

weight_boxplot_final <- weight_boxplot + publication_style

png("~/Desktop/R_Folder/weight_boxplot.png", width = 7, height = 5, units = 'in', res = 600)
weight_boxplot_final 
dev.off()

  
#Determination of pup implantation within the uterine horn
implantation <- tctest%>%
  filter(parameter == "left.horn"|parameter == "right.horn",day == "5"|day == "10"| day == "14"| day == "17")%>%
  arrange(day)%>%
  group_by(day,parameter)%>%
  summarise(N = length(value),        
            mean = mean(value), 
            sd = sd(value),
            se = sd / sqrt(N))%>%
  ggplot(aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25,position=position_dodge(.9)) +
  xlab("Gestational Age") +  # X axis label
  ylab("Mean Number of Pups") +  # Y axis label
  #scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + #removes legend
  ggtitle("") + scale_y_continuous(expand=c(0,0), limits = c(0,8)) # Plot title

implantation_final <- implantation + publication_style

implantation_final

png("~/Desktop/R_Folder/implantation_plot.png", width = 7, height = 5, units = 'in', res = 600)
implantation_final 
dev.off()
