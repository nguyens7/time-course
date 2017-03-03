
#To Clear working environment
rm(list=ls())
graphics.off()


library(tidyverse)
library(cowplot)
library(broom)
library(ggvis)
library(pwr)

setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/")
setwd("~/GitHub/time-course")

standards <- "standards_MASTER-ExperimentSummary.csv" 
rawdata <- "MASTER-ExperimentSummary.csv"
timecourse <- "timecourse2017.csv"
restandards <- "std_reanalysis.csv"

data <- read_csv(rawdata)
tc <- read_csv(timecourse, na = c("","NA"))
std <- read_csv(standards)
re_std <- read_csv(restandards)

std1 <- std %>%
  gather(Sample,Count,2:70)

std2 <- std1 %>% 
  separate(Sample, into=c("Sample_ID","When","Dilution_factor","Nano_day","Injection","Tech_Rep", sep = "_")) %>% 
  select(-`_`)

str(std2)
std2$Sample_ID <- as.factor(std2$Sample_ID)
std2$When <- as.factor(std2$When)
std2$Dilution_factor <- as.numeric(std2$Dilution_factor)
std2$Injection<- as.factor(std2$Injection)
std2$Nano_day <- as.numeric(std2$Nano_day)
str(std2)

#Back-Calculate
std2 <- std2 %>% 
  mutate(True_Count=Dilution_factor*Count)

std2$Nano_day <-  factor(std2$Nano_day, levels=c('1','2','3','4','5','6'))
std2$When <- factor(std2$When, levels=c('before','after'))

#Summarize the 3 technical reps
std3 <- std2 %>% 
  group_by(particle_size,Sample_ID,When,Dilution_factor,Nano_day,Injection) %>% 
  summarise( tech_N = length(True_Count),
             tech_mean = mean(True_Count),
             tech_sd = sd(True_Count),
             tech_se = tech_sd/sqrt(tech_N))
std3

#Summarize samples by injection
std4 <- std3 %>% 
  group_by(Nano_day,When,particle_size) %>% 
  summarise( inj_N = length(tech_mean),
             inj_mean = mean(tech_mean),
             inj_sd = sd(tech_mean),
             inj_se = inj_sd/sqrt(inj_N))
std4



##TROUBLESHOOTING###
filter(!Nano_day=="2",!When=="after")

test <- std2 %>% 
  group_by(particle_size,Sample_ID,When,Dilution_factor,Nano_day,Injection) %>%
  filter(!Nano_day=="2"|!When=="before") %>% 
  summarise( tech_N = length(True_Count),
             tech_mean = mean(True_Count),
             tech_sd = sd(True_Count),
             tech_se = tech_sd/sqrt(tech_N))

test %>%
  ggplot(aes(x=particle_size, y=tech_mean,color=When ))+ #plot
  geom_ribbon(aes(ymin=tech_mean-tech_se, ymax=tech_mean+tech_se),alpha=0.2,fill = alpha('grey12', 0.2)) + #error bars
  geom_line(size=1.0) + xlim(0,300)+ #line size, x-axis scale
  scale_y_continuous(expand=c(0,0))+ #set bottom of graph
  xlab("Particle Size") + # X axis label
  ylab("\nMean Particle Concentration/ml\n") + # Y axis label
  ggtitle("Nanosight Histogram of\nVirgin Mouse Plasma")+ #title
  labs(color="Injection")+ #Label table title
  facet_wrap(When ~ Nano_day,nrow=1)




########################

#Counting
count <- std2 %>% 
  group_by(When,Nano_day,Injection) %>% 
  summarise(particle_conc=sum(True_Count))



count1 <- count %>%
  group_by(When,Nano_day) %>% 
  summarise( std_N = length(particle_conc),
             std_mean = mean(particle_conc),
             std_sd = sd(particle_conc),
             std_se = std_sd/sqrt(std_N))



#Graphing data at the injection level
graph1 <- std3 %>%
  ggplot(aes(x=particle_size, y=tech_mean,color=When ))+ #plot
  geom_ribbon(aes(ymin=tech_mean-tech_se, ymax=tech_mean+tech_se),alpha=0.2,fill = alpha('grey12', 0.2)) + #error bars
  geom_line(size=1.0) + xlim(0,300)+ #line size, x-axis scale
  scale_y_continuous(expand=c(0,0))+ #set bottom of graph
  xlab("Particle Size") + # X axis label
  ylab("\nMean Particle Concentration/ml\n") + # Y axis label
  ggtitle("Nanosight Histogram of\nVirgin Mouse Plasma")+ #title
  labs(color="Injection")+ #Label table title
  facet_wrap( ~ Nano_day, ncol=6)
graph1


#Graphing data at the day level
graph2 <- std4 %>%
  ggplot(aes(x=particle_size, y=inj_mean,color=When ))+ #plot
  geom_ribbon(aes(ymin=inj_mean-inj_se, ymax=inj_mean+inj_se),alpha=0.2,fill = alpha('grey12', 0.2)) + #error bars
  geom_line(size=2) + xlim(0,500)+ #line size, x-axis scale
  scale_y_continuous(expand=c(0,0))+ #set bottom of graph
  xlab("Particle Size") + # X axis label
  ylab("\nMean Particle Concentration/ml\n") + # Y axis label
  ggtitle("Nanosight Histogram of\nVirgin Mouse Plasma")+ #title
  labs(color="Condition")+ #Label table title
  facet_wrap(~ Nano_day, ncol=6)
graph2

#Graphing counts

graph3 <- count %>% 
  ggplot(aes(x=Nano_day,y=particle_conc,fill=When))+
  geom_col(position="dodge")+
  scale_y_continuous(expand=c(0,0))+ #set bottom of graph
  xlab("Experimental Day") + # X axis label
  ylab("\nMean Particle Concentration/ml\n") + # Y axis label
  ggtitle("Malvern 100nm standard Interassay Variation")+ #title
  labs(color="Reading During\nExperiment") #Label table title




graph4 <- count1 %>% 
  group_by(Nano_day) %>% 
  ggplot(aes(x=Nano_day,y=std_mean,fill=When))+
  geom_col(position="dodge")+
  geom_errorbar(aes(ymin=std_mean-std_se, ymax=std_mean+std_se), width=.5, 
                size=0.8, colour="black", position=position_dodge(.9))+
  scale_y_continuous(expand=c(0,0))+ #set bottom of graph
  xlab("Experimental Day") + # X axis label
  ylab("\nMean Particle Concentration/ml\n") + # Y axis label
  ggtitle("Malvern 100nm standard Interassay Variation")+ #title
  labs(color="Reading During\nExperiment") #Label table title

ggsave("\Users\NanoSight\Documents\GitHub\time-course\Variation")




# New analysis ------------------------------------------------------------

std4 %>% 
  filter(Nano_day == '1') %>% 
  ggvis(~particle_size,~inj_mean) %>% 
  layer_points(size := input_slider(100, 1000, value = 100)) %>% 
  layer_lines() 

# Power Analysis ----------------------------------------------------------


pwr.anova.test(f= ,k=6, n=6, sig.level=0.05, power=0.8)


