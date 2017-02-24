
#To Clear working environment
rm(list=ls())
graphics.off()

library(tidyverse)
library(cowplot)
library(broom)
library(pwr)

setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/")
setwd("~/GitHub/time-course")

rawdata <- "MASTER-ExperimentSummary.csv"
timecourse <- "timecourse2017.csv"
day1rerun <- "Day1_second_attempt-ExperimentSummary.csv"

data <- read_csv(rawdata)
tc <- read_csv(timecourse, na = c("","NA"))
day1 <- read_csv(day1rerun)


dim(data)
dim(tc)

head(data)

tail(data)


data1 <- data %>%
  gather(Sample,Count,2:223)


#Separate samples by identifiers 
data2 <- data1 %>% 
  separate(Sample, into=c("Sample_ID","Dilution_factor","Injection","Tech_rep", sep = "_")) %>% 
  select(-`_`)

str(data2)
data2$Sample_ID <- as.factor(data2$Sample_ID)
data2$Dilution_factor <- as.numeric(data2$Dilution_factor)
data2$Injection<- as.factor(data2$Injection)
data2$Tech_rep <- as.numeric(data2$Tech_rep)

str(data2)


#Back-Calculate

data2 <- data2 %>% 
  mutate(True_Count=Dilution_factor*Count)


data3 <- data2 %>% 
  group_by(particle_size,Sample_ID,Dilution_factor,Injection) %>% 
  summarise( tech_N = length(True_Count),
             tech_mean = mean(True_Count),
             tech_sd = sd(True_Count),
             tech_se = tech_sd/sqrt(tech_N))
data3

#Refactoring to get the sample ID in the correct order

str(tc)
tc$Sample_ID <- as.factor(tc$Sample_ID)
tc$Day <- as.factor(tc$Day)
tc$Weight <- as.numeric(tc$Weight)
tc$TEI_Day <- as.factor(tc$TEI_Day)

tc1 <- tc %>% 
  select(Day:Pups)


str(tc1)

test1 <- inner_join(tc1,data3, by= "Sample_ID")


#Summarize samples by injection

data4 <- data3 %>% 
  group_by(particle_size,Sample_ID,Dilution_factor) %>% 
  summarise( inj_N = length(tech_mean),
             inj_mean = mean(tech_mean),
             inj_sd = sd(tech_mean),
             inj_se = inj_sd/sqrt(inj_N))
data4

test2 <- left_join(tc1,data4, by= "Sample_ID")



#Graphing data

test1$Sample_ID_correct = factor(test1$Sample_ID, levels=c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36'))

graph1 <- test1 %>%
  ggplot(aes(x=particle_size, y=tech_mean,color=Injection ))+ #plot
  geom_ribbon(aes(ymin=tech_mean-tech_se, ymax=tech_mean+tech_se),alpha=0.2,fill = alpha('grey12', 0.2)) + #error bars
  geom_line(size=2.0) + xlim(0,500)+ #line size, x-axis scale
  scale_y_continuous(expand=c(0,0))+ #set bottom of graph
  xlab("Particle Size") + # X axis label
  ylab("\nMean Particle Concentration/ml\n") + # Y axis label
  ggtitle("Nanosight Histogram of\nVirgin Mouse Plasma")+ #title
  labs(color="Injection")+ #Label table title
  facet_wrap( ~ Sample_ID_correct, nrow=6)
graph1

graph2 <- test2 %>%
  group_by(TEI_Day) %>% 
  ggplot(aes(x=particle_size, y=inj_mean,color=Day ))+ #plot
  #geom_ribbon(aes(ymin=inj_mean-inj_se, ymax=inj_mean+inj_se),alpha=0.2,fill = alpha('grey12', 0.2)) + #error bars
  geom_line(size=2) + xlim(0,500)+ #line size, x-axis scale
  scale_y_continuous(expand=c(0,0))+ #set bottom of graph
  xlab("Particle Size") + # X axis label
  ylab("\nMean Particle Concentration/ml\n") + # Y axis label
  ggtitle("Nanosight Histogram of\nVirgin Mouse Plasma")+ #title
  labs(color="Condition")+ #Label table title
  facet_wrap(~ TEI_Day, ncol=6)
graph2

plot_grid(graph1,graph2, labels = c("Individual\nInjections","Averaged"),nrow = 2, align = "v")

test3 <- test2 %>% 
  group_by(Day,particle_size) %>% 
  summarise(animal_N=length(Sample_ID),
            animal_mean = mean(inj_mean),
            animal_sd = sd(inj_mean),
            animal_se = animal_sd/sqrt(animal_N))

test3 %>% 
  ggplot(aes(x=Day, y=animal_mean,color=Day))+
  geom_col()



test4 <- test3 %>% 
  group_by(Day) %>% 
  summarise(particle_conc=sum(animal_mean))

test4 %>% 
  ggplot(aes(x=Day, y=particle_conc,fill=Day))+
  geom_bar(stat="identity")


test5 <- test2 %>% 
  filter(!Sample_ID=="15") %>% 
  group_by(Day,Sample_ID) %>% 
  summarise(particle_conc=sum(inj_mean))

test6 <- test5 %>% 
  filter(!Sample_ID=="15") %>% 
  group_by(Day) %>% 
  summarise(Day_N=length(particle_conc),
            Day_mean = mean(particle_conc),
            Day_sd = sd(particle_conc),
            Day_se = Day_sd/sqrt(Day_N))

write_csv(test5,"~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/particle_counts.csv")
write_csv(test6,"~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/particle_summary.csv")
write_csv(stats,"~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/ANOVA.csv")
write_csv(tukey,"~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/Tukey.csv")



plot1 <- test5 %>% 
  filter(!Sample_ID=="12") %>% 
  group_by(Day) %>% 
  ggplot(aes(factor(Day),particle_conc, color=Day)) +
  geom_boxplot(colour="black",fill=NA) + 
  geom_point(position='jitter')+
  xlab("\nDay of Gestation\n") + # X axis label
  ylab("\nExosomes/ml\n") + # Y axis label
  ggtitle("Plasma Exosome Concentration\nThroughout Pregnancy\n")+ #title
  labs(color="Condition")#Label table title


png("~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/particle_boxplot.png", width = 7, height = 5, units = 'in', res = 600)
plot1
dev.off()


jpeg("~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/particle_boxplot.jpg", width = 7, height = 5, units = 'in', res = 600)
plot1
dev.off()

plot <- test6 %>% 
  ggplot(aes(x=Day, y=Day_mean, fill=Day ))+ #plot
  geom_col()+
  geom_errorbar(aes(ymin=Day_mean-Day_se, ymax=Day_mean+Day_se), width=.5, 
                size=0.8, colour="black", position=position_dodge(.9)) + #error bars
  scale_y_continuous(expand=c(0,0))+ #set bottom of graph
  xlab("\nDay of Gestation\n") + # X axis label
  ylab("\nExosomes/ml\n") + # Y axis label
  ggtitle("Plasma Exosome Concentration\nThroughout Pregnancy\n")+ #title
  labs(color="Condition")#Label table title


png("~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/particle_summary.png", width = 7, height = 5, units = 'in', res = 600)
plot
dev.off()



fit <- aov(particle_conc ~ Day, data=test5)


stats <- tidy(fit)

HSD <- TukeyHSD(fit)

tukey <- tidy(HSD)

power <- pwr.anova.test(k = 6 , n =  , f = 1, sig.level =.05 , power =0.8  )

tidy(power)

pwr.anova.test(k = 6 , n =6 , f = 0.8, sig.level =.05 , power =)









# Test --------------------------------------------------------------------

test3_1 <- test2 %>% 
  filter(!TEI_Day %in% c( '4','5','6')) %>%
  group_by(Day,particle_size) %>% 
  summarise(animal_N=length(Sample_ID),
            animal_mean = mean(inj_mean),
            animal_sd = sd(inj_mean),
            animal_se = animal_sd/sqrt(animal_N))


test4_1 <- test2 %>% 
  filter(!TEI_Day %in% c( '1','4','5','6')) %>%
  group_by(Day,Sample_ID,TEI_Day) %>% 
  summarise(particle_conc=sum(inj_mean))

test4_1 %>% 
  ggplot(aes(factor(Day),particle_conc, color=Day)) +
  geom_boxplot(colour="black",fill=NA) + 
  geom_point(position='jitter')+
  xlab("\nDay of Gestation\n") + # X axis label
  ylab("\nExosomes/ml\n") + # Y axis label
  ggtitle("Plasma Exosome Concentration\nThroughout Pregnancy\n")+ #title
  labs(color="Condition")#Label table title


fit <- aov(particle_conc ~ Day, data=test4_1)
tidy(fit)

HSD <- TukeyHSD(fit)

tukey <- tidy(HSD)

tukey %>% 
  filter(adj.p.value<0.05) %>% 
  arrange(adj.p.value)



# Reanalyzing day 1 -------------------------------------------------------

day1_1 <- day1 %>%
  gather(Sample,Count,2:37)

day1_2 <- day1_1 %>% 
  separate(Sample, into=c("Sample_ID","Dilution_factor","Injection","Tech_rep", sep = "_")) %>% 
  select(-`_`)

day1_2$Sample_ID <- as.factor(day1_2$Sample_ID)
day1_2$Dilution_factor <- as.numeric(day1_2$Dilution_factor)
day1_2$Injection<- as.factor(day1_2$Injection)
day1_2$Tech_rep <- as.numeric(day1_2$Tech_rep)

day1_2 <- day1_2 %>% 
  mutate(True_Count=Dilution_factor*Count)

day1_3 <- day1_2 %>% 
  group_by(particle_size,Sample_ID,Dilution_factor,Injection) %>% 
  summarise( tech_N = length(True_Count),
             tech_mean = mean(True_Count),
             tech_sd = sd(True_Count),
             tech_se = tech_sd/sqrt(tech_N))

merge1 <- inner_join(tc1,day1_3, by= "Sample_ID")

#Summarize samples by injection

day1_4 <- day1_3 %>% 
  group_by(particle_size,Sample_ID,Dilution_factor) %>% 
  summarise( inj_N = length(tech_mean),
             inj_mean = mean(tech_mean),
             inj_sd = sd(tech_mean),
             inj_se = inj_sd/sqrt(inj_N))

day1_4

merge2 <- right_join(tc1,day1_4, by= "Sample_ID")

merge3 <- merge2 %>% 
  filter(!Sample_ID == '12') %>% 
  group_by(Day,Sample_ID,TEI_Day) %>% 
  summarise(particle_conc=sum(inj_mean))

merge3

merge4 <- bind_rows(test4_1,merge3)

merge4$Sample_ID <- as.factor(merge4$Sample_ID)

new <- aov(particle_conc ~ Day,data=merge4)

tidy(new)

HSD2 <- TukeyHSD(new)
tukey2 <- tidy(HSD2)


merge4 %>% 
  ggplot(aes(factor(Day),particle_conc, color=Day)) +
  geom_boxplot(colour="black",fill=NA) + 
  geom_point(position='jitter',size=3)+
  xlab("\nDay of Gestation\n") + # X axis label
  ylab("\nExosomes/ml\n") + # Y axis label
  ggtitle("Plasma Exosome Concentration\nThroughout Pregnancy\n")+ #title
  labs(color="Condition")#Label table title

tukey2 %>% 
  filter(adj.p.value<0.05) %>% 
  arrange(adj.p.value)

normality <- shapiro.test(merge4$particle_conc)
normal <- tidy(normality)
