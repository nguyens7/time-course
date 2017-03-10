
# To Clear working environment
rm(list=ls())
graphics.off()

library(tidyverse)
library(cowplot)
library(broom)
library(pwr)

setwd("~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/data")
setwd("~/GitHub/time-course/data")

rawdata <- "revised_MASTER-ExperimentSummary.csv"
timecourse <- "timecourse2017.csv"


data <- read_csv(rawdata)
tc <- read_csv(timecourse, na = c("","NA"))

data1 <- data %>%
  gather(Sample,Count,2:226)

# Separate samples by identifiers 
data2 <- data1 %>% 
  separate(Sample, into=c("Sample_ID","Dilution_factor","Injection","Tech_rep", sep = "_")) %>% 
  select(-`_`)

# Refactoring Columns for samples
str(data2)
data2$Sample_ID <- as.factor(data2$Sample_ID)
data2$Dilution_factor <- as.numeric(data2$Dilution_factor)
data2$Injection<- as.factor(data2$Injection)
data2$Tech_rep <- as.numeric(data2$Tech_rep)


# Refactoring COlumns for timecourse
str(tc)
tc$Sample_ID <- as.factor(tc$Sample_ID)
tc$Day <- as.factor(tc$Day)
tc$Weight <- as.numeric(tc$Weight)
tc$TEI_Day <- as.factor(tc$TEI_Day)

tc1 <- tc %>% 
  select(Day:Pups)

# Back-Calculate

data2 <- data2 %>% 
  mutate(True_Count=Dilution_factor*Count)


# Average the three technical replicate readings

data3 <- data2 %>% 
  group_by(particle_size,Sample_ID,Dilution_factor,Injection) %>% 
  summarise( tech_N = length(True_Count),
             tech_mean = mean(True_Count),
             tech_sd = sd(True_Count),
             tech_se = tech_sd/sqrt(tech_N))
data3


test1 <- left_join(tc1,data3, by= "Sample_ID")

# Summarize samples by injection (average both injections)

data4 <- data3 %>% 
  group_by(particle_size,Sample_ID,Dilution_factor) %>% 
  summarise( inj_N = length(tech_mean),
             inj_mean = mean(tech_mean),
             inj_sd = sd(tech_mean),
             inj_se = inj_sd/sqrt(inj_N))
data4

test2 <- left_join(tc1,data4, by= "Sample_ID")

test2


#  Graphing ---------------------------------------------------------------


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


# Average 
test3 <- test2 %>% 
  group_by(Day,particle_size) %>% 
  summarise(animal_N=length(Sample_ID),
            animal_mean = mean(inj_mean),
            animal_sd = sd(inj_mean),
            animal_se = animal_sd/sqrt(animal_N))

test4 <- test3 %>% 
  group_by(Day) %>% 
  summarise(particle_conc=sum(animal_mean))

test4 %>% 
  ggplot(aes(x=Day, y=particle_conc,fill=Day))+
  geom_bar(stat="identity")


test5 <- test2 %>% 
  group_by(Day,Sample_ID) %>% 
  summarise(particle_conc=sum(inj_mean))

test6 <- test5 %>% 
  group_by(Day) %>% 
  summarise(Day_N=length(particle_conc),
            Day_mean = mean(particle_conc),
            Day_sd = sd(particle_conc),
            Day_se = Day_sd/sqrt(Day_N))

# write_csv(test5,"~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/particle_counts.csv")
# write_csv(test6,"~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/particle_summary.csv")
# write_csv(stats,"~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/ANOVA.csv")
# write_csv(tukey,"~/Library/Mobile\ Documents/com~apple~CloudDocs/time-course/Tukey.csv")



plot1 <- test5 %>% 
  group_by(Day) %>% 
  ggplot(aes(factor(Day),particle_conc, color=Day)) +
  geom_boxplot(colour="black",fill=NA) + 
  geom_point(position='jitter',size=3)+
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



# Looking at Data by Day --------------------------------------------------

test7 <- test5 %>% 
  left_join(tc1)

test_data <- test7 %>%
  #filter(!TEI_Day %in% c(4,5), !Sample_ID %in% c(12,16)) %>% 
  ggplot(aes(x = Day, y = particle_conc))+
  geom_point(position= 'jitter',size=6)+
  #scale_shape_manual(values=c(15,16,17,22,23,24))+
  geom_line(aes(group=Day))+
  #geom_boxplot(colour="black",fill=NA) + 
  xlab("\nDay of Gestation\n") + # X axis label
  ylab("\nExosomes/ml\n") + # Y axis label
  ggtitle("Plasma Exosome Concentration\nThroughout Pregnancy\n")+ #title
  labs(color="Condition")#Label table title

test_data <- test7 %>%
  filter(!TEI_Day %in% c(4,5), !Sample_ID %in% c(12,16))


ANOVA <- aov(particle_conc ~ Day, data=test_data)
tidy(ANOVA)

Tukey <- tidy(TukeyHSD(ANOVA)) 
Tukey%>% 
  filter(adj.p.value<0.05) %>% 
  arrange(adj.p.value)

pwr.anova.test(f = 1, k = 6, n = 3, power = ,sig.level =0.05 )

