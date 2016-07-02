#########This is a guideline for importing and formating the data as well as the code to generate all the graphs/figures

# Set the correct root folder
setwd("~/Desktop/R_Folder")

#Load ggplot2,plyr and dplyr packages
library(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(reshape2)

#Function to convert a factor into a numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#Import group1.csv
my_data <- "tc_wide.csv"
tc <- read.csv(my_data, sep = ",", header = TRUE)

#Make sure you imported the CSV file
summary(tc)

#Look at the headers for the data
head(tc)

##### MAY HAVE TO REPOSITION THIS FUNCTION TO ANOTHER SPOT
#We want to be able to average the three representative placentas from each individual mouse
# ie- we want to average placentas 1,2 and 3 and make it a new column

tc <- transform(tc, plac.avg = rowMeans(tc[,6:8], #create a new row named "plac.avg" that avegages columsn 6-8 (placenta.1-placenta.3)
                                        na.rm = TRUE)) #ignore NA values
#see if tc works
tc

#Data is formated in "wide format" which is conducive for us to interpret but we need to make it 
#into "long format" so that R can understand it better by using the "melt" function of the dplyr package

melt(tc) #makes the column values organized by a single column named variable

#rename the "melted" data as "tcl" [t]ime [c]ourse [l]ong format
tcl <- melt(tc, id.vars = c("day"),
            variable.name = "parameter", 
            value.name = "value")


#check if the data tcl is correct
head(tcl)
tail(tcl)

#If there's missing values in the data it will show up at NA, you can tell R to ignore it by using the following function
tclc<- na.omit(tcl) #[t]ime [c]ourse [l]ong format [c]orrected

tclc

#Check that the blank values no longer include "NA"
summary(tclc)
head(tclc)

#Check how R interprets the columns in the data
#Sometimes you have your data in a particular order but R will organize them in alaphabetical order
str(tclc)


# Make day column as factor
# This effectlively tells R to treat your data as it and DON'T alphabetize it
# You tell R which column you want it to read as a factor (leave it in the order that you entered the data)
tclc$day <- factor(tclc$day, levels=c("0","5.5","10.5","14.5","17.5","1.p"))  #Adjust the day column and make it in the correct order

## Test to see if parameter needs to be factored uniquely*****
tclc$parameter <- factor(tclc$parameter, levels=unique(tclc$parameter))

#Verify the factor is in the correct order
str(tclc)

#Summarize the data
tc_data <- ddply(tclc, c("day","parameter"), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd / sqrt(N))


#check if data is truly summarized
tc_data

#Plot all the data
tc_plot <- ggplot(tc_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mouse parameter (in g or #)") +  # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("Changes within mouse during pregnancy") + scale_y_continuous(expand=c(0,0)) # Plot title


# Check if plot worked
tc_plot

#fix axes
tc_plot_final <- tc_plot +theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
                                axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
                                panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                                panel.grid.minor=element_line(colour=NA),            #Remove grid lines
                                axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
                                axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                                plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30


#Check the final plot
tc_plot_final



#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_hi_res_300.png", width = 14, height = 10, units = 'in', res = 300)
tc_plot_final
dev.off()

########################################################################

#Filter weight
tc_weight_data <- tc_data[tc_data$parameter %in% c("weight"),]

summary(tc_data)
summary(tc_weight_data)

#plot weight data
weight_plot <- ggplot(tc_weight_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Weight (g)") +  # Y axis label
  scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("Weight of Mice During Pregnancy") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
weight_plot

#fix axes
tc_weight_final <- weight_plot +theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
                                      axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
                                      panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                                      panel.grid.minor=element_line(colour=NA),            #Remove grid lines
                                      axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
                                      axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                                      plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30


#Check the final plot
tc_weight_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_Weight_hi_res_300.png", width = 14, height = 10, units = 'in', res = 300)
tc_weight_final
dev.off()


###########################################################################


#Filter pups
tc_pup_data <- tc_data[tc_data$parameter %in% c("pups"),]

summary(tc_data)
summary(tc_pup_data)

#plot weight data
pup_plot <- ggplot(tc_pup_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Days of Gestation") +  # X axis label
  ylab("Number of Pups") +  # Y axis label
  scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("Number of Mouse Pups During Pregnancy") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
pup_plot

#fix axes
tc_pup_final <- pup_plot +theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
                                axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
                                panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                                panel.grid.minor=element_line(colour=NA),            #Remove grid lines
                                axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
                                axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                                plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30


#Check the final plot
tc_pup_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_pup_hi_res_300.png", width = 14, height = 10, units = 'in', res = 300)
tc_pup_final
dev.off()


########################################################################################

#Calculation of Pup Implantation Within Uterine Horns
tc_horn_data <- tc_data[tc_data$parameter %in% c("left.horn","right.horn"),]

summary(tc_data)

#Check if horn data works
summary(tc_horn_data)
head(tc_horn_data)

#plot weight data
horn_plot <- ggplot(tc_horn_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Implantation Sites") +  # X axis label
  ylab("Number of Pups") +  # Y axis label
  #scale_fill_hue(name="Uterine Horn", breaks=c("Left Horn", "Right Horn"), labels=c("Left Horn", "Right Horn")) + 
  ggtitle("Uterine Horn Implantation Distribution") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
horn_plot

#fix axes
tc_horn_final <- horn_plot +theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
                                  axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
                                  panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                                  panel.grid.minor=element_line(colour=NA),            #Remove grid lines
                                  axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
                                  axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                                  plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30


#Check the final plot
tc_horn_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_horn_hi_res_300.png", width = 14, height = 10, units = 'in', res = 300)
tc_horn_final
dev.off()


##########################################################################

#Calculation of mean placenta weight throughout gestation

#Calculation of Pup Implantation Within Uterine Horns
tc_plac_data <- tc_data[tc_data$parameter %in% c("placenta.1","placenta.2","placenta.3"),]

#Check if placenta data works
summary(tc_plac_data)
head(tc_plac_data)

#plot placenta weight data
plac_plot <- ggplot(tc_plac_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Placenta Weight (g)") +  # Y axis label
  #scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + 
  ggtitle("Individual Placenta Weight Averages From Mice") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
plac_plot

#fix axes
tc_plac_final <- plac_plot +theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
                                  axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
                                  panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                                  panel.grid.minor=element_line(colour=NA),            #Remove grid lines
                                  axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
                                  axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                                  plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30


#Check the final plot
tc_plac_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_plac_hi_res_300.png", width = 14, height = 10, units = 'in', res = 300)
tc_plac_final
dev.off()

##########################################################################################

#Calculation of finalized mean placenta weight throughout gestation

#Calculation of Pup Implantation Within Uterine Horns
tc_mean_plac_data <- tc_data[tc_data$parameter %in% c("plac.avg"),]

#Check if placenta data works
summary(tc_mean_plac_data)
head(tc_mean_plac_data)

#plot placenta weight data
mean_plac_plot <- ggplot(tc_mean_plac_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Placenta Weight (g)") +  # Y axis label
  scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + 
  ggtitle("Mean Placenta Weight") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
mean_plac_plot

#fix axes
tc_mean_plac_final <- mean_plac_plot +theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
                                            axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
                                            panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                                            panel.grid.minor=element_line(colour=NA),            #Remove grid lines
                                            axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
                                            axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                                            plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30


#Check the final plot
tc_mean_plac_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_mean_plac_hi_res_300.png", width = 14, height = 10, units = 'in', res = 300)
tc_mean_plac_final
dev.off()

























##########################################################################################

#Do not use.

#Plot data for mean placenta weight
summary(tc_plac_data)
head(tc_plac_data)



#Determine mean from placenta.1, placenta.2 placenta.3 
tc_plac_data$true.mean <- ddply(tc_plac_data, c("day","parameter"), summarise,
                                N    = length(mean),
                                mean = mean(mean),
                                sd   = sd(mean),
                                se   = sd / sqrt(N))

tc_plac_data$true.mean













###################################################################################################################

#Do not use yet.
t_c$weight <- factor(t_c$weight, levels=unique(t_c$weight))
t_c$pup <- factor(t_c$pup, levels=unique(t_c$pup))
t_c$left.horn <- factor(t_c$left.horn, levels=unique(t_c$left.horn))
t_c$right.horn <- factor(t_c$right.horn, levels=unique(t_c$right.horn))
t_c$resorption.sites <- factor(t_c$resorption.sites, levels=unique(t_c$resorption.sites))

#Check that is does change it
str(t_c)

#Make a new column names "plac.mean" with placenta weight average
t_c$plac.mean <- ave(t_c$placenta.1,t_c$placenta.2,t_c$placenta.3, FUN = mean)

#Check to make sure the column was made
summary(t_c)


#Omit NAs in data, (if there's no value for placenta IE virigin females then R will ignore them)
t_c <- na.omit(t_c)

summary(t_c)

head(t_c)










###########################################################################################################################


# Experimental section
#annotate
group1_final_plot + annotate("text", x = 4, y = 25, label = "Some text")

