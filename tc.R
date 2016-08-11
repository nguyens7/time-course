#########This is a guideline for importing and formating the data as well as the code to generate all the graphs/figures

#To Clear working environment
rm(list=ls())

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
tail(tc)

##### MAY HAVE TO REPOSITION THIS FUNCTION TO ANOTHER SPOT
#We want to be able to average the three representative placentas from each individual mouse
# ie- we want to average placentas 1,2 and 3 and make it a new column

tc <- transform(tc, plac.avg = rowMeans(tc[,6:8], #create a new row named "plac.avg" that averages columns 6-8 (placenta.1-placenta.3)
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
tail(tclc)
#Check how R interprets the columns in the data
#Sometimes you have your data in a particular order but R will organize them in alaphabetical order
str(tclc)

#************************************FOR LINE GRAPHS***************
l_data <- ddply(tclc, c("day","parameter"), summarise,
                N    = length(value),
                mean = mean(value),
                sd   = sd(value),
                se   = sd / sqrt(N))




# ***********************************FOR BAR GRAPHS***************
# Make day column as factor
# This effectlively tells R to treat your data as it and DON'T alphabetize it
# You tell R which column you want it to read as a factor (leave it in the order that you entered the data)
b_tclc <- tclc
b_tclc$day <- factor(b_tclc$day, levels=c("0","5","10","14","17","19"))  #Adjust the day column and make it in the correct order

## Test to see if parameter needs to be factored uniquely*****
b_tclc$parameter <- factor(tclc$parameter, levels=unique(tclc$parameter))

#Verify the factor is in the correct order
str(b_tclc)


# Make the bar graph data easy to recall by renaming it "b"
b <- b_tclc
#Summarize the data
b_data <- ddply(b, c("day","parameter"), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd / sqrt(N))


#check if data is truly summarized
b_data

#Plot all the data
tc_plot <- ggplot(b_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mouse parameter (in g or #)") +  # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title


# Check if plot worked
tc_plot


#Make a default theme for optimizeing graph aesthetics
publication_style <- theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
      axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
      panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
      panel.grid.minor=element_line(colour=NA),            #Remove grid lines
      axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
      axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
      plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30

#fix axes
tc_plot_final <- tc_plot + publication_style

#Check the final plot
tc_plot_final



#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
tc_plot_final
dev.off()

########################################################################

#Filter weight
weight_data <- b_data[b_data$parameter %in% c("weight"),]

summary(b_data)
summary(weight_data)

#plot weight data
weight_plot <- ggplot(weight_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Weight (g)") +  # Y axis label
  scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
weight_plot

#fix axes (using publication style)
tc_weight_final <- weight_plot + publication_style

#Check the final plot
tc_weight_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_Weight_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
tc_weight_final
dev.off()

######################################
#Test for line graph generation ######

l_weight_data <- l_data[l_data$parameter %in% c("weight"),]

weight_plot_line <- ggplot(l_weight_data, aes(x=day, y=mean,  group=1, colour=parameter)) +
  geom_line(size=2, colour="black") + geom_point(size=3, colour="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.8,  # Width of the error bars
                size=1.5,   # Size of the error bars
                colour="black", #color of the error bars
                position=position_dodge(.9)) +
  theme(legend.position="none") + #remove legend
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Weight (g)") +  # Y axis label  
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title

  
#test is the line graph worked
weight_plot_line

#Format axes correctly using (publication_style)
weight_plot_line + publication_style

#Make finalize line graph 
weight_line_final <- weight_plot_line + publication_style + scale_y_continuous(limits=c(15, 35)) #Adjust y axis

#Check to see if it worked
weight_line_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/line_tc_Weight_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
weight_line_final
dev.off()



###########################################################################


#Filter pups
pup_data <- b_data[b_data$parameter %in% c("pups"),]

summary(b_data)
summary(pup_data)

#plot weight data
pup_plot <- ggplot(pup_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Days of Gestation") +  # X axis label
  ylab("Number of Pups") +  # Y axis label
  scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
pup_plot

#fix axes
tc_pup_final <- pup_plot + publication_style

#Check the final plot
tc_pup_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_pup_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
tc_pup_final
dev.off()


########################################################################################

#Calculation of Pup Implantation Within Uterine Horns
horn_data <- b_data[b_data$parameter %in% c("left.horn","right.horn"),]

#Check if horn data works
summary(horn_data)
head(horn_data)

#plot weight data
horn_plot <- ggplot(horn_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Implantation Sites") +  # X axis label
  ylab("Number of Pups") +  # Y axis label
  #scale_fill_hue(name="Uterine Horn", breaks=c("Left Horn", "Right Horn"), labels=c("Left Horn", "Right Horn")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
horn_plot

#fix axes
tc_horn_final <- horn_plot + publication_style

#Check the final plot
tc_horn_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_horn_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
tc_horn_final
dev.off()


##########################################################################

#Calculation of mean placenta weight throughout gestation

#Calculation of Pup Implantation Within Uterine Horns
plac_data <- b_data[b_data$parameter %in% c("placenta.1","placenta.2","placenta.3"),]

#Check if placenta data works
summary(b_data)
head(plac_data)

#plot placenta weight data
plac_plot <- ggplot(plac_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Placenta Weight (g)") +  # Y axis label
  #scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title


#Check if weight plot worked
plac_plot

#fix axes
plac_final <- plac_plot + publication_style

#Check the final plot
plac_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_plac_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
plac_final
dev.off()

##########################################################################################

#Calculation of finalized mean placenta weight throughout gestation

#Calculation of Pup Implantation Within Uterine Horns
mean_plac_data <- b_data[b_data$parameter %in% c("plac.avg"),]

#Check if placenta data works
summary(mean_plac_data)
head(mean_plac_data)

#plot placenta weight data
mean_plac_plot <- ggplot(mean_plac_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Placenta Weight (g)") +  # Y axis label
  scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
mean_plac_plot

#fix axes
mean_plac_final <- mean_plac_plot + publication_style

#Check the final plot
mean_plac_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_mean_plac_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
mean_plac_final
dev.off()



###########################################################################################################################


# Experimental section
#annotate
##annotate("text", x = 4, y = 25, label = "Some text")

