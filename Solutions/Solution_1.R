#From the SIB training : Explore the mice_data.csv dataset

#Open a new script in R studio, comment it and save it

#Have look at the csv file in R studio's file explorer. What do you need to check in order to be able to read in the file correctly?
 getwd() # check where you are
 setwd() #set the directory to where you want to be

#Import Data
 mice_data <- read.csv("C:/Users/ASUS/Downloads/mice_data.csv")
 view(mice_data)

#How many observations and variables does the dataset have?
 nrow(mice_data) #Observations
 ncol(mice_data) #Variables

#What is the structure of the dataset? What are the names and classes of the variables?
names(mice_data) #names of the variables
class(mice_data) #Structure of the dataset
str(mice_data)   #Show all of them, recommended to be used 

#Which variables appear to be categorical? Convert them to factors
class(mice_data[,1]) #check the class of the first variable
class(mice_data[,2]) #check the class of the second variable
class(mice_data[,3]) #check the class of the third variable

#Convert them to factors
mice_data[,1]<- factor(mice_data[,1]) 
mice_data[,2]<- factor(mice_data[,2])
##OR
mice_data$diet <- factor(mice_data$diet)
mice_data$genotype <- factor(mice_data$genotype)

##Check the class change : 
class(mice_data[,1])
class(mice_data[,2]) 

#Get the summary statistics of "mice_data" 
summary(mice_data) 

#Use the function table() to compute the number of observations in different mouse groups
##How many mice are included of each genotype (WT, KO)?
table(mice_data$genotype)
##How many mice are included per diet (HFD, CHOW)?
table(mice_data$diet)
##Make a 2x2 table by genotype and diet crossed
table(mice_data$genotype, mice_data$diet )

##Subset()

#Isolate the observations for the mice on high fat diet (HFD) using subset()
mice_HFD <- subset(mice_data, diet=="HFD")

#Compute the average weights of the subset
summary(mice_HFD$weight)

#Do the same for the mice on regular chow diet (CHOW)
mice_CHOW <- subset(mice_data, diet=="CHOW") 
summary(mice_CHOW$weight)

#Export the data of each subgroup to a csv file
write.csv(mice_data_HFD,"mice_HFD.csv",row.names=FALSE,quote=FALSE)
write.csv(mice_data_HFD,"mice_CHOW.csv",row.names=FALSE,quote=FALSE)

##Compute the means and standard deviations for WT and KO mouse weights using tapply(). Then do the same for CHOW and HFD groups.
tapply(mice_data$weight, mice_data$genotype, mean)
tapply(mice_data$weight, mice_data$genotype, sd)
tapply(mice_data$weight, mice_data$diet, mean)
tapply(mice_data$weight, mice_data$diet, sd)
