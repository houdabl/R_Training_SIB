#Set working directory
setwd()
##Part I

#get data
mice_data <- read.csv("C:/Users/ASUS/Desktop/SIB_R_training/mice_data.csv", 
                      colClasses=c("factor", "factor", "numeric")) # define classes for columns

#Considering WT mice weight and KO mice weight separately
WT_weights <- mice_data$weight[mice_data$genotype == "WT"]
KO_weights <- mice_data$weight[mice_data$genotype == "KO"]

#check the assumption of normality graphically
par(mfrow=c(2,2))
#WT_Weights
hist(WT_weights,
     freq = FALSE, xlab = "TW",
     main = "Normality of WT_Weights",
     col = "blue")
qqnorm(WT_weights, col = "blue")
qqline(WT_weights, col ="red")
#KO_Weights
hist(KO_weights,
     freq = FALSE, xlab = "TW",
     main = "Normality of KO_Weights",
     col = "orange")
qqnorm(KO_weights, col = "orange")
qqline(KO_weights, col ="red")
par(mfrow=c(1,1)) # removes setting in par

#Make an appropriate plot to visualize the mouse weights grouped by genotype
boxplot(mice_data$weight~mice_data$genotype,
        main="Mouse Weights by Genotype",
        col = c("pink", "violet"),
        xlab = "Genotype", ylab = "Weight")

t.test(KO_weights, WT_weights)
#Perform a test to see whether the mouse weight is different between the two genotypes
#NB : here the data is not paired : WT and KO mice are different individuals
# we do not reject HO : the is no significant difference between WT and KO mice mean weights for any reasonable significance level
# (p=0.8514)

#Repeat steps 1 to 3 for the diet variable
HFD_weights = mice_data$weight[mice_data$diet=="HFD"]
CHOW_weights = mice_data$weight[mice_data$diet=="CHOW"]

par(mfrow=c(2,2))

hist(HFD_weights, freq=FALSE, col="green",
     xlab="HFD", main="weights of HFD mice")
qqnorm(HFD_weights, col ="green")
qqline(HFD_weights, col ="red")

hist(CHOW_weights, freq=FALSE, col="blue",
     xlab="CHOW", main="weights of CHOW mice")
qqnorm(CHOW_weights, col = "blue")
qqline(CHOW_weights, col = "red")

par(mfrow=c(1,1))

boxplot(weight ~ diet, data=mice_data, col=c('orange','pink'), main="Mouse Weight by Diet")
t.test(HFD_weights, CHOW_weights )
#NB : here the data is not paired : HFD and CHOW mice are different individuals
# we can reject HO : there is a significant difference between HFD and CHOW mice mean weights at significance level p=0.05
# (CHOW weight are on average lower)

#Part II : Pima dataset

#Load the package MASS using library()
library(MASS)
# Load the dataset Pima.tr using data(Pima.tr)
data(Pima.tr)
#Use ? to get an idea which variables it contains
?Pima.tr

#Hypothesis: Blood glucose level (glu) is associated with diastolic blood pressure (bp). Run a linear model to test the hypothesis.
model<- lm(formula = glu~bp, data = Pima.tr)
summary(model)
### the linear association between glucose and blood pressure is significant at the 0.05 alpha level
# (p = 0.000115)

#Visualize the fit with a scatter plot and a trend line
plot(Pima.tr$glu, Pima.tr$bp, pch =20,
     main = "Relationship between Glucose and BP",
     xlab = "Blood Glucose Level", ylab = "Diastolic Blood Pressure",
     col = "darkgrey")
abline(model, col ="red", lwd =2)

#Check assumptions of the model (homoscedasticity, mean of residual at 0, normality of the residuals) graphically
par(mfrow=c(1,2))
plot(model, which=1)
plot(model, which=2)
