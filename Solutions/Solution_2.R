#Practice II
#Import the mouse data from the file dataset/ mice_data_mod.csv
##Part I
getwd() 
setwd()
mice_data_mod <- read.csv("C:/Users/ASUS/Desktop/SIB_R_training/mice_data_mod.csv")

#Run str() to check your data frame: did it load correctly?
str(mice_data_mod)

#Convert genotype and diet to factor variables 
mice_data_mod[,1]<- factor(mice_data_mod[,1]) 
mice_data_mod[,2]<- factor(mice_data_mod[,2])
#OR
# define the order of factor levels
mice_data_mod$genotype <- factor(mice_data_mod$genotype, levels=c("WT", "KO"))
mice_data_mod$diet <- factor(mice_data_mod$diet, levels=c("CHOW", "HFD"))

#Make a scatter plot of respiratory rate against mouse weights using the function plot().
plot(mice_data_mod$weight, mice_data_mod$respiratoryRate, 
     pch = 19, 
     main = "Plot of Respiratory Rate against mouse Weights", 
     col= c("pink", "blue")[mice_data_mod$genotype],
     xlab= "Weight [g]",
     ylab= "Respiratory Rate [bpm]")

#Fit a trend line using the function abline()
abline(lm(mice_data_mod$respiratoryRate ~ mice_data_mod$weight))
#To add some esthetics 
abline(lm(mice_data_mod$respiratoryRate ~ mice_data_mod$weight),
       col = "red",
       lwd = 1.5)

#Add a legend for the genotype
legend("bottomright",
       legend=levels(mice_data_mod$genotype),
       col=c("pink","blue"),
       pch=19)

##Part II
#Plot a histogram of mouse weight and customize it with title, labels, colors
hist(mice_data_mod$weight,
     breaks = 10,
     freq = FALSE,
     xlab = "Weight",
     main = "Mouse Weight",
     col = "grey")
#Represent the density line on top 
lines(density(mice_data_mod$weight),
      type= "l", col="red") #l is for lines

#Make boxplots of weights from WT and KO mice. Customize with title, labels, colors
boxplot(mice_data_mod$weight ~ mice_data_mod$genotype, 
        col= c("pink", "blue"),
        main="Weight by Genotype",
        ylab = "Weight", xlab = "Genotype")
points(weight ~ genotype, data= mice_data_mod, col="red")

#Optional: Repeat 2 with diet instead of genotype
boxplot(mice_data_mod$weight ~ mice_data_mod$diet, 
        col= c("green", "violet"),
        main="Weight by Diet",
        ylab = "Weight", xlab = "Diet")
points(weight ~ diet, data= mice_data_mod, col="red")

##Part III
#Make a multi-panel figure with the four graphics (from the previous exercises) on one page

###PNG
#Open a PNG device
png(filename = "mice_data_mod_graphics.png", width = 800, height = 800)
# Set up a 2x2 plotting area
par(mfrow = c(2, 2))

# Plot the four graphics
#plot1 
plot(mice_data_mod$weight, mice_data_mod$respiratoryRate, 
     pch = 19, 
     main = "Plot of Respiratory Rate against mouse Weights", 
     col= c("pink", "blue")[mice_data_mod$genotype],
     xlab= "Weight [g]",
     ylab= "Respiratory Rate [bpm]")

abline(lm(mice_data_mod$respiratoryRate ~ mice_data_mod$weight),
       col = "red",
       lwd = 1.5)

legend("bottomright",
       legend=levels(mice_data_mod$genotype),
       col=c("pink","blue"),
       pch=19)

#part2
hist(mice_data_mod$weight,
     breaks = 10,
     freq = FALSE,
     xlab = "Weight",
     main = "Mouse Weight",
     col = "grey")

lines(density(mice_data_mod$weight),
      type= "l", col="red") 

#plot3
boxplot(mice_data_mod$weight ~ mice_data_mod$genotype, 
        col= c("pink", "blue"),
        main="Weight by Genotype",
        ylab = "Weight", xlab = "Genotype")
points(weight ~ genotype, data= mice_data_mod, col="red")

#plot4
boxplot(mice_data_mod$weight ~ mice_data_mod$diet, 
        col= c("green", "violet"),
        main="Weight by Diet",
        ylab = "Weight", xlab = "Diet")
points(weight ~ diet, data= mice_data_mod, col="red")


# Turn off the PNG device
dev.off() 

#Change the code to export the figure to a pdf file
#Open a PDF device
pdf(file = "mice_data_mod_graphics.pdf", width = 8, height = 8)
# Set up a 2x2 plotting area
par(mfrow = c(2, 2))

# Plot the four graphics
#plot1 
plot(mice_data_mod$weight, mice_data_mod$respiratoryRate, 
     pch = 19, 
     main = "Plot of Respiratory Rate against mouse Weights", 
     col= c("pink", "blue")[mice_data_mod$genotype],
     xlab= "Weight [g]",
     ylab= "Respiratory Rate [bpm]")

abline(lm(mice_data_mod$respiratoryRate ~ mice_data_mod$weight),
       col = "red",
       lwd = 1.5)

legend("bottomright",
       legend=levels(mice_data_mod$genotype),
       col=c("pink","blue"),
       pch=19)

#part2
hist(mice_data_mod$weight,
     breaks = 10,
     freq = FALSE,
     xlab = "Weight",
     main = "Mouse Weight",
     col = "grey")

lines(density(mice_data_mod$weight),
      type= "l", col="red") 

#plot3
boxplot(mice_data_mod$weight ~ mice_data_mod$genotype, 
        col= c("pink", "blue"),
        main="Weight by Genotype",
        ylab = "Weight", xlab = "Genotype")
points(weight ~ genotype, data= mice_data_mod, col="red")

#plot4
boxplot(mice_data_mod$weight ~ mice_data_mod$diet, 
        col= c("green", "violet"),
        main="Weight by Diet",
        ylab = "Weight", xlab = "Diet")
points(weight ~ diet, data= mice_data_mod, col="red")


# Turn off the PDF device
dev.off() 

#Export an histogram to a png file 
png("Mouse_Weight.png", width = 800, height = 600)

hist(mice_data_mod$weight,
     breaks = 10,
     freq = FALSE,
     xlab = "Weight",
     main = "Mouse Weight",
     col = "grey")

lines(density(mice_data_mod$weight),
      type= "l", col="red") 

dev.off()
#Rq : 
## width and height : are in pixels by default in PNG and inch in PDF
###END
