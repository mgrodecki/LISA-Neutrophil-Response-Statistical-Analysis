#install.packages("emmeans")
library(emmeans)

options(max.print=1000000)

# Read Filepath
data=read.table("C:/Users/ericg/Documents/Boulder PhD/Hind Lab/R/Chris_Extrav.txt",header=T)

experiment=as.factor(data$experiment)
condition=as.factor(data$condition)
count=data$count


fit=lm(count~experiment+condition)

par(mfrow=c(2,2))
plot(fit)

summary(fit)

emmeans=emmeans(fit,"condition")  #Calculate the adjusted means for the different conditions
emmeans

Comparisons=contrast(emmeans, "revpairwise") #Conducts pairwise comparisons between conditions
Comparisons

