# Install and load necessary packages if not yet done
install.packages(c("lme4", "lmerTest"))
library(lme4)
library(lmerTest)

# Load the data
boxplot(output)

boxplot(leaf_thickness$Thickness)
hist(leaf_thickness$Thickness)


 
summary(leaf_thickness)
View(model)
str(leaf_thickness)

chloro1.blues <- fixef(model)
View(chloro1.blues)

chloro1.int<-fixef(model)[1]
chloro1.int

chloro1.int<-fixef(model)
chloro1.blues[-1]<-chloro1.blues[-1]+chloro1.int

View(chloro1.blues)
hist(chlof$blues)
boxplot(chlof$blues)
str(chlof)
summary(chlof)
boxplot(chlof$blues)
hist(chlof$blues)
library(ggplot2)
ggplot(data = chlof, aes(x = genotype, y = blues))+geom_point()+geom_smooth()
scatter.smooth(chlof$blues)
chlof_final<-chlof[seq(from=1, to=716, by=10),]
ggplot(data = chlof_final, aes(x = genotype, y = blues))+geom_point()+geom_smooth()
scatter.smooth(chlof_final$blues)
library(lme4)
