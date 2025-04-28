library(lme4)
library(reshape2)
library(ggplot2)


### The yield data can be found in the file "PhenomeForce_Yield.csv". This can be read 
### into R using read.csv().

head(BLUES_file)

hist(BLUES_file$Relative_Chlorophyll)

### The yield data contains 39 trials. Each trial contains different genotypes except for 
### two repeated checks: gids 4755014 and 6176013

boxplot(BLUES_file$Relative_Chlorophyll)


### The variables "gid", "trial", "rep", and "block"  are currently characters/integers. 
### These need to be categorical variables to be able to analyze the field design. The 
### factor() function changes these variables to categorical variables, or "factors".

str(BLUES_file)


### BLUEs for grain yield can be calculated using the lmer() function of the "lme4"
### package. "gid" is set as a fixed effect, while "trial", "rep", and "block" are 
### set as random effects. "rep" is nested within "trial", and "block" is nested within
### "rep" and "trial".

chlorophyll.blues<-lmer(Relative_Chlorophyll~Genotype+(1|Row)+(1|Column:Row),data=BLUES_file)
names(BLUES_file)
### The fixef() function returns the fixed effects of the model. Notice that the first 
### fixed effect is the intercept. This is also the fixed effect for the first "gid" level,
### which is 4755014. 


fixef(chlorophyll.blues)

fixef(yield.blue.mod)[1:10]

yield.int<-fixef(yield.blue.mod)[1]
yield.int

### The intercept can be added to the BLUE values to center the BLUEs around the mean

chlorophyll.blues<-fixef()
yield.blues[-1]<-yield.blues[-1]+yield.int

### The gsub() function can be used to take off the "gid" character string in the names
### of the BLUEs. 


names(chlorophyll.blues[1:752]
gsub("Genotype", "",names(chlorophyll.blues))
names(yield.blues)[1:10]
names(yield.blues)[1]<-levels(yield$gid)[1]
gsub("gid", "", names(yield.blues))[1:10]

names(yield.blues)<-gsub("gid", "", names(yield.blues))
yield.blues[1:10]

### Using dataframe(), the data can be stored in a data frame with two columns
### for "gid" and "grain.yield.blue".

yield.blues<-data.frame(names(yield.blues), yield.blues)
colnames(yield.blues)<-c("gid", "grain.yield.blue")
head(yield.blues)

hist(yield.blues$grain.yield.blue)

match(chloro_blues_match)
hist(chlorophyll_blues$chlorophyll.blues)
boxplot(chlorophyll_blues$chlorophyll.blues)
str(chlorophyll_blues)
summary(chlorophyll_blues)
