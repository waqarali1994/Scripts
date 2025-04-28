library(readxl)
library(openxlsx)
install.packages("openxlsx")
write.xlsx("mergedDf_Blues")
write.xlsx(mergedDf_Blues, "mergedDf_Blues.xlsx")

hist(mergedDf_Blues$`Ear Length CM`)
hist(mergedDf_Blues$`Ear Width CM`)
hist(mergedDf_Blues$`Kernels per row`)
hist(mergedDf_Blues$`Number of rows`)
hist(mergedDf_Blues$`Ear weight (grams)`)
hist(mergedDf_Blues$`Plot weight (grams)`)
hist(mergedDf_Blues$`Percent moisture`)
hist(mergedDf_Blues$`100 K weight`)
boxplot(mergedDf_Blues$`Kernels per row`)
boxplot(mergedDf_Blues$`Number of rows`)
boxplot(mergedDf_Blues$`Ear Width CM`)
boxplot(mergedDf_Blues$`Ear weight (grams)`)
boxplot(mergedDf_Blues$`Plot weight (grams)`)
boxplot(mergedDf_Blues$`Percent moisture`)
boxplot(mergedDf_Blues$`Fill in CM (Measurement )`)
boxplot(mergedDf_Blues$Fill_cm)



# Add alphabets to Row based on Year
mergedDf_Blues$Row <- ifelse(mergedDf_Blues$Year == 2020, paste0(mergedDf_Blues$Row, "A"), 
                 ifelse(mergedDf_Blues$Year == 2021, paste0(mergedDf_Blues$Row, "B"), mergedDf_Blues$Row))


mergedDf_Blues$Column <- ifelse(mergedDf_Blues$Year == 2020, paste0(mergedDf_Blues$Column, "C"), 
                             ifelse(mergedDf_Blues$Year == 2021, paste0(mergedDf_Blues$Column, "D"), mergedDf_Blues$Column))
# Add alphabets to Column based on Year
df$Column <- ifelse(df$Year == 2020, paste0(df$Column, "C"), 
                    ifelse(df$Year == 2021, paste0(df$Column, "D"), df$Column))










library(lme4)


yield.blues<-lmer(Ear_length~Genotypes+(1|Column)+(1|Row)+(1|Year:Genotypes),data=mergedDf_Blues)
