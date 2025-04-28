library(readxl)
library(openxlsx)
install.packages("openxlsx")
write.xlsx("mergedDf_Blues")
write.xlsx(mergedDf_Blues, "mergedDf_Blues.xlsx")

hist(combined_20_21_v1$Ear_Length)
hist(combined_20_21_v1$Ear_Width)
hist(combined_20_21_v1$Fill_cm)
hist(combined_20_21_v1$Kernels_row)
hist(combined_20_21_v1$Number_rows)
hist(combined_20_21_v1$Ear_weight)
hist(combined_20_21_v1$Plot_weight)
hist(combined_20_21_v1$Percent_moisture)
hist(combined_20_21_v1$Weight)

boxplot(combined_20_21_v1$Percent_moisture)
boxplot(combined_20_21_v1$Weight)
boxplot(combined_20_21_v1$Plot_weight)
boxplot(combined_20_21_v1$Ear_weight)
boxplot(combined_20_21_v1$Number_rows)
boxplot(combined_20_21_v1$Kernels_row)
boxplot(combined_20_21_v1$Fill_cm)
boxplot(combined_20_21_v1$Ear_Width)
boxplot(combined_20_21_v1$Ear_Length)



mergedDf_Blues$Row <- ifelse(mergedDf_Blues$Year == 2020, paste0(mergedDf_Blues$Row, "A"), 
                 ifelse(mergedDf_Blues$Year == 2021, paste0(mergedDf_Blues$Row, "B"), mergedDf_Blues$Row))


mergedDf_Blues$Column <- ifelse(mergedDf_Blues$Year == 2020, paste0(mergedDf_Blues$Column, "C"), 
                             ifelse(mergedDf_Blues$Year == 2021, paste0(mergedDf_Blues$Column, "D"), mergedDf_Blues$Column))

mergedDf_Blues$Replication <- ifelse(mergedDf_Blues$Year == 2020, paste0(mergedDf_Blues$Replication, "E"), 
                             ifelse(mergedDf_Blues$Year == 2021, paste0(mergedDf_Blues$Replication, "F"), mergedDf_Blues$Replication))

library(lme4)

model_blues <- lmer(Ear_Length ~ + (1|Row), data = combined_2021_datav2)

model_blues2 <- lmer(Ear_Length ~ Genotypes + (1|Row), data = combined_2021_datav2)

model_blues4 <- lmer(Ear_Length ~ Genotypes + (1|Row) + (1|Year), data = combined_2021_datav2)

model_blues3 <- lmer(Ear_Length ~ Genotypes + (1|Row) + (1|Column), data = combined_2021_datav2)

model_blues5 <- lmer(Ear_Length ~ Genotypes + (1|Row) + (1|Year) + (1|Column), data = combined_2021_datav2)

model_blues6 <- lmer(Ear_Length ~ Genotypes + (1|Replicate), data = combined_2021_datav2)

anova(model_blues, model_blues2, model_blues4, model_blues5, model_blues6)

VarCorr(model_blues6)

# Changing column name from "oldName" to "newName" in df
colnames(combined_2021_datav2)[colnames(combined_2021_datav2) == "plot _weight"] <- "plot_weight"


# BLUEs Calculation

Ear_length_blues <- lmer(Ear_Length ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Ear_width_blues <- lmer(Ear_Width ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Fill_cm_blues <- lmer(Fill_CM ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

kernels_row_blues <- lmer(Kernels_row ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Number_rows_blues <- lmer(Number_Rows ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Ear_weight_blues <- lmer(ear_weight ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Plot_weight_blues <- lmer(plot_weight ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Percent_moisture_blues <- lmer(Percent_moisture ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

k_blues <- lmer(K_weight ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)



Ear_length_blues_view <- fixef(Ear_length_blues)

Ear_width_blues_view <- fixef(Ear_width_blues)

kernels_row_blues_view <- fixef(kernels_row_blues)

Number_rows_blues_view <- fixef(Number_rows_blues)

Fill_cm_blues_view <- fixef(Fill_cm_blues)

Ear_weight_blues_view <- fixef(Ear_weight_blues)

Plot_weight_blues_view <- fixef(Plot_weight_blues)

Percent_moisture_blues_view <- fixef(Percent_moisture_blues)

k_blues_view <- fixef(k_blues)



model_intercept_Ear_length <- fixef(Ear_length_blues)[1]

model_intercept_Ear_width <- fixef(Ear_width_blues)[1]

model_intercept_Fill_cm <- fixef(Fill_cm_blues)[1]

model_intercept_Kernels_row <- fixef(kernels_row_blues)[1]

model_intercept_Number_rows <- fixef(Number_rows_blues)[1]

model_intercept_Ear_weight <- fixef(Ear_weight_blues)[1]

model_intercept_Plot_weight <- fixef(Plot_weight_blues)[1]

model_intercept_Percent_moisture <- fixef(Percent_moisture_blues)[1]

model_intercept_k <- fixef(k_blues)[1]


adjusted_blues_Ear_length <- Ear_length_blues_view + model_intercept_Ear_length

adjusted_blues_Ear_width <- Ear_width_blues_view + model_intercept_Ear_width

adjusted_blues_Fill_cm <- Fill_cm_blues_view + model_intercept_Fill_cm

adjusted_blues_kernels_row <- kernels_row_blues_view + model_intercept_Kernels_row

adjusted_blues_Number_rows <- Number_rows_blues_view + model_intercept_Number_rows

adjusted_blues_Ear_weight <- Ear_weight_blues_view + model_intercept_Ear_weight

adjusted_blues_Plot_weight <- Plot_weight_blues_view + model_intercept_Plot_weight

adjusted_blues_Percent_moisture <- Percent_moisture_blues_view  + model_intercept_Percent_moisture

adjusted_blues_k <- k_blues_view + model_intercept_k


write.csv(adjusted_blues_Ear_length, "adjusted_blues_Ear_length.csv")

write.csv(adjusted_blues_Ear_width, "adjusted_blues_Ear_width.csv")

write.csv(adjusted_blues_Fill_cm, "adjusted_blues_Fill_cm.csv")

write.csv(adjusted_blues_kernels_row, "adjusted_blues_kernels_row.csv")

write.csv(adjusted_blues_Number_rows, "adjusted_blues_Number_rows.csv")

write.csv(adjusted_blues_Ear_weight, "adjusted_blues_Ear_weight.csv")

write.csv(adjusted_blues_Plot_weight, "adjusted_blues_Plot_weight.csv")

write.csv(adjusted_blues_Percent_moisture, "adjusted_blues_Percent_moisture.csv")

write.csv(adjusted_blues_k, "adjusted_blues_k.csv")






