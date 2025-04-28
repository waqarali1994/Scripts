hips_2022 <- HIPS_2022_V4_8_INBREDS
hips_2022v1 <- subset.data.frame(HIPS_2022_V4_8_INBREDS)
library(dplyr)
hips_2022v1 <- subset.data.frame(hips_2022, select = c(location, sublocation, block, row, range, genotype, kernelRowNumber))
# Assuming hips_2022v1 is your dataframe
hips_2022v1 <- hips_2022v1[c("genotype", "block", "row", "range", "location", "sublocation", "kernelRowNumber")]

boxplot(hips_2022v1$kernelRowNumber)
hist(hips_2022v1$kernelRowNumber)
library(lme4)



kernelrow_blues <- lmer(kernelRowNumber ~ genotype + (1|row) + (1|block) + (1|range) + (1|location), data = hips_2022v1)
kernelrow_bluesv1 <- lmer(kernelRowNumber ~ genotype + (1|row) + (1|block) + (1|range) + (1|location) + (1|sublocation), data = hips_2022v1)

kernelrow_blues_view <- fixef(kernelrow_bluesv1)
model_intercept_kernelrow <- fixef(kernelrow_bluesv1)[1]
adjusted_blues_kernelrow <- kernelrow_blues_view + model_intercept_kernelrow
write.csv(adjusted_blues_kernelrow, "adjusted_blues_kernelrow.csv")
                                                                                                    