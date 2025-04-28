library(readxl)
library(lme4)
install.packages("lme4")


#NPQt
model <- lmer(NPQt ~ (1|Genotype) + (1|Plots) + (1|Row) + (1|Column) + 
                (1|Block) + (1|Biolreps) + (1|time) + (1|ECStmAU) + (1|`gH+`) + 
                (1|`vH+`) + (1|Lightintensity) + (1|LeafTemperature) + (1|Phi2) + 
                (1|PhiNPQ) + (1|PhiNO), data = fixedmultispeq6)

summary(model)
VarCorr(model)
avarage