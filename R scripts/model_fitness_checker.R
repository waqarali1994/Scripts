library(lmerTest)  # for ANOVA with mixed-effects models
library(lme4)  # for fitting mixed-effects models

# Assuming your data is already read into 'data'
data <- read.csv("mergedmultispeqblups.csv")

# List of phenotypes to analyze
phenotypes <- c('Relative_Chlorophyll', 'ECS_.mAU', 'ECS_tau', 'gH.', 'vH.',
                'Phi2', 'PhiNPQ', 'PhiNO', 'qL', 'NPQt', 'FvP_over_FmP',
                'PS1_Active.Centers', 'PS1_Open.Centers', 
                'PS1_Over.Reduced.Centers', 'PS1_Oxidized.Centers')

# Loop through each phenotype
for (trait in phenotypes) {
  # Define the base model (simpler model)
  base_model_formula <- as.formula(paste(trait, "~ 1 + (1|Genotype)"))
  base_model <- lmer(base_model_formula, data = data)
  
  # Define the full model (more complex model)
  full_model_formula <- as.formula(paste(trait, "~ (1|Genotype) + (1|Ambient_Temperature)"))
  full_model <- lmer(full_model_formula, data = data)
  
  # Compare the base model and the full model using ANOVA
  anova_result <- anova(base_model, full_model)
  print(paste("ANOVA results for", trait))
  print(anova_result)
}



# Exclude rows with any missing values in the relevant columns
data_clean <- na.omit(data[, c("Genotype", "Ambient_Temperature")])

names(data_clean)
# Loop through each phenotype
for (trait in phenotypes) {
  # Ensure no NA values affect the fitting process
  phenotype_data <- na.omit(data_clean[, c(trait, "Genotype", "Ambient_Temperature")])
  
  # Define the base model (simpler model)
  base_model_formula <- as.formula(paste(trait, "~ (1|Genotype)"))
  base_model <- lmer(base_model_formula, data = phenotype_data)
  
  # Define the full model (more complex model)
  full_model_formula <- as.formula(paste(trait, "~ (1|Genotype) + (1|Ambient_Temperature)"))
  full_model <- lmer(full_model_formula, data = phenotype_data)
  
  # Compare the base model and the full model using ANOVA
  anova_result <- anova(base_model, full_model)
  print(paste("ANOVA results for", trait))
  print(anova_result)
}










