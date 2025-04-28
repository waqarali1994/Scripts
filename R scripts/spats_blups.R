# Load necessary libraries
library(data.table)
library(dplyr)

# Read the data
data <- fread("extremevalueremoved.csv")

# Define the phenotypes and other columns of interest
phenotypes <- c('Relative_Chlorophyll', 'ECS_.mAU', 'ECS_tau', 'gH.', 'vH.', 'Phi2', 
                'PhiNPQ', 'PhiNO', 'qL', 'NPQt', 'FvP_over_FmP', 'PS1_Active.Centers', 
                'PS1_Open.Centers', 'PS1_Over.Reduced.Centers', 'PS1_Oxidized.Centers',
                'Light_Intensity', 'Ambient_Temperature', 'Ambient_Humidity', 
                'Leaf_Temperature', 'Leaf_Temperature_Differential', 'LEF')

# Compute the average of each phenotypic trait for each PlotID
averaged_data <- data %>%
  group_by(PlotID, Genotypes, Row, Column) %>%
  summarise(across(all_of(phenotypes), mean, na.rm = TRUE), Date=mean.Date(as.Date(Day, format=c("%m/%d/%Y"))))
# Days = paste(unique(Day), collapse = "; "
# Save the averaged data to a CSV file
write.csv(averaged_data, "averaged_phenotypic_data.csv", row.names = FALSE)

# Print the averaged data
print(averaged_data)





# Load necessary libraries
library(SpATS)
library(data.table)
library(dplyr)

# Set working directory (change this to your actual working directory)
setwd("~/Documents/PhD work/Multispeq project/multispeq")

# Read the averaged data
data <- fread("averaged_phenotypic_data.csv")

# Define the phenotypes
phenotypes <- c('Relative_Chlorophyll', 'ECS_.mAU', 'ECS_tau', 'gH.', 'vH.', 'Phi2', 
                'PhiNPQ', 'PhiNO', 'qL', 'NPQt', 'FvP_over_FmP', 'PS1_Active.Centers', 
                'PS1_Open.Centers', 'PS1_Over.Reduced.Centers', 'PS1_Oxidized.Centers')

# Convert necessary columns to factors
data$Date <- as.factor(data$Date)

# Initialize the data frame to store results
spatiallyCorrectedBLUPs <- data

# Number of knots
column_knots <- 15
row_knots <- 31

# Loop over all phenotype columns
for (phenotype in phenotypes) {
  
  # Fit the SpATS model
  model <- SpATS(
    response = phenotype,
    genotype = 'PlotID',
    genotype.as.random = TRUE,
    spatial = ~ SAP(Column, Row, nseg = c(column_knots, row_knots)),
    random = ~ Date,
    data = data
    )
  
  # Extract BLUPs for each PlotID
  blup_data <- as.data.frame(model$coeff[1:length(data$PlotID)])
  colnames(blup_data) <- phenotype
  blup_data$PlotID <- rownames(blup_data)
  
  # Ensure PlotID is of the same type in both data frames
  blup_data$PlotID <- as.character(blup_data$PlotID)
  spatiallyCorrectedBLUPs$PlotID <- as.character(spatiallyCorrectedBLUPs$PlotID)
  
  # Merge BLUPs with the original data to keep them per observation
  spatiallyCorrectedBLUPs <- left_join(spatiallyCorrectedBLUPs, blup_data, by = "PlotID")
  
  # Save the fitted spatial trend plot
  png_filename <- paste0("SpATS_fitted_trend_final_", phenotype, ".png")
  png(png_filename)
  plot(model, main = phenotype)
  dev.off()
}

# Save the BLUPs values to a CSV file
write.csv(spatiallyCorrectedBLUPs, "blups_after_spatail_correction.csv", row.names = FALSE)












  





mergedblups <- fread("averaged_phenotypic_data.csv")

# Sort the data by Light_Intensity
mergedblups <- mergedblups %>%
  arrange(Light_Intensity)


# Create equal intervals for Light_Intensity
mergedblups$Light_Intensity_Level <- cut(mergedblups$Light_Intensity, 
                                         breaks = 4, 
                                         labels = c("C", "D", "E", "F"))

# Create unique identifiers for Light_Intensity based on levels
mergedblups <- mergedblups %>%
  mutate(Light_Intensity_Unique = paste0("Level", Light_Intensity_Level))




write.csv(mergedblups,"mergedblups_v1_before_spatial.csv")
