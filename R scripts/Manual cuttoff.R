df <- fread('spatially_multispeq_blues_filtered_ordered_extremed_with_deviceid.csv')



hist(df$Relative_Chlorophyll, breaks=40)
hist(df$ECS_tau, breaks = 40)
hist(df$gH., breaks = 40)
hist(df$vH., breaks = 40)
hist(df$Phi2, breaks = 40)
hist(df$PhiNPQ, breaks = 40)
hist(df$PhiNO, breaks = 40)
hist(df$qL, breaks = 40)
hist(df$NPQt, breaks = 40)
hist(df$FvP_over_FmP, breaks = 40)
hist(df$PS1_Active.Centers, breaks = 40)
hist(df$PS1_Open.Centers, breaks = 40)
hist(df$PS1_Over.Reduced.Centers, breaks = 40)
hist(df$PS1_Oxidized.Centers, breaks = 40)



# Read the input data
multispeq_manual_cutoff <- read.csv("spatially_multispeq_blues_filtered_ordered_with_deviceid.csv")

# Define cutoff values for filtering
cutoffs <- list(
  Relative_Chlorophyll = list(lower = 56, upper = 76),
  ECS_tau = list(upper = 0.008),
  gH. = list(upper = 300),
  vH. = list(lower = 0.09),
  Phi2 = list(lower = 0.37, upper = 0.51),
  PhiNPQ = list(upper = 0.44),
  PhiNO = list(lower = 0.13 ,upper = 0.215),
  qL = list(lower = 0.425, upper = 0.62),
  NPQt = list(lower = 1.5, upper = 3.45),
  FvP_over_FmP = list(lower = 0.52),
  PS1_Active.Centers = list(lower = 1.7, upper = 5.3),
  PS1_Open.Centers = list(lower = 0.17, upper = 1.1),
  PS1_Over.Reduced.Centers = list(lower = -0.25),
  PS1_Oxidized.Centers = list(lower = -0.15, upper = 0.7)
)

# Initialize the filtered data frame with original values
filtered_multispeq_cutoff <- multispeq_manual_cutoff

# Loop through each trait in the cutoffs list
for (trait in names(cutoffs)) {
  if (trait %in% colnames(multispeq_manual_cutoff)) {  # Check if the column exists
    # Extract bounds for the current trait
    bounds <- cutoffs[[trait]]
    
    # Initialize a logical vector for filtering
    condition <- rep(TRUE, nrow(multispeq_manual_cutoff))
    
    # Apply lower bound if it exists
    if (!is.null(bounds$lower)) {
      condition <- condition & multispeq_manual_cutoff[[trait]] >= bounds$lower
    }
    
    # Apply upper bound if it exists
    if (!is.null(bounds$upper)) {
      condition <- condition & multispeq_manual_cutoff[[trait]] <= bounds$upper
    }
    
    # Filter values: retain only those that meet the condition, set others to NA
    filtered_multispeq_cutoff[[trait]] <- ifelse(condition, multispeq_manual_cutoff[[trait]], NA)
  } else {
    # Inform the user if a trait is missing from the dataset
    warning(paste("Trait", trait, "not found in the dataset."))
  }
}

# View the filtered data (Optional, for debugging)
print(head(filtered_multispeq_cutoff))

# Save the filtered data to a CSV file
output_file <- "spatially_multispeq_blues_filtered_ordered_extremed_with_deviceid.csv"
write.csv(filtered_multispeq_cutoff, output_file, row.names = FALSE)

