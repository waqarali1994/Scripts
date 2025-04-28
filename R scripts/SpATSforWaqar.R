library(SpATS)
library(tidyverse)

# Replace the path with the actual path to your CSV file
multispeq <- read.csv(path.expand("filteredcuttoffwithoutblups.csv"))

phenotypes <- c('ECS_.mAU',	'ECS_tau',	'gH.',	'vH.',	'Phi2	PhiNPQ',	'PhiNO',	'qL',	'NPQt',	'FvP_over_FmP',	'PS1_Active.Centers',	'PS1_Open.Centers',	
                'PS1_Over.Reduced.Centers', 'PS1_Oxidized.Centers',	'Light_Intensity',	
                'Ambient_Temperature',	'Ambient_Humidity',	'Leaf_Temperature',	'Leaf_Temperature_Differential', 'Relative_Chlorophyll', 'LEF') # Make this a vector of your phenotype column names as strings

columnKnots <- 31 # Change this to be as.integer(numberOfColumns / 2) + 1 and so forth for rows
rowKnots <- 15
numGenotypes <- length(unique(multispeq$Genotype)) # Change this to access your genotype column

multispeq$Day <- as.factor(multispeq$Day)
#multispeq$Ambient_Temperature <- as.factor(multispeq$Ambient_Temperature)
#multispeq$Light_Intensity <- as.factor(multispeq$Light_Intensity)
class(multispeq$Day)
names(multispeq)
# Do the first phenotype, so we have a dataframe to add columns to
# Change the genotype argument to match the name of your genotype column as a string
# And change genotype.as.random to TRUE if you want BLUPs (if you have wildly different numbers of plots for each genotype, i.e. if there are checks)
# And change range, row to be corresponding columns in your dataframe 
model <- SpATS(response = phenotypes[1], genotype = 'Genotype', genotype.as.random = TRUE, spatial = ~SAP(Column, Row, nseg = c(columnKnots, rowKnots)),
               random = ~ Light_Intensity + Ambient_Temperature + Day, data = multispeq)

spatiallyCorrectedBLUEs <- as.data.frame(model$coeff[1:numGenotypes])
colnames(spatiallyCorrectedBLUEs) <- phenotypes[1]
spatiallyCorrectedBLUEs <- as_tibble(spatiallyCorrectedBLUEs, rownames = 'Genotype')
# Plot the fitted spatial trend and title the plot with the phenotype we're doing now so we can look at this later
plot.SpATS(model, main = phenotypes[1])
# Now let's do the rest of the phenotypes and add them to the dataframe spatiallyCorrectedBLUEs so we have our phenotype dataframe ready for GWAS
# We start at 2 because we already did the first phenotype
for(i in 2:length(phenotypes))
{
  model <- SpATS(response = phenotypes[i], genotype = 'Genotype', genotype.as.random = FALSE, spatial = ~SAP(Column, Row, nseg = c(columnKnots, rowKnots)),
                 random = ~ Light_Intensity + Ambient_Temperature + Day,data = multispeq)
  # Temporary dataframe that only exists in the loop
  df <- as.data.frame(model$coeff[1:numGenotypes])
  colnames(df) <- phenotypes[i]
  df <- as_tibble(df, rownames = 'Genotype')
  # Add it to our bigger dataframe spatiallyCorrectedBLUEs, lining it up by the column 'genotype'
  spatiallyCorrectedBLUEs <- full_join(spatiallyCorrectedBLUEs, df, join_by(genotype), suffix = c('', ''), keep = FALSE)
  # Plot the fitted spatial trend and title the plot with the phenotype we're doing now so we can look at this later
  plot.SpATS(model, main = phenotypes[i])
}