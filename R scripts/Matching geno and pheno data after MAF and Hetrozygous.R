library(data.table)
library(dplyr)

#setwd("~/Documents/Books")
# Load the .fam file
fam_data <- fread("speq_ids.csv", header = FALSE)

# Extract genotype IDs (assuming they are in the first column)
genotype_ids <- fam_data$V1
# Load the CSV file with phenotype data
phenotype_data <- fread("Feb1_excluding_ambient_extreme_removed.csv")

colnames(phenotype_data)


# Filter the phenotype data to keep only the rows with genotype IDs present in the .fam file
filtered_phenotype_data <- phenotype_data %>%
  filter(Genotype %in% Genotypes)

# Save the filtered phenotype data to a new CSV file
write.csv(filtered_phenotype_data, "jan21_blues_filtered.csv", row.names = FALSE)





library(data.table)
library(dplyr)

# Load the .fam file
fam_data <- fread("speq_ids.csv", header = FALSE)

# Extract genotype IDs (assuming they are in the first column)
genotype_ids <- fam_data$V1

# Load the CSV file with phenotype data
phenotype_data <- fread("Feb1_excluding_ambient_extreme_removed.csv")

# Ensure genotype_ids is treated as character if needed
genotype_ids <- as.character(genotype_ids)
phenotype_data$Genotype <- as.character(phenotype_data$Genotype)

# Filter the phenotype data to keep only the rows with genotype IDs present in the .fam file
filtered_phenotype_data <- phenotype_data %>%
  filter(Genotype %in% genotype_ids)

# Display the first few rows of the filtered data
head(filtered_phenotype_data)


write.csv(filtered_phenotype_data, 'feb21_filtered_extreme_removed.csv')
















































library(data.table)
library(dplyr)

setwd('~/Documents/PhD work/Yield project/2023_yield_data/')

# Load the .fam file and assign column names
fam_data <- fread("speq_ids.csv", header = FALSE, col.names = c("Genotype"))

# Load the phenotype data
phenotype_data <- fread("Feb1_excluding_ambient_extreme_removed.csv")

# Ensure columns are character for comparison
fam_data$Genotype <- as.character(fam_data$Genotype)
phenotype_data$genotype <- as.character(phenotype_data$genotype)

# Filter the phenotype data based on Genotype
filtered_phenotype_data <- phenotype_data %>%
  filter(genotype %in% fam_data$Genotype)

# Save the filtered phenotype data
write.csv(filtered_phenotype_data, "Feb1_blues_filtered.csv", row.names = FALSE)
