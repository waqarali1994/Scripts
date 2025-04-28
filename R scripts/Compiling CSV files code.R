library(dplyr)
library(readr)

setwd("~/Documents/PhD work/Photosyntheiss_project/Photosynthsis_paper_data/Anlysis_data/Excluding_ambient_temperature//")

# List of file names and corresponding traits
files_and_traits <- data.frame(
  file = c('ZPC1signals.csv', 'ZPC2signals.csv', 'ZPC3signals.csv'),
  
  trait = c('PC1', 'PC2', 'PC3')
)

# Display the reordered data frame
print(files_and_traits)

# Function to read a file, add a trait column, and standardize column names
read_and_label <- function(filename, traitname) {
  df <- read_csv(filename)
  df$Trait <- traitname
  # Assuming 'CHROM', 'POS', 'P', and 'support' are the common columns you want to keep
  df <- df %>% select(CHROM, POS, P, support, Trait)
  return(df)
}

# Read and combine all datasets
multispeqjames <- bind_rows(lapply(1:nrow(files_and_traits), function(i) {
  read_and_label(files_and_traits$file[i], files_and_traits$trait[i])
}))

# Now 'multispeqjames' contains all your observations with an additional 'Trait' column
write_csv(multispeqjames, "z_PCs.csv")








library(dplyr)
library(readr)

setwd("~/Documents/PhD work/New_Kyle_GWAS/")

# List of file names and corresponding traits (only "_R" files)
files_and_traits <- data.frame(
  file = c('ZBAR_Rsignals.csv', 'ZBIL_Rsignals.csv', 'ZDTA_Rsignals.csv', 'ZIR_Rsignals.csv', 'ZPBN_Rsignals.csv', 'ZPH_Rsignals.csv', 'ZRDL_Rsignals.csv',
           'ZRDU_Rsignals.csv', 'ZRL_Rsignals.csv', 'ZSDL_Rsignals.csv', 'ZSDU_Rsignals.csv',
           'ZSV_Rsignals.csv', 'ZTN_Rsignals.csv'),
  
  trait = c('BAR_R', 'BIL_R', 'DTA_R', 'IR_R', 'PBN_R', 'PH_R', 'RDL_R',
            'RDU_R', 'RL_R', 'SDL_R', 'SDU_R', 'SV_R', 'TN_R')
)

# Display the filtered data frame
print(files_and_traits)

# Function to read a file, add a trait column, and standardize column names
read_and_label <- function(filename, traitname) {
  df <- read_csv(filename, show_col_types = FALSE)  # Suppress column type messages
  df$Trait <- traitname
  # Keep only relevant columns: 'CHROM', 'POS', 'P', 'support', 'Trait'
  df <- df %>% select(CHROM, POS, P, support, Trait)
  return(df)
}

# Read and combine all "_R" datasets
multispeqjames <- bind_rows(lapply(1:nrow(files_and_traits), function(i) {
  read_and_label(files_and_traits$file[i], files_and_traits$trait[i])
}))

# Save the combined dataset with only "_R" files
write_csv(multispeqjames, "netural_nitrogen_kyle.csv")

# Print confirmation message
print("File 'high_nitrogen_kyle.csv' successfully created with only '_R' data.")







library(dplyr)
library(readr)

setwd("~/Documents/PhD work/Kyle project/results")

# List of file names
files <- c('ZBAR_N..1signals.csv', 'ZBIL_N..1signals.csv', 'ZDTA_N..1signals.csv', 'ZIR_N..1signals.csv',
           'ZMOB_N..1signals.csv', 'ZPBN_N..1signals.csv', 'ZPH_N..1signals.csv', 'ZRDL_N..1signals.csv',
           'ZRDU_N..1signals.csv', 'ZRL_N..1signals.csv', 'ZSDL_N..1signals.csv',
           'ZSDU_N..1signals.csv', 'ZSV_N..1signals.csv', 'ZTN_N..1signals.csv')

# Function to extract trait name from file name
extract_trait <- function(filename) {
  trait_name <- gsub("Z|_N..1signals.csv", "", filename)
  return(trait_name)
}

# Function to read a file, add a trait column, and standardize column names
read_and_label <- function(filename) {
  traitname <- extract_trait(filename)
  df <- read_csv(filename)
  df$Trait <- traitname
  # Assuming 'CHROM', 'POS', 'P', and 'support' are the common columns you want to keep
  df <- df %>% select(CHROM, POS, P, support, Trait)
  return(df)
}

# Read and combine all datasets
multispeqjames <- bind_rows(lapply(files, function(file) {
  read_and_label(file)
}))

# Now 'multispeqjames' contains all your observations with an additional 'Trait' column
write_csv(multispeqjames, "low_nitrogen.csv")




#High_Nitrogen

library(dplyr)
library(readr)

setwd("~/Documents/PhD work/Kyle project/results")

# Updated list of file names
files <- c('ZBAR_N.signals.csv', 'ZDTA_N.signals.csv', 'ZBIL_N.signals.csv', 'ZIR_N.signals.csv',
           'ZMOB_N.signals.csv', 'ZPBN_N.signals.csv', 'ZPH_N.signals.csv', 'ZRDL_N.signals.csv',
           'ZRDU_N.signals.csv', 'ZRL_N.signals.csv', 'ZSDL_N.signals.csv', 
           'ZSDU_N.signals.csv', 'ZSV_N.signals.csv', 'ZTN_N.signals.csv')

# Function to extract trait name from file name
extract_trait <- function(filename) {
  trait_name <- gsub("Z|_N.signals.csv", "", filename)
  return(trait_name)
}

# Function to read a file, add a trait column, and standardize column names
read_and_label <- function(filename) {
  traitname <- extract_trait(filename)
  df <- read_csv(filename)
  df$Trait <- traitname
  # Assuming 'CHROM', 'POS', 'P', and 'support' are the common columns you want to keep
  df <- df %>% select(CHROM, POS, P, support, Trait)
  return(df)
}

# Read and combine all datasets
multispeqjames <- bind_rows(lapply(files, function(file) {
  read_and_label(file)
}))

# Now 'multispeqjames' contains all your observations with an additional 'Trait' column
write_csv(multispeqjames, "high_nitrogen.csv")