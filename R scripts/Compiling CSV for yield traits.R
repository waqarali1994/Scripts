library(dplyr)
library(readr)


setwd("~/Documents/again merged manhattan plot for multispeq")

# List of file names and corresponding traits
files_and_traits <- data.frame(
  file = c("ZNumber_Rowssignals.csv", "ZEar_Lengthsignals.csv", "ZEar_Widthsignals.csv", 
           "ZK_weightsignals.csv", "Zkernels_rowsignals.csv", "Zear_weightsignals.csv", 
           "Zplot_weightsignals.csv"),
  trait = c("Kernel Row Number", "Ear Length",	"Ear Width", "Kernel Weight", "Kernels Per Row",	
            "Ear Weight",	"Plot Weight")
)

# Function to read a file, add a trait column, and standardize column names
read_and_label <- function(filename, traitname) {
  df <- read_csv(filename)
  df$Trait <- traitname
  # Assuming 'CHROM', 'POS', and 'support' are the common columns you want to keep
  df <- df %>% select(CHROM, POS, P, support, Trait)
  return(df)
}

# Read and combine all datasets
all_data <- bind_rows(lapply(1:nrow(files_and_traits), function(i) {
  read_and_label(files_and_traits$file[i], files_and_traits$trait[i])
}))

# Now 'all_data' contains all your observations with an additional 'Trait' column

write.csv(all_data, "newyieldgwas_z.csv")
