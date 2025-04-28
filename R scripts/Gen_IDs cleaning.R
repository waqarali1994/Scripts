# Load necessary libraries
library(dplyr)
library(stringr)  # for str_trim and str_to_upper functions
df1 <- read.csv('expression_spatiallyCorrectedBLUPs_all.csv', stringsAsFactors = FALSE)
df2 <- read.csv('transposed_gene_genotype_data.csv', stringsAsFactors = FALSE)

# Define a function to clean GeneID
clean_gene_id <- function(gene_id) {
  gene_id <- gsub(' ', '', gene_id)  # Remove spaces
  gene_id <- str_trim(gene_id)       # Trim whitespace
  gene_id <- str_to_upper(gene_id)   # Convert to uppercase for consistency
  return(gene_id)
}

# Apply the cleaning function to the GeneID columns
df1$GeneID <- clean_gene_id(df1$GeneID)
df2$GeneID <- clean_gene_id(df2$GeneID)

# Merge the datasets based on the cleaned GeneID
merged_df <- merge(df1, df2, by = "GenotypeID")

write.csv(merged_df,"filterted_allele_file.csv")
# Remove duplicate rows based on GeneID
filtered_df <- merged_df %>% distinct(GeneID, .keep_all = TRUE)

# Check the number of unique GeneIDs in the filtered dataset
unique_gene_ids <- length(unique(filtered_df$GeneID))
print(paste("Number of unique GeneIDs in the filtered dataset:", unique_gene_ids))

# Save the filtered data frame to a new CSV file
write.csv(filtered_df, 'filtered_dataset1.csv', row.names = FALSE)






