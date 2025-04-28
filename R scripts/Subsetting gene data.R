# Load necessary libraries
library(data.table)
library(dplyr)

# Load the gene expression data
expression_data <- fread("expression_data_final.csv")

# Load the gene IDs you are interested in
genes_of_interest <- fread("Harshitha_genes.csv")

# Remove the suffix from gene IDs in the expression data
colnames(expression_data) <- sub("_T[0-9]+$", "", colnames(expression_data))

# Convert the genes_of_interest to a vector of gene IDs
genes_vector <- genes_of_interest[[1]]  # Assuming the gene IDs are in the first column

# Ensure the gene IDs in genes_vector are unique and match the format in expression_data
genes_vector <- unique(genes_vector)

# Subset the expression_data to include only the columns of iexprnterest (gene IDs and genotype ID)
# Assuming the first column in expression_data contains the genotype IDs
selected_data <- expression_data %>% select(Genotype = 2, all_of(genes_vector))

# Ensure the genotype_id column is named correctly
colnames(selected_data)[1] <- "Genotype"

# Save the selected data to a new CSV file
write.csv(selected_data, "Harshitha_filtered_genes_data.csv", row.names = FALSE)

# Confirm the file has been written
print("Filtered expression data has been saved to filtered_expression_data.csv")
