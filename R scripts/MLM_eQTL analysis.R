# Install and load the necessary packages
# install.packages("rMVP")
# install.packages("data.table")
library(rMVP)
library(data.table)

# Preprocess the VCF and phenotype files
MVP.Data(fileVCF="filtered_SAP2021.vcf",
         filePhe="Sorghum2021_spatially_corrected_BLUPs_final.csv", sep.phe=",",
         fileKin=FALSE,
         filePC=FALSE,
         out="mvp")

# Load the processed genotype and map data
genotype <- attach.big.matrix("mvp.geno.desc")
map <- fread("mvp.geno.map", data.table = FALSE)

# Read the phenotype data
phenotype <- fread("Sorghum2021_spatially_corrected_BLUPs_final.csv", sep=',', header = TRUE)

# Counter for run identifiers
run_id <- 1

# Loop over each trait in the phenotype dataframe
for(trait in colnames(phenotype)[-1]) {  # Assuming the first column is IDs
  # Subset the phenotype data for the current trait
  current_phenotype <- phenotype[, .(Genotype, trait_value = get(trait))]
  
  # Rename columns for clarity if needed
  setnames(current_phenotype, c("Genotype", "trait_value"), c("Genotype", trait))
  
  # Run MVP for the current trait using MLM model and EMMA variance method
  imMVP <- MVP(
    phe=current_phenotype,
    geno=genotype,
    map=map,
    nPC.MLM=3, 
    priority="memory", 
    vc.method="EMMA", 
    maxLoop=10,
    method.bin="static",
    threshold=0.05,
    method="MLM",
    file.output = FALSE  # Ensure no files are saved automatically
  )
  
  # Combine results
  imMVP_results <- cbind(imMVP$map, imMVP$mlm.results)
  
  # Verify the structure of the combined results
  str(imMVP_results)
  print(head(imMVP_results))
  
  # Define output file name
  output_file <- paste0("results/SAP_sr", run_id, "_results_", trait, ".csv.gz")
  
  # Create the results directory if it doesn't exist
  if (!dir.exists("results")) {
    dir.create("results")
  }
  
  # Write results to a gzipped CSV file
  fwrite(imMVP_results, output_file, compress = "gzip")
  
  # Increment the run identifier
  run_id <- run_id + 1
}
