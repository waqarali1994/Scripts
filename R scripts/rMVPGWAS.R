# Install and load the rMVP package
library(rMVP)

# Preprocess the VCF and phenotype files
MVP.Data(fileVCF="phenogenomatchv3.vcf",
         filePhe="outlier_adjusted_blues_2021.csv", sep.phe=",",
         fileKin=FALSE,
         filePC=FALSE,
         out="mvp")

# Load the processed genotype and map data
genotype <- attach.big.matrix("mvp.geno.desc")
map <- read.table("mvp.geno.map", head = TRUE)

# Read the phenotype data
phenotype <- read.table("outlier_adjusted_blues_2021.csv", sep=',', head = TRUE)

# Loop over each trait in the phenotype dataframe
for(trait in colnames(phenotype)[-1]) {  # Assuming the first column is IDs
  # Subset the phenotype data for the current trait
  current_phenotype <- phenotype[c("Genotypes", trait)]  # Replace "ID" with the actual ID column name
  
  # Run MVP for the current trait
  imMVP <- MVP(
    phe=current_phenotype,
    geno=genotype,
    map=map,
    nPC.GLM=5, 
    nPC.MLM=3, 
    nPC.FarmCPU=3,
    priority="memory", 
    vc.method="BRENT", 
    maxLoop=10,
    method.bin="static",
    threshold=0.05,
    method=c("GLM", "MLM", "FarmCPU")
  )
  
  # Handle the results for each trait
  # This could involve saving the results to files, generating plots, etc.
  # ...
}
