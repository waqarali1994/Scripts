library(rMVP)

# Read data
genotype <- attach.big.matrix("mvp.geno.desc")
map <- read.table("mvp.geno.map" , head = TRUE)
phenotype <- read.table("merged_yieldtraits.csv",  sep=',', head = TRUE)

# Loop over each trait in the phenotype dataframe
for(trait in colnames(phenotype)[-1]) {  # Assuming first column is IDs and others are traits
  # Subset the phenotype data for the current trait
  current_phenotype <- phenotype[c("ID", trait)]  # Replace "ID" with the actual ID column name
  
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
  
  # Handle the results (e.g., save them to a file)
  # ...
}
