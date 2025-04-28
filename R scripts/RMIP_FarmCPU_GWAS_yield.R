library(rMVP)
library(data.table)
library(purrr)
setwd("~/Documents/PhD work/Yield project/NEW_YIELD_GWAS_Dec_2024/")
## regular phenotype inpu

# load rmvp formatted data
MVP.Data(fileVCF ="yield_newv3.vcf",
         filePhe="yield_blues_extreem_removed.csv", #sep.hmp="\t",
         sep.phe=",",
         SNP.effect="Add",
         fileKin=TRUE,
         filePC=TRUE,
         priority="memory",
         #maxLine=10000,
         out="mvp.vcf"
)

genotype <- attach.big.matrix("mvp.vcf.geno.desc")
phenotype <- read.table("mvp.vcf.phe",head=TRUE)
map <- read.table("mvp.vcf.geno.map" , head = TRUE)
Kinship <- attach.big.matrix("mvp.vcf.kin.desc")
Covariates_PC <- bigmemory::as.matrix(attach.big.matrix("mvp.vcf.pc.desc"))

#FarmCPU bootstrapFarmCPU_signals
# args=commandArgs(TRUE)# receive argument x from terminal
# x=as.numeric(args) # x is the no. of bootstrap from 1 to 100

for(x in 1:100){
  phe1=phenotype # make a copy of phenotype
  nline = nrow(phe1)
  phe1[sample(c(1:nline), as.integer(nline*0.1)), 2:ncol(phe1)]=NA  # randomly choose 10% phenotype to be NA
  colnames(phe1)=paste0(colnames(phenotype),x)  # rename the phenotype by attaching bootstrap number
  for(i in 2:ncol(phe1)){
    imMVP <- MVP(phe = phe1[,c(1,i)], geno = genotype, map = map, K=Kinship, CV.FarmCPU=Covariates_PC, file.output="pmap.signal",
                 nPC.FarmCPU = 3, maxLoop = 10, method = "FarmCPU", priority = 'memory',threshold=0.123, p.threshold=8.37e-9)
  }
}

# Define your traits
traits = c('Ear_Length',	'Ear_Width',	'fill_cm',	'Number_Rows',	'kernels_row',
           'ear_weight',	'plot_weight',	'K_weight')

# Function to summarize the occurrence of signals
get.support = function(trait) {
  # List files matching the pattern for each trait
  files = list.files(pattern = paste0(trait, ".*FarmCPU_signals.csv"))
  
  if (length(files) >= 1) {
    # Read and combine all matching CSV files
    signals = files %>%
      map_df(~read.csv(., skip = 1, header = F, colClasses = c("factor", "factor", "integer", "factor", "factor", "numeric", "numeric", "numeric")))
    
    # Set column headers
    header = c("SNP", "CHROM", "POS", "REF", "ALT", "Effect", "SE", "pvalue")
    colnames(signals) = header
    
    # Summarize the signals by SNP, CHROM, and POS
    signals = signals %>%
      group_by(SNP, CHROM, POS) %>%
      summarise(P = mean(pvalue), support = n() / 100)
    
    # Write the output file for each trait
    write.table(signals, file = paste0("Z", trait, "signals.csv"), quote = F, row.names = F, sep = ",")
  } else {
    print(paste0("file not found: ", trait))
  }
}

# Run the function for each trait
for(x in traits) {
  get.support(x)
}

