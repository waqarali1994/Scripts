library(rMVP)
MVP.Data(fileVCF="cornfilter6.vcf",
         filePhe="cornfilter6.csv",sep.phe=",",
         fileKin=FALSE,
         filePC=FALSE,
         out="mvp")
         genotype <- attach.big.matrix("mvp.geno.desc")
         map <- read.table("mvp.geno.map" , head = TRUE)
         phenotype <- read.table("cornfilter6.csv",  sep=',', head = TRUE)
         
# This loop goes over each phenotype in your phenotype file; runs MLM, GLM, & FarmCPU models for each
for (i in 2:(ncol(phenotype)))
{
  imMVP <- MVP(phe = phenotype[, c(1, i)], # Subsets phenotype file to the genotype names and the phenotype in column i
              geno = genotype, # Tells MVP where to get the genotype SNP data
               map = map, # Tells MVP where to get the map of the SNPs
                # Tells MVP where to get the kinship matrix
               nPC.GLM = 5, # These next 3 lines tell MVP how many principal components to fit as covariates to account for population structure in each model
               nPC.MLM = 3,
               nPC.FarmCPU = 3,
               priority = 'speed', # Prioritize minimizing the time used to fit the models, rather than the memory
               vc.method = 'BRENT', # Method to use for estimating variance components
               maxLoop = 10, # Max loops to use when fitting FarmCPU
               method.bin = 'static', # How it bins SNPs in FarmCPU
               threshold = 0.05, # the significance threshold; this is Bonferroni method (usually too conservative)
               method = c('GLM', 'MLM', 'FarmCPU'), # what models to fit
               file.output = TRUE, # yes, we want the output files
               outpath = '~/Documents/Waqar Ali/multitraitgwas') # the folder to put the output files in --there will be a lot
  gc()
}