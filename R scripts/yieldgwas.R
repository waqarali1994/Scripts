install.packages("rMVP")
library(rMVP)
MVP.Data(fileVCF="yieldfilter3.vcf",
         filePhe="merged_yieldtraits.csv",sep.phe=",",
         fileKin=FALSE,
         filePC=FALSE,
         out="mvp.vcf") 
genotype <- attach.big.matrix("mvp.geno.desc")
map <- read.table("mvp.geno.map" , head = TRUE)
phenotype <- read.table("merged_yieldtraits.csv",  sep=',', head = TRUE)

imMVP <- MVP(
  phe=phenotype,
  geno=genotype,
  map=map,
  #K=Kinship,
  #CV.GLM=Covariates,  ##if you have additional covariates, please keep there open.
  #CV.MLM=Covariates,
  #CV.FarmCPU=Covariates,
  nPC.GLM=5,   ##if you have added PCs into covariates, please keep there closed.
  nPC.MLM=3,  ##if you don't want to add PCs as covariates, please comment out the parameters instead of setting the nPC to 0.
  nPC.FarmCPU=3,
  priority="memory",   ##for Kinship construction
  #ncpus=10,
  vc.method="BRENT",  ##only works for MLM
  maxLoop=10,
  method.bin="static",   ## "FaST-LMM", "static" (#only works for FarmCPU)
  #permutation.threshold=TRUE,
  #permutation.rep=100,
  threshold=0.05,
  method=c("GLM", "MLM", "FarmCPU"))