"~/Desktop/sptas_trends/TWAS2020_genes_eQTL_results/TPM vs Genotype.svg"


setwd("~/Documents/PhD work/Yield project/SpATS BLUEs/Spatially_corrected_yield_RMIP_results")
## regular phenotype input

# load rmvp formatted data
MVP.Data(fileVCF ="phenogenomatchv3.vcf",
         filePhe="filtered_spataillycorrected_yieldblues.csv",
        #sep.hmp="\t",
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
                 nPC.FarmCPU = 3, maxLoop = 10, method = "FarmCPU", priority = 'memory',threshold=0.126, p.threshold=1.07e-8)
  }
}

traits=c('Ear_Length',	'Ear_Width',	'Fill_CM',	'Number_Rows',	'Kernels_row',	
         'ear_weight',	'plot_weight',	'Percent_moisture',	'K_weight')

get.support=function(trait){ # write a function to summarise the occurrence of signals, trait is what i have in the rmvp output filenames, disregarding the number of bootstrap
  files = list.files(pattern = paste0(trait,".*FarmCPU_signals.csv"))
  if (length(files)>=1){  
    signals <-
      files %>%
      map_df(~read.csv(.,skip=1,header=F,colClasses = c("factor","factor","integer","factor","factor","numeric","numeric","numeric")))
    header <- c("SNP","CHROM","POS","REF","ALT","Effect","SE","pvalue")
    colnames(signals)=header
    signals=signals %>%
      group_by(SNP,CHROM,POS) %>%
      summarise(P=mean(pvalue), support = n()/100) #%>% ## if {{trait}} doesnot work otherwise change this name to something else 
    #separate(SNP, c("CHR","BP"),remove=F)
    write.table(signals, file=paste0("Z", trait, "signals.csv"), quote = F,row.names = F,sep=",")
  }
  else{
    print(paste0("file not found", trait))
  }
}

for(x in traits){get.support(x)}







# Set working directory
setwd("~/Documents/PhD work/Kyle project")

# Load necessary libraries
library(data.table)
library(bigmemory)
library(rMVP)
library(purrr)
library(dplyr)

# Load PLINK formatted data
MVP.Data(fileBed ="filtered_genotype",
         fileBim ="filtered_genotype",
         fileFam ="filtered_genotype",
         filePhe="filtered_phenotype_data.csv",
         sep.phe=",",
         SNP.effect="Add",
         fileKin=TRUE,
         filePC=TRUE,
         priority="memory",
         out="mvp_plink"
)

genotype <- attach.big.matrix("mvp_plink.geno.desc")
phenotype <- read.table("mvp_plink.phe", header = TRUE)
map <- read.table("mvp_plink.geno.map", header = TRUE)
Kinship <- attach.big.matrix("mvp_plink.kin.desc")
Covariates_PC <- bigmemory::as.matrix(attach.big.matrix("mvp_plink.pc.desc"))


# FarmCPU bootstrapFarmCPU_signals
for (x in 1:100) {
  phe1 <- phenotype  # make a copy of phenotype
  nline <- nrow(phe1)
  phe1[sample(c(1:nline), as.integer(nline * 0.1)), 2:ncol(phe1)] <- NA  # randomly choose 10% phenotype to be NA
  colnames(phe1) <- paste0(colnames(phenotype), x)  # rename the phenotype by attaching bootstrap number
  for (i in 2:ncol(phe1)) {
    imMVP <- MVP(phe = phe1[, c(1, i)], geno = genotype, map = map, K = Kinship, CV.FarmCPU = Covariates_PC, file.output = "pmap.signal",
                 nPC.FarmCPU = 3, maxLoop = 10, method = "FarmCPU", priority = 'memory', threshold = 0.256, p.threshold = 5.44e-8)
  }
}

# Updated traits list
traits <- c('DTA_N.', 'PH_N.', 'TN_N.', 'SDL_N.', 'SDU_N.', 'SV_N.', 'IR_N.', 'RL_N.', 'RDL_N.', 'RDU_N.', 
            'PBN_N.', 'BAR_N.', 'BIL_N.', 'MOB_N.', 'DTA_N..1', 'PH_N..1', 'TN_N..1', 'SDL_N..1', 'SDU_N..1', 'SV_N..1', 
            'IR_N..1', 'RL_N..1', 'RDL_N..1', 'RDU_N..1', 'PBN_N..1', 'BAR_N..1', 'BIL_N..1', 'MOB_N..1')

get.support <- function(trait) {
  files <- list.files(pattern = paste0(trait, ".*FarmCPU_signals.csv"))
  if (length(files) >= 1) {
    signals <- files %>%
      map_df(~read.csv(., skip = 1, header = FALSE, colClasses = c("factor", "factor", "integer", "factor", "factor", "numeric", "numeric", "numeric")))
    header <- c("SNP", "CHROM", "POS", "REF", "ALT", "Effect", "SE", "pvalue")
    colnames(signals) <- header
    signals <- signals %>%
      group_by(SNP, CHROM, POS) %>%
      summarise(P = mean(pvalue), support = n() / 100)
    write.table(signals, file = paste0("Z", trait, "signals.csv"), quote = FALSE, row.names = FALSE, sep = ",")
  } else {
    print(paste0("file not found ", trait))
  }
}

for (x in traits) {
  get.support(x)
}







