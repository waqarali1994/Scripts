if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
install.packages("devtools")
BiocManager::install("snpStats")
devtools::install_github("yangjl/LDheatmap")
## Select Variants for the interval
# ml gatk4
# ml java/1.8
#
#gatk SelectVariants -L chr1:7710000-7810000 -V ram.vcf.gz -O mahi.vcf
#gatk SelectVariants -L chr1:7710000-7810000 -V ram.vcf.gz -O mahi.vcf
library(snpStats)
library(LDheatmap)
library(vcfR)
library(data.table)
# library(patchwork)
library(ggplot2)
library(grid)
setwd("Desktop")
df=read.vcfR('phi2_9.vcf')
df=vcfR2SnpMatrix(df)
# df$data
df1=fread('phi2_9.vcf', sep = '\t')
colnames(df1)[1]='Chrom'
df1=df1[df1$Chrom=='chr9',]
df1=df1[df1$POS>=93775951 & df1$POS<=95775951,]
CEUDist1=df1$POS
rgb.palette <- colorRampPalette(rev(c("grey", "orange", "red")), space = "rgb")
# p=ggplot2::ggplot(data=df1, aes(x=POS))+geom_histogram()
# Open a PNG device with higher resolution and larger dimensions
#png('qL_LD.png', width = 2000, height = 2000, res = 300)
# Create the heatmap
MyHeatmap <- LDheatmap(df$data, CEUDist1, LDmeasure="r", title=NULL, add.map=T, SNP.name=c("chr9_94775951","chr9_94321207"),
                       color=rgb.palette(18), flip=T, name="myLDgrob", add.key=TRUE, geneMapLocation=0.15)
# Modify grid graphics elements
grid.edit(gPath("myLDgrob", "geneMap","symbols"), gp = gpar(cex=4, col=c('blue','green'), pch=c(10,10)))
grid.edit(gPath("myLDgrob", "geneMap","SNPnames"), gp = gpar(cex=1, col='white'))
grid.edit(gPath("myLDgrob", "Key","box"), gp = gpar(cex=14, col='black'))
grid.edit(gPath("myLDgrob", "Key","labels"), gp = gpar(cex=1, col='black'))
grid.edit(gPath("myLDgrob", "Key","ticks"), gp = gpar(cex=22, col='black'))
# Close the PNG device
#dev.off()