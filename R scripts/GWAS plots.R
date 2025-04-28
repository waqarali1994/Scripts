library(data.table)
library(dplyr)
library(scales)
library(jcolors)
library(ggplot2)
library(ggsci)
install.packages("R.utils")
library(RColorBrewer)
install.packages("patchwork")
library(patchwork)


theme_set(theme_classic(base_size = 16))
theme_update(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))

###

gwas.dat <- fread("Anthesis.sp.MI.assoc.txt.gz", data.table = F)
colnames(gwas.dat)
nrow(gwas.dat)
nCHR <- length(unique(gwas.dat$chr))
gwas.dat$BPcum <- NA
s <- 0
nbp <- c()

for (i in sort(unique(gwas.dat$chr))){
  nbp[i] <- max(gwas.dat[gwas.dat$chr == i,]$ps)
  gwas.dat[gwas.dat$chr == i,"BPcum"] <- gwas.dat[gwas.dat$chr == i,"ps"] + s
  s <- s + nbp[i]
}

axis.set <- gwas.dat %>% 
  group_by(chr) %>% 
  summarize(center = (max(BPcum) + min(BPcum)) / 2, maxBP=max(BPcum))

gwas.dat.RNA <- gwas.dat[order(gwas.dat$p_wald)[1:200000],]

gGWAS1 <- ggplot() + 
  geom_point(data=gwas.dat.RNA, aes(BPcum, -log10(p_wald), colour=factor(chr, levels = c(1:10)))) + 
  geom_hline(yintercept = -log10(0.01/15659765), linetype=2) + 
  scale_color_manual(values = c('#03193F','#28708C','#BF930F','#0f3bbf','#295E52','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')) +  
  annotate("text", label="", y=11, x=713290558, parse=T, size=5) +
  annotate("text", label="", y=12, x=1758239801, parse=T, size=5) +
  scale_x_continuous(label = axis.set$chr, breaks = axis.set$center) +
  scale_y_continuous(limits = c(2, 13), breaks = seq(2, 13, 2)) +
  theme(legend.position = "none") + 
  ylab(expression(-log[10](p-value))) + 
  xlab("Chromosome") + 
  ylim(3,13)
gGWAS1

###

gwas.dat2 <- fread("Silking.sp.MI.assoc.txt.gz", data.table = F)
colnames(gwas.dat2)
nrow(gwas.dat2)
nCHR <- length(unique(gwas.dat2$chr))
gwas.dat2$BPcum <- NA
s <- 0
nbp <- c()

for (i in sort(unique(gwas.dat2$chr))){
  nbp[i] <- max(gwas.dat2[gwas.dat2$chr == i,]$ps)
  gwas.dat2[gwas.dat2$chr == i,"BPcum"] <- gwas.dat2[gwas.dat2$chr == i,"ps"] + s
  s <- s + nbp[i]
}

axis.set <- gwas.dat2 %>% 
  group_by(chr) %>% 
  summarize(center = (max(BPcum) + min(BPcum)) / 2, maxBP=max(BPcum))

gwas.dat2.RNA <- gwas.dat2[order(gwas.dat2$p_wald)[1:200000],]

gGWAS2 <- ggplot() + 
  geom_point(data=gwas.dat2, aes(BPcum, -log10(p_wald), colour=factor(chr, levels = c(1:10)))) + 
  geom_hline(yintercept = -log10(0.01/15659765), linetype=2) + 
  scale_color_manual(values = c('#03193F','#28708C','#BF930F','#0f3bbf','#295E52','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')) + 
  annotate("text", label="", y=11, x=713290558, parse=T, size=5) +
  annotate("text", label="", y=12, x=1758239801, parse=T, size=5) +
  scale_x_continuous(label = axis.set$chr, breaks = axis.set$center) + 
  scale_y_continuous(limits = c(2, 13), breaks = seq(2, 13, 2)) +
  theme(legend.position = "none") + 
  ylab(expression(-log[10](p-value))) + 
  xlab("Chromosome") + 
  ylim(3,13)
gGWAS2

###

#gGWAS <- gGWAS1 + plot_annotation(tag_levels = "a")



ggsave(filename = "Manhattan_Plot_anthesis.png", plot = gGWAS1, width = 7, height = 4.5)
ggsave(filename = "Manhattan_Plot_anthesis.pdf", plot = gGWAS1, width = 7, height = 4.5)

ggsave(filename = "Manhattan_Plot_silking.png", plot = gGWAS2, width = 7, height = 4.5)
ggsave(filename = "Manhattan_Plot_silking.pdf", plot = gGWAS2, width = 7, height = 4.5)
