library(stringr)
library(dplyr)
library(ggplot2)
library(wesanderson)
install.packages("wesanderson")

# setwd("~/Documents/Sorghumseedimage/351_biallelic/GWAS_biallelic/")

df=read.csv("ZRelative_Chlorophyllsignals.csv")

df$CHROM=as.numeric(gsub('chr','',df$CHROM))

df= df[sample(1:nrow(df)), ]
# df$support=df$support/100

data_cum <- df %>% 
  group_by(CHROM) %>% 
  summarise(max_bp = max(POS)) %>% 
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>% 
  select(CHROM, bp_add)


gwas_data <- df %>% 
  inner_join(data_cum, by = "CHROM") %>% 
  mutate(bp_cum = POS + bp_add)

axis_set <- gwas_data %>% 
  group_by(CHROM) %>% 
  summarize(center = mean(bp_cum))

ylim <- gwas_data %>% 
  filter(support == min(support)) %>% 
  mutate(ylim = support + 2) %>% 
  pull(ylim)

sig <- 0.05


ggplot(gwas_data, aes(x = bp_cum, y = support, colour=as.factor(CHROM))) + 
  geom_vline(xintercept = data_cum$bp_add, color = "darkgrey", linetype = "dashed", lwd=0.2) +
  geom_point(alpha = 5, size=3)+
  geom_hline(yintercept = 0.2, color = "black", linetype = "dashed", lwd=0.5) +
  geom_hline(yintercept = 0.1, color = "gray", linetype = "dashed", lwd=0.5) +
  scale_x_continuous(breaks = axis_set$center, n.breaks = 2000, expand = c(0,0), labels = c('Chr01','Chr02','Chr03',
                                                                                            'Chr04','Chr05', 'Chr06',
                                                                                            'Chr07', 'Chr08', 'Chr09', 'Chr10')) +
  scale_y_continuous(expand = c(0,0), limits = c(0.0, 0.6)) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0, size = 12, vjust = 0.5, color = "black", margin = margin(t = 0, r = 0, b = 10, l = 0)),
    axis.text.y = element_text(size = 12, vjust = 0.9, color = "black", margin = margin(t = 0, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    plot.margin = unit(c(2,2,7,2), "lines"),
    legend.text = element_text(size=12),
    legend.title =  element_text(NULL),
    legend.position ='None'
  )

  ###SeedShape
  # scale_color_manual(values=c("#6565ZNumber_of_rowssignals65", "black", "grey"))+
  ##SeedColor
  scale_color_manual(values=c("darkblue","#26755d","darkblue","#26755d","darkblue","#26755d","darkblue","#26755d","darkblue","#26755d"))+#, breaks=c('Red Channel', 'Green Channel', 'Blue Channel', 'PC1', 'PC2','PC3'))+
  ylab("RMIP") + 
  xlab("Chromosome") +  xlab("Chromosome") +
  labs(color ="")
  labs(color ="")


dev.off()dev.off()fill_alpha()

