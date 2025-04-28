library(stringr)
library(dplyr)
library(ggplot2)
# Assuming wesanderson is already installed, no need to install again.
library(wesanderson)

setwd("~/Documents/Yield traits merged manhattan plot")
df = read.csv("spatiallycorrected_combined_yield_zfile.csv")
df$CHROM = as.numeric(gsub('chr','',df$CHROM))
df = df[sample(1:nrow(df)), ]

# Calculate cumulative base pair positions for each chromosome
data_cum <- df %>% 
  group_by(CHROM) %>% 
  summarise(max_bp = max(POS)) %>% 
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>% 
  select(CHROM, bp_add)

# Combine cumulative data with GWAS data
gwas_data <- df %>% 
  inner_join(data_cum, by = "CHROM") %>% 
  mutate(bp_cum = POS + bp_add)

# Calculate axis settings for the plot
axis_set <- gwas_data %>% 
  group_by(CHROM) %>% 
  summarize(center = mean(bp_cum))

# Define the traits to plot
traits <- c("Kernel Row Number", "Ear Length",	"Ear Width", "k Weight", "Kernels Per Row",	
            "Ear Weight",	"Plot Weight",	"Percent Moisture", "Cob filling")

# Define a vector with your custom hex colors for each trait
hex_colors <- setNames(c("#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", 
                "#9a6324", "#800000", "#000075","#ff4444"),
                traits)

# Make sure the number of colors matches the number of unique traits in your data
if(length(hex_colors) != length(unique(gwas_data$Trait))) {
  stop("The number of hex colors must match the number of unique traits.")
}

# Use the hex color vector for the color palette
color_palette <- hex_colors


# Modify the loop to map 'color' to 'Trait' and remove 'as.factor(CHROM)'.
# Instead of adding color scales within the loop, define it once outside the loop.

# Initialize the ggplot object
p <- ggplot() +
  geom_vline(xintercept = data_cum$bp_add, color = "darkgrey", linetype = "dashed", lwd=0.2) +
  scale_x_continuous(breaks = axis_set$center, labels = c('Chr01','Chr02','Chr03', 
                                                          'Chr04','Chr05', 'Chr06',
                                                          'Chr07', 'Chr08', 'Chr09', 'Chr10')) +
  scale_y_continuous(expand = c(0,0), limits = c(0.0, 0.6)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5))

# Loop over the traits to add points to the plot
for (i in seq_along(traits)) {
  trait_data <- gwas_data %>% 
    filter(Trait == traits[i])
  
  p <- p + geom_point(data = trait_data, 
                      aes(x = bp_cum, y = support, color = Trait), 
                      alpha = 0.75, size = 3)
}

# Add horizontal lines for threshold levels
threshold_value <- 0.1 # Example threshold, adjust to your needs
p <- p + geom_hline(yintercept = threshold_value, color = "black", linetype = "dashed", lwd=0.5)

# Add the color scale for 'Trait' outside the loop
p <- p + scale_color_manual(values = color_palette, name = "Trait",
                            breaks = unique(gwas_data$Trait), labels = unique(gwas_data$Trait))

# Final touches to the plot
p <- p + labs(x = "Chromosome", y = "RMIP") + 
  guides(color = guide_legend(title = "Trait"))

# Print the plot
print(p)













library(stringr)
library(dplyr)
library(ggplot2)
library(wesanderson)

setwd("~/Documents/PhD work/Yield project/SpATS BLUEs/spatially_corrected_merged manhattan plot")
df <- read.csv("multispeqcutoff_final.csv")
df$CHROM <- as.numeric(gsub('chr', '', df$CHROM))
df <- df[sample(1:nrow(df)), ]

# Calculate cumulative base pair positions for each chromosome
data_cum <- df %>% 
  group_by(CHROM) %>% 
  summarise(max_bp = max(POS)) %>% 
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>% 
  select(CHROM, bp_add)

# Combine cumulative data with GWAS data
gwas_data <- df %>% 
  inner_join(data_cum, by = "CHROM") %>% 
  mutate(bp_cum = POS + bp_add)

# Calculate axis settings for the plot
axis_set <- gwas_data %>% 
  group_by(CHROM) %>% 
  summarize(center = mean(bp_cum))

# Define the traits to plot
traits <- c("Ear_Length", "Ear_Width", "Fill_CM", "Number_Rows", "ear_weight",
            "plot_weight", "Percent_moisture", "K_weight","Kernels_row")

# Define a vector with your custom hex colors for each trait
hex_colors <- setNames(c("#f58231","#9a6324","#ff4444","#ffe119","#3cb44b","#911eb4","pink","#4363d8","#000075"),
                       traits)

# Check that the number of colors matches the number of unique traits in your data
if(length(hex_colors) != length(unique(gwas_data$Trait))) {
  stop("The number of hex colors must match the number of unique traits.")
}

# Use the hex color vector for the color palette
color_palette <- hex_colors

# Initialize the ggplot object
p <- ggplot() +
  geom_vline(xintercept = data_cum$bp_add, color = "darkgrey", linetype = "dashed", lwd = 0.2) +
  scale_x_continuous(breaks = axis_set$center, labels = paste0('Chr', sprintf("%02d", 1:10))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.0, 0.6)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5))

# Loop over the traits to add points to the plot
for (i in seq_along(traits)) {
  trait_data <- gwas_data %>% 
    filter(Trait == traits[i])
  
  p <- p + geom_point(data = trait_data, 
                      aes(x = bp_cum, y = support, color = Trait), 
                      alpha = 0.75, size = 3)
}

# Add horizontal lines for threshold levels
threshold_value <- 0.2 # Example threshold, adjust to your needs
p <- p + geom_hline(yintercept = threshold_value, color = "black", linetype = "dashed", lwd = 0.5)

# Add the color scale for 'Trait' outside the loop
p <- p + scale_color_manual(values = color_palette, name = "Trait",
                            breaks = unique(gwas_data$Trait), labels = unique(gwas_data$Trait))

# Final touches to the plot
p <- p + labs(x = "Chromosome", y = "RMIP") + 
  guides(color = guide_legend(title = "Trait"))

# Print the plot
print(p)












library(stringr)
library(dplyr)
library(ggplot2)
library(wesanderson)

setwd("~/Documents/Yield traits merged manhattan plot")
df <- read.csv("ZNumber_Rowssignals.csv")
df$CHROM <- as.numeric(gsub('chr', '', df$CHROM))
df <- df[sample(1:nrow(df)), ]

# Ensure Trait column exists and is correct
if (!"Trait" %in% colnames(df)) {
  df$Trait <- "Number_Rows"  # Set the trait for this specific file
}

# Calculate cumulative base pair positions for each chromosome
data_cum <- df %>% 
  group_by(CHROM) %>% 
  summarise(max_bp = max(POS)) %>% 
  mutate(bp_add = lag(cumsum(max_bp), default = 0)) %>% 
  select(CHROM, bp_add)

# Combine cumulative data with GWAS data
gwas_data <- df %>% 
  inner_join(data_cum, by = "CHROM") %>% 
  mutate(bp_cum = POS + bp_add)

# Calculate axis settings for the plot
axis_set <- gwas_data %>% 
  group_by(CHROM) %>% 
  summarize(center = mean(bp_cum))

# Define the traits to plot
traits <- c("Number_Rows")

# Define a vector with your custom hex colors for each trait
hex_colors <- setNames(c("#f58231"), traits)

# Check that the number of colors matches the number of unique traits in your data
if (length(hex_colors) != length(unique(gwas_data$Trait))) {
  stop("The number of hex colors must match the number of unique traits.")
}

# Use the hex color vector for the color palette
color_palette <- hex_colors

# Initialize the ggplot object
p <- ggplot() +
  geom_vline(xintercept = data_cum$bp_add, color = "darkgrey", linetype = "dashed", lwd = 0.2) +
  scale_x_continuous(breaks = axis_set$center, labels = paste0('Chr', sprintf("%02d", 1:10))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.0, 0.6)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5))

# Loop over the traits to add points to the plot
for (i in seq_along(traits)) {
  trait_data <- gwas_data %>% 
    filter(Trait == traits[i])
  
  p <- p + geom_point(data = trait_data, 
                      aes(x = bp_cum, y = support, color = Trait), 
                      alpha = 0.75, size = 3)
}

# Add horizontal lines for threshold levels
threshold_value <- 0.1 # Example threshold, adjust to your needs
p <- p + geom_hline(yintercept = threshold_value, color = "black", linetype = "dashed", lwd = 0.5)

# Add the color scale for 'Trait' outside the loop
p <- p + scale_color_manual(values = color_palette, name = "Trait",
                            breaks = unique(gwas_data$Trait), labels = unique(gwas_data$Trait))

# Final touches to the plot
p <- p + labs(x = "Chromosome", y = "RMIP") + 
  guides(color = guide_legend(title = "Trait"))

# Print the plot
print(p)
