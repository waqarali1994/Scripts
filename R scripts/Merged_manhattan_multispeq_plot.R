library(stringr)
library(dplyr)
library(ggplot2)
library(wesanderson)

#setwd("~/Documents/Multispeq resampling GWAS results")

library(dplyr)
library(ggplot2)

# Load data
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

# Check if the 'Trait' column exists in gwas_data
if (!"Trait" %in% colnames(gwas_data)) {
  stop("The 'Trait' column does not exist in gwas_data.")
}

# Print summary of gwas_data to inspect its structure
print(summary(gwas_data))

# Define the raw and formatted trait names
raw_trait_names <- c("ECS_.mAU", "ECS_tau", "gH.", "vH.", "Phi2", "PhiNPQ",
                     "PhiNO", "qL", "NPQt", "FvP_over_FmP", "PS1_Active.Centers",
                     "PS1_Open.Centers", "PS1_Over.Reduced.Centers",
                     "PS1_Oxidized.Centers", "Relative_Chlorophyll")

formatted_trait_names <- c("ECS mAU", "ECS tAU", "gH+", "vH+", "Phi2", "PhiNPQ",
                           "PhiNO", "qL", "NPQt", "FvP/FmP", "PS1 Active Centers",
                           "PS1 Open Centers", "PS1 Over Reduced Centers",
                           "PS1 Oxidized Centers", "Relative Chlorophyll")

# Create a named vector for color mapping
hex_colors <- c("#1F77B4", "#d6a851", "brown", "#D62728", "#9467BD", "#8C564B",
                "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#321b16", "#FF7F0E",
                "#1444BD", "#437f85", "#2CA02C")

colors_for_traits <- setNames(hex_colors, formatted_trait_names)

# Create a mapping from raw to formatted trait names
trait_name_mapping <- setNames(formatted_trait_names, raw_trait_names)

# Check unique values in gwas_data$Trait
unique_phenotypes <- unique(gwas_data$Trait)
print(unique_phenotypes)

# Check which phenotypes are not in the mapping
missing_traits <- setdiff(unique_phenotypes, names(trait_name_mapping))
print(missing_traits)

# Ensure all phenotypes are included in the mapping
if (length(missing_traits) > 0) {
  stop("The following traits are missing in the trait_name_mapping: ", paste(missing_traits, collapse = ", "))
}

# Map the raw trait names in gwas_data to the formatted trait names
gwas_data$Formatted_Trait <- trait_name_mapping[gwas_data$Trait]

# Ensure 'Formatted_Trait' is a factor and set the levels as they should appear in the plot
gwas_data$Formatted_Trait <- factor(gwas_data$Formatted_Trait, levels = formatted_trait_names)

# Check to ensure the number of colors matches the number of unique traits in your data
if (length(hex_colors) != length(unique(gwas_data$Formatted_Trait))) {
  stop("The number of hex colors must match the number of unique traits.")
}

# Define the labels with expressions for the traits
trait_labels <- c(expression(ECS[tAU]), expression(ECS[mAU]), expression(gH^"+"), 
                  expression(vH^"+"), expression(phi[2]), expression(phi[NPQ]), 
                  expression(phi[NO]), "qL", "NPQt", "FvP/FmP", "PS1 Active Centers", 
                  "PS1 Open Centers", "PS1 Over Reduced Centers", "PS1 Oxidized Centers", 
                  "Relative Chlorophyll")

# Calculate the cumulative positions for chromosome rectangles
chr_limits <- gwas_data %>%
  group_by(CHROM) %>%
  summarise(start = min(bp_cum), end = max(bp_cum))

# Calculate axis settings for the plot
axis_set <- gwas_data %>%
  group_by(CHROM) %>%
  summarize(center = mean(bp_cum))

# Initialize the ggplot object
p <- ggplot() +
  geom_rect(data = chr_limits, aes(xmin = start, xmax = end, ymin = 0, ymax = 0.005),
            fill = rep(c("darkgrey", "black"), length.out = nrow(chr_limits)), alpha = 0.75) +
  scale_x_continuous(breaks = axis_set$center, labels = paste0("Chr", sprintf("%02d", axis_set$CHROM))) +
  scale_y_continuous(expand = c(0, 0.02), limits = c(0, 0.5)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(size = 0.8),
        legend.position = "top",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(1, "lines"),
        legend.spacing.x = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5))


# Loop over the traits to add points to the plot
for (i in seq_along(formatted_trait_names)) {
  trait_data <- gwas_data %>%
    filter(Formatted_Trait == formatted_trait_names[i])
  if (nrow(trait_data) > 0) {
    p <- p + geom_point(data = trait_data,
                        aes(x = bp_cum, y = support, color = Formatted_Trait),
                        alpha = 0.9, size = 3)
  }
}

# Add horizontal lines for threshold levels
threshold_value <- 0.2
p <- p + geom_hline(yintercept = threshold_value, color = "black", linetype = "dashed", lwd=0.5)

# Add the color scale for 'Trait' outside the loop
p <- p + scale_color_manual(values = colors_for_traits, name = "Trait",
                            breaks = formatted_trait_names, labels = trait_labels)

# Final touches to the plot
p <- p + labs(x = "", y = "RMIP") +
  guides(color = guide_legend(title = "", nrow = 1, byrow = TRUE))

# Print the plot
print(p)
