# Load necessary libraries
library(stringr)
library(dplyr)
library(ggplot2)
library(wesanderson)

# Set working directory
setwd("~/Documents/PhD work/Yield project/Effectivesnp adjusted RMIP yield GWAS")

# Define a common theme with increased tick length and legend text size
common_theme <- theme_classic() +
  theme(
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title = element_text(size = 22, color = "black"),
    axis.line = element_line(size = 1.4, color = "black"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.ticks = element_line(size = 1.2),
    legend.position = "top",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(1, "lines"),
    legend.spacing.x = unit(0.5, "cm"),
    legend.text = element_text(size = 22),
    plot.title = element_text(hjust = 0.5, color = "black")
  )

# Load data
df <- read.csv("yield_adjustedsnp_data.csv")
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

# Define the raw and formatted trait names in the correct order for the specified traits
raw_trait_names <- c("Ear Length", "Ear Weight", "Ear Width", "Kernel Row Number", 
                     "Kernel Weight", "Kernels per row", "Plot Weight")
formatted_trait_names <- c("Ear Length", "Ear Weight", "Ear Width", 
                           "Kernel Row Number", "Kernel Weight", "Kernels per row", "Plot Weight")

# Create a named vector for color mapping
hex_colors <- c("#1444BD", "#17BECF", "#321b16", "#8625B6",
                "#BCBD22", "#FF7F0E","#2CA02C")
colors_for_traits <- setNames(hex_colors, formatted_trait_names)

# Create a mapping from raw to formatted trait names
trait_name_mapping <- setNames(formatted_trait_names, raw_trait_names)

# Map the raw trait names in gwas_data to the formatted trait names
gwas_data$Formatted_Trait <- trait_name_mapping[gwas_data$Trait]

# Ensure 'Formatted_Trait' is a factor and set the levels as they should appear in the plot
gwas_data$Formatted_Trait <- factor(gwas_data$Formatted_Trait, levels = formatted_trait_names)

# Calculate the cumulative positions for chromosome rectangles
chr_limits <- gwas_data %>%
  group_by(CHROM) %>%
  summarise(start = min(bp_cum), end = max(bp_cum))

# Calculate axis settings for the plot
axis_set <- gwas_data %>%
  group_by(CHROM) %>%
  summarize(center = mean(bp_cum))

# Add narrow strips above the x-axis line to distinguish chromosomes, with no legend
p <- ggplot() +
  geom_rect(data = chr_limits, 
            aes(xmin = start, xmax = end, ymin = -0.02, ymax = -0.01,  # Set y-axis limits for the strip
                fill = as.factor(CHROM %% 2)),  # Alternate fill based on chromosome number
            alpha = 0.6, show.legend = FALSE) +  # Remove legend for strips
  scale_fill_manual(values = c("black", "grey")) +  # Black and grey alternating colors
  scale_x_continuous(breaks = axis_set$center, labels = paste0("Chr", sprintf("%02d", axis_set$CHROM))) +
  scale_y_continuous(expand = c(0, 0.02), limits = c(-0.02, 0.7)) +
  common_theme
  theme(legend.position = "none")  # Remove fill legend for rectangles

# Loop over the traits to add points to the plot
for (i in seq_along(formatted_trait_names)) {
  trait_data <- gwas_data %>%
    filter(Formatted_Trait == formatted_trait_names[i])
  if (nrow(trait_data) > 0) {
    p <- p + geom_point(data = trait_data,
                        aes(x = bp_cum, y = support, color = Formatted_Trait),
                        alpha = 0.9, size = 4)
  }
}

# Add horizontal lines for threshold levels
threshold_value <- 0.2
p <- p + geom_hline(yintercept = threshold_value, color = "black", linetype = "dashed", lwd = 0.5)

# Add the color scale for 'Trait' outside the loop
p <- p + scale_color_manual(values = colors_for_traits, name = "Trait")

# Final touches to the plot
p <- p + 
  labs(x = "", y = "RMIP") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  common_theme

# Print the plot
print(p)


