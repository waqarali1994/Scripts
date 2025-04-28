library(tidyverse)
library(svglite)
library(data.table)
library(patchwork)  # Library to combine plots

setwd("~/Documents/PhD work/Multispeq project/multispeq/Waqar_MultiSpeQ")

# Read and prepare the dataset
multispq_densityplot <- fread("filtered_multispeq_cutoff.csv")
multispq_densityplot <- multispq_densityplot[-1]
if ("Genotype" %in% colnames(multispq_densityplot)) {
  multispq_densityplot[, Genotype := NULL]
}

# Define hex colors and trait names
hex_colors <- c("#1F77B4", "#d6a851", "brown", "#D62728", "#9467BD", "#8C564B",
                "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#321b16", "#FF7F0E",
                "#1444BD", "#437f85","#2CA02C") # Replace with your actual hex colors
trait_names <- c("ECS_.mAU", "ECS_tau", "gH.", "vH.", "Phi2", "PhiNPQ",
                 "PhiNO", "qL", "NPQt", "FvP_over_FmP", "PS1_Active.Centers",
                 "PS1_Open.Centers", "PS1_Over.Reduced.Centers",
                 "PS1_Oxidized.Centers", "Relative_Chlorophyll")
colors_for_traits <- setNames(hex_colors, trait_names)

# Initialize an empty list to store plots
plot_list <- list()

# Generate plots and store them in the list
for (trait in names(colors_for_traits)) {
  x_label <- switch(trait,
                    "Phi2" = expression(phi[2]),
                    "PhiNPQ" = expression(phi[NPQ]),
                    "PhiNO" = expression(phi[NO]),
                    "FvP_over_FmP" = expression("FvP/FmP"),
                    "ECS_.mAU"= expression(ECS[tAU]), 
                    "ECS_tau" = expression(ECS[mAU]), 
                    "gH." = expression(gH^"+"), 
                    "vH." = expression(vH^"+"),
                    "PS1_Active.Centers"= expression("PS1 Active Centers"), 
                    "PS1_Open.Centers" = expression("PS1 Open Centers"),
                    "PS1_Over.Reduced.Centers" = expression("PS1 Over Reduced Centers"),
                    "PS1_Oxidized.Centers" = expression("PS1 Oxidized Centers"),
                    "Relative_Chlorophyll" = expression("Relative Chlorophyll"),
                    trait)  # Default to the trait name if no special formatting is needed
  
  plot <- ggplot(multispq_densityplot, aes(x = .data[[trait]])) +
    geom_density(fill = colors_for_traits[trait], alpha = 0.7) +
    labs(x = x_label, y = "Density", title = NULL) +
    theme_classic()
  plot_list[[trait]] <- plot
}

# Combine all plots into a single panel using patchwork
combined_plot <- wrap_plots(plot_list, ncol = 3)

# Print the combined plot
print(combined_plot)

# Optionally, save the combined plot
ggsave("combined_density_plots.svg", plot = combined_plot, width = 15, height = 10, dpi = 300)