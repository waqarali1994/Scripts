library(ggplot2)
library(rlang)
# No need to install svglite each time, just load it with library() if it's already installed
library(svglite)

multispeq_traits <- c("ECS_.mAU", "ECS_tau", "gH.", "vH.", "Phi2", "PhiNPQ", "PhiNO", "qL", "NPQt",
                      "FvP_over_FmP", "PS1_Active.Centers", "PS1_Open.Centers",
                      "PS1_Over.Reduced.Centers", "PS1_Oxidized.Centers", "Relative_Chlorophyll")

plot_list <- list()

for (trait in multispeq_traits) {
  plot <- ggplot(matched_yield_scatter, aes(x = !!sym(paste0(trait, ".x")), y = !!sym(paste0(trait, ".y")))) +
    geom_point(size = 2, alpha = 0.6, color = "black") +  # Set points to black
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +  # Line in black
    labs(
      x = paste0(trait, " (BLUPs)"), 
      y = paste0(trait, " (BLUEs)"), 
      title = paste0("Comparison of ", trait),
      subtitle = "BLUPs vs BLUEs",
      caption = "Multipeq Dataset"
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    ) +
    guides(color = guide_legend(override.aes = list(size = 4)))
  
  plot_list[[trait]] <- plot
}

# Continue with the directory creation and plot saving as you have done


# Continue with the directory creation and plot saving as you have done


# Create a directory for plots if it doesn't exist
plots_dir <- "plots"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Save each plot in the plots directory
for (i in 1:length(multispeq_traits)) {
  ggsave(
    filename = paste0(plots_dir, "/scatter_plot_", multispeq_traits[i], ".svg"),
    plot = plot_list[[multispeq_traits[i]]],
    width = 8, height = 6, units = "in"
  )
}

# Zip the plots directory
zip(zipfile = "plots.zip", files = plots_dir)
