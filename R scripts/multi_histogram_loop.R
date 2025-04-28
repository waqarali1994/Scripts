# Load libraries
library(ggplot2)
library(data.table)
setwd("~/Desktop")
# Read data
expression_phenotypes=fread("merged_gene_tpms_longestT.csv")
expression_phenotypes <- expression_phenotypes[, -1, with = FALSE]
# Correctly subsetting specific columns from a data frame
#photo_histv1 <- colnames(expression_phenotypes)

str(expression_phenotypes)

# Traits list
expression_phenotypes_v1 <- colnames(expression_phenotypes)

# Initialize list to store plots
plot_list <- list()

# Assuming photo_hist is your data frame and phototraitsq1 contains trait names
for (trait in expression_phenotypes_v1) {
  plot <- ggplot(expression_phenotypes, aes(x = !!sym(trait))) +  # Corrected to use only x aesthetic
    geom_histogram(bins = 30, fill = "gray", color = "black") +  # Adjust fill for visibility
    labs(
      x = trait, 
      y = "Count", 
      title = paste0("Histogram of ", trait),
      subtitle = "Histogram",
      caption = "Expression Dataset"
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
    )
  
  plot_list[[trait]] <- plot
}


# Directory for plots
plots_dir <- "plots_expression_histograms"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Save each histogram
for (i in 1:length(expression_phenotypes_v1)) {
  ggsave(
    filename = paste0(plots_dir, "/histogram_", expression_phenotypes_v1[i], ".png"),
    plot = plot_list[[expression_phenotypes_v1[i]]],
    width = 8, height = 6, units = "in"
  )
}
# Zip the plots directory
zip(zipfile = "expression_data_histoplots.zip", files = plots_dir)