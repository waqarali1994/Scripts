# Load libraries
library(ggplot2)

setwd("~/Desktop")
# Read data
photo_hist<- read.csv("~/Desktop/yield_setcutoff_blues.csv")

str(photo_hist)

# Traits list
phototraitsq1 <- colnames(photo_hist)[-(1:2)]
str(phototraitsq1)

# Initialize list to store plots
plot_list <- list()

# Assuming photo_hist is your data frame and phototraitsq1 contains trait names
for (trait in phototraitsq1) {
  plot <- ggplot(photo_hist, aes(x = !!sym(trait))) +  # Corrected to use only x aesthetic
    geom_histogram(bins = 30, fill = "gray", color = "black") +  # Adjust fill for visibility
    labs(
      x = trait, 
      y = "Count", 
      title = paste0("Histogram of ", trait),
      subtitle = "Histogram",
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
    )
  
  plot_list[[trait]] <- plot
}


# Directory for plots
plots_dir <- "plots_histograms"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Save each histogram
for (i in 1:length(phototraitsq1)) {
  ggsave(
    filename = paste0(plots_dir, "/histogram_", phototraitsq1[i], ".png"),
    plot = plot_list[[phototraitsq1[i]]],
    width = 8, height = 6, units = "in"
  )
}

# Zip the plots directory
zip(zipfile = "histograms.zip", files = plots_dir)

boxplot.stats(yield_hist$Relative_Chlorophyll)
summary(yield_hist$Relative_Chlorophyll)

