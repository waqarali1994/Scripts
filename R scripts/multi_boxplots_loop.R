# Load libraries
library(ggplot2)
library(data.table)
setwd("~/Desktop")
# Read data
expression_phenotypes=fread("expression_data_final.csv")
expression_phenotypes <- expression_phenotypes[, -1, with = FALSE]
# Correctly subsetting specific columns from a data frame
#photo_histv1 <- colnames(expression_phenotypes)
#expression_phenotypes=as.data.frame(t(expression_phenotypes))
#colnames(expression_phenotypes)=expression_phenotypes[1,]
#expression_phenotypes=expression_phenotypes[-1,]

#genotype=rownames(expression_phenotypes)
#expression_phenotypes=cbind(genotype,expression_phenotypes)
#rownames(expression_phenotypes)=NULL
#str(expression_phenotypes)
#write.csv(expression_phenotypes, "expression_data_final.csv")


# Traits list
expression_phenotypes_v1 <- colnames(expression_phenotypes)

# Initialize list to store plots
plot_list <- list()


# Assuming photo_hist is your data frame and phototraitsq1 contains trait names
for (trait in expression_phenotypes_v1) {
  plot <- ggplot(expression_phenotypes, aes(x = "", y = !!sym(trait))) +  # Use a constant for x to create a single boxplot per trait
    geom_boxplot(fill = "lightblue", color = "black") +  # Set colors for the boxplot
    labs(
      x = "",  # No x-axis label necessary as there's only one boxplot per plot
      y = trait, 
      title = paste0("Boxplot of ", trait),
      subtitle = "Distribution and Outliers",
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
plots_dir <- "expression_boxplots_cutoff"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Save each histogram
for (i in 1:length(expression_phenotypes_v1)) {
  ggsave(
    filename = paste0(plots_dir, "/expression_boxplots", expression_phenotypes_v1[i], ".png"),
    plot = plot_list[[expression_phenotypes_v1[i]]],
    width = 8, height = 6, units = "in"
  )
}

# Zip the plots directory
zip(zipfile = "expression_boxplots.zip", files = plots_dir)









summary(expression_phenotypes)
str(expression_phenotypes)
boxplot.stats(expression_phenotypes)


hist(expression_phenotypes[["29MIBZ2"]])
boxplot(expression_phenotypes[["29MIBZ2"]])

boxplot(expression_phenotypes[["29MIBZ2"]], main = "Boxplot of 29MIBZ2", ylab = "Values")


# Define manual thresholds for the trait
lower_threshold <- 0  # Replace with your determined lower threshold
upper_threshold <- 1000  # Replace with your determined upper threshold

# Remove outliers based on these thresholds
filtered_data <- expression_phenotypes[expression_phenotypes[["29MIBZ2"]]
                                       >= lower_threshold & expression_phenotypes[["29MIBZ2"]] <= upper_threshold, ]


library(ggplot2)
boxplot(filtered_data[["29MIBZ2"]], main = "Boxplot of 29MIBZ2", ylab = "Values", bins=30)
ggplot(data=expression_phenotypes, aes(x=)) hist(filtered_data[["29MIBZ2"]], bins=30)



column_name <- "29MIBZ2"  # Replace with the actual column name
boxplot_stats <- boxplot.stats(expression_phenotypes[[column_name]])
print(boxplot_stats)

