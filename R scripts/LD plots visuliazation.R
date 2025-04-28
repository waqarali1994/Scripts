library(ggplot2)
install.packages("jcolors")
library(jcolors)
setwd("~/Desktop")
# Subset dataframe based on the range of BP_B values
LD_chr9_21755970=fread('ld_chr9.ld')
chr9_21755970_ld_chlorophyll<-LD_chr9_21755970
chr9_21755970_ld_chlorophyll <- subset(chr9_21755970_ld_chlorophyll, BP_B >= 21500000 & BP_B <= 22155286 , select = c("BP_B", "R2"))
# Print the resulting subset dataframe
#chr_9_ld_phi2=chr_9_ld_phi2[order(chr_9_ld_phi2$R2, decreasing = F),]
# Create a scatter plot
# Create a scatter plot
ggplot(chr9_21755970_ld_chlorophyll, aes(x = BP_B, y = R2, color = R2)) +
  geom_point() +
  geom_point(data=chr9_21755970_ld_chlorophyll[(chr9_21755970_ld_chlorophyll$BP_B==21755970),], shape = 24, size=4, fill='black')+
  geom_point(data=chr9_21755970_ld_chlorophyll[(chr9_21755970_ld_chlorophyll$BP_B==21785915),], shape = 24, size=4, fill='red')+# Add points
  scale_colour_gradientn(colours = rev(jcolors('rainbow')), name = "LD") +  # Use a reversed rainbow color gradient
  labs(x = "BP_B", y = "R2", title = "chlrophyll_ld for chr9_21755970") +
  theme_classic() +  # Use a classic theme as a base
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the plot title
    #panel.grid.major = element_line(color = "grey", size = 0.5),  # Enable solid, gray major grid lines
    #panel.grid.minor = element_line(color = "lightgrey", size = 0.25),  # Enable solid, light gray minor grid lines
    plot.background = element_rect(fill = "white", color = "white"),  # Set plot background to white
    panel.background = element_rect(fill = "white", color = "white"),  # Set panel background to white
    axis.line = element_line(color = "black")  # Add solid black axis lines
  )







# Load required libraries
library(ggplot2)
library(jcolors)
library(data.table)

# Set working directory
setwd("~/Desktop")

# Load the LD data
LD_data <- fread('LD_results.ld')

# Define the SNP positions of interest (replace with your actual SNP positions)
snp_positions <- c(169059592,112138032,6196875,131875344,116625913, 105426671, 179085156, 38849136, 141858967,
                   141821720,112611134,118029370, 177483368, 205642523, 58593266)  # Add more SNP positions here

# Define the range around the SNP positions (e.g., +/- 500 kb)
range <- 50000

# Loop through each SNP position
for (snp in snp_positions) {
  
  # Subset data for the current SNP
  subset_data <- subset(LD_data, BP_B >= (snp - range) & BP_B <= (snp + range), select = c("BP_B", "R2"))
  
  # Create the scatter plot
  plot <- ggplot(subset_data, aes(x = BP_B, y = R2, color = R2)) +
    geom_point() +
    # Highlight the focal SNP
    geom_point(data = subset_data[subset_data$BP_B == snp,], shape = 24, size = 4, fill = 'black') +
    # Use a reversed rainbow color gradient
    scale_colour_gradientn(colours = rev(jcolors('rainbow')), name = "LD") +
    labs(x = "BP_B", y = "R2", title = paste("LD for SNP", snp)) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),  # Center the plot title
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      axis.line = element_line(color = "black")
    )
  
  # Save the plot as a PNG file
  output_file <- paste0("LD_plot_SNP_", snp, ".png")
  ggsave(output_file, plot, width = 8, height = 6)
  
  # Optionally print the plot in RStudio viewer
  print(plot)
}









# Load required libraries
library(ggplot2)
library(jcolors)
library(data.table)

# Set working directory
setwd("~/Desktop")

# Load the LD data
LD_data <- fread('LD_results.ld')

# Define the SNP positions of interest (replace with your actual SNP positions)
snp_positions <- c(169059592,112138032,6196875,131875344,116625913, 105426671, 179085156, 38849136, 141858967,
                   141821720,112611134,118029370, 177483368, 205642523, 58593266)  # Add more SNP positions here

# Define the range around the SNP positions (e.g., +/- 500 kb)
range <- 1000000

# Create a folder to store the SVG files
output_folder <- "LD_Plots_PNG"
if (!dir.exists(output_folder)) dir.create(output_folder)

# Loop through each SNP position
for (snp in snp_positions) {
  
  # Subset data for the current SNP
  subset_data <- subset(LD_data, BP_B >= (snp - range) & BP_B <= (snp + range), select = c("BP_B", "R2"))
  
  # Create the scatter plot
  plot <- ggplot(subset_data, aes(x = BP_B, y = R2, color = R2)) +
    geom_point() +
    # Highlight the focal SNP
    geom_point(data = subset_data[subset_data$BP_B == snp,], shape = 24, size = 4, fill = 'black') +
    # Use a reversed rainbow color gradient
    scale_colour_gradientn(colours = rev(jcolors('rainbow')), name = "LD") +
    labs(x = "BP_B", y = "R2", title = paste("LD for SNP", snp)) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),  # Center the plot title
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      axis.line = element_line(color = "black")
    )
  
  # Save the plot as an SVG file
  output_file <- file.path(output_folder, paste0("LD_plot_SNP_", snp, ".png"))
  ggsave(output_file, plot, width = 8, height = 6, device = "png")
}

# Create a zip file containing all SVG files
zip_file <- "LD_Plots.zip"
zip(zipfile = zip_file, files = list.files(output_folder, full.names = TRUE))

# Print message
cat("All plots have been saved as SVG files and zipped into", zip_file, "\n")

df <- clean_data %>%
  rowwise() %>%
  mutate(genotype_state = str_c(str_c(Genotype_ID, Genotype, sep = '-')))
   
