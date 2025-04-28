corr_2021 <- adjustedblues_kerenlrownumber_2021
corr_2022 <- adjusted_blues_hipskernelrow2022

# Assuming dataset1 has underscores in sampleID and dataset2 does not.
corr_2021$Genotypes <- gsub("_", "", corr_2021$Genotypes)

corr_2021$Genotypes <- toupper(corr_2021$Genotypes)
corr_2022$Genotypes <- t oupper(corr_2022$Genotypes)

write.csv(corr_2021, "corr_2021.csv")

write.csv(corr_2022, "corr_2022.csv")

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           # Merge datasets by sampleID
merged_data <- merge(corr_2021, corr_2022, by = "Genotypes", suffixes = c("_1", "_2"))

cor_coefficient <- cor(merged_data$kernelrow_blues_1, merged_data$kernelrow_blues_2)



install.packages("ggplot2")
library(ggplot2)



ggplot(merged_data, aes(x = kernelrow_blues_1, y = kernelrow_blues_2)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add a linear regression line, without confidence interval
  labs(x = "Kernel Row Number 2021", y = "Kernel Row Number 2022", title = "Correlation between Kernel Row Number Traits") +
  geom_text(x = Inf, y = Inf, label = paste("Correlation:", round(cor_coefficient, 3)), hjust = 1.1, vjust = 2, size = 5, color = "blue") +
  theme_minimal() +  # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.background = element_blank(),  # Remove plot background
        panel.background = element_blank(),  # Remove panel background
        axis.line = element_line(color = "black"))  # Add axis lines

