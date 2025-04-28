#Correlation matrix final plot

library(ggplot2)
library(dplyr)
library(reshape2)
# Load and clean the dataset
multispeq_paper_coreelation <- read.csv("filtered_multispeq_cutoff.csv")
multispeq_paper_coreelation <- multispeq_paper_coreelation[-1]  # Assuming first column is to be removed

# Remove any non-numeric columns which are not suitable for correlation
df <- multispeq_paper_coreelation[, sapply(multispeq_paper_coreelation, is.numeric)]
# Calculate the correlation matrix
cor_matrix <- cor(df, use = "pairwise.complete.obs")
# Fill the p-value matrix by looping over pairs of variables
variable_names <- names(df)
p_matrix <- matrix(NA, nrow = length(variable_names), ncol = length(variable_names),
                   dimnames = list(variable_names, variable_names))
for (i in 1:(length(variable_names) - 1)) {
  for (j in (i + 1):length(variable_names)) {
    test_result <- cor.test(df[[variable_names[i]]], df[[variable_names[j]]], method = "pearson")
    p_matrix[i, j] <- test_result$p.value
    p_matrix[j, i] <- test_result$p.value  # Symmetric filling
  }
}
# Create a significance matrix
significance_matrix <- p_matrix < 0.05
# Melt the correlation and significance matrices
cor_melted <- melt(cor_matrix)
sig_melted <- melt(significance_matrix)
# Combine the melted data
cor_melted$significance <- sig_melted$value
# Generate a key from the row and column names of cor_matrix to determine positioning
names_list <- rownames(cor_matrix)
name_positions <- setNames(seq_along(names_list), names_list)
cor_melted$triangle <- ifelse(name_positions[cor_melted$Var1] < name_positions[cor_melted$Var2], "upper",
                              ifelse(name_positions[cor_melted$Var1] > name_positions[cor_melted$Var2], "lower", "diag"))


# ...
# Plotting setup
p <- ggplot(data = cor_melted, aes(x = Var1, y = Var2)) +
  geom_tile(data = subset(cor_melted, triangle == "diag"), fill = NA, color = NA) +  # Removing display for the diagonal
  # Upper triangle significant correlations with circle size representing the magnitude
  geom_point(data = subset(cor_melted, triangle == "upper" & significance),
             aes(size = abs(value), fill = value), shape = 21, stroke = 0) +
  # Lower triangle significant correlations with text color representing the correlation strength
  geom_text(data = subset(cor_melted, triangle == "lower" & significance),
            aes(label = sprintf("%.2f", value), color = value), size = 5) +
  geom_text(data = subset(cor_melted, triangle == "upper" & !significance), aes(label = "X"), color = "black", size = 5) +
  geom_text(data = subset(cor_melted, triangle == "lower" & !significance), aes(label = "X"), color = "black", size = 5) +
  scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  scale_color_gradient2(low = "blue", mid = "lightgrey", high = "red", midpoint = 0, limit = c(-1, 1), space = "Lab", guide = "none") +  # Disable the guide for color gradient
  scale_size_continuous(range = c(3, 12), guide = "none") +  # Adjust the size range and remove legend for size
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.margin = unit(1, "mm"),
        axis.line = element_line(color = "black"))
p <- p +
  scale_x_discrete(labels = c(expression(ECS[mAU]), expression(ECS[tAU]), expression(gH^"+"),
                              expression(vH^"+"), expression(phi[2]), expression(phi[NPQ]),
                              expression(phi[NO]), "qL", "NPQt", "FvP/FmP", "PS1 Active Centers",
                              "PS1 Open Centers", "PS1 Over Reduced Centers",
                              "PS1 Oxidized Centers", "Relative Chlorophyll")) +
  scale_y_discrete(labels = c(expression(ECS[mAU]), expression(ECS[tAU]), expression(gH^"+"),
                              expression(vH^"+"), expression(phi[2]), expression(phi[NPQ]),
                              expression(phi[NO]), "qL", "NPQt", "FvP/FmP", "PS1 Active Centers",
                              "PS1 Open Centers", "PS1 Over Reduced Centers",
                              "PS1 Oxidized Centers", "Relative Chlorophyll"))
# Print the plot
print(p)