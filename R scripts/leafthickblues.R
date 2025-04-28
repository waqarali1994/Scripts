Q1 <- quantile(leaf_thickness$Thickness, 0.25, na.rm = TRUE)
Q3 <- quantile(leaf_thickness$Thickness, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Compute the bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter the data frame to include only rows within the bounds
data_no_outliers <- leaf_thickness[leaf_thickness$Thickness >= lower_bound & leaf_thickness$Thickness <= upper_bound,]
boxplot(data_no_outliers$Thickness)
hist(data_no_outliers$Thickness)
summary(data_no_outliers)

# Fit a mixed-effects model
model <- lmer(Thickness ~ Genotype + (1|Person)+(1|time)++(1|biol.reps), data = data_no_outliers)
bluesfix<- fixef(model)
intercept <- bluesfix[1]

# Add the intercept to the genotypic effects (assuming genotypic effects start from the 2nd element)
bluesfixfinal <- bluesfix[-1] + intercept
print(bluesfixfinal)


# Convert the vector to a data frame
bluesfixthick <- data.frame(blues = bluesfixfinal)

# Write the data frame to a CSV file
write.csv(bluesfixthick, file = "thickblues.csv", row.names = TRUE)