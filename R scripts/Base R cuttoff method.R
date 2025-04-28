
# Load data
data <- read.csv("~/Documents/PhD work/Multispeq project/RMIP GWAS Multispeq/LD_multispeqq/Nebraska2020MultiSpeQPostFlowering.csv")



# Define cutoffs
lower_cutoff <- -0.00150  # Adjusted to remove negative values
upper_cutoff <- 0.00300  # Slightly broader than the upper whisker to retain more data
#ECS.tAU
lower_cutoff <- 0.0004  # Adjusted to remove negative values
upper_cutoff <- 0.0190  # Slightly broader than the upper whisker to retain more data
#gH.
lower_cutoff <- 22.0210  # Adjusted to remove negative values
upper_cutoff <- 400  # Slightly broader than the upper whisker to retain more data
#vH.
lower_cutoff <- -0.0355  # Adjusted to remove negative values
upper_cutoff <- 0.3000  # Slightly broader than the upper whisker to retain more data
#PhiN0
lower_cutoff <- 0.080  # Adjusted to remove negative values
upper_cutoff <- 0.300  # Slightly broader than the upper whisker to retain more data
#qL
lower_cutoff <- 0.200  # Adjusted to remove negative values
upper_cutoff <- 0.900  # Slightly broader than the upper whisker to retain more data
#NPQt
lower_cutoff <- 0.2000  # Adjusted to remove negative values
upper_cutoff <- 10  # Slightly broader than the upper whisker to retain more data
#Fvp/Fmp
lower_cutoff <- 0.300  # Adjusted to remove negative values
upper_cutoff <- 0.955  # Slightly broader than the upper whisker to retain more data
#Active centers
lower_cutoff <- -0.7000  # Adjusted to remove negative values
upper_cutoff <- 9  # Slightly broader than the upper whisker to retain more data
#open centers
lower_cutoff <- -0.5000  # Adjusted to remove negative values
upper_cutoff <- 2  # Slightly broader than the upper whisker to retain more data
#over reduced centers
lower_cutoff <- -0.9000 # Adjusted to remove negative values
upper_cutoff <- 2  # Slightly broader than the upper whisker to retain more data
#oxidized center
lower_cutoff <- -0.9000 # Adjusted to remove negative values
upper_cutoff <- 2 # Slightly broader than the upper whisker to retain more data
#Cholorophyll
lower_cutoff <- 38 # Adjusted to remove negative values
upper_cutoff <- 90 # Slightly broader than the upper whisker to retain more data










# Apply cutoffs
data$ECS_.mAU <- ifelse(data$ECS_.mAU >= lower_cutoff & data$ECS_.mAU <= upper_cutoff, data$ECS_.mAU, NA)
data$ECS_tau <- ifelse(data$ECS_tau >= lower_cutoff & data$ECS_tau <= upper_cutoff, data$ECS_tau, NA)
data$gH. <- ifelse(data$gH. >= lower_cutoff & data$gH. <= upper_cutoff, data$gH., NA)
data$vH. <- ifelse(data$vH. >= lower_cutoff & data$vH. <= upper_cutoff, data$vH., NA)
data$PhiNO <- ifelse(data$PhiNO >= lower_cutoff & data$PhiNO <= upper_cutoff, data$PhiNO, NA)
data$qL <- ifelse(data$qL >= lower_cutoff & data$qL <= upper_cutoff, data$qL, NA)
data$NPQt <- ifelse(data$NPQt >= lower_cutoff & data$NPQt <= upper_cutoff, data$NPQt, NA)
data$FvP_over_FmP <- ifelse(data$FvP_over_FmP >= lower_cutoff & data$FvP_over_FmP <= upper_cutoff, data$FvP_over_FmP, NA)
data$PS1_Active.Centers <- ifelse(data$PS1_Active.Centers >= lower_cutoff & data$PS1_Active.Centers <= upper_cutoff, data$PS1_Active.Centers, NA)
data$PS1_Open.Centers <- ifelse(data$PS1_Open.Centers>= lower_cutoff & data$PS1_Open.Centers <= upper_cutoff, data$PS1_Open.Centers, NA)
data$PS1_Over.Reduced.Centers <- ifelse(data$PS1_Over.Reduced.Centers>= lower_cutoff & data$PS1_Over.Reduced.Centers <= upper_cutoff, data$PS1_Over.Reduced.Centers, NA)
data$PS1_Oxidized.Centers <- ifelse(data$PS1_Oxidized.Centers>= lower_cutoff & data$PS1_Oxidized.Centers <= upper_cutoff, data$PS1_Oxidized.Centers, NA)
data$Relative_Chlorophyll <- ifelse(data$Relative_Chlorophyll>= lower_cutoff & data$Relative_Chlorophyll<= upper_cutoff, data$Relative_Chlorophyll, NA)






hist(data$vH.)
boxplot(data$vH.)
boxplot.stats(data$vH.)

# Optionally, check the results with a summary
summary(data$ECS_.mAU)
boxplot.stats(data$ECS_tau)
view(data)




# Loop through each column in the data frame
for (col in names(data)) {
  # Check if the column is of logical type
  if (is.logical(data[[col]])) {
    # Convert logical to numeric (TRUE to 1 and FALSE to 0)
    data[[col]] <- as.numeric(data[[col]])
  }
}

# Optionally, print the structure of the data frame to confirm changes
str(data)


view(data)
# Print and save the filtered data
print(data)
write.csv(data, "~/Documents/PhD work/Multispeq project/RMIP GWAS Multispeq/LD_multispeqq/extremevalueremoved.csv")
