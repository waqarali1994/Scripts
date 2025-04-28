# Load necessary libraries
library(dplyr)
library(stringr)
library(data.table)
library(SpATS)

setwd("~/Documents/PhD work/Yield project/SpATS BLUEs")
# Read the CSV file into a data frame
SpATS_yield_BLUEs <- fread("combined_2021_datav2.csv")

# Define the phenotypes
phenotypes <- c('Ear_Length',	'Ear_Width',	'Fill_CM',	'Number_Rows',	'Kernels_row',
                'ear_weight',	'plot _weight',	'Percent_moisture',	'K_weight')

# Convert columns to factors
SpATS_yield_BLUEs$Year <- as.factor(SpATS_yield_BLUEs$Year)


# Initialize the data frame to store results
spatiallyCorrectedBLUPs <- data.frame(Genotypes = unique(SpATS_yield_BLUEs$Genotypes))


# Number of knots
column_knots <- 22
row_knots <- 31

# Loop over all gene columns
for (phenotype in phenotypes) {
  # Fit the SpATS model
  model <- SpATS(
    response = phenotype, 
    genotype = 'Genotypes', 
    genotype.as.random = FALSE, 
    spatial = ~ SAP(Column, Row, nseg = c(column_knots, row_knots)),  
    random = ~ Year, 
    data = SpATS_yield_BLUEs
  )
  
  # Extract BLUEs
  blue_data <- as.data.frame(model$coeff[1:numGenotypes])
  colnames(blue_data) <- phenotype
  blue_data$Genotypes <- rownames(blue_data)
  
  # Join the extracted data with the results data frame
  spatiallyCorrectedBLUEs <- left_join(spatiallyCorrectedBLUEs, blue_data, by = "Genotypes")
  
  # Plot the fitted spatial trend
  plot.SpATS(model, main = phenotype)
}

# Save the final results to a CSV file
write.csv(spatiallyCorrectedBLUEs, "SpATS__spatially_corrected_Yield_BLUEs.csv", row.names = FALSE)


summary(model)






setwd('~/Desktop/sptas_trends/')
histogram_expression<- fread('Sorghum2021_spatially_corrected_BLUPs_final.csv')

upper_cutoff4<-15
hist(histogram_expression$Sobic.001G000200)
boxplot(histogram_expression$Sobic.001G000200)
summary(histogram_expression$Zm00001eb403750)
boxplot.stats(histogram_expression$Zm00001eb403750)
histogram_expression$Zm00001eb403750<-ifelse(histogram_expression$Zm00001eb403750<=upper_cutoff4,histogram_expression$Zm00001eb403750,NA)

upper_cutoff5<-24
hist(histogram_expression$Sobic.001G000700)
boxplot(histogram_expression$Sobic.001G000700)
boxplot.stats(histogram_expression$Zm00001eb059970)
histogram_expression$Zm00001eb059970<-ifelse(histogram_expression$Zm00001eb059970<=upper_cutoff5,histogram_expression$Zm00001eb059970,NA)

upper_cutoff3<-63
hist(histogram_expression$Sobic.001G007100)
boxplot(histogram_expression$Sobic.001G007100)
summary(histogram_expression$Zm00001eb001670)
boxplot.stats(histogram_expression$Zm00001eb001670)
histogram_expression$Zm00001eb001670<-ifelse(histogram_expression$Zm00001eb001670<=upper_cutoff3,histogram_expression$Zm00001eb001670,NA)

hist(histogram_expression$Sobic.001G007300)
boxplot(histogram_expression$Sobic.001G007300)


hist(histogram_expression$Sobic.001G007400)
boxplot(histogram_expression$Sobic.001G007400)
summary(histogram_expression$Zm00001eb037440)
boxplot.stats(histogram_expression$Zm00001eb037440)
upper_cutoff<-2.5




hist(histogram_expression$Sobic.001G468100)
boxplot(histogram_expression$Sobic.001G468100)
summary(histogram_expression$Zm00001eb037440)
boxplot.stats(histogram_expression$Zm00001eb037440)
upper_cutoff<-2.5




hist(histogram_expression$Sobic.001G529400)
boxplot(histogram_expression$Sobic.001G529400)
summary(histogram_expression$Zm00001eb037440)
boxplot.stats(histogram_expression$Zm00001eb037440)
upper_cutoff<-2.5



hist(histogram_expression$Sobic.001G529900)
boxplot(histogram_expression$Sobic.001G529400)
summary(histogram_expression$Sobic.001G529400)
boxplot.stats(histogram_expression$Sobic.001G529400)
upper_cutoff<-2.5







histogram_expression$Zm00001eb037440 <- ifelse(histogram_expression$Zm00001eb037440 <= upper_cutoff, histogram_expression$Zm00001eb037440, NA)

write.csv(histogram_expression,"TWAS_readytospatial_ananlysis.csv")










setwd("~/Documents/PhD work/Yield project/SpATS BLUEs")
Harshitha_expression_genes<- fread('combined_2021_datav3.csv')

hist(Harshitha_expression_genes$ear_weight, breaks = 40)
upper_cutoff_sr1<-1300
lower_cutoff_sr1<-
Harshitha_expression_genes$Sobic.001G000200 <- ifelse(Harshitha_expression_genes$Sobic.001G000200 >= lower_cutoff_sr1 & 
                                                     Harshitha_expression_genes$Sobic.001G000200
                                                     <= upper_cutoff_sr1, Harshitha_expression_genes$Sobic.001G000200, NA)




hist(Harshitha_expression_genes$Ear_Width)
boxplot(Harshitha_expression_genes$Ear_Width)
upper_cutoff_hr2<-250
Harshitha_expression_genes$Sobic.001G000700 <- ifelse(Harshitha_expression_genes$Sobic.001G000700
                                                     <= upper_cutoff_hr2, Harshitha_expression_genes$Sobic.001G000700, NA)



hist(Harshitha_expression_genes$Fill_CM)
boxplot(Harshitha_expression_genes$Fill_CM)
upper_cutoff_hr3<-150
Harshitha_expression_genes$Zm00001eb000080 <- ifelse(Harshitha_expression_genes$Zm00001eb000080 
                                                     <= upper_cutoff_hr3, Harshitha_expression_genes$Zm00001eb000080, NA)


hist(Harshitha_expression_genes$Number_Rows)
boxplot(Harshitha_expression_genes$Number_Rows)
upper_cutoff_hr4<-10
Harshitha_expression_genes$Zm00001eb000020 <- ifelse(Harshitha_expression_genes$Zm00001eb000020 
                                                     <= upper_cutoff_hr4, Harshitha_expression_genes$Zm00001eb000020, NA)



hist(Harshitha_expression_genes$Kernels_row)
boxplot(Harshitha_expression_genes$Kernels_row)
upper_cutoff_hr4<-10
Harshitha_expression_genes$Zm00001eb000020 <- ifelse(Harshitha_expression_genes$Zm00001eb000020 
                                                     <= upper_cutoff_hr4, Harshitha_expression_genes$Zm00001eb000020, NA)



hist(Harshitha_expression_genes$ear_weight)
boxplot(Harshitha_expression_genes$ear_weight)
upper_cutoff_hr4<-10
Harshitha_expression_genes$Zm00001eb000020 <- ifelse(Harshitha_expression_genes$Zm00001eb000020 
                                                     <= upper_cutoff_hr4, Harshitha_expression_genes$Zm00001eb000020, NA)



hist(Harshitha_expression_genes$plot_weight)
boxplot(Harshitha_expression_genes$plot_weight)
upper_cutoff_hr4<-75
Harshitha_expression_genes$Sobic.001G529400 <- ifelse(Harshitha_expression_genes$Sobic.001G529400 
                                                     <= upper_cutoff_hr4, Harshitha_expression_genes$Sobic.001G529400, NA)



hist(Harshitha_expression_genes$Percent_moisture)
boxplot(Harshitha_expression_genes$Sobic.001G529900)
upper_cutoff_hr4<-10
Harshitha_expression_genes$Zm00001eb000020 <- ifelse(Harshitha_expression_genes$Zm00001eb000020 
                                                     <= upper_cutoff_hr4, Harshitha_expression_genes$Zm00001eb000020, NA)




hist(Harshitha_expression_genes$K_weight)
boxplot(Harshitha_expression_genes$K_weight)
upper_cutoff_hr4<-15
Harshitha_expression_genes$Sobic.001G530000 <- ifelse(Harshitha_expression_genes$Sobic.001G530000 
                                                     <= upper_cutoff_hr4, Harshitha_expression_genes$Sobic.001G530000, NA)


hist(Harshitha_expression_genes$Sobic.002G002200)
boxplot(Harshitha_expression_genes$Sobic.002G002200)
upper_cutoff_hr4<-10
Harshitha_expression_genes$Zm00001eb000020 <- ifelse(Harshitha_expression_genes$Zm00001eb000020 
                                                     <= upper_cutoff_hr4, Harshitha_expression_genes$Zm00001eb000020, NA)




write.csv(Harshitha_expression_genes,"SAP2021_10genes_spatiallyready_file.csv")









setwd("~/Documents/PhD work/Yield project/SpATS BLUEs")

# Read the CSV file into a data frame
SpATS_yield_BLUEs <- fread("combined_2021_datav2.csv")

View(SpATS_yield_BLUEs)
# Define the phenotypes
phenotypes <- c('Ear_Length', 'Ear_Width', 'Fill_CM', 'Number_Rows', 'Kernels_row',
                'ear_weight', 'plot_weight', 'Percent_moisture', 'K_weight')

# Convert columns to factors
SpATS_yield_BLUEs$Year <- as.factor(SpATS_yield_BLUEs$Year)

# Initialize the data frame to store results
spatiallyCorrectedBLUEs <- data.frame(Genotypes = unique(SpATS_yield_BLUEs$Genotypes))

# Number of knots
column_knots <- 22
row_knots <- 31

# Loop over all gene columns
for (phenotype in phenotypes) {
  # Fit the SpATS model
  model <- SpATS(
    response = phenotype, 
    genotype = 'Genotypes', 
    genotype.as.random = FALSE, 
    spatial = ~ SAP(Column, Row, nseg = c(column_knots, row_knots)),  
    random = ~ Year, 
    data = SpATS_yield_BLUEs
  )
  
  # Extract BLUEs
  blue_data <- as.data.frame(model$coeff[1:length(unique(SpATS_yield_BLUEs$Genotypes))])
  colnames(blue_data) <- phenotype
  blue_data$Genotypes <- rownames(blue_data)
  
  # Join the extracted data with the results data frame
  spatiallyCorrectedBLUEs <- left_join(spatiallyCorrectedBLUEs, blue_data, by = "Genotypes")
  
  # Plot the fitted spatial trend
  plot.SpATS(model, main = phenotype)
}

# Save the results to a CSV file
write.csv(spatiallyCorrectedBLUEs, "spatiallyCorrected_Yield_BLUEs.csv", row.names = FALSE)

