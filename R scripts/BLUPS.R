library(readxl)
library(openxlsx)
install.packages("openxlsx")
write.xlsx("mergedDf_Blues")
write.xlsx(mergedDf_Blues, "mergedDf_Blues.xlsx")

hist(combined_20_21_v1$Ear_Length)
hist(combined_20_21_v1$Ear_Width)
hist(combined_20_21_v1$Fill_cm)
hist(combined_20_21_v1$Kernels_row)
hist(combined_20_21_v1$Number_rows)
hist(combined_20_21_v1$Ear_weight)
hist(combined_20_21_v1$Plot_weight)
hist(combined_20_21_v1$Percent_moisture)
hist(combined_20_21_v1$Weight)

boxplot(combined_20_21_v1$Percent_moisture)
boxplot(combined_20_21_v1$Weight)
boxplot(combined_20_21_v1$Plot_weight)
boxplot(combined_20_21_v1$Ear_weight)
boxplot(combined_20_21_v1$Number_rows)
boxplot(combined_20_21_v1$Kernels_row)
boxplot(combined_20_21_v1$Fill_cm)
boxplot(combined_20_21_v1$Ear_Width)
boxplot(combined_20_21_v1$Ear_Length)



mergedDf_Blues$Row <- ifelse(mergedDf_Blues$Year == 2020, paste0(mergedDf_Blues$Row, "A"), 
                 ifelse(mergedDf_Blues$Year == 2021, paste0(mergedDf_Blues$Row, "B"), mergedDf_Blues$Row))


mergedDf_Blues$Column <- ifelse(mergedDf_Blues$Year == 2020, paste0(mergedDf_Blues$Column, "C"), 
                             ifelse(mergedDf_Blues$Year == 2021, paste0(mergedDf_Blues$Column, "D"), mergedDf_Blues$Column))

mergedDf_Blues$Replication <- ifelse(mergedDf_Blues$Year == 2020, paste0(mergedDf_Blues$Replication, "E"), 
                             ifelse(mergedDf_Blues$Year == 2021, paste0(mergedDf_Blues$Replication, "F"), mergedDf_Blues$Replication))

library(lme4)

model_blues <- lmer(Ear_Length ~ + (1|Row), data = combined_2021_datav2)

model_blues2 <- lmer(Ear_Length ~ Genotypes + (1|Row), data = combined_2021_datav2)

model_blues4 <- lmer(Ear_Length ~ Genotypes + (1|Row) + (1|Year), data = combined_2021_datav2)

model_blues3 <- lmer(Ear_Length ~ Genotypes + (1|Row) + (1|Column), data = combined_2021_datav2)

model_blues5 <- lmer(Ear_Length ~ Genotypes + (1|Row) + (1|Year) + (1|Column), data = combined_2021_datav2)

model_blues6 <- lmer(Ear_Length ~ Genotypes + (1|Replicate), data = combined_2021_datav2)

anova(model_blues, model_blues2, model_blues4, model_blues5, model_blues6)

VarCorr(model_blues6)

# Changing column name from "oldName" to "newName" in df
colnames(combined_2021_datav2)[colnames(combined_2021_datav2) == "plot _weight"] <- "plot_weight"


# BLUEs Calculation

Ear_length_blues <- lmer(Ear_Length ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Ear_width_blues <- lmer(Ear_Width ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Fill_cm_blues <- lmer(Fill_CM ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

kernels_row_blues <- lmer(Kernels_row ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Number_rows_blues <- lmer(Number_Rows ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Ear_weight_blues <- lmer(ear_weight ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Plot_weight_blues <- lmer(plot_weight ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

Percent_moisture_blues <- lmer(Percent_moisture ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)

k_blues <- lmer(K_weight ~ Genotypes + (1|Row) + (1|Column) + (1|Year), data = combined_2021_datav2)



Ear_length_blues_view <- fixef(Ear_length_blues)

Ear_width_blues_view <- fixef(Ear_width_blues)

kernels_row_blues_view <- fixef(kernels_row_blues)

Number_rows_blues_view <- fixef(Number_rows_blues)

Fill_cm_blues_view <- fixef(Fill_cm_blues)

Ear_weight_blues_view <- fixef(Ear_weight_blues)

Plot_weight_blues_view <- fixef(Plot_weight_blues)

Percent_moisture_blues_view <- fixef(Percent_moisture_blues)

k_blues_view <- fixef(k_blues)



model_intercept_Ear_length <- fixef(Ear_length_blues)[1]

model_intercept_Ear_width <- fixef(Ear_width_blues)[1]

model_intercept_Fill_cm <- fixef(Fill_cm_blues)[1]

model_intercept_Kernels_row <- fixef(kernels_row_blues)[1]

model_intercept_Number_rows <- fixef(Number_rows_blues)[1]

model_intercept_Ear_weight <- fixef(Ear_weight_blues)[1]

model_intercept_Plot_weight <- fixef(Plot_weight_blues)[1]

model_intercept_Percent_moisture <- fixef(Percent_moisture_blues)[1]

model_intercept_k <- fixef(k_blues)[1]


adjusted_blues_Ear_length <- Ear_length_blues_view + model_intercept_Ear_length

adjusted_blues_Ear_width <- Ear_width_blues_view + model_intercept_Ear_width

adjusted_blues_Fill_cm <- Fill_cm_blues_view + model_intercept_Fill_cm

adjusted_blues_kernels_row <- kernels_row_blues_view + model_intercept_Kernels_row

adjusted_blues_Number_rows <- Number_rows_blues_view + model_intercept_Number_rows

adjusted_blues_Ear_weight <- Ear_weight_blues_view + model_intercept_Ear_weight

adjusted_blues_Plot_weight <- Plot_weight_blues_view + model_intercept_Plot_weight

adjusted_blues_Percent_moisture <- Percent_moisture_blues_view  + model_intercept_Percent_moisture

adjusted_blues_k <- k_blues_view + model_intercept_k


write.csv(adjusted_blues_Ear_length, "adjusted_blues_Ear_length.csv")

write.csv(adjusted_blues_Ear_width, "adjusted_blues_Ear_width.csv")

write.csv(adjusted_blues_Fill_cm, "adjusted_blues_Fill_cm.csv")

write.csv(adjusted_blues_kernels_row, "adjusted_blues_kernels_row.csv")

write.csv(adjusted_blues_Number_rows, "adjusted_blues_Number_rows.csv")

write.csv(adjusted_blues_Ear_weight, "adjusted_blues_Ear_weight.csv")

write.csv(adjusted_blues_Plot_weight, "adjusted_blues_Plot_weight.csv")

write.csv(adjusted_blues_Percent_moisture, "adjusted_blues_Percent_moisture.csv")

write.csv(adjusted_blues_k, "adjusted_blues_k.csv")






library(tidyverse)
library(SpATS)
# Returns a data frame filtered to rows of data that have values for trait 
# that are less than quantile 1 - 1.5*iqr or greater than quantile 3 + 1.5*iqr
# data is a data frame
# trait is a string with the name of the column of the trait of interest
idOutliers <- function(data, trait)
{
  df <- data %>% 
    mutate(trait = .data[[trait]])
  q1 <- quantile(df$trait, probs = 0.25, na.rm = TRUE)
  q3 <- quantile(df$trait, probs = 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lowCutoff <- q1 - (1.5*iqr)
  highCutoff <- q3 + (1.5*iqr)
  
  df_filt <- filter(df, trait<lowCutoff|trait>highCutoff) %>%
    select(!trait)
  return(df_filt)
}

# Returns a pivoted data frame with a column for each observation of each phenotype of a genotype within a treatment 
# and location
# It also creates a scatter plot and prints the correlation value
# data is a dataframe
# treatmentVar is a string name of the column for the treatment
# genotype is a string name of the genotype column
# phenotypes is a string vector of the phenotypes to plot 
# facet is a string name of the column to create plot facets by - I usually use location for this
plotRepCorr <- function(data, treatmentVar, genotype, phenotypes, facet)
{
  df.wide <- data %>%
    group_by(.data[[genotype]], .data[[treatmentVar]], .data[[facet]]) %>%
    mutate(rep = 1:n()) %>%
    ungroup() %>%
    pivot_longer(all_of(phenotypes), names_to = 'var', values_to = 'val') %>%
    select(c(all_of(genotype), all_of(treatmentVar), all_of(facet), rep, var, val)) %>%
    pivot_wider(id_cols = c(.data[[genotype]], .data[[treatmentVar]], .data[[facet]]), names_from = c(var, rep), values_from = val, names_sep = '.')
  
  for(i in phenotypes)
  {
    rep1 <- paste0(i, '.1')
    rep2 <- paste0(i, '.2')
    print(i)
    print(cor(df.wide[[rep1]], df.wide[[rep2]], use = 'complete.obs'))
    
    p <- ggplot(df.wide, aes(.data[[rep1]], .data[[rep2]], color = .data[[treatmentVar]])) + 
      geom_point() + 
      facet_wrap(vars(all_of(.data[[facet]])))
    print(p)
  }
  return(df.wide)
}

# Correlation plot for 2 variables
# data is a data frame
# x is the first variable
# y is the second variable
plotVarCorr <- function(data, x, y)
{
  x.str <- deparse(substitute(x))
  y.str <- deparse(substitute(y))
  p <- ggplot({{data}}, aes({{x}}, {{y}}, color = nitrogenTreatment)) + 
    geom_point() +
    facet_wrap(vars(location)) +
    labs(subtitle = str_c('R = ', cor(data[[x.str]], data[[y.str]], use = 'complete.obs')))
  print(p)
}

# ***Specific to HIPS data** Modify as needed. 
# Returns a dataframe with a column where the values are the fitted values after spatial correction using SpATS
# Fits plot identifier as the genotype so we get values for every plot. 
# data is a dataframe
# response is a string name of the phenotype column to spatially adjust
getSpatialCorrections <- function(data, response)
{
  # Declare empty df and levels of locations
  df.sp <- tibble(location = NULL, plotNumber = NULL, '{response}':= NULL, nitrogenTreatment = NULL)
  locations <-  c('Missouri Valley', 'Lincoln', 'Scottsbluff', 'North Platte1', 'North Platte2', 'North Platte3', 'Ames', 'Crawfordsville')
  # Loop over locationations
  for(currlocation in locations)
  {
    location.df <- filter(data, location==currlocation & !is.na(row) & !is.na(range) & !is.na(.data[[response]] & !is.na(nitrogenTreatment)))
    if(length(location.df$plotNumber)==0)
    {
      print(paste0('No data for ', response, ' at ', currlocation))
      next
    }
    nitrogenTreatments <- unique(location.df$nitrogenTreatment)
    
    # Loop over nitrogen treatments
    for(currTrt in nitrogenTreatments)
    {
      if(is.na(currTrt)|currTrt=='Border')
      {
        next
      }
      location.n.df <- filter(location.df, nitrogenTreatment==currTrt) %>%
        mutate(as.factor(plotNumber))
      rangeKnots <- floor(max(location.n.df$range, na.rm = TRUE)/2) + 1
      rowKnots <- floor(max(location.n.df$row, na.rm = TRUE)/2) + 1
      print(currlocation)
      print(currTrt)
      model <- SpATS(response, genotype = 'plotNumber', genotype.as.random = TRUE, spatial = ~ SAP(range, row, nseg = c(rangeKnots, rowKnots)), data = location.n.df)
      # Plot model
      # plot.SpATS(model, main = paste0(response, ':', currlocation, ':', currTrt))
      # Extract BLUPS
      summary <- summary(model)
      if(cor(location.n.df[[response]], summary$fitted + summary$residuals) > 0.99)
      {
        sp <- tibble(location = currlocation,
                     nitrogenTreatment = currTrt, 
                     plotNumber = location.n.df$plotNumber,
                     '{response}':=summary$fitted)
      }
      else
      {
        print(paste0('Fitted values misordered. r =', cor(location.n.df[[response]], summary$fitted + summary$residuals), '; ', currlocation, '; ', currTrt))
        next
      }
      # Bind to df
      df.sp <- bind_rows(df.sp, sp) %>%
        mutate(plotNumber = as.numeric(plotNumber))
    }
  }
  print(length(df.sp$plotNumber))
  # Return df
  return(df.sp)
}

# Function to run variance partitioning
# ***Specific to HIPS data** Modify as needed. 
# Returns data frame with variance components
# df is the data frame
# response is the name of the response variable column
# label is a string to label the response on a plot
partitionVariance2 <- function(df, response, label) 
{
  df <- filter(df, !is.na(response))
  lm_formula <- as.formula(paste(response, "~ (1|location/nitrogenTreatment) + (1|genotype) + (1|location:genotype) + (1|nitrogenTreatment:genotype)"))
  model <- lmer(lm_formula, data = df, na.action = na.omit)
  vc <- as.data.frame(VarCorr(model), row.names = TRUE, order = 'cov.last', comp = 'Variance') %>%
    as_tibble() %>%
    mutate(responseVar = response)
  totalVar <- sum(vc$vcov)
  vc <- vc %>%
    rowwise() %>%
    mutate(pctVar = vcov/totalVar*100, 
           label = label) %>%
    select(responseVar, grp, vcov, pctVar, label)
  return(vc)
}

# Least-squares estimate of FW linear plasticity
# From BQTP textbook pg 190
# ***Specific to HIPS data** Modify as needed. 
# Returns data frame with the linear plasticity
# data is a data frame
# trait is the response phenotype as a string
# envs is the column identifying the environments as a string
estimatePlasticity <- function(data, trait, envs)
{
  mu <- mean(data[[trait]], na.rm = TRUE)
  df <- data %>%
    group_by(.data[[envs]]) %>%
    mutate(traitMean = mean(.data[[trait]], na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(genotype, traitMean) %>%
    group_by(genotype)
  denominator <- sum((unique(df$traitMean) - mu)^2, na.rm = TRUE)
  df <- df %>%
    summarise('{trait}':= sum(.data[[trait]] * (traitMean - mu), na.rm = TRUE)/denominator)
  return(df)
}

# Least-squares estimate of FW linear plasticity across nitrogen treatments within a location
# From BQTP textbook pg 190
# ***Specific to HIPS data** Modify as needed. 
# Returns data frame with the linear plasticity
# data is a data frame
# response is the response phenotype as a string
# location is the column identifying the locations
getNitrogenPlasticityByLocation <- function(data, response, locations)
{
  print(response)
  response.out <- response %>% 
    str_replace('.sp', '.pl')
  response.df <- tibble(location = NULL, genotype = NULL, '{response.out}':= NULL,)
  for (currlocation in locations)
  {
    location.df <- filter(data, !is.na(genotype) & location==currlocation & nitrogenTreatment!='Border' & !is.na(nitrogenTreatment)) %>%
      group_by(genotype, nitrogenTreatment) %>%
      summarise('{response}' := mean(.data[[response]], na.rm = TRUE))
    # fw <- FW(y = location.df[[response]], VAR = location.df$genotype, ENV = location.df$nitrogenTreatment, saveAt = paste0('analysis/gibbs-samples-', response, '-', currlocation),
    #          nIter = 51000, burnIn = 1000, thin = 10, seed = 3425656, saveVAR = c(1:2), saveENV = c(1:2))
    # pl <- fw$b %>%
    #   as_tibble(rownames = 'genotype') %>%
    #   mutate(location = currlocation, 
    #          '{response.out}':= Init1) %>%
    #   select(!Init1)
    pl <- estimatePlasticity(location.df, response, 'nitrogenTreatment') %>%
      mutate(location = currlocation)
    response.df <- bind_rows(response.df, pl)
  }
  return(response.df)
}

# First, a function to calculate GDDs for a single day in fahrenheit
# Returns GDD value for the given day
# minTemp is the daily minimum temperature in Fahrenheit
# maxTemp is the daily maximum temperature in Fahrenheit
getGDDs <- function(minTemp, maxTemp)
{
  cropMinTemp <- 50
  cropMaxTemp <- 86
  min <- minTemp
  max <- maxTemp
  
  # Reassign min and max if they are outside the bounds of the crop's min and max temps for growth
  if(min <= cropMinTemp)
  {
    min <- cropMinTemp
  }
  
  if(max <= cropMinTemp)
  {
    max <- cropMinTemp
  }
  
  if(max >= cropMaxTemp)
  {
    max <- cropMaxTemp
  }
  
  if(min >= cropMaxTemp)
  {
    min <- cropMaxTemp
  }
  GDD <- (min + max)/2 - cropMinTemp
  return(GDD)
}


# Function to calculate GDDs accumulated between 2 dates at a given location
# ***Specific to HIPS data** Modify as needed. 
# start is the start date as a POSIX date
# end is the end date as a POSIX date
# weather is the data frame with a GDD column of daily GDDs
# location is the location to get the cumulative GDDs at
getCumulativeGDDs <- function(start, end, weather, location)
{
  if(is.na(start) | is.na(end))
  {
    return(NA)
  }
  weather.df <- filter(weather, location==location & date %in% seq(min(start, end), max(start, end), 'days'))
  cumulativeGDDs <- sum(weather.df$GDD)
  return(cumulativeGDDs)
}