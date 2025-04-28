cutoff <- 3

  # Function to calculate bounds for numeric columns only
  calculate_bounds <- function(column) {
  mean_value <- mean(column, na.rm = TRUE)
     std_deviation <- sd(column, na.rm = TRUE)
    upper_bound <- mean_value + cutoff * std_deviation
      lower_bound <- mean_value - cutoff * std_deviation
    return(c(lower_bound, upper_bound))
    }
  
     # Select only the numeric columns for processing
    yielddata_cutoff <- yield_hist[, sapply(yield_hist, is.numeric)]
     # Apply the function to each numeric column in the dataframe
       bounds <- sapply(yielddata_cutoff, calculate_bounds)
       # Convert bounds to a more readable format
         colnames(bounds) <- colnames(yielddata_cutoff)
         rownames(bounds) <- c("Lower Bound", "Upper Bound")
         print(bounds)
        
        write.csv(bounds,"bounds.csv")
        
        yield_filtered_data <- yielddata_cutoff
        for (i in seq_along(yielddata_cutoff)) {
           trait <- yielddata_cutoff[[i]]
            bounds_vec <- bounds[, i]
             yield_filtered_data[[i]] <- ifelse(trait >= bounds_vec[1] & trait <= bounds_vec[2], trait, NA)
           }
         # Combine the GenotypeID with the filtered data
           yield_filtered_data_v1 <- cbind(yield_hist["Genotype"], yield_filtered_data)
           # Show filtered data (Note: NAs may appear where outliers were removed)
            print(yield_filtered_data_v1)