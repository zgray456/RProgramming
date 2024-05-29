sales_df <- data.frame(UnitPrice = c(10.01, 15.07, 41.62, 49.49, 43.84),
                       Quantity = c(2L, 9L, 10L, 9L, 10L),
                       COGS = c(170.12, 174.55, 284.76, 709.60, 955.90),
                       CustomerType = factor(c("N", "M", "M", "N", "N")))

# Apply fix to incorrectly recorded row values in a given column
shift_and_double <- function(df, col = 1) {
  new_vals <- NULL  # Will store corrected values
  rows <- nrow(df)
  
  # Apply correction and store as vector
  for (i in 1:rows) {
    new_vals[[i]] <- df[[i - 1, col]] * 2
  }
  
  df[, col] <- new_vals
  
  return(df)
}

# For a given column, get the mean after applying fix
mean_shftdbl <- function(df, ...) {
  shftdbl_df <- shift_and_double(df, ...)  # Fix the data first
  
  mean_df <- data.frame(Mean = sapply(shftdbl_df, mean))
  
  # Return both the fixed data and the means in one list
  return(list(Data = shftdbl_df, Means = mean_df))
}