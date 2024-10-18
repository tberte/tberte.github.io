---
title: Certified Data Scientist
date: 2023-10-26
tags:
---

## Create subscales

creating subscales can be a burden for sure. Especially if you have
several measuring times and several subscales that all are different.
Below is my best approach to calculating subscales

::: cell
``` {.r .cell-code}
library(dplyr)

# Custom function to calculate total score with imputation of missing items
impute_and_sum <- function(values, min_items = 1) {
  non_na_values <- values[!is.na(values)]  # Get non-NA values
  
  if (length(non_na_values) >= min_items) {
    mean_non_na <- mean(non_na_values)  # Calculate mean of non-NA values
    values[is.na(values)] <- mean_non_na  # Impute NA values with the mean
    return(sum(values))  # Return the sum of the imputed values
  } else {
    return(NA)  # Return NA if there are no valid items
  }
}

# Define your time points
time_points <- c("Pre", "Post", "mo3")  # Add more as needed

# Define the subscale items
subscale_items <- c(1, 3, 10, 13)

# Loop through each time point and calculate subscale total scores with imputation
for (time_point in time_points) {
  # Dynamically create the column names for subscale items
  subscale_columns <- paste0(time_point, "_SCAS_item_", subscale_items)
  
  # Create the subscale score column for each time point
  df <- df %>%
    rowwise() %>%
    mutate(!!paste0(time_point, "_SCAS_subscale") := impute_and_sum(c_across(all_of(subscale_columns)))) %>%
    ungroup()
}

print(df)
```
:::
