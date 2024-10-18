---
title: Data cleaning step 1
date: 2023-10-26
tags:
---

## Step 1. Create a codebook

Before cleaning data we need to define what exactl we mean by "clean".
For this purpose we need a codebook where we describe the
characteristics we would like our data to have. First we need to define
the general category (numeric, Date, factor or Unqiue).

Numeric are values that should be coded as a number.

Factor includes both ordered and unordered characters.

Date is value representing dates or times.

Unique represent values that should be unique (e.i., IDs, Names).

::: cell
``` {.r .cell-code}
library(tibble)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)

df <- tibble(
  bad_numeric = c("4 (maybe)", "4 sq.m,", -99, 4),
  date_var = c("18-10-2024", 18102024, "18-10-2024", "18-10-2024"),
  property_type = c("Semi", "semi-detached", "terr.", "terraced"),
  ID = c(1, 1, 2, 3)
)
```
:::

::: cell
``` {.r .cell-code}
numeric_vars <- c("bad_numeric")
date_vars <- c("date_var")        
# factor_vars <- c("var6", "var7")
ordered_vars <- c("property_type")  
unique_vars <- c("another_var")
```
:::

## Step 2. Fix names

Make sure all variable names are easy to interpret and follow a standard
convention. Difficult to understand: hscty. Easy to understand
house_sales_city. I use this format good_movies_comedy.

## Step 3. Fix numeric values

### 3.1 Find values that should not be there

Start by finding out if there are any non-numeric values in any of the
numeric vars.

::: cell
``` {.r .cell-code}
find_non_numeric <- function(df, var) {
  df %>%
    filter(!is.na(!!sym(var)) & is.na(suppressWarnings(as.numeric(!!sym(var))))) %>%
    select(!!sym(var))
}

# Apply the function to each variable in numeric_vars
non_numeric_results <- map(numeric_vars, ~ find_non_numeric(df, .x))

# Name the results based on the variable names
names(non_numeric_results) <- numeric_vars

# View the non-numeric values for each variable
non_numeric_results
```
:::

Also try to see if there are any values that are out of range. If you
know that the scale only goes between 0-100

::: cell
``` {.r .cell-code}
range_df <- tibble(
  variable = c("bad_numeric"),
  min_value = c(0),  # Minimum values for each variable
  max_value = c(10)   # Maximum values for each variable
)

check_out_of_range_single_var <- function(variable, min_val, max_val) {
  df %>%
    filter(!!sym(variable) < min_val | !!sym(variable) > max_val) %>%
    select(!!sym(variable))
}

# Apply the function across all variables using pmap()
out_of_range_results <- range_df %>%
  rowwise() %>%  # Make it row-wise to iterate over each row of range_df
  mutate(out_of_range = list(check_out_of_range_single_var(variable, min_value, max_value)))

out_of_range_results$out_of_range[[1]]
```
:::

### 3.2 Replace values that should not be there

Values that are strings or contain strings may be handled using either
stringr or mutate. If the faulty value is reoccuring it would probably
be best to use stringr. If it is just a single occurence it is better to
use mutate.

::: cell
``` {.r .cell-code}
# single occurence
df <- df %>%
  mutate(bad_numeric = ifelse(bad_numeric == "4 (maybe)", "4", bad_numeric))

# recurring occurence
df <- df %>%
  mutate(bad_numeric = as.numeric(str_remove(bad_numeric, " sq.m.")))
```
:::

values that are outside range can be either NA (sometimes coded as -99)
or miss-types (1000, where max is 100) or ambigous (1234567 where range
is 0-100).

::: cell
``` {.r .cell-code}
df <- df %>%
  mutate(bad_numeric = ifelse(bad_numeric == -99, NA, bad_numeric))
```
:::

### 3.3 Set as numeric

::: cell
``` {.r .cell-code}
df <- df %>%
  mutate(across(all_of(numeric_vars), ~ round(as.numeric(.),2)))
```
:::

## Step 4. Fix Date

### 4.1 Set as Date.time

Typically date.time is not erronously encoded but may sometimes be read
in or saved by others as a numeric or string, below we handle both

::: cell
``` {.r .cell-code}
# if date is 18102024
df <- df %>%
  mutate(date_var = dmy(as.character(date_var)))
```
:::

### Step 4. set as Date

::: cell
``` {.r .cell-code}
df <- df %>%
  mutate(across(all_of(date_vars), ~ as.Date(.)))
```
:::

## Step 5. Fix factors

### 5.1 Find values that should not be there

Typical faulty values are misstypes. The easiest way to find these is to
look for what values a variable can take.

::: cell
``` {.r .cell-code}
df %>%
  select(all_of(ordered_vars)) %>%  # Select only the factor variables in your list
  map_df(~ unique(.)) 
```
:::

Then we can go over any erronous types and retype them

::: cell
``` {.r .cell-code}
df <- df %>%
  mutate(property_type = recode(property_type,
                              "Semi" = "Semi-detached",
                              "Det." = "Detached",
                              "Terr." = "Terraced"))
```
:::

### 5.2 set as factor and ordered

``` r
df <- df %>%
  mutate(across(all_of(factor_vars), as.factor))
```

::: cell
``` {.r .cell-code}
df <- df %>%
  mutate(property_type = factor(property_type, 
                              levels = c("Detached", "Semi-detached",  "Terraced"),
                              ordered = TRUE))
```
:::

## 6. Unique values

This is easy, just count how many times each unique value occurs and
ensure that it is correct

::: cell
``` {.r .cell-code}
# Count the occurrences of each ID
id_counts <- df %>%
  count(ID, name = "count")

# View the result
rows_with_id_gt_2 <- df %>%
  semi_join(id_counts %>% filter(count > 1), by = "ID")

# View the result
rows_with_id_gt_2
```
:::
