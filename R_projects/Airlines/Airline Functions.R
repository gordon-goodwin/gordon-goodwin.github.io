

# FILE CONTAINS ALL CUSTOM FUNCTIONS REFERENCED IN MAIN FILE
# Packages Required -------------------------------------------------------


library(tidyverse)
library(janitor)
library(readr)
library(readxl)
library(writexl)
library(patchwork)
library(ggthemes)
library(ggcorrplot)
library(ggrepel)
library(gt)

# Clean Reader ------------------------------------------------------------

# Read CSV, clean names, and specify coltype as date

cleanreader <- function(filepath){
  
    read_csv(filepath) |> 
      clean_names() 
}


# Quantiler ---------------------------------------------------------------

quantiler <- function(x, probs = seq(0, 1, .1)){
  quantile(x, probs = probs, na.rm = T)
}

# DateFixer Vectorized ---------------------------------------------------------------


# Vectorized Sub-Fn
datefixer <- function(x) {
  
  # REGEX for 2023-10-19 format
  pattern1 <- "^\\d{4}-\\d{2}-\\d{2}$"
  # REGEX for 2/3/19 or similar
  pattern2 <- "^(0?[1-9]|1[0-2])/(0?[1-9]|[12][0-9]|3[01])/(\\d{2})$"
  
  case_when(
    str_detect(x, pattern1) ~ ymd(x),
    str_detect(x, pattern2) ~ mdy(x),
    TRUE ~ NA_Date_
  )
}

# Date Fixer Dataset --------------------------------------------------------

datefixer_data <- function(data){
  
  # REGEX for 2023-10-19 format
  pattern1 <- "^\\d{4}-\\d{2}-\\d{2}$"
  # REGEX for 2/3/19 or similar
  pattern2 <- "^(0?[1-9]|1[0-2])/(0?[1-9]|[12][0-9]|3[01])/(\\d{2})$"
  
  # Vectorized Sub-Fn
  datefixer <- function(x) {
    case_when(
      str_detect(x, pattern1) ~ ymd(x),
      str_detect(x, pattern2) ~ mdy(x),
      TRUE ~ NA_Date_
    )
  }
  
  # Apply conditionally to cols
  data |> 
    mutate(across(.cols = where(function(x) any(str_detect(x, pattern1) | str_detect(x, pattern2))),
                  .fns = datefixer))
}



# Missing Var Summary  ---------------------------------------------------

missingval_summary <- function(data){
  
  
  data |> 
    summarize(across(.cols = everything(), 
                     .fns = list(count_valid = ~sum(!is.na(.x)),
                                 count_missing = ~sum(is.na(.x))),
                     .names = "{col}_{.fn}")) |> 
    pivot_longer(names_to = "cols",
                 values_to = "values",
                 cols = everything()) |> 
    mutate(count_type = rep(c("Valid","Missing"),length(colnames(flights)))) |> 
    mutate(cols = str_remove_all(cols, "_valid"),
           cols = str_remove_all(cols, "_missing")) |> 
    pivot_wider(names_from = count_type,
                values_from = values) |> 
    mutate(Prop_Missing = round(Valid/(Valid + Missing), 3))
  
}


# Single Bar Charter ------------------------------------------------------

singlebar_viz <- function(data, x, nrecs = 10){
  
  # Store x-axis label
  deparse(substitute(x)) -> xaxislab
  
data |> 
    count({{x}}, sort = T) |>
    slice_head(n = nrecs) |> 
    ggplot(aes(x = reorder({{x}}, -n),
               y = n,
               fill = {{x}})) +
    geom_col() +
  geom_text(aes(label = n,
                y = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color = "black")) +
    scale_fill_tableau() +
    labs(x = xaxislab,
         y = "Count")
}


# Double Bar Charter ------------------------------------------------------

doublebar_viz <- function(data, x,grp1, nrecs = 10, labels = T){
  
  # Store x-axis label
  deparse(substitute(x)) -> xaxislab
  
  if(labels == T){
  data |> 
    count({{x}},{{grp1}}, sort = T) |>
    slice_head(n = nrecs) |> 
    ggplot(aes(x = reorder({{x}}, -n),
               y = n,
               fill = {{grp1}})) +
    geom_col() +
    geom_text(aes(label = n,
                  y = n), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              size = 3) +
    theme_minimal() +
    theme(legend.position = "top",
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color = "black")) +
    scale_fill_tableau() +
    labs(x = xaxislab,
         y = "Count")
  } else {
    data |> 
      count({{x}},{{grp1}}, sort = T) |>
      slice_head(n = nrecs) |> 
      ggplot(aes(x = reorder({{x}}, -n),
                 y = n,
                 fill = {{grp1}})) +
      geom_col() +
      theme_minimal() +
      theme(legend.position = "top",
            axis.title = element_text(face = "bold"),
            axis.text = element_text(color = "black")) +
      scale_fill_tableau() +
      labs(x = xaxislab,
           y = "Count")
}
}


# Missing Val Viz - Pct Valid ---------------------------------------------


missingval_pctvalid_viz <- function(data){
  
  # Prop Valid
  
  data |> 
    summarize(across(.cols = everything(),
                     .fns = ~round(mean(!is.na(.x)),3))) |> 
    pivot_longer(names_to = "cols",
                 values_to = "pct_valid",
                 cols = everything()) |> 
    filter(pct_valid < 1) |> 
    ggplot(aes(x = fct_reorder(cols,-pct_valid),
               y = pct_valid*100)) +
    geom_col(fill = "sienna4") +
    geom_text(aes(label = paste0(sprintf("%.2f", pct_valid*100),"%"),
                  y = pct_valid*100), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              size = 3) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = "Variables Missing Vals",
         y = "Valid %")
}

# Missing Val Viz - Count Missing -----------------------------------------

missingval_misscount_viz <- function(data){
  
  # Count miss
  data |> 
    summarize(across(.cols = everything(),
                     .fns = ~sum(is.na(.x)))) |> 
    pivot_longer(names_to = "cols",
                 values_to = "count_missing",
                 cols = everything()) |> 
    filter(count_missing > 0) |> 
    ggplot(aes(x = fct_reorder(cols,-count_missing),
               y = count_missing)) +
    geom_col(fill = "firebrick") +
    geom_text(aes(label = count_missing,
                  y = count_missing), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              size = 3) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = "Variables Missing Vals",
         y = "Missing Count")
  
}

# Missing Value Viz Both --------------------------------------------------

missingval_viz <- function(data){
  
  # Prop Valid
  data |> 
    summarize(across(.cols = everything(),
                     .fns = ~round(mean(!is.na(.x)),3))) |> 
    pivot_longer(names_to = "cols",
                 values_to = "pct_valid",
                 cols = everything()) |> 
    filter(pct_valid < 1) |> 
    ggplot(aes(x = fct_reorder(cols,-pct_valid),
               y = pct_valid*100)) +
    geom_col(fill = "sienna4") +
    geom_text(aes(label = paste0(sprintf("%.2f", pct_valid*100),"%"),
                  y = pct_valid*100), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              size = 3) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = "Variables Missing Vals",
         y = "Valid %") -> p1
  
  # Count miss
  data |> 
    summarize(across(.cols = everything(),
                     .fns = ~sum(is.na(.x)))) |> 
    pivot_longer(names_to = "cols",
                 values_to = "count_missing",
                 cols = everything()) |> 
    filter(count_missing > 0) |> 
    ggplot(aes(x = fct_reorder(cols,-count_missing),
               y = count_missing)) +
    geom_col(fill = "firebrick") +
    geom_text(aes(label = count_missing,
                  y = count_missing), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              size = 3) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = "Variables Missing Vals",
         y = "Missing Count") -> p2
  
  # Display
  p1 + p2 +  plot_annotation(
    title = 'Fields w/ Missing Values: Pct Valid & Count Missing',
    caption = 'Valid % are only shown for vars with >= 0.01% missing'
  ) 
}


# Identify Common Fields for Join -----------------------------------------

# Takes in 2+ dataframes and returns common and unique fields
compare_df_cols <- function(...) {
  dfs <- list(...)
  
  # Check if at least 2 dataframes are provided
  if (length(dfs) < 2) {
    stop("At least two dataframes are required.")
  }
  
  common_cols <- unique(intersect(colnames(dfs[[1]]), colnames(dfs[[2]])))
  unique_cols <- vector(mode = "list", length = length(dfs))
  
  # Compare column names across dataframes
  for (i in seq_along(dfs)) {
    unique_cols[[i]] <- setdiff(colnames(dfs[[i]]), common_cols)
  }
  
  return(list(common_cols = common_cols, unique_cols = unique_cols))
}


# Basic Descriptives with or without Groupings ----------------------------


describer_stats <- function(data, x, grp1 = NULL, grp2 = NULL, grp3){
  
  data |> 
    summarize(min_val = min({{x}}),
              max_val = max({{x}}),
              iqr = IQR({{x}}),
              avg_val = round(mean({{x}}), 2),
              median_val = round(median({{x}}), 2),
              .by = c({{grp1}}, {{grp2}}, {{grp3}}))
}

# Apply Filter and View -----------------------------------------------------------------

filtview <- function(data, condition = NULL){
  data |> 
    filter({{condition}}) |> 
    View()
}


# Col Names Containing Phrase --------------------------------------------

namefinder <- function(data, condition){
  
  data |> select({{condition}}) |> names()
}

# CrossProdder ------------------------------------------------------------

crossprodder <- function(data,complete = F, ...){
  
  if(complete == F){
    
    data |> expand(...)
    
  }else{
    data |> 
      complete(...)
  }
}

# Single Box Plotter ------------------------------------------------------

singlebox_viz <- function(data, x){
  
  data |> 
    ggplot(aes(x = {{x}})) +
    geom_boxplot(width = 0.5) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.x = element_text(face = "bold"),
          axis.text.x = element_text(color = "black"))
}


# Double Box Plotter ------------------------------------------------------


doublebox_viz <- function(data, x, y){
  
  data |> 
    ggplot(aes(x = {{x}},
               fill = {{x}},
               y = {{y}})) +
    geom_boxplot(width = 0.25) +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "none",
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color = "black"))
}


# Trim or Replace by Quantiles --------------------------------------------

trim_or_replace_quantiles <- function(data, x, grp1 = NULL, replace = F, low = 0.05, high = 0.95){
  if(replace == T){
    data |> 
      mutate({{x}} := case_when({{x}} >= quantile({{x}}, high) ~ quantile({{x}}, high),
                               {{x}} <= quantile({{x}}, low) ~ quantile({{x}}, low)),
             .by = {{grp1}})
  } else{
    data |> 
      filter({{x}} >= quantile({{x}}, low),
             {{x}} <= quantile({{x}}, high),
             .by = {{grp1}})
  }
}


# Round Mean --------------------------------------------------------------

roundmean <- function(x, digits = 2, ...){
  round(mean(x, ...), digits = digits)
}


# Crow Flies Distance -----------------------------------------------------


# Function to calculate Haversine distance in miles
crowfly_distance_miles <- function(lat1, lon1, lat2, lon2) {
  
  # Function to convert degrees to radians
  deg2rad <- function(deg) {
    return(deg * pi / 180)
  }
  
  R <- 3959  # Earth radius in miles
  
  # Convert latitude and longitude from degrees to radians
  lat1 <- deg2rad(lat1)
  lon1 <- deg2rad(lon1)
  lat2 <- deg2rad(lat2)
  lon2 <- deg2rad(lon2)
  
  # Calculate differences
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Haversine formula
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  # Distance in miles
  distance <- R * c
  return(distance)
}


# Quartile Bucketer -------------------------------------------------------

quartile_cutter <- function(x, n_breaks = 4, labels = NULL){
  
  cut(x, 
      breaks = n_breaks, 
      labels = labels,
      include.lowest = TRUE, 
      ties.method = "first")
}



# Flight Dicer ------------------------------------------------------------


flightdicer <- function(data,grp1 = NULL, grp2 = NULL, 
                        ntiles = 20,min_flights = 0){
  
  # Takes in dataframe, groups by 0, 1, or 2 desired variables, 
  # filter for groups achieving a minimum flight threshold (default = 0)
  # Outputs summary stats on all numeric variables and ranks with X quantiles
  data |> 
    summarize(n_flights = n(),
              n_planes = n_distinct(tail_num),
              n_flightdays = n_distinct(fl_date),
              # Total Profit, Cost, Rev
              total_profit = sum(est_profit_total),
              avg_profit = roundmean(est_profit_total),
              median_profit = median(est_profit_total),
              avg_revenue_total = roundmean(est_all_revenue_total),
              avg_cost_total = roundmean(est_all_costs_total),
              # Revebye and cost subcategories
              avg_fare_revenue = roundmean(est_fare_revenue_total),
              avg_bag_revenue = roundmean(est_bag_revenue_total),
              avg_delay_costs = roundmean(arr_dep_delay_cost_total),
              avg_mileage_var_costs = roundmean(mileage_var_costs_total),
              avg_airport_costs = roundmean(airport_landing_cost_total),
              # Components of revenue and cost
              ## Revenue components
              avg_median_fare = roundmean(med_fare_imputed),
              avg_n_passengers = roundmean(n_passengers_total),
              avg_occupancy_rate = roundmean(avg_occ_rate_bothlegs),
              avg_n_bags = roundmean(n_bags_total),
              ## Cost components
              avg_distance = roundmean(distance_crowfly_total),
              avg_delay_time = roundmean(arr_dep_delay_time_total),
              avg_airport_cost = roundmean(airport_landing_cost_total),
              .by = c({{grp1}},{{grp2}})) |> 
    filter(n_flights > min_flights) |> 
    mutate(across(.cols = where(is.numeric),
                  .fns = ~ntile(.x, n = ntiles),
                  .names = "{.col}_decile"))
}


# Zero Truncater ----------------------------------------------------------

zero_trunc <- function(x){
  if_else(x < 0, 0, x)
}
