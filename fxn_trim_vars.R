library(dplyr)
library(dtplyr)
library(cli)

# create trim related variables
# 
fxn_trim_vars <- function(data, ..., date_var, trimonly_var) {
  
  # 1. Capture names for checking
  # Use enquo to "look" at the names provided without evaluating them yet
  vars_to_check <- c(as_label(enquo(date_var)), as_label(enquo(trimonly_var)))
  
  # 2. Check if required columns exist
  missing_vars <- setdiff(vars_to_check, colnames(data))
  
  if (length(missing_vars) > 0) {
    cli_abort(c(
      "x" = "Column{?s} not found in the input data: {.val {missing_vars}}",
      "i" = "Available columns are: {.val {colnames(data)}}"
    ))
  }
  
  # 3. Check if the ordering variable is actually a Date/Time
  if (!inherits(pull(data, {{ date_var }}), c("Date", "POSIXt", "difftime"))) {
    cli_abort(c(
      "x" = "The {.var date_var} column must be a {.cls Date} or {.cls POSIXct} object.",
      "i" = "Your input column is of class: {.cls {class(pull(data, {{ order_var }}))}}",
      "hint" = "Try converting it with {.fn as.Date} or {.fn lubridate::ymd} before calling this function."
    ))
  }
  
  # 4. Execution logic
  data %>%
    #lazy_dt() %>%
    arrange(pick(...), {{ date_var }}) %>% 
    group_by(...) %>%
    mutate(
      times_trimmed = row_number(), 
      
      type_prev_trim = lag({{ trimonly_var }}),
      date_prev_trim = lag({{ date_var }}),
      
      days_from_prev_trim = as.numeric({{ date_var }} - date_prev_trim),
      
      days_from_prev_trimonly_lesion = if_else({{ trimonly_var }} == 0 & type_prev_trim == 1,
                               days_from_prev_trim, NA),
      
      days_to_next_trim = lead(days_from_prev_trim),
      days_trimonly_to_lesion = lead(days_from_prev_trimonly_lesion)
    ) %>%
    ungroup() #%>%
    #as_tibble()
}