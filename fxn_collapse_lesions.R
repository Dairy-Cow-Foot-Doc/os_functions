# to deal with lesion and trim only 
# 
# 
library(dtplyr)

fxn_clean_trim <- function(data,
                           lesions){
  
  # deal with trim only and lame on same day
  data <- data |> 
  lazy_dt() |> 
    # delete footrim if lame/footrim on same day
    group_by(farm, id_animal, date_event) |>
    slice_min(trimonly) |> 
    ungroup() |> 
    as_tibble()
  
  # data set to summarize
  data_sum <- lame_events |> 
    group_by(farm, id_animal, date_event) |>
    summarise(across(.cols = all_of(lesions), max)
    )|> 
    ungroup() 
  
  data <- data |> 
    select(-c(all_of(lesions))) |> 
    distinct(farm, id_animal, date_event, .keep_all = TRUE) |> 
    left_join(data_sum, 
              by = join_by(farm, id_animal, date_event)) 
  
}