# to deal with lesion and trim only occuring on same day
# 
# 

library(dtplyr)

fxn_collapse_lesions <- function(data,
                                 farm_col = location_event ,
                                 lesions){
  
  # deal with trim only and lame on same day
  data <- data |> 
    rename(farm = {{ farm_col }}) |> 
    lazy_dt() |> 
    # delete footrim if lame/footrim on same day
    group_by(farm, id_animal, date_event) |>
    slice_min(trimonly) |> 
    ungroup() |> 
    as_tibble()
  
  # data set to summarize
  data_sum <- data |> 
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