# function to create new columns summarizing disease events
# usefull for disease with different types 
# (e.g., lameness with different lesions)
# (e.g., mastitis with different pathogens)
# need to filter to just disease events (trimonly == 0)
# for each disease, create life_times_<disease> and status_<disease>
# life_times_<disease> is cumulative count of disease events for that disease
# needs a disease_cols argument with the names of the disease columns
# status_<disease> is New, Chronic, or New (Recurrence)

fxn_dz_status <- function(data, 
                          farm_col = location_event, 
                          id_col = id_animal,
                          disease_cols, 
                          prefix = "lame",
                          event_filter = TRUE, # New argument: defaults to no filter
                          gap = 1) {
  
  # 1. Standardize names and Apply the flexible filter
  # Using .data[[...]] or {{ }} allows complex logic to be passed in
  data_filtered <- data %>%
    rename(location_event = {{farm_col}}, 
           id_animal = {{id_col}}) %>%
    filter({{ event_filter }}) 
  
  # 2. Process Disease History (using the filtered data)
  disease_history <- data_filtered %>%
    select(location_event, id_animal, date_event, lact_number,
           all_of(disease_cols)) %>%
    pivot_longer(cols = all_of(disease_cols), 
                 names_to = "disease", 
                 values_to = "present") %>%
    filter(present == 1) %>%
    summarise(present = max(present), .by = c(location_event, 
                                              id_animal, date_event,
                                              lact_number, disease)) %>% 
    group_by(location_event, id_animal, disease) %>%
    arrange(date_event) %>%
    mutate(
      count = row_number(),
      days_since_last = as.numeric(date_event - lag(date_event)),
      status = case_when(
        is.na(days_since_last) ~ "New",
        days_since_last <= gap ~ "Repeat",
        days_since_last > gap ~ "Chronic",
        .default = "Unknown"
      )
    ) %>%
    ungroup()
  
  # 3. Wide versions of specific lesions
  counts_wide <- disease_history %>%
    select(location_event, id_animal, date_event, disease, lact_number, count) %>%
    pivot_wider(names_from = disease, values_from = count, names_prefix = "life_times_")
  
  status_wide <- disease_history %>%
    select(location_event, id_animal, date_event, disease, lact_number, status) %>%
    pivot_wider(names_from = disease, values_from = status, names_prefix = "status_")
  
  # 4. General Stats (Life counts for the 'prefix' event type)
  general_stats <- data_filtered %>%
    select(location_event, id_animal, date_event, 
           lact_number, all_of(disease_cols)) %>%
    distinct(location_event, id_animal, date_event, lact_number, .keep_all = TRUE) %>% 
    group_by(location_event, id_animal) %>%
    arrange(date_event) %>%
    mutate(
      !!paste0("life_times_", prefix) := row_number(),
      !!paste0("days_from_", prefix) := 
        as.numeric(date_event - lag(date_event)),
      !!paste0("days_to_", prefix) := 
        as.numeric(lead(date_event) - date_event)
    ) %>%
    group_by(location_event, id_animal, lact_number) %>%
    mutate(!!paste0("lact_times_", prefix) := row_number()) %>%
    ungroup() %>%
    select(location_event, id_animal, date_event, lact_number, starts_with("life_times_"), 
           starts_with("days_"), starts_with("lact_times_"))
  
  # 5. Final Join to the ORIGINAL (unfiltered) data
  # This ensures we keep trim-only rows but fill them with the last known counts
  data %>%
    rename(location_event = {{farm_col}}, id_animal = {{id_col}}) %>%
    left_join(general_stats, by = c("location_event", "id_animal", "date_event", "lact_number")) %>%
    left_join(counts_wide, by = c("location_event", "id_animal", "date_event", "lact_number")) %>%
    left_join(status_wide, by = c("location_event", "id_animal", "date_event", "lact_number")) %>%
    arrange(location_event, id_animal, date_event) %>%
    group_by(location_event, id_animal) %>%
    mutate(across(starts_with("life_times"), ~ replace_na(.x, 0) %>% cummax())) %>%
    group_by(location_event, id_animal, lact_number) %>%
    mutate(across(starts_with("lact_times"), ~ replace_na(.x, 0) %>% cummax())) %>%
    ungroup()
}