# to deal with lesion and trim only occuring on same day
#
#

library(dtplyr)

fxn_collapse_lesions <- function(data,
                                 farm_col = location_event,
                                 id_col = id_animal,
                                 date_col = date_event,
                                 lesions) {
  # deal with trim only and lame on same day
  data <- data |>
    # rename(location_event = {{ farm_col }},
    #       id_animal = {{ id_col }}) |>
    lazy_dt() |>
    # delete footrim if lame/footrim on same day
    group_by({{ location_event }}, {{ id_animal }}, {{ date_col }}) |>
    slice_min(trimonly, n = 1, with_ties = FALSE) |>
    ungroup() |>
    as_tibble()

  # data set to summarize lesion so only 1 row/date event
  data_sum <- data |>
    lazy_dt() |>
    group_by({{ location_event }}, {{ id_animal }}, {{ date_col }}) |>
    summarise(across(.cols = all_of(lesions), max, na.rm = TRUE)) |>
    ungroup() |>
    as_tibble()

  # merge back to orginal data
  data <- data |>
    # remove lesion columns and get distinct rows so only 1/date event
    select(-c(all_of(lesions))) |>
    distinct({{ location_event }}, {{ id_animal }}, {{ date_col }},
      .keep_all = TRUE
    ) |>
    left_join(data_sum,
      by = join_by(
        {{ location_event }},
        {{ id_animal }}, {{ date_col }}
      )
    )
}
