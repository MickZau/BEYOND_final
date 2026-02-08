import_single_cgm <- function(file_path){
  read_csv(
    file = file_path
    ) |>
    rename(
      datetime = "Timestamp (YYYY-MM-DDThh:mm:ss)",
      type = "Event Type",
      glucose = "Glucose Value (mmol/L)"
    ) |>
    filter(
      type == "EGV"
    ) |>
    mutate(
      glucose = str_replace_all(glucose, "High", "22.2"),
      glucose = str_replace_all(glucose, "Low", "2.2"),
      glucose = as.numeric(glucose)
    )  |>
    mutate(
      date = as_date(datetime),
      hour = hour(datetime),
      min = minute(datetime),
      .before = datetime
    ) |>
    select(
      date,
      type,
      hour,
      min,
      glucose
    )
}

import_single_bt <- function(file_path){
read_csv2(
  file = file_path,
  skip = 51,
  col_names = FALSE
  ) |>
  rename(
    hour = X1,
    min = X2,
    sys = X3,
    map = X4,
    dia = X5,
    hf = X6,
    error = X7
  ) |>
  filter(
    is.na(error),
    !is.na(hf)
  ) |>
  select(
    -error
  ) |>
  mutate(
    hour = as.numeric(hour),
    min = as.numeric(min)
  )
}

import_bt_date <- function(file_path){
  read_csv2(
    file = file_path,
    skip = 17,
    n_max = 1,
    col_names = FALSE
  ) |>
    mutate(
      date = X1
    ) |>
    select(
      date
    )
}
