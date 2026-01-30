import_single_cgm <- function(file_path){
  read_csv(
    file = file_path,
    col_names = FALSE,
    trim_ws = TRUE,
    skip = 12
  ) |>
    rename(
      datetime = X2,
      glucose = X8
    )  |>
    mutate(
      date = as_date(datetime),
      hour = hour(datetime),
      min = minute(datetime),
      .before = datetime
    ) |>
    select(
      date,
      hour,
      min,
      glucose
    ) |>
    mutate(
      glucose = str_replace_all(glucose, "High", "22.2"),
      glucose = str_replace_all(glucose, "Low", "2.2"),
      glucose = as.numeric(glucose)
    )
}
