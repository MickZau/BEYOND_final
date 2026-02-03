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
