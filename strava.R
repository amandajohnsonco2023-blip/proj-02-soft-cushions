# app/strava.R

# defined variables to avoid magic numbers and hard-coded paths
SECONDS_PER_MINUTE <- 60


parse_strava_upload <- function(filepath) {
  raw <- read.csv(filepath, stringsAsFactors = FALSE)
  
  raw |>
    select(`Activity.Date`, `Activity.Type`, `Elapsed.Time`) |>
    mutate(
      date = as.Date(`Activity.Date`, format = "%b %d, %Y, %I:%M:%S %p"),
      exercise_minutes = round(`Elapsed.Time` / SECONDS_PER_MINUTE),
      type_of_exercise = case_when(
        str_to_lower(`Activity.Type`) == "yoga"                               ~ "Flexibility",
        str_to_lower(`Activity.Type`) %in% c("weight training", "workout")    ~ "Strength",
        .default = "Cardio"
      )
    ) |>
    select(date, exercise_minutes, type_of_exercise)
}

render_strava_ui <- function() {
  tagList(
    h3("Import from Strava"),
    p("Export your data from Strava: Settings → My Account → Download or Delete Your Account → Download Request → activities.csv"),
    fileInput("strava_file", "Upload activities.csv", accept = ".csv"),
    uiOutput("strava_preview_ui"),
    uiOutput("strava_import_btn")
  )
}