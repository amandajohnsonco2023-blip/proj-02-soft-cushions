# app/goals.R

# defined variables to avoid magic numbers and hard-coded paths
GOALS_DIR <- "data/goals"
DEFAULT_WEIGHT_KG <- 70
DEFAULT_MUSCLE_PCT <- 35

default_goals <- function() {
  data.frame(
    exercise_minutes   = 30,
    steps              = 8000,
    calories_eaten     = 2200,
    water_cups_drank   = 8,
    hours_of_sleep     = 7,
    sleep_quality      = 4,
    fruit_servings     = 3,
    vegetable_servings = 3,
    meditation_minutes = 10,
    pages_read         = 20,
    weight             = NA,
    muscle_percentage  = NA,
    stringsAsFactors   = FALSE
  )
}

user_goals_path <- function(username) {
  paste0(GOALS_DIR, "/", username, ".csv")
}

load_user_goals <- function(username) {
  path <- user_goals_path(username)

  if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE)
  } else {
    default_goals()
  }
}

save_user_goals <- function(username, goals_df) {
  path <- user_goals_path(username)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(goals_df, path, row.names = FALSE)
}

goals_server <- function(
  input,
  output,
  session,
  logged_in,
  current_user,
  goals_version
) {
  user_goals <- reactive({
    req(logged_in())
    goals_version()
    load_user_goals(current_user())
  })

  output$goals_title <- renderUI({
    "My Goals"
  })

  output$goals_form <- renderUI({
    if (!logged_in()) {
      return(p("Log in to set your personal goals."))
    }

    g <- user_goals()

    tagList(
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2),
        value_box(
          title = "Exercise",
          value = paste0(g$exercise_minutes, " min"),
          showcase = bs_icon("activity"),
          p(paste0(format(g$steps, big.mark = ","), " steps per day")),
          theme = "danger"
        ),
        value_box(
          title = "Calories",
          value = format(g$calories_eaten, big.mark = ","),
          showcase = bs_icon("fire"),
          p("Daily maximum"),
          theme = "warning"
        ),
        value_box(
          title = "Water",
          value = paste0(g$water_cups_drank, " cups"),
          showcase = bs_icon("droplet"),
          p("Daily minimum"),
          theme = "info"
        ),
        value_box(
          title = "Sleep",
          value = paste0(g$hours_of_sleep, " hrs"),
          showcase = bs_icon("moon-stars"),
          p(paste0("Quality target: ", g$sleep_quality, "/5")),
          theme = "primary"
        ),
        value_box(
          title = "Fruits & Veggies",
          value = paste0(g$fruit_servings + g$vegetable_servings, " servings"),
          showcase = bs_icon("basket"),
          p("Daily minimum"),
          theme = "success"
        )
      ),
      layout_columns(
        col_widths = c(4, 4, 4, 6, 6),
        card(
          card_header(bs_icon("activity"), " Activity"),
          numericInput(
            "goal_exercise",
            "Minimum exercise minutes",
            g$exercise_minutes,
            min = 0,
            max = 300
          ),
          numericInput(
            "goal_steps",
            "Daily steps",
            g$steps,
            min = 0,
            max = 50000
          )
        ),
        card(
          card_header(bs_icon("droplet"), " Nutrition"),
          numericInput(
            "goal_calories",
            "Maximum calories",
            g$calories_eaten,
            min = 0,
            max = 5000
          ),
          numericInput(
            "goal_water",
            "Minimum water cups",
            g$water_cups_drank,
            min = 0,
            max = 20
          ),
          numericInput(
            "goal_fruit",
            "Minimum fruit servings",
            g$fruit_servings,
            min = 0,
            max = 15
          ),
          numericInput(
            "goal_veg",
            "Minimum vegetable servings",
            g$vegetable_servings,
            min = 0,
            max = 15
          )
        ),
        card(
          card_header(bs_icon("moon-stars"), " Sleep"),
          numericInput(
            "goal_sleep_hrs",
            "Minimum sleep hours",
            g$hours_of_sleep,
            min = 0,
            max = 12
          ),
          numericInput(
            "goal_sleep_quality",
            "Minimum sleep quality (1-5)",
            g$sleep_quality,
            min = 1,
            max = 5
          )
        ),
        card(
          card_header(bs_icon("heart-pulse"), " Wellness"),
          numericInput(
            "goal_meditation",
            "Minimum meditation minutes",
            g$meditation_minutes,
            min = 0,
            max = 120
          ),
          numericInput(
            "goal_pages",
            "Minimum pages read",
            g$pages_read,
            min = 0,
            max = 200
          )
        ),
        card(
          card_header(bs_icon("person-standing"), " Body"),
          numericInput(
            "goal_weight",
            "Target weight (kg)",
            ifelse(is.na(g$weight), DEFAULT_WEIGHT_KG, g$weight),
            min = 30,
            max = 300
          ),
          numericInput(
            "goal_muscle",
            "Target muscle %",
            ifelse(is.na(g$muscle_percentage), DEFAULT_MUSCLE_PCT, g$muscle_percentage),
            min = 0,
            max = 100
          )
        )
      ),
      br(),
      actionButton("save_goals", "Save My Goals", class = "btn-danger"),
      uiOutput("goals_feedback")
    )
  })

  observeEvent(input$save_goals, {
    req(logged_in())

    new_goals <- data.frame(
      exercise_minutes = input$goal_exercise,
      steps = input$goal_steps,
      calories_eaten = input$goal_calories,
      water_cups_drank = input$goal_water,
      hours_of_sleep = input$goal_sleep_hrs,
      sleep_quality = input$goal_sleep_quality,
      fruit_servings = input$goal_fruit,
      vegetable_servings = input$goal_veg,
      meditation_minutes = input$goal_meditation,
      pages_read = input$goal_pages,
      weight = input$goal_weight,
      muscle_percentage = input$goal_muscle,
      stringsAsFactors = FALSE
    )

    save_user_goals(current_user(), new_goals)
    goals_version(goals_version() + 1)

    output$goals_feedback <- renderUI({
      validate(need(FALSE, "Goals saved. Your tracker now reflects your personal targets."))
    })
  })
}
