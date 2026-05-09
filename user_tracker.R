# app/user_tracker.R

# defined variables to avoid magic numbers and hard-coded paths
PCT_ON_TRACK     <- 80
PCT_SLIGHT_BEHIND <- 65
PCT_HABITS_GREAT <- 70
PCT_HABITS_OK    <- 40
N_GOALS_TRACKED  <- 4
N_GOALS_PARTIAL  <- 2
LOGS_DIR <- file.path("data", "logs")
WATER_YAXIS_MAX   <- 13
PRODUCE_YAXIS_MAX <- 8
SLEEP_QUALITY_MIN <- 1
SLEEP_QUALITY_MAX <- 5

user_log_path <- function(username) {
  file.path(LOGS_DIR, paste0(username, ".csv"))
}

goal_status <- function(pct) {
  if (pct >= PCT_ON_TRACK) {
    "On track"
  } else if (pct >= PCT_SLIGHT_BEHIND) {
    "Slightly behind"
  } else {
    "Needs focus"
  }
}

goal_value_box <- function(label, threshold, days_met, total_days, emoji, theme) {
  pct <- round(days_met / total_days * 100)

  value_box(
    title = label,
    value = paste0(pct, "%"),
    showcase = emoji,
    p(paste0(threshold, " — ", days_met, "/", total_days, " days")),
    theme = theme,
    height = "350px",
    showcase_layout = "top right"
  )
}

empty_state <- function(message) {
  card(p(message))
}


user_tracker_server <- function(
  input,
  output,
  session,
  logged_in,
  current_user,
  user_data,
  log_version,
  badge_version
) {
  # Page title
  output$page_title <- renderUI({
    if (logged_in()) {
      paste0(current_user(), "'s Tracker Dashboard")
    } else {
      "Your Health Tracker"
    }
  })

  # Goal completion overview
  output$user_goal_overview <- renderUI({
    if (!logged_in()) {
      return(empty_state("Log in to see your goal progress."))
    }

    df <- user_data()
    g <- tryCatch(load_user_goals(current_user()), error = function(e) {
      default_goals()
    })

    if (nrow(df) == 0) {
      return(empty_state(
        "No data yet. Log your first entry below to see your goal progress."
      ))
    }

    total_days <- nrow(df)
    produce_goal <- g$fruit_servings + g$vegetable_servings

    cards <- list(
      goal_value_box(
        "Exercise",
        paste0("At least ", g$exercise_minutes, " min / day"),
        sum(df$exercise_minutes >= g$exercise_minutes, na.rm = TRUE),
        total_days,
        "🏃",
        "danger"
      ),
      goal_value_box(
        "Calories",
        paste0("At most ", g$calories_eaten, " / day"),
        sum(df$calories_eaten <= g$calories_eaten, na.rm = TRUE),
        total_days,
        "🔥",
        "warning"
      ),
      goal_value_box(
        "Water",
        paste0("At least ", g$water_cups_drank, " cups / day"),
        sum(df$water_cups_drank >= g$water_cups_drank, na.rm = TRUE),
        total_days,
        "💧",
        "info"
      ),
      goal_value_box(
        "Sleep",
        paste0("At least ", g$hours_of_sleep, " hrs / night"),
        sum(df$hours_of_sleep >= g$hours_of_sleep, na.rm = TRUE),
        total_days,
        "🌙",
        "primary"
      ),
      goal_value_box(
        "Fruits & Veggies",
        paste0("At least ", produce_goal, " servings / day"),
        sum(
          (df$fruit_servings + df$vegetable_servings) >= produce_goal,
          na.rm = TRUE
        ),
        total_days,
        "🍎",
        "success"
      )
    )

    date_range <- paste0(
      format(min(df$date), "%b %d"),
      " to ",
      format(max(df$date), "%b %d, %Y")
    )
    overall_pct <- round(mean(
      df$exercise_minutes >= g$exercise_minutes &
        df$calories_eaten <= g$calories_eaten &
        df$water_cups_drank >= g$water_cups_drank &
        df$hours_of_sleep >= g$hours_of_sleep &
        (df$fruit_servings + df$vegetable_servings) >= produce_goal,
      na.rm = TRUE
    ) * 100)

    tagList(
      "Goal Completion Overview",
      do.call(layout_columns, c(list(col_widths = c(2, 2, 2, 2, 2)), cards)),
      p(paste0("ⓘ ", overall_pct, "% of days met every goal • ", date_range))
    )
  })

  # Sleep heatmap
  output$user_sleep_heatmap_plot <- renderPlot({
    if (!logged_in() || nrow(user_data()) == 0) {
      return(NULL)
    }

    sleep_data <- user_data() |>
      mutate(
        week = floor_date(date, "week"),
        day_name = factor(
          weekdays(date),
          levels = c(
            "Sunday",
            "Saturday",
            "Friday",
            "Thursday",
            "Wednesday",
            "Tuesday",
            "Monday"
          )
        )
      ) |>
      group_by(week, day_name) |>
      summarise(
        avg_quality = mean(sleep_quality, na.rm = TRUE),
        .groups = "drop"
      )
    week_breaks <- sort(unique(sleep_data$week))
    week_breaks <- week_breaks[seq(1, length(week_breaks), by = 2)]

    ggplot(sleep_data, aes(x = week, y = day_name, fill = avg_quality)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradientn(
        colors = c("lavender", "plum1", "orchid", "purple", "purple4"),
        limits = c(SLEEP_QUALITY_MIN, SLEEP_QUALITY_MAX),
        name = "Sleep quality"
      ) +
      scale_x_date(
        breaks = week_breaks,
        date_labels = "%b %d",
        expand = expansion(add = days(4))
      ) +
      labs(
        subtitle = paste0(
          "Average Sleep Quality (",
          SLEEP_QUALITY_MIN, " = Poor, ",
          SLEEP_QUALITY_MAX, " = Excellent)"
        ),
        x = NULL,
        y = NULL,
        caption = "X-axis shows logged weeks. Y-axis shows the day of the week for each log entry."
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank(),
        legend.position = "bottom"
      )
  })


  # Water chart
  output$user_water_chart_plot <- renderPlotly({
    if (!logged_in() || nrow(user_data()) == 0) {
      return(plot_ly() |> config(displayModeBar = FALSE))
    }

    monthly <- user_data() |>
      mutate(month = floor_date(date, "month")) |>
      group_by(month) |>
      summarise(
        avg_water = mean(water_cups_drank, na.rm = TRUE),
        .groups = "drop"
      )
    x_range <- c(min(monthly$month) - days(15), max(monthly$month) + days(15))

    plot_ly(monthly, x = ~month) |>
      add_trace(
        y = ~avg_water,
        type = "bar",
        name = "Water (cups)",
        marker = list(color = "lightblue")
      ) |>
      add_trace(
        x = c(min(monthly$month), max(monthly$month) + 15),
        y = c(g$water_cups_drank, g$water_cups_drank),
        type = "scatter",
        mode = "lines",
        name = paste0("Goal (", g$water_cups_drank, " cups)"),
        line = list(color = "steelblue", dash = "dash", width = 2)
      ) |>
      layout(
        xaxis = list(
          title = "",
          tickmode = "array",
          tickvals = monthly$month,
          ticktext = format(monthly$month, "%b"),
          range = x_range
        ),
        yaxis = list(title = "Cups", range = c(0, WATER_YAXIS_MAX)),
        legend = list(orientation = "h")
      ) |>
      config(displayModeBar = FALSE)
  })


  # Produce chart
  output$user_produce_chart_plot <- renderPlotly({
    if (!logged_in() || nrow(user_data()) == 0) {
      return(plot_ly() |> config(displayModeBar = FALSE))
    }

    monthly <- user_data() |>
      mutate(month = floor_date(date, "month")) |>
      group_by(month) |>
      summarise(
        avg_veg = mean(vegetable_servings, na.rm = TRUE),
        avg_fruit = mean(fruit_servings, na.rm = TRUE),
        .groups = "drop"
      )
    x_range <- c(min(monthly$month) - days(15), max(monthly$month) + days(15))

    plot_ly(monthly, x = ~month) |>
      add_trace(
        y = ~avg_veg,
        type = "scatter",
        mode = "lines+markers",
        name = "Vegetables",
        line = list(color = "forestgreen", width = 2.5),
        marker = list(color = "forestgreen", size = 7)
      ) |>
      add_trace(
        y = ~avg_fruit,
        type = "scatter",
        mode = "lines+markers",
        name = "Fruits",
        line = list(color = "orange", width = 2.5),
        marker = list(color = "orange", size = 7)
      ) |>
      add_trace(
        x = c(min(monthly$month), max(monthly$month) + 15),
        y = c(5, 5),
        type = "scatter",
        mode = "lines",
        name = "Goal (5)",
        line = list(color = "grey", dash = "dash", width = 1.5)
      ) |>
      layout(
        xaxis = list(
          title = "",
          tickmode = "array",
          tickvals = monthly$month,
          ticktext = format(monthly$month, "%b"),
          range = x_range
        ),
        yaxis = list(title = "Servings", range = c(0, PRODUCE_YAXIS_MAX)),
        legend = list(orientation = "h")
      ) |>
      config(displayModeBar = FALSE)
  })

  # Overall summary (text interpretation for each plot)
  output$user_overall_summary <- renderUI({
    if (!logged_in() || nrow(user_data()) == 0) {
      return(p("Log data to see your overall summary."))
    }

    df <- user_data()
    g <- tryCatch(load_user_goals(current_user()), error = function(e) default_goals())

    avg_sleep   <- round(mean(df$hours_of_sleep, na.rm = TRUE), 1)
    avg_quality <- round(mean(df$sleep_quality, na.rm = TRUE), 1)
    avg_water   <- round(mean(df$water_cups_drank, na.rm = TRUE), 1)
    pct_water   <- round(mean(df$water_cups_drank >= g$water_cups_drank, na.rm = TRUE) * 100)
    avg_veg     <- round(mean(df$vegetable_servings, na.rm = TRUE), 1)
    avg_fruit   <- round(mean(df$fruit_servings, na.rm = TRUE), 1)
    pct_goals   <- round(mean(
      df$exercise_minutes >= g$exercise_minutes &
        df$water_cups_drank >= g$water_cups_drank &
        df$hours_of_sleep >= g$hours_of_sleep &
        (df$fruit_servings + df$vegetable_servings) >=
          (g$fruit_servings + g$vegetable_servings),
      na.rm = TRUE
    ) * 100)

    sleep_msg <- if (avg_sleep >= g$hours_of_sleep) {
      paste0(
        "You average ", avg_sleep, " hrs of sleep with a quality rating of ",
        avg_quality, "/5, consistently meeting your rest goal."
      )
    } else {
      paste0(
        "You average ", avg_sleep, " hrs of sleep with a quality rating of ",
        avg_quality, "/5. Try to get closer to your ", g$hours_of_sleep, "-hr target."
      )
    }

    water_msg <- if (pct_water >= PCT_ON_TRACK) {
      paste0(
        "You drink ", avg_water, " cups per day on average, hitting your goal on ",
        pct_water, "% of days — great hydration consistency."
      )
    } else {
      paste0(
        "You drink ", avg_water, " cups per day on average, meeting your goal on only ",
        pct_water, "% of days. Aim for more consistent daily intake."
      )
    }

    produce_goal <- g$fruit_servings + g$vegetable_servings
    produce_msg <- if ((avg_veg + avg_fruit) >= produce_goal) {
      paste0(
        "You average ", avg_veg, " veg and ", avg_fruit,
        " fruit servings per day, on track with your produce goal."
      )
    } else {
      paste0(
        "You average ", avg_veg, " veg and ", avg_fruit,
        " fruit servings per day, below your ", produce_goal,
        "-serving goal. Try adding more variety to your meals."
      )
    }

    habits_msg <- if (pct_goals >= PCT_HABITS_GREAT) {
      paste0(
        "You met all four core goals on ", pct_goals,
        "% of days — excellent overall habit consistency."
      )
    } else if (pct_goals >= PCT_HABITS_OK) {
      paste0(
        "You met all four core goals on ", pct_goals,
        "% of days — solid progress with room to improve."
      )
    } else {
      paste0(
        "You met all four core goals on ", pct_goals,
        "% of days. Focus on building more consistent daily habits."
      )
    }

    layout_columns(
      card(card_header("Sleep"), p(sleep_msg)),
      card(card_header("Water"), p(water_msg)),
      card(card_header("Produce"), p(produce_msg)),
      card(card_header("Daily Habits"), p(habits_msg))
    )
  })

  # Habit calendar
  output$user_habit_calendar <- renderPlot({
    if (!logged_in() || nrow(user_data()) == 0) {
      return(NULL)
    }

    g <- tryCatch(load_user_goals(current_user()), error = function(e) {
      default_goals()
    })

    cal_data <- user_data() |>
      mutate(
        week = floor_date(date, "week"),
        day_name = factor(
          weekdays(date),
          levels = c(
            "Sunday",
            "Saturday",
            "Friday",
            "Thursday",
            "Wednesday",
            "Tuesday",
            "Monday"
          )
        ),
        goals_met = (exercise_minutes >= g$exercise_minutes) +
          (water_cups_drank >= g$water_cups_drank) +
          (hours_of_sleep >= g$hours_of_sleep) +
          ((fruit_servings + vegetable_servings) >=
            (g$fruit_servings + g$vegetable_servings)),
        status = case_when(
          goals_met >= N_GOALS_TRACKED ~ "Goal Met",
          goals_met >= N_GOALS_PARTIAL ~ "Partial",
          TRUE ~ "Not Met"
        ),
        status = factor(status, levels = c("Goal Met", "Partial", "Not Met"))
      )
    week_breaks <- sort(unique(cal_data$week))
    week_breaks <- week_breaks[seq(1, length(week_breaks), by = 2)]

    pct_overall <- round(mean(cal_data$goals_met >= N_GOALS_TRACKED) * 100)

    ggplot(cal_data, aes(x = week, y = day_name, fill = status)) +
      geom_tile(color = "white", linewidth = 0.7) +
      scale_fill_manual(
        values = c(
          "Goal Met" = "forestgreen",
          "Partial" = "gold",
          "Not Met" = "grey80"
        ),
        name = NULL
      ) +
      scale_x_date(
        breaks = week_breaks,
        date_labels = "%b %d",
        expand = expansion(add = days(4))
      ) +
      labs(
        title = "Habit Tracker Calendar",
        x = NULL,
        y = NULL,
        caption = paste0(
          "You met your goals on ",
          pct_overall,
          "% of all days."
        )
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank(),
        legend.position = "top"
      )
  })

  # Log form
  output$log_form <- renderUI({
    if (!logged_in()) {
      return(empty_state("Log in to start tracking your daily habits."))
    }

    tagList(
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2),
        value_box(
          title = "Exercise",
          value = "Train",
          showcase = bs_icon("activity"),
          p("Minutes and steps"),
          theme = "danger"
        ),
        value_box(
          title = "Calories",
          value = "Fuel",
          showcase = bs_icon("fire"),
          p("Daily intake"),
          theme = "warning"
        ),
        value_box(
          title = "Water",
          value = "Hydrate",
          showcase = bs_icon("droplet"),
          p("Cups per day"),
          theme = "info"
        ),
        value_box(
          title = "Sleep",
          value = "Rest",
          showcase = bs_icon("moon-stars"),
          p("Hours and quality"),
          theme = "primary"
        ),
        value_box(
          title = "Fruits & Veggies",
          value = "Green",
          showcase = bs_icon("basket"),
          p("Daily servings"),
          theme = "success"
        )
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          card_header(bs_icon("activity"), " Activity"),
          dateInput("date", "Date", value = Sys.Date()),
          numericInput("exercise_minutes", "Exercise minutes", 30, min = 0),
          numericInput("steps", "Steps", 8000, min = 0),
          selectInput(
            "type_of_exercise",
            "Exercise type",
            c("Cardio", "Strength", "Flexibility")
          ),
          selectInput(
            "intensity_of_exercise",
            "Intensity",
            c("Low", "Moderate", "High")
          )
        ),
        card(
          card_header(bs_icon("droplet"), " Nutrition"),
          numericInput("calories_eaten", "Calories eaten", 2000, min = 0),
          numericInput("water_cups_drank", "Water cups", 8, min = 0),
          numericInput("fruit_servings", "Fruit servings", 2, min = 0),
          numericInput("vegetable_servings", "Vegetable servings", 3, min = 0)
        ),
        card(
          card_header(bs_icon("moon-stars"), " Recovery"),
          numericInput("hours_of_sleep", "Hours of sleep", 7, min = 0, max = 24),
          numericInput(
            "sleep_quality",
            paste0("Sleep quality (", SLEEP_QUALITY_MIN, "-", SLEEP_QUALITY_MAX, ")"),
            3, min = SLEEP_QUALITY_MIN, max = SLEEP_QUALITY_MAX
          ),
          numericInput("meditation_minutes", "Meditation minutes", 0, min = 0),
          numericInput("pages_read", "Pages read", 0, min = 0),
          numericInput("weight", "Weight (kg)", 70, min = 0),
          numericInput("muscle_percentage", "Muscle %", 35, min = 0, max = 100)
        )
      ),
      br(),
      actionButton("submit", "Save Entry", class = "btn-danger")
    )
  })

  # Log entry submission
  observeEvent(input$submit, {
    req(logged_in())

    new_row <- data.frame(
      date = as.character(input$date),
      calories_eaten = input$calories_eaten,
      water_cups_drank = input$water_cups_drank,
      hours_of_sleep = input$hours_of_sleep,
      pages_read = input$pages_read,
      exercise_minutes = input$exercise_minutes,
      steps = input$steps,
      fruit_servings = input$fruit_servings,
      vegetable_servings = input$vegetable_servings,
      weight = input$weight,
      muscle_percentage = input$muscle_percentage,
      type_of_exercise = input$type_of_exercise,
      intensity_of_exercise = input$intensity_of_exercise,
      protein_grams = NA,
      carb_grams = NA,
      fat_grams = NA,
      sleep_quality = input$sleep_quality,
      meditation_minutes = input$meditation_minutes
    )

    log_path <- user_log_path(current_user())

    if (file.exists(log_path)) {
      existing <- read.csv(log_path) |>
        filter(date != as.character(input$date))
      write.csv(rbind(existing, new_row), log_path, row.names = FALSE)
    } else {
      write.csv(new_row, log_path, row.names = FALSE)
    }

    goals_df <- tryCatch(load_user_goals(current_user()), error = function(e) {
      data.frame()
    })
    full_log <- load_user_log(current_user())

    evaluate_badges(current_user(), full_log, goals_df)
    badge_version(badge_version() + 1)
    log_version(log_version() + 1)

    output$feedback <- renderUI({
      validate(need(
        FALSE,
        paste0(
          "Saved for ",
          format(input$date, "%B %d, %Y"),
          "."
        )
      ))
    })
  })

  output$log_title <- renderUI({
    "Log Your Data"
  })
}
