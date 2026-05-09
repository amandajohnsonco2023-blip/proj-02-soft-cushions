# app/sample_user.R
# Sample User tab server outputs

sample_user_server <- function(input, output, session, sample_df) {
  output$sample_title <- renderUI({
    "Sample User's Tracker Dashboard"
  })

  # Goal completion overview
  output$sample_goal_overview <- renderUI({
    total_days <- nrow(sample_df)
    overall_pct <- round(
      mean(
        sample_df$exercise_minutes >= 30 &
          sample_df$calories_eaten <= 2200 &
          sample_df$water_cups_drank >= 8 &
          sample_df$hours_of_sleep >= 7 &
          (sample_df$fruit_servings + sample_df$vegetable_servings) >= 5,
        na.rm = TRUE
      ) *
        100
    )

    cards <- list(
      goal_value_box(
        "Exercise",
        "At least 30 min / day",
        sum(sample_df$exercise_minutes >= 30, na.rm = TRUE),
        total_days,
        "🏃",
        "danger"
      ),
      goal_value_box(
        "Calories",
        "At most 2,200 / day",
        sum(sample_df$calories_eaten <= 2200, na.rm = TRUE),
        total_days,
        "🔥",
        "warning"
      ),
      goal_value_box(
        "Water",
        "At least 8 cups / day",
        sum(sample_df$water_cups_drank >= 8, na.rm = TRUE),
        total_days,
        "💧",
        "info"
      ),
      goal_value_box(
        "Sleep",
        "At least 7 hrs / night",
        sum(sample_df$hours_of_sleep >= 7, na.rm = TRUE),
        total_days,
        "🌙",
        "primary"
      ),
      goal_value_box(
        "Fruits & Veggies",
        "At least 5 servings / day",
        sum(
          (sample_df$fruit_servings + sample_df$vegetable_servings) >= 5,
          na.rm = TRUE
        ),
        total_days,
        "🍎",
        "success"
      )
    )

    tagList(
      "Goal Completion Overview",
      do.call(layout_columns, c(list(col_widths = c(2, 2, 2, 2, 2)), cards)),
      p(paste0(
        "ⓘ ",
        overall_pct,
        "% of days met every goal • Jan 1 to Jun 29, 2025"
      ))
    )
  })

  # Sleep heatmap
  output$sample_sleep_heatmap_plot <- renderPlot({
    sleep_data <- sample_df |>
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
    month_breaks <- seq(
      floor_date(min(sleep_data$week), "month"),
      floor_date(max(sleep_data$week), "month"),
      by = "1 month"
    )

    ggplot(sleep_data, aes(x = week, y = day_name, fill = avg_quality)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradientn(
        colors = c("lavender", "plum1", "orchid", "purple", "purple4"),
        limits = c(1, 5),
        name = "Sleep quality"
      ) +
      scale_x_date(
        breaks = month_breaks,
        date_labels = "%b",
        expand = expansion(add = days(4))
      ) +
      labs(
        subtitle = "Average Sleep Quality (1 = Poor, 5 = Excellent)",
        x = NULL,
        y = NULL,
        caption = "X-axis shows logged weeks. Y-axis shows the day of the week for each log entry."
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(size = 8, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank(),
        legend.position = "bottom"
      )
  })

  # Water chart
  output$sample_water_chart_plot <- renderPlotly({
    monthly <- sample_df |>
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
        y = c(8, 8),
        type = "scatter",
        mode = "lines",
        name = "Goal (8 cups)",
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
        yaxis = list(title = "Cups", range = c(0, 13)),
        legend = list(orientation = "h")
      ) |>
      config(displayModeBar = FALSE)
  })

  # Produce chart
  output$sample_produce_chart_plot <- renderPlotly({
    monthly <- sample_df |>
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
        yaxis = list(title = "Servings", range = c(0, 8)),
        legend = list(orientation = "h")
      ) |>
      config(displayModeBar = FALSE)
  })

  # Habit calendar
  output$sample_habit_calendar <- renderPlot({
    cal_data <- sample_df |>
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
        goals_met = (exercise_minutes >= 30) +
          (water_cups_drank >= 8) +
          (hours_of_sleep >= 7) +
          ((fruit_servings + vegetable_servings) >= 5),
        status = case_when(
          goals_met >= 4 ~ "Goal Met",
          goals_met >= 2 ~ "Partial",
          TRUE ~ "Not Met"
        ),
        status = factor(status, levels = c("Goal Met", "Partial", "Not Met"))
      )

    pct_overall <- round(mean(cal_data$goals_met >= 4) * 100)

    month_breaks <- seq(
      floor_date(min(cal_data$week), "month"),
      floor_date(max(cal_data$week), "month"),
      by = "1 month"
    )

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
        breaks = month_breaks,
        date_labels = "%b",
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
        axis.text.x = element_text(size = 9, hjust = 0.5),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank(),
        legend.position = "top"
      )
  })

  # Overall summary (text interpretation for each plot)
  output$sample_overall_summary <- renderUI({
    avg_sleep <- round(mean(sample_df$hours_of_sleep, na.rm = TRUE), 1)
    avg_quality <- round(mean(sample_df$sleep_quality, na.rm = TRUE), 1)
    avg_water <- round(mean(sample_df$water_cups_drank, na.rm = TRUE), 1)
    pct_water <- round(
      mean(sample_df$water_cups_drank >= 8, na.rm = TRUE) * 100
    )
    avg_veg <- round(mean(sample_df$vegetable_servings, na.rm = TRUE), 1)
    avg_fruit <- round(mean(sample_df$fruit_servings, na.rm = TRUE), 1)
    pct_goals <- round(
      mean(
        sample_df$exercise_minutes >= 30 &
          sample_df$water_cups_drank >= 8 &
          sample_df$hours_of_sleep >= 7 &
          (sample_df$fruit_servings + sample_df$vegetable_servings) >= 5,
        na.rm = TRUE
      ) *
        100
    )

    sleep_msg <- paste0(
      "The sample user averages ",
      avg_sleep,
      " hrs of sleep with a quality rating of ",
      avg_quality,
      "/5. Sleep quality tends to be higher on weekends and improved ",
      "in the later months."
    )

    water_msg <- if (pct_water >= 80) {
      paste0(
        "The sample user drinks ",
        avg_water,
        " cups per day on average, meeting the ",
        "8-cup goal on ",
        pct_water,
        "% of days — strong hydration habits overall."
      )
    } else {
      paste0(
        "The sample user drinks ",
        avg_water,
        " cups per day on average, meeting the ",
        "8-cup goal on ",
        pct_water,
        "% of days. Hydration was more consistent in ",
        "some months than others."
      )
    }

    produce_msg <- paste0(
      "The sample user averages ",
      avg_veg,
      " veg and ",
      avg_fruit,
      " fruit servings per day. Vegetable intake is closer to the 5-serving goal ",
      "while fruit intake remains consistently below target across all months."
    )

    habits_msg <- paste0(
      "All four core goals (exercise, water, sleep, produce) were met together on ",
      pct_goals,
      "% of days. The calendar shows most days fall in the partial or ",
      "goal-met range, with few complete misses."
    )

    layout_columns(
      card(card_header("Sleep"), p(sleep_msg)),
      card(card_header("Water"), p(water_msg)),
      card(card_header("Produce"), p(produce_msg)),
      card(card_header("Daily Habits"), p(habits_msg))
    )
  })
}
