# app/badges.R

# defined variables to avoid magic numbers and hard-coded paths
STREAK_THRESHOLD <- 7
LOG_THRESHOLD <- 30
BADGES_DIR <- "data/badges"

all_badges <- list(
  list(
    id = "first_step",
    name = "First Step",
    desc = "Log your very first entry",
    icon = "flag",
    theme = "pink"
  ),
  list(
    id = "goal_setter",
    name = "Goal Setter",
    desc = "Save your personal goals for the first time",
    icon = "bullseye",
    theme = "purple"
  ),
  list(
    id = "perfect_day",
    name = "Perfect Day",
    desc = "Meet every single goal in one day",
    icon = "stars",
    theme = "bg-gradient-pink-orange"
  ),
  list(
    id = "seven_day_streak",
    name = "7-Day Streak",
    desc = "Log data for 7 consecutive days",
    icon = "calendar-week",
    theme = "teal"
  ),
  list(
    id = "hydration_hero",
    name = "Hydration Hero",
    desc = "Meet your water goal 7 days in a row",
    icon = "droplet",
    theme = "cyan"
  ),
  list(
    id = "sleep_champion",
    name = "Sleep Champion",
    desc = "Meet your sleep goal 7 days in a row",
    icon = "moon-stars",
    theme = "bg-gradient-purple-cyan"
  ),
  list(
    id = "exercise_streak",
    name = "Exercise Streak",
    desc = "Meet your exercise goal 7 days in a row",
    icon = "activity",
    theme = "orange"
  ),
  list(
    id = "thirty_day_logger",
    name = "30-Day Logger",
    desc = "Log data for 30 total days",
    icon = "calendar-check",
    theme = "bg-gradient-teal-cyan"
  )
)

user_badges_path <- function(username) {
  paste0(BADGES_DIR, "/", username, ".csv")
}

load_earned_badges <- function(username) {
  path <- user_badges_path(username)

  if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE)$badge_id
  } else {
    character(0)
  }
}

save_earned_badge <- function(username, badge_id) {
  path <- user_badges_path(username)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  earned <- load_earned_badges(username)

  if (!badge_id %in% earned) {
    df <- data.frame(
      badge_id = c(earned, badge_id),
      earned_on = c(rep(NA, length(earned)), as.character(Sys.Date())),
      stringsAsFactors = FALSE
    )

    write.csv(df, path, row.names = FALSE)
  }
}

evaluate_badges <- function(username, df, goals_df) {
  earned <- load_earned_badges(username)
  newly <- character(0)

  award <- function(id) {
    if (!id %in% earned && !id %in% newly) {
      save_earned_badge(username, id)
      newly <<- c(newly, id)
    }
  }

  if (nrow(df) >= 1) {
    award("first_step")
  }
  if (nrow(df) >= LOG_THRESHOLD) {
    award("thirty_day_logger")
  }

  if (file.exists(user_goals_path(username))) {
    award("goal_setter")
  }

  if (nrow(df) > 0 && nrow(goals_df) > 0) {
    g <- goals_df[1, ]

    df <- df |>
      dplyr::mutate(date = as.Date(date)) |>
      dplyr::arrange(date)

    perfect_days <- df |>
      dplyr::mutate(
        all_met = exercise_minutes >= g$exercise_minutes &
          steps >= g$steps &
          calories_eaten <= g$calories_eaten &
          water_cups_drank >= g$water_cups_drank &
          hours_of_sleep >= g$hours_of_sleep &
          fruit_servings >= g$fruit_servings &
          vegetable_servings >= g$vegetable_servings &
          meditation_minutes >= g$meditation_minutes &
          pages_read >= g$pages_read
      ) |>
      dplyr::pull(all_met)

    if (any(perfect_days, na.rm = TRUE)) {
      award("perfect_day")
    }

    max_consecutive <- function(bool_vec) {
      bool_vec[is.na(bool_vec)] <- FALSE

      if (length(bool_vec) == 0) {
        return(0)
      }

      rle_out <- rle(bool_vec)

      if (!any(rle_out$values)) {
        return(0)
      }

      max(rle_out$lengths[rle_out$values])
    }

    dates_logged <- sort(unique(df$date))
    date_diffs <- as.numeric(diff(dates_logged))

    max_log_streak <- if (length(dates_logged) == 0) {
      0
    } else if (length(dates_logged) == 1) {
      1
    } else {
      max_consec <- 1
      streak <- 1

      for (d in date_diffs) {
        if (d == 1) {
          streak <- streak + 1
          max_consec <- max(max_consec, streak)
        } else {
          streak <- 1
        }
      }

      max_consec
    }

    if (max_log_streak >= STREAK_THRESHOLD) {
      award("seven_day_streak")
    }

    if (max_consecutive(df$water_cups_drank >= g$water_cups_drank) >= STREAK_THRESHOLD) {
      award("hydration_hero")
    }

    if (max_consecutive(df$hours_of_sleep >= g$hours_of_sleep) >= STREAK_THRESHOLD) {
      award("sleep_champion")
    }

    if (max_consecutive(df$exercise_minutes >= g$exercise_minutes) >= STREAK_THRESHOLD) {
      award("exercise_streak")
    }
  }

  newly
}

badges_server <- function(
  input,
  output,
  session,
  logged_in,
  current_user,
  badge_version
) {
  output$badges_title <- renderUI({
    "My Badges"
  })

  output$badges_display <- renderUI({
    if (!logged_in()) {
      return(p("Log in to see and earn badges."))
    }

    badge_version()

    df <- load_user_log(current_user())
    goals_df <- load_user_goals(current_user())

    evaluate_badges(current_user(), df, goals_df)

    badge_file <- user_badges_path(current_user())
    earned_df <- if (file.exists(badge_file)) {
      read.csv(badge_file, stringsAsFactors = FALSE)
    } else {
      data.frame(badge_id = character(0), earned_on = character(0))
    }
    earned <- earned_df$badge_id

    badge_cards <- lapply(all_badges, function(b) {
      badge_earned <- b$id %in% earned
      earned_on <- earned_df$earned_on[earned_df$badge_id == b$id][1]
      status <- if (badge_earned) {
        if (is.na(earned_on) || earned_on == "") {
          "Earned"
        } else {
          paste0("Earned on ", earned_on)
        }
      } else {
        "Not yet earned"
      }

      value_box(
        title = b$name,
        value = if (badge_earned) "Earned" else "Locked",
        showcase = if (badge_earned) bs_icon(b$icon) else bs_icon("lock"),
        p(b$desc),
        p(status),
        theme = if (badge_earned) b$theme else "secondary"
      )
    })

    progress_box <- value_box(
      title = "Badge Progress",
      value = paste0(length(earned), " / ", length(all_badges)),
      showcase = bs_icon("award"),
      p("Earned badges"),
      theme = "primary"
    )

    do.call(
      layout_columns,
      c(
        list(col_widths = c(12, rep(3, length(badge_cards)))),
        list(progress_box),
        badge_cards
      )
    )
  })
}
