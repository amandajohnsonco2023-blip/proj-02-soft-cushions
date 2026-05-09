# app/authentication.R

# defined variables to avoid magic numbers and hard-coded paths
LOGS_DIR <- file.path("data", "logs")


user_log_path <- function(username) {
  file.path(LOGS_DIR, paste0(username, ".csv"))
}

load_user_log <- function(username) {
  path <- user_log_path(username)
  if (file.exists(path)) {
    read.csv(path) |> mutate(date = as.Date(date))
  } else {
    sample_cols <- names(read.csv("data/Sample_User_Health_Data.csv"))
    empty <- data.frame(matrix(ncol = length(sample_cols), nrow = 0))
    names(empty) <- sample_cols
    write.csv(empty, path, row.names = FALSE)
    empty
  }
}

# ── Auth server logic ─────────────────────────────────────────────────────────
auth_server <- function(input, output, session, logged_in, current_user) {
  # Login
  observeEvent(input$login_btn, {
    req(input$login_user, input$login_pass)

    users <- read.csv("data/users.csv", stringsAsFactors = FALSE)
    match <- users |> filter(username == input$login_user)

    if (nrow(match) == 0) {
      output$login_status <- renderUI({
        tags$p(class = "text-danger small mt-2", "Username not found.")
      })
      return()
    }

    if (!sodium::password_verify(match$password_hash, input$login_pass)) {
      output$login_status <- renderUI({
        tags$p(class = "text-danger small mt-2", "Incorrect password.")
      })
      return()
    }

    logged_in(TRUE)
    current_user(input$login_user)

    output$login_status <- renderUI({
      tags$p(
        class = "text-success small mt-2",
        paste0("✓ Welcome back, ", input$login_user, "!")
      )
    })
  })

  # Sign up
  observeEvent(input$signup_btn, {
    req(input$signup_user, input$signup_pass, input$signup_pass2)

    if (input$signup_pass != input$signup_pass2) {
      output$signup_status <- renderUI({
        tags$p(class = "text-danger small mt-2", "Passwords do not match.")
      })
      return()
    }

    if (nchar(input$signup_user) < 3) {
      output$signup_status <- renderUI({
        tags$p(
          class = "text-danger small mt-2",
          "Username must be at least 3 characters."
        )
      })
      return()
    }

    users <- read.csv("data/users.csv", stringsAsFactors = FALSE)

    if (input$signup_user %in% users$username) {
      output$signup_status <- renderUI({
        tags$p(class = "text-danger small mt-2", "Username already taken.")
      })
      return()
    }

    hashed <- sodium::password_store(input$signup_pass)
    new_user <- data.frame(
      username = input$signup_user,
      password_hash = hashed,
      stringsAsFactors = FALSE
    )
    write.csv(rbind(users, new_user), "data/users.csv", row.names = FALSE)

    load_user_log(input$signup_user)

    logged_in(TRUE)
    current_user(input$signup_user)

    output$signup_status <- renderUI({
      tags$p(
        class = "text-success small mt-2",
        paste0("✓ Account created! Welcome, ", input$signup_user, "!")
      )
    })
  })
}
