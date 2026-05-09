# logs/

This folder contains one CSV file per registered user, storing their daily
health log entries. Files are created automatically when a user logs their
first entry and are named `<username>.csv`.

These files are excluded from version control via `.gitignore` to protect
user privacy. A `.gitkeep` file is included to preserve this folder in Git.

## Schema

Each file contains one row per day logged, with the following columns:

| Column | Type | Description |
|--------|------|-------------|
| `date` | date (`YYYY-MM-DD`) | Calendar date of the log entry |
| `calories_eaten` | integer | Total calories consumed that day (kcal) |
| `water_cups_drank` | integer | Number of cups of water consumed |
| `hours_of_sleep` | numeric | Hours of sleep the previous night |
| `pages_read` | integer | Number of book pages read that day |
| `exercise_minutes` | integer | Total minutes of intentional exercise |
| `steps` | integer | Total step count for the day |
| `fruit_servings` | integer | Number of fruit servings eaten |
| `vegetable_servings` | integer | Number of vegetable servings eaten |
| `weight` | numeric | Body weight recorded that day (kg) |
| `muscle_percentage` | numeric | Body muscle percentage (%) |
| `type_of_exercise` | character | Type of workout: `Cardio`, `Strength`, or `Flexibility` |
| `intensity_of_exercise` | character | Workout intensity: `Low`, `Moderate`, or `High` |
| `protein_grams` | integer | Grams of protein consumed |
| `carb_grams` | integer | Grams of carbohydrates consumed |
| `fat_grams` | integer | Grams of fat consumed |
| `sleep_quality` | integer | Self-rated sleep quality on a 1–5 scale (1 = poor, 5 = excellent) |
| `meditation_minutes` | integer | Minutes spent meditating that day |

Fields not entered by the user on a given day are stored as `NA`.