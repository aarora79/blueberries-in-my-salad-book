# Global constants
P1_NAME <- "Nidhi"
P2_NAME <- "Amit"

P1_DATA_URL <- glue("https://raw.githubusercontent.com/aarora79/biomettracker/master/data/{P1_NAME}.csv")
P2_DATA_URL <- glue("https://raw.githubusercontent.com/aarora79/biomettracker/master/data/{P2_NAME}.csv")
IMPORTANT_DATES_FPATH <- "https://raw.githubusercontent.com/aarora79/biomettracker/master/data/important_dates.csv"
P1_FORECAST_FPATH <- glue("https://raw.githubusercontent.com/aarora79/biomettracker/master/data/forecast_{P1_NAME}.csv")
P2_FORECAST_FPATH <- glue("https://raw.githubusercontent.com/aarora79/biomettracker/master/data/forecast_{P2_NAME}.csv")

P1_TARGET_ACHIEVED_FPATH <- glue("https://raw.githubusercontent.com/aarora79/biomettracker/master/data/target_achievement_{P1_NAME}.csv")
P2_TARGET_ACHIEVED_FPATH <- glue("https://raw.githubusercontent.com/aarora79/biomettracker/master/data/target_achievement_{P2_NAME}.csv")

MEASUREMENTS_FPATH <- "https://raw.githubusercontent.com/aarora79/biomettracker/master/raw_data/measurements.csv"
EXERCISE_DATES_URL <- "https://raw.githubusercontent.com/aarora79/biomettracker/master/raw_data/exercise_dates.csv"

# desired and ideal weights, ideal weight as per NIH website
# https://www.nhlbi.nih.gov/health/educational/healthdisp/pdf/tipsheets/Are-You-at-a-Healthy-Weight.pdf
P1_TARGET_WEIGHT <- 128
P1_IDEAL_WEIGHT <- 127

P2_TARGET_WEIGHT <- 190
P2_IDEAL_WEIGHT <- 158

if(!knitr:::is_html_output()) {
  CHART_ELEMENT_TEXT_SIZE <- 25
}
  

# global constants
# most of these should be configuration parameters

APP_NAME <- "biometric_tracker"
LOG_FILE <- file.path("/tmp", glue("{APP_NAME}.log"))
LOGGER <- APP_NAME
ABOUT_FILE <- "about.md"

START_DATE <- "2020-02-17"
DATA_DIR <- "data"
RAW_DATA_DIR <- "raw_data"
MEASUREMENTS_TO_KEEP <- c("Neck", "Waist", "Hips", "Biceps", "Thighs")

# person 1 and 2 details
P1_NAME_INITIAL <- "N"
P2_NAME_INITIAL <- "A"

P1_TARGET_WEIGHT <- 128
P1_WEIGHT_CAP <- 160
P1_WEIGHT_FLOOR <- 120

P2_TARGET_WEIGHT <- 190
P2_WEIGHT_CAP <- 260
P2_WEIGHT_FLOOR <- 180

CAPTION <- "Source: Daily measurements done @home"
MONTH_ABB <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


# charting related, how far should be the annotation from a point on the graph
NUDGE_X <- 1
NUDGE_Y <- 5
CHART_ELEMENT_TEXT_SIZE <- 20
CHART_ELEMENT_TEXT_SIZE_MOBILE <- 10

MAIN_PAGE_CHART_TITLE <- "Journey to health"
HOW_EACH_POUND_WAS_LOST_TITLE <- "How each pound was lost..."
N_FOR_LAST_N_POUNDS_OF_INTREST <- 10

# gauge
SUCCESS_RANGE <- c(75, 100)
WARNING_RANGE <- c(40, 74)
DANGER_RANGE <- c(0, 39)

# slider inputs
FORECAST_DURATIONS <- c(60, 90, 180, 365)
SELECTED_FORECAST_DURATION <- 180
FRACTION_FOR_MIN_WEIGHT <- 0.7
WEIGHT_SLIDER_STEP_SIZE <- 1
