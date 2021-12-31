library(gt)
library(xts)
library(zoo)
library(glue)
library(dplyr)
library(tidyr)
library(waffle)
library(ggplot2)
library(stringr)
library(janitor)
library(ggthemes)
library(kableExtra)
library(lubridate)
library(tidyverse)
library(futile.logger)

source("globals.R")


workouts <- read_csv("https://raw.githubusercontent.com/aarora79/biomettracker/master/raw_data/exercises_w_details.csv")

col_for_list <- list(exercise=md("**Exercise**"), 
                     muscle_group_or_body_part=md("**Body part/Muscle Group**"))

strength_workout_table <- workouts %>%
  filter(exercise_type=='Strength') %>%
  select(-exercise_type) %>%
  gt(
    #  rowname_col = "exercise",
    #  groupname_col = "type"
  ) %>%
  
  cols_label(.list = col_for_list)


strength_and_conditioning_workout_table <- workouts %>%
  filter(exercise_type=='Strength & Conditioning') %>%
  gt(
    #  rowname_col = "exercise",
    #  groupname_col = "type"
  ) %>%
  select(-exercise_type) %>%
  cols_label(.list = col_for_list)

core_workout_table <- workouts %>%
  filter(exercise_type=='Core') %>%
  gt(
    #  rowname_col = "exercise",
    #  groupname_col = "type"
  ) %>%
  select(-exercise_type) %>%
  cols_label(.list = col_for_list)

warmup_workout_table <- workouts %>%
  filter(exercise_type=='Warmup') %>%
  gt(
    #  rowname_col = "exercise",
    #  groupname_col = "type"
  ) %>%
  select(-exercise_type) %>%
  cols_label(.list = col_for_list)

# read the data, this comes from the biomettracker repo
# sometimes due to a bug in the weight measurement machine
# the lean mass and weight come as the same value, this is not 
# correct so we set the lean mass to NA in that case.
df_P1 <- read_csv(P1_DATA_URL) %>%
  mutate(Date=ymd(Date)) %>%
  mutate(`Lean Mass` = ifelse(`Lean Mass` == Weight, NA, Weight))
df_P2 <- read_csv(P2_DATA_URL) %>%
  mutate(Date=ymd(Date))%>%
  mutate(`Lean Mass` = ifelse(`Lean Mass` == Weight, NA, Weight))

df_P1 %>% filter(year(Date) == 2020)
df_p1_starting_weight <- df_P1 %>% filter(year(Date) == 2020) %>% filter(Date == min(Date, na.rm=TRUE)) %>% pull(Weight)
df_p2_starting_weight <- df_P2 %>% filter(Date == min(Date, na.rm=TRUE)) %>% pull(Weight)

df_starting_and_target_weights <- data.frame(name=c(P1_NAME, P2_NAME),
                                             Starting=c(df_p1_starting_weight, df_p2_starting_weight),
                                             Target=c(P1_TARGET_WEIGHT, P2_TARGET_WEIGHT),
                                             Ideal=c(P1_IDEAL_WEIGHT, P2_IDEAL_WEIGHT))

df_starting_and_target_weights <- df_starting_and_target_weights %>% 
  gather(metric, value, -name) %>%
  mutate(metric = paste0(metric, " Weight"),
         value = as.numeric(value))

# in this section of the code we will do all our data reading, cleaning, wrangling...basically
# everything except the timeseries forecasting bit so that the rest of the sections simply display the 
# charts based on the data analysis done here. The forecasting is left to its own section later in the code
# because it is based on user input and so it needs to be done redone whenever the input changes.

# read the raw data for person 1, print basic summary and metadata
df_P1 <- read_csv(P1_DATA_URL) %>%
  mutate(name=P1_NAME) %>%
  arrange(Date) %>%
  mutate(Date=ymd(Date)) %>%
  filter(Date >= START_DATE) %>%
  mutate(`Lean Mass` = ifelse(`Lean Mass` == Weight, NA, Weight))
# read the raw data for person 2, ultimately we want to have this dashboard work the same way
# even if there was only person 1 so put the following in an if checl
if(!is.na(P2_NAME)) {
  df_P2 <- read_csv(P2_DATA_URL) %>%
    mutate(name=P2_NAME) %>%
    arrange(Date) %>%
    mutate(Date=ymd(Date)) %>%
    filter(Date >= START_DATE) %>%
    mutate(`Lean Mass` = ifelse(`Lean Mass` == Weight, NA, Weight))
}
# read the important dates csv file. This is needed because we would like to annotate this journey
# so that we can say oh right there was an increase in weight for these days and it followed a birthday party, for example...
if(!is.na(IMPORTANT_DATES_FPATH)) {
  important_dates <- read_csv(IMPORTANT_DATES_FPATH)
}
# combine the dataframes, we want to do a side by side analysis for both people
if(!is.na(df_P2)) {
  df <- bind_rows(df_P1, df_P2)
} else {
  df <- df_P1
}
# get the data in tidy format i.e. Each variable must have its own column.
# Each observation must have its own row.
# Each value must have its own cell.
# see https://r4ds.had.co.nz/tidy-data.html
df_tidy <- df %>%
  gather(metric, value, -Date, -name) %>%
  mutate(value=as.numeric(value))

# determine the per day weight loss dataframe by
# calculating loss as weight - the one previous value of weight
# this is done by first grouping the dataframe by name since it has
# data for two people and then arranging by date while maintaining
# the grouping (NOTE: .by_group=TRUE)
df_wt_loss <- df_tidy %>%
  filter(metric=="Weight") %>%
  select(name, Date, value) %>%
  group_by(name) %>%
  arrange(Date, .by_group=TRUE) %>%
  mutate(loss_per_day = -1*(value-lag(value, 1)))  %>%
  mutate(loss_per_day_7_day_ma=rollapply(loss_per_day, 7, mean,align='right',fill=NA))
# is the curse of the weekend real? Assign the day to each date so that we can determine
# if say the weight loss eventually after the weekend was very less or maybe not even there...
df_wt_loss <- df_wt_loss %>%
  mutate(day = weekdays(as.Date(Date)))
# determine how much of theweight loss target has been achieved, this is done by finding the starting
# weight (configured), target weight (configured) and seeing how far each person has reached based on
# what their current weight is. This percentage is used to display a gauge (like the needle of a speedometer)
p1_starting_weight <- df_tidy %>% filter(name==P1_NAME & metric=="Weight") %>% head(1) %>% pull(value)
p1_latest_weight <- df_tidy %>% filter(name==P1_NAME & metric=="Weight") %>% tail(1) %>% pull(value)
# weight loss would be negative when calculated so multiply by -1
p1_wt_lost_as_pct <- -1*100*((p1_latest_weight-p1_starting_weight)/p1_starting_weight)
p2_starting_weight <- df_tidy %>% filter(name==P2_NAME & metric=="Weight") %>% head(1) %>% pull(value)
p2_latest_weight <- df_tidy %>% filter(name==P2_NAME & metric=="Weight") %>% tail(1) %>% pull(value)
p2_wt_lost_as_pct <- -1*100*((p2_latest_weight-p2_starting_weight)/p2_starting_weight)
p1_target_achieved_pct <- (p1_starting_weight-p1_latest_weight)/(p1_starting_weight-P1_TARGET_WEIGHT)*100
p2_target_achieved_pct <- (p2_starting_weight-p2_latest_weight)/(p2_starting_weight-P2_TARGET_WEIGHT)*100
# daily weight loss, this is important for a lot of charts and tables
# not the use of group by (name) and then lag. The dataframe is already sorted
# in asc order of time, so if the weight is reducing the daily_wt_loss would be a 
# -ve number, for several charts and tables this is multiplied with -1 so provide
# the absolute loss
df_daily_wt_loss <- df_tidy %>%
  filter(metric == "Weight") %>%
  group_by(name) %>%
  mutate(daily_wt_loss = value - lag(value))
# how many days did it take for each pound to drop? This is found by finding the max date i.e. the last date
# on which each weight (as a whole number, so 230, 229 etc) was seen and then subtracting that date from
# the last date of the previous highest weight. So if 230 was say the 20th pound to drop (if we started from 250 say)
# then the number of days between 231 and 230 becomes the number of days it took to lose the 20th pound.
df_days_to_next_drop <- df_daily_wt_loss %>%
  mutate(value = floor(value)) %>%
  ungroup() %>%
  group_by(name, value) %>%
  summarize(Date=max(Date)) %>%
  arrange(desc(Date)) %>%
  mutate(value_diff=value-lag(value), days=abs(as.numeric(Date-lag(Date)))) %>%
  replace_na(list(value_diff = 0, days = 0)) %>%
  mutate(value=value-min(value)) %>%
  filter(value != 0)
# read the precalculated forecasts and target achievement data 
# this is needed because shinyapps.io does not support Prophet (in the sense there are errors in installing it)
df_forecast_p1 <- read_csv(P1_FORECAST_FPATH) %>%
  select(y, yhat, yhat_lower, yhat_upper, ds) %>%
  mutate(ds=as.Date(ds)) %>%
  left_join(df_tidy %>%
              select(Date, metric, value, name) %>%
              filter(name==P1_NAME & metric == "Weight") %>%
              group_by(Date) %>%
              filter(value==min(value)) %>%
              ungroup(),
            by = c("ds"="Date")) %>%
  mutate(y = value, ds=ymd(ds)) %>%
  select(-metric, -value, -name)


df_target_achieved_p1 <- read_csv(P1_TARGET_ACHIEVED_FPATH)
df_forecast_p2 <- read_csv(P2_FORECAST_FPATH) %>%
  select(y, yhat, yhat_lower, yhat_upper, ds) %>%
  mutate(ds=as.Date(ds)) %>%
  left_join(df_tidy %>% 
              select(Date, metric, value, name) %>% 
              filter(name==P2_NAME & metric == "Weight") %>%
              group_by(Date) %>%
              filter(value==min(value)) %>%
              ungroup(),
            by = c("ds"="Date")) %>%
  mutate(y = value, ds=ymd(ds)) %>%
  select(-metric, -value, -name)


df_target_achieved_p2 <- read_csv(P2_TARGET_ACHIEVED_FPATH)
# read body measurements file
df_measurements <- read_csv(MEASUREMENTS_FPATH)
df_measurements <- df_measurements %>%
  filter(measurement %in% MEASUREMENTS_TO_KEEP)

# table for different types of exercises
df_exercises <- read_csv(EXERCISES_URL) 

# exercise dates for calendar plot
df_exercise_dates <- read_csv(EXERCISE_DATES_URL)  %>%
  mutate(date=ymd(date)) 
df_deadlifts <- read_csv(P2_DEADLIFT_URL)

# references
df_references <- read_csv(REFERENCES_URL)


# clean eating list
df_clean_eating_list <- read_csv(CLEAN_EATING_URL) %>%
  replace_na(list(`(Optional) Notes` = ""))

# break up of days, how many days did we lose wieght, gain weight, no change
p1_days_counts <- df_wt_loss %>%
  filter(name == P1_NAME) %>%
  ungroup() %>%
  select(Date, loss_per_day) %>%
  drop_na() %>%
  mutate(category = case_when(
    loss_per_day > 0 ~ "Weight Loss",
    loss_per_day < 0 ~ "Weight Gain",
    loss_per_day == 0 ~ "No Change"
  )) %>%
  count(category, sort=FALSE) %>%
  mutate(category_label = glue("{category} ({n} days)")) %>%
  select(-category)

p1_days_total <- sum(p1_days_counts$n)
p1_days_counts_as_list <- unlist(p1_days_counts$n)
names(p1_days_counts_as_list) <- p1_days_counts$category_label

p2_days_counts <- df_wt_loss %>%
  filter(name == P2_NAME) %>%
  ungroup() %>%
  select(Date, loss_per_day) %>%
  drop_na() %>%
  mutate(category = case_when(
    loss_per_day > 0 ~ "Weight Loss",
    loss_per_day < 0 ~ "Weight Gain",
    loss_per_day == 0 ~ "No Change"
  )) %>%
  count(category, sort=FALSE) %>%
  mutate(category_label = glue("{category} ({n} days)")) %>%
  select(-category)

p2_days_total <- sum(p2_days_counts$n)
p2_days_counts_as_list <- unlist(p2_days_counts$n)
names(p2_days_counts_as_list) <- p2_days_counts$category_label

workouts <- read_csv("https://raw.githubusercontent.com/aarora79/biomettracker/master/raw_data/workout_tracker_data.csv")

workouts <- workouts %>%
  janitor::clean_names() %>%
  select(strength, strength_conditioning, core, warmup)

workouts <- tibble(exercise_type="Strength",
                   exercise=workouts$strength) %>%
  bind_rows(tibble(exercise_type="Strength & Conditioning",
                   exercise=workouts$strength_conditioning)) %>%
  bind_rows(tibble(exercise_type="Core",
                   exercise=workouts$core)) %>%
  bind_rows(tibble(exercise_type="Warmup",
                   exercise=workouts$warmup)) %>%
  drop_na()

muscle_group <- c("Biceps", "Lower back", "Calf", "Lats", "Triceps", "Thighs")
workouts$muscle_group <- sample(muscle_group, nrow(workouts), replace=TRUE)

col_for_list <- list(exercise_type=md("**exercise**"), 
                     'Muscle Group'=md("**Muscle Group**"))

strength_workout_table <- workouts %>%
  filter(exercise_type=='Strength') %>%
  rename('Muscle Group' = muscle_group) %>%
  gt(
    rowname_col = "exercise",
    groupname_col = "exercise_type"
  ) %>%
  cols_label(.list = col_for_list)

strength_and_conditioning_workout_table <- workouts %>%
  filter(exercise_type=='Strength & Conditioning') %>%
  rename('Muscle Group' = muscle_group) %>%
  gt(
    rowname_col = "exercise",
    groupname_col = "exercise_type"
  ) %>%
  cols_label(.list = col_for_list)

core_workout_table <- workouts %>%
  filter(exercise_type=='Core') %>%
  rename('Muscle Group' = muscle_group) %>%
  gt(
    rowname_col = "exercise",
    groupname_col = "exercise_type"
  ) %>%
  cols_label(.list = col_for_list)


warmup_workout_table <- workouts %>%
  filter(exercise_type=='Warmup') %>%
  rename('Muscle Group' = muscle_group) %>%
  gt(
    rowname_col = "exercise",
    groupname_col = "exercise_type"
  ) %>%
  cols_label(.list = col_for_list)

y <- 2021
df_exercise_dates <- df_exercise_dates %>%
  filter(year(date) == y) %>%
  gather(name, exercise, -date) %>%
  rename(Date=date)

df <- df %>%
  filter(year(Date) == y) %>%
  left_join(df_exercise_dates, by=c("Date", "name")) %>%
  mutate(exercise = ifelse(is.na(exercise), 0, exercise))

df <- df %>%
  left_join(df_wt_loss %>%
              filter(year(Date) == y) %>%
              select(Date, name, loss_per_day) %>%
              group_by(Date, name) %>% 
              filter(loss_per_day==max(loss_per_day)), by=c("Date", "name")) %>%
  rename(DateCol=Date, ValueCol=loss_per_day) %>%
  mutate(ValueCol = ifelse(is.na(ValueCol), 0, ValueCol))

View(df)
#View(df_wt_loss %>% filter(name == P2_NAME))
dfPlot <- df %>% 
  mutate(weekday = wday(DateCol, label = T, week_start = 7), # can put week_start = 1 to start week on Monday
         month = month(DateCol, label = T),
         date = yday(DateCol),
         week = week(DateCol))
View(dfPlot)
# isoweek makes the last week of the year as week 1, so need to change that to week 53 for the plot
dfPlot$week[dfPlot$month=="Dec" & dfPlot$week ==1] = 53 

dfPlot <- dfPlot %>% 
  group_by(month) %>% 
  mutate(monthweek = 1 + week - min(week))

View(dfPlot)
if (y == 2020) {
  subtitle <- "Daily weight tracking started Feb 17, 2020. Exercise days marked as [E]."
} else {
  subtitle <- "Exercise days marked as [E]."
}

#View(dfPlot %>% filter(name == P2_NAME))
dfPlot %>%
  filter(name == P2_NAME) %>%
  ggplot(aes(weekday,-week, fill = ValueCol)) +
  geom_tile(colour = "white")  + 
  # geom_point(data=dfPlot %>% filter(exercise==1), aes(weekday, -week), position="dodge2") + 
  geom_text(aes(label = ifelse(exercise == 1, glue("{day(DateCol)} [E]"), day(DateCol))), size = 4, color = "black") + # day(DateCol)
  #geom_text(aes(label = ifelse(exercise == 1, glue("{round(ValueCol, 2)} [I]"), round(ValueCol, 2))), size = 2.5, color = "black") + # day(DateCol)
  theme(aspect.ratio = 1/2,
        legend.position = "top",
        legend.key.width = unit(3, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title.align = 0.5,
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 20),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold",
                                  margin = margin(0,0,0.5,0, unit = "cm"))) +
  scale_fill_gradientn(colours = c("#6b9235", "white", "red"),
                       values = scales::rescale(c(-1, -0.05, 0, 0.05, 1)), 
                       name = "Weight Change (lb), red = weight loss, green = weight gain",
                       guide = guide_colorbar(title.position = "top", 
                                              direction = "horizontal")) +
  facet_wrap(~month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Daily Weight Change Tracker", subtitle=subtitle) +
  theme(plot.subtitle = element_text(size=15, face = "bold"))


df_deadlifts

df_daily_wt_loss %>%
  mutate(week=as.numeric(round(difftime(Date, min(df_daily_wt_loss$Date), unit="week"))))
# mutate(week=week(Date)-week(min(df_daily_wt_loss$Date))+1)

df_weekly_weight <- df_daily_wt_loss %>%
  mutate(week=1+as.numeric(round(difftime(Date, min(df_daily_wt_loss$Date), unit="week"))))
# mutate(week=week(Date)-week(min(df_daily_wt_loss$Date))+1)
df_plateau2 <- read_csv(WT_LOSS_PLATEAUS_URL)

df_weekly_weight %>%
  filter(week <= 52) %>%
  ggplot(aes(x=as.factor(week), y=value, col=name)) +
  geom_boxplot() +
  geom_rect(data=df_plateau2, aes(x = NULL,y = NULL,xmin=week_start, xmax=week_end, 
                                  ymin=-Inf, ymax=Inf, 
                                  alpha=0.05,fill=name, col=name)) +
  labs(title="Boxplot for weekly weight trend",
       subtitle="Shaded regions represent weeks where weight loss plateaued. The line in the center of each box represents the median weight in that week.",
       caption="Data Source: data collected at home") +
  facet_wrap(~name, scales="free", ncol=1) +
  theme_fivethirtyeight() + 
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #scale_x_continuous(breaks = scales::pretty_breaks())
  scale_color_tableau() +
  scale_fill_tableau() +
  theme(legend.title = element_blank(), legend.position = "none",
        text = element_text(size=CHART_ELEMENT_TEXT_SIZE-10)) +
  theme(axis.title = element_text()) + 
  xlab("Weeks since start of training") + 
  ylab("Weight (lb)")


library(png)
library(grid)
getwd()
img <- readPNG("pictures/sep6_2021.png")
grid.raster(img)

library(jpeg)
library(grid)
img <- readJPEG("pictures/sep6_2021.png")
grid.raster(img)


df_bw <- read_csv(P2_DATA_URL) %>%
  mutate(Date=ymd(Date), Weight=as.numeric(Weight))
df_deadlifts <- read_csv(P2_DEADLIFT_URL) %>%
  mutate(date=ymd(date))

# get the max deadlift on a particular data
df_deadlifts_only <- df_deadlifts %>%
  filter(excercise == "deadlift")

df_deadlift_max <- df_deadlifts_only %>%
  group_by(date) %>%
  summarize(weight = max(weight, na.rm=TRUE)) %>%
  arrange(desc(date))

# join with the weight data
df_deadlift_and_bodyweight <- df_deadlift_max %>%
  left_join(df_bw %>% select(Date, Weight) %>% 
              rename(bodyweight=Weight), by=c("date" = "Date")) %>%
  mutate(dl_to_bw_ratio = round(weight/bodyweight, 4))

title <- "Striving towards a deadlift goal of 400 lb..."
maxbw <- max(df_deadlift_and_bodyweight$bodyweight)
minbw <- min(df_deadlift_and_bodyweight$bodyweight)

maxdl <- max(df_deadlift_and_bodyweight$weight)
mindl <- min(df_deadlift_and_bodyweight$weight)

subtitle <- glue("Starting bodyweight={maxbw} lb, deadlift PR={mindl} lb, current bodyweight={minbw} lb, deadlift PR={maxdl} lb!")
journey_text <- glue("{min(df_deadlift_and_bodyweight$weight)} lb to {max(df_deadlift_and_bodyweight$weight)} lb deadlift in {ceiling(difftime(max(df_deadlift_and_bodyweight$date), min(df_deadlift_and_bodyweight$date), units='days')/30)} months")

options(repr.plot.width=20, repr.plot.height=8)
RATIO_GOAL <- 2
RATIO_STRETCH_GOAL <- 400/185
two_x_text <- glue("Body weight 185 lb, Deadlift weight 370 lb")
fh_text <- glue("Body weight 185 lb, Deadlift weight 400 lb")

p <- df_deadlift_and_bodyweight %>%
  ggplot(aes(x=date, y=dl_to_bw_ratio)) +
  geom_point() + 
  geom_line() +
  geom_hline(yintercept=RATIO_GOAL, color=I("red")) +
  geom_hline(yintercept=RATIO_STRETCH_GOAL, color=I("red"), linetype="dashed") +
  geom_smooth() +
  geom_segment(aes(x = min(date), y = min(dl_to_bw_ratio), xend = max(date), yend = min(dl_to_bw_ratio)), arrow = arrow(length = unit(0.5, "cm"))) +
  geom_label(aes(x = min(date)+250, y = RATIO_GOAL, label = two_x_text), fill = "lightgreen") +
  geom_label(aes(x = min(date)+250, y = RATIO_STRETCH_GOAL, label = fh_text), fill = "lightgreen") +
  geom_label(aes(x = min(date)+250, y = min(dl_to_bw_ratio), label = journey_text), fill = "lightgreen") +
  scale_y_continuous(breaks=seq(0, 2.5, 0.1), sec.axis = sec_axis(~ . * 1, breaks = seq(0, 2.5, 0.1))) +
  scale_x_date(date_breaks = "3 month", date_labels="%b-%Y") +
  theme_fivethirtyeight() + 
  labs(title=title,
       subtitle=subtitle) +
  theme(legend.position="none") +
  theme(axis.title = element_text(), axis.title.x = element_blank(), text = element_text(size=20)) + 
  xlab("") +
  ylab('Deadlift to Body Weight Ratio')
#p

one_bw_dl <- df_deadlift_and_bodyweight %>%
  filter(between(dl_to_bw_ratio, 1, 1.05)) %>%
  filter(date == min(date) & dl_to_bw_ratio == min(dl_to_bw_ratio))
months_to_one_x_bw_dl <- ceiling(difftime(one_bw_dl$date, min(df_deadlift_and_bodyweight$date), units="days")/30)

one_and_half_bw_dl <- df_deadlift_and_bodyweight %>%
  filter(between(dl_to_bw_ratio, 1.5, 1.55)) %>%
  filter(date == min(date))
months_to_one_and_half_bw_dl <- ceiling(difftime(one_and_half_bw_dl$date, min(df_deadlift_and_bodyweight$date), units="days")/30)

p +  
  annotate(geom = "point", x = min(df_deadlift_and_bodyweight$date), y = min(df_deadlift_and_bodyweight$dl_to_bw_ratio), colour = "cadetblue1", size = 3) +
  annotate(geom = "point", x = one_bw_dl$date, y = one_bw_dl$dl_to_bw_ratio, colour = "red", size = 3) +
  annotate(geom = "text", x = one_bw_dl$date, y = one_bw_dl$dl_to_bw_ratio-0.05, , label = glue("1 x body weight, {months_to_one_x_bw_dl} months"), hjust = "left") +
  annotate(geom = "text", x = one_and_half_bw_dl$date, y = one_and_half_bw_dl$dl_to_bw_ratio+0.06, , label = glue("1.5 x body weight, {months_to_one_and_half_bw_dl} months"), hjust = "left") +
  annotate(geom = "point", x = one_and_half_bw_dl$date, y = one_and_half_bw_dl$dl_to_bw_ratio, colour = "blue", size = 3) +
  annotate(geom = "point", x = max(df_deadlift_and_bodyweight$date)-1, y = max(df_deadlift_and_bodyweight$dl_to_bw_ratio), colour = "green", size = 3) +
  annotate(geom = "text", x = max(df_deadlift_and_bodyweight$date)+1, y = max(df_deadlift_and_bodyweight$dl_to_bw_ratio), label = glue("{max(df_deadlift_and_bodyweight$weight)} lb\ndeadlift"), hjust = "left") +
  annotate(geom = "text", x = min(df_deadlift_and_bodyweight$date)-5, y = min(df_deadlift_and_bodyweight$dl_to_bw_ratio), label = glue("{min(df_deadlift_and_bodyweight$weight)} lb\ndeadlift"), hjust = "right") 




title <- "Body weight is going down and deadlift PR is going up..."
maxbw <- max(df_deadlift_and_bodyweight$bodyweight)
minbw <- min(df_deadlift_and_bodyweight$bodyweight)

maxdl <- max(df_deadlift_and_bodyweight$weight)
mindl <- min(df_deadlift_and_bodyweight$weight)

options(repr.plot.width=20, repr.plot.height=8)

df_dl_and_bw_tidy <- df_deadlift_and_bodyweight %>%
  select(-dl_to_bw_ratio) %>%
  rename(`deadlift weight`=weight, `body weight`=bodyweight) %>%
  gather(k, v, -date)

p <- df_dl_and_bw_tidy %>%
  ggplot(aes(x=date, y=v, col=k)) +
  geom_point(size=2, show.legend=FALSE) + 
  geom_line(size=1) +
  scale_y_continuous(breaks=seq(min(df_dl_and_bw_tidy$v, na.rm=TRUE), max(df_dl_and_bw_tidy$v, na.rm=TRUE), 20)) +
  scale_x_date(date_breaks = "3 month", date_labels="%b-%Y") +
  scale_color_tableau() +
  theme_fivethirtyeight() + 
  geom_segment(aes(xend = min(date), yend = min(df_deadlift_and_bodyweight$weight), x = min(date), y = max(df_deadlift_and_bodyweight$bodyweight)), arrow = arrow(length = unit(0.3, "cm")), color="red") +
  geom_segment(aes(x = max(date), y = min(df_deadlift_and_bodyweight$bodyweight), xend = max(date), yend = max(df_deadlift_and_bodyweight$weight)), arrow = arrow(length = unit(0.3, "cm")), color="darkgreen") +
  labs(title=title,
       subtitle=subtitle) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(axis.title = element_text(), axis.title.x = element_blank(), text = element_text(size=20)) + 
  xlab("") +
  ylab('Weight (lb)')
p +
  annotate(geom = "text", x = max(df_deadlift_and_bodyweight$date)+2, y = max(df_deadlift_and_bodyweight$weight), label = glue("{max(df_deadlift_and_bodyweight$weight)} lb\ndeadlift"), hjust = "left") +
  annotate(geom = "text", x = min(df_deadlift_and_bodyweight$date)-5, y = min(df_deadlift_and_bodyweight$weight), label = glue("{min(df_deadlift_and_bodyweight$weight)} lb\ndeadlift"), hjust = "right") +
  annotate(geom = "text", x = max(df_deadlift_and_bodyweight$date)+1, y = min(df_deadlift_and_bodyweight$bodyweight), label = glue("{min(df_deadlift_and_bodyweight$bodyweight)} lb\nbodyweight"), hjust = "left") +
  annotate(geom = "text", x = min(df_deadlift_and_bodyweight$date)-4, y = max(df_deadlift_and_bodyweight$bodyweight), label = glue("{max(df_deadlift_and_bodyweight$bodyweight)} lb\nbodyweight"), hjust = "right") 


df_P1
df_p1_starting_weight <- df_P1 %>% filter(Date == min(Date, na.rm=TRUE)) %>% pull(Weight)

df_starting_and_target_weights
df_p1_starting_weight <- df_P1 %>% filter(Date == min(Date, na.rm=TRUE)) %>% pull(Weight)
df_p2_starting_weight <- df_P2 %>% filter(Date == min(Date, na.rm=TRUE)) %>% pull(Weight)

df_starting_and_target_weights <- data.frame(name=c(P1_NAME, P2_NAME),
                                             Starting=c(df_p1_starting_weight, df_p2_starting_weight),
                                             Target=c(P1_TARGET_WEIGHT, P2_TARGET_WEIGHT),
                                             Ideal=c(P1_IDEAL_WEIGHT, P2_IDEAL_WEIGHT))

df_starting_and_target_weights <- df_starting_and_target_weights %>% 
  gather(metric, value, -name) %>%
  mutate(metric = paste0(metric, " Weight"),
         value = as.numeric(value))
