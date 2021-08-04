library(gt)
library(xts)
library(zoo)
library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(waffle)
library(ggthemes)
library(kableExtra)
library(lubridate)
library(tidyverse)
library(futile.logger)

source("globals.R")
EASTERN_TZ <- "America/New_York"
FASTING_DURATION_IN_HOURS <- 72
df_p1 <- read_csv("https://raw.githubusercontent.com/aarora79/biomettracker/master/raw_data/72hours_fasting_tracker_nidhi.csv")

head(df_p1)
#View(df_p1)
df_p1 <- df_p1 %>%
  mutate(name = P1_NAME) %>%
  mutate(date_hour = ymd_hms(glue("{date} {hour}:00:00"), tz=EASTERN_TZ)) %>%
  filter(difftime(date_hour, min(date_hour), units = "hours") <= FASTING_DURATION_IN_HOURS) 

min(df_p1$date_hour)
max(df_p1$date_hour)
df_p2 <- read_csv("https://raw.githubusercontent.com/aarora79/biomettracker/master/raw_data/72hours_fasting_tracker_amit.csv")

head(df_p2)
#View(df_p2)
df_p2 <- df_p2 %>%
  mutate(name = P2_NAME) %>%
  mutate(date_hour = ymd_hms(glue("{date} {hour}:00:00"), tz=EASTERN_TZ)) %>%
  filter(difftime(date_hour, min(date_hour), units = "hours") <= FASTING_DURATION_IN_HOURS) 



df <- bind_rows(df_p1, df_p2) %>%
  mutate(asleep = case_when(
    is.na(energy_level) & is.na(hunger) & is.na(headache) & is.na(drink1) & is.na(drink2) & is.na(drink3) & is.na(workout) & is.na(sleepy) ~ TRUE,
    TRUE ~ FALSE
  ))


df
#View(df)
df_asleep <- df %>%
  count(name, asleep) %>%
  group_by(name) %>%
  mutate(n_pct = round((100*n)/sum(n), 4))
df_asleep

field <- "energy_level"
df %>%
  filter(!asleep) %>%
  group_by(name) %>%
  count(!!sym(field)) %>%
  mutate(field=field) %>%
  rename(category=!!sym(field), count=n) %>%
  select(name, field, category, count)
  
get_category_counts <- function(field, df) {
  df %>%
    filter(!asleep) %>%
    count(name, !!sym(field)) %>%
    group_by(name) %>%
    mutate(pct = round((100*n)/sum(n), 4)) %>%
    mutate(field=field) %>%
    rename(category=!!sym(field), count=n) %>%
    select(name, field, category, count, pct)
}

col_list <- c("energy_level", "hunger", "headache", "sleepy")
df_category_counts <- map(col_list, get_category_counts, df) %>%
  reduce(bind_rows)

df_category_counts


df_category_counts %>%
  ggplot(aes(x=category, y=pct/100, col=name, fill=name)) +
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~field, scales="free") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_tableau() +
  scale_color_tableau() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("Percentage of time") +
  labs(title=glue("How we did in terms of energy, hunger, headache and sleepiness during the 72 hour water fast?"),
       subtitle="Not too bad! Normal energy levels for 70% or more of the time we were awake with only mild headache for a short duration.",
       caption="Data only includes non-sleeping hours") +
  theme(legend.position = "bottom", legend.title = element_blank())




energy_level_to_numeric <- function(x) {
  case_when(
    x == "sleeping" ~ 0,
    x == "low energy level" ~ -1,
    x == "normal energy level" ~ 1,
    x == "higher than normal energy level" ~ 2,
    TRUE ~ 0
  )
}

unique(df$hunger)
hunger_to_numeric <- function(x) {
  case_when(
    x == "sleeping" ~ 0,
    x == "not hungry at all" ~ -1,
    x == "somewhat hungry" ~ 1,
    x == "not hungry but want to eat" ~ 2,
    x == "extremely hungry" ~ 2,
    TRUE ~ 0
  )
}




df %>%
  #filter(name == "Amit") %>%
  group_by(name) %>%
  arrange(date_hour) %>%
  mutate(hour = as.numeric(difftime(date_hour, min(date_hour), units = "hour"))) %>%
  mutate(hour = factor(hour, levels=seq(min(hour), max(hour), 1))) %>%
  mutate(energy_level = factor(energy_level, c("sleeping", "low energy level", "normal energy level", "higher than normal energy level"), ordered = TRUE)) %>%
  replace_na(list(energy_level="sleeping")) %>%
  #filter(!asleep) %>%
  #mutate(energy_level = factor(energy_level, c("sleeping", "low energy level", "normal energy level", "higher than normal energy level"), ordered = TRUE)) %>%
  #mutate(energy_level = energy_level_to_numeric(energy_level)) %>%
  select(name, hour, energy_level) %>%
  #filter(name == P2_NAME) %>%
  ggplot(aes(x=hour, y=energy_level, fill=name)) +
  geom_bar(stat="identity", col=I("black")) +
  #geom_line() +
  #geom_point() +
  facet_wrap(~name, ncol=1)


df %>%
  #filter(name == "Amit") %>%
  group_by(name) %>%
  arrange(date_hour) %>%
  mutate(hour = as.numeric(difftime(date_hour, min(date_hour), units = "hour"))) %>%
  mutate(hour = factor(hour, levels=seq(min(hour), max(hour), 1))) %>%
  mutate(hunger = factor(hunger, c("sleeping", "not hungry at all", "somewhat hungry", "extremely hungry"), ordered = TRUE)) %>%
  replace_na(list(hunger="sleeping")) %>%
  select(name, hour, hunger) %>%
  ggplot(aes(x=hour, y=hunger, fill=name)) +
  geom_bar(stat="identity", col=I("black")) +
  facet_wrap(~name, ncol=1)

library(plotly)

m <- matrix(rnorm(9), nrow = 3, ncol = 3)
fig <- plot_ly(
  x = c("a", "b", "c"), y = c("d", "e", "f"),
  z = m, type = "heatmap"
)

fig


dfw <- df %>%
  filter(!asleep) %>%
  count(name, energy_level, hunger, sleepy, sort=TRUE) 
dfw
library(ggalluvial)
titanic_wide <- data.frame(Titanic)
head(titanic_wide)
#>   Class    Sex   Age Survived Freq
#> 1   1st   Male Child       No    0
#> 2   2nd   Male Child       No    0
#> 3   3rd   Male Child       No   35
#> 4  Crew   Male Child       No    0
#> 5   1st Female Child       No    0
#> 6   2nd Female Child       No    0
ggplot(data = titanic_wide,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,
           y = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.2, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")


person<- "Nidhi"
dfw %>%
  filter(name == person) %>%
  ggplot(aes(axis1 = energy_level, axis2 = hunger, axis3 = sleepy,
           y = n)) +
  scale_x_discrete(limits = c("Energy", "Hunger", "Sleep"), expand = c(.2, .05)) +
  xlab("") + 
  geom_alluvium(aes(fill = energy_level)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  #scale_fill_tableau() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("Duration (Hours)") +
  labs(title=glue("How did you {person} feel overall during the 72 hour water fast?"),
       subtitle="Follow the pink, green and blue colored flows from left to right.\nEvaluated across three axis, felt normal for the most part with normal energy level, no extreme hunger pangs and not very sleepy either!",
       caption="Data only includes non-sleeping hours") +
  theme(legend.position = "bottom", legend.title = element_blank())


df_drinks <-  df %>%
  mutate(day = as.numeric(difftime(date, min(date), units = "days"))) %>%
  select(name, day, drink1, drink2, drink3) %>%
  gather(k, v, -name, -day) %>%
  drop_na() %>%
  count(name, day, v)
df_drinks

df_drinks %>%
  mutate(v = str_to_lower(v)) %>%
  mutate(day = glue("Day {day}")) %>%
  ggplot(aes(x=reorder(v, n), y=n, col=v, fill=v)) +
  geom_bar(stat="identity") +
  scale_y_continuous(breaks=seq(0, max(df_drinks$n), 1)) +
  scale_color_tableau() +
  scale_fill_tableau() +
  facet_grid(vars(name), vars(day)) +
  coord_flip() +
  labs(title = "What did we drink during the 72 hour water fast?",
       subtitle = "Amit drank a variety of beverages, Nidhi stuck to just water and the occasional tea.") +
  theme_fivethirtyeight() +
  xlab("") +
  theme(axis.title = element_text(),
        strip.text = element_text(face="bold", size=9,lineheight=5.0),
        legend.title = element_blank(), legend.position = "none") +
  ylab("Number of Servings")
  