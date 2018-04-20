# To run this make sure these packages have been installed (to install them run 
# file with the next line of code uncommented, you only need to do this once per
# machine)

# install.packages(c("httr", "jsonlite", "dplyr", "anytime", "tidyr"))

library(httr)
library(jsonlite)
library(dplyr)
library(anytime)
library(tidyr)

# Sets up information for TBA API
tba_auth <- " EdSrcK5eTwGAwsnfIuBbS8HUTe4nK4GlMIYC8AwPNDaPSgNmKPaGTrZwDLhqVzfR "
base <- "www.thebluealliance.com/api/v3/"

# tur for turing, waamv for auburn mountainview
# you can look these up in blue alliance
year <- "2018"
event <- "waahs"
team <- 2557

# Queries TBA for match data for 2557 at an event
query <- paste0("team/frc", team, "/event/", year, event, "/matches")
url <- paste0(base, query)
API_KEY <- " EdSrcK5eTwGAwsnfIuBbS8HUTe4nK4GlMIYC8AwPNDaPSgNmKPaGTrZwDLhqVzfR "
httpResponse <- GET(url, add_headers("X-TBA-Auth-Key" = API_KEY), accept_json())
results <- fromJSON(content(httpResponse, "text"))

# Extracts data for each alliance
both_alliances <- results %>% select(alliances)

red_alliance <- both_alliances[, 1]$red
red_score <- red_alliance %>% select(score)
red_score <- as.numeric(red_score[, 1])
red_alliance_teams <- do.call(rbind, red_alliance[["team_keys"]])
red_alliance_teams <- as.data.frame(red_alliance_teams)
colnames(red_alliance_teams) <- c("Team_1", "Team_2", "Team_3")
red_alliance_teams <- red_alliance_teams %>%
  unite(Teams, Team_1, Team_2, Team_3, sep = " ", remove = TRUE)
red_alliance_teams <- red_alliance_teams[["Teams"]]

blue_alliance <- both_alliances[, 1]$blue
blue_score <- blue_alliance %>% select(score)
blue_score <- as.numeric(blue_score[, 1])
blue_alliance_teams <- do.call(rbind, blue_alliance[["team_keys"]])
blue_alliance_teams <- as.data.frame(blue_alliance_teams)
colnames(blue_alliance_teams) <- c("Team_1", "Team_2", "Team_3")
blue_alliance_teams <- blue_alliance_teams %>%
  unite(Teams, Team_1, Team_2, Team_3, sep = " ", remove = TRUE)
blue_alliance_teams <- blue_alliance_teams[["Teams"]]

# Builds match data dataframe
match_data <- results %>% subset(select = c(
  actual_time, comp_level, match_number, time, winning_alliance
))

# Adds scores to match data
match_data <- match_data %>% mutate(
  "red_score" = red_score,
  "blue_score" = blue_score,
  "red_alliance" = red_alliance_teams,
  "blue_alliance" = blue_alliance_teams
)

# Converts time from epoch to 24 hour (sorry you'll have to do the match to
# 12 hour)
time_of_match <- match_data %>% select(actual_time)
time <- match_data %>% select(time)
match_time_na <- is.na(match_data %>% select(actual_time))
time_of_match[match_time_na] <- time[match_time_na]
if (!all(is.na(time_of_match))) {
  time_of_match <- anytime(time_of_match[!is.na(time_of_match)])
  match_data[["actual_time"]] <- time_of_match
}
match_time <- match_data %>% select(time)
if (length(match_time[!is.na(match_time)]) != 0) {
  match_time <- anytime((match_time[!is.na(match_time)]))
  match_data[["time"]] <- match_time
}

levels <- match_data %>% select(comp_level)
levels <- levels[["comp_level"]]
levels_num <- gsub("qm", 1, levels)
levels_num <- gsub("qf", 2, levels_num)
levels_num <- gsub("sf", 3, levels_num)
levels_num <- gsub("f", 4, levels_num)

# Builds score data data frame
score_data <- results %>% select(score_breakdown)
score_data <- score_data[, 1]
if (!all(is.na(score_data))) {
  score_data_blue <- score_data[["blue"]]
  colnames(score_data_blue) <- paste0("blue_", colnames(score_data_blue))
  score_data_red <- score_data[["red"]]
  colnames(score_data_red) <- paste0("red_", colnames(score_data_red))
  score_data_combined <- data.frame(c(score_data_blue, score_data_red))
  match_numbers <- match_data %>% select(match_number)
  match_numbers <- match_numbers[, 1]
  score_data_combined <- score_data_combined %>% mutate(
    "red_alliance" = red_alliance_teams,
    "blue_alliance" = blue_alliance_teams,
    "comp_levels_num" = levels_num,
    "comp_level" = levels,
    "match_number" = match_numbers
  )
  score_data_combined <- score_data_combined %>% arrange(comp_levels_num, match_number)
}

match_data <- match_data %>%  mutate("comp_levels_num" = levels_num)

# Arranges data by match number
match_data <- match_data %>% arrange(comp_levels_num, match_number)



# Prints match and score data to csv files for excel
match_file <- paste0(team, event, year, "_matches.csv")
score_file <- paste0(team, event, year, "_scores.csv")
write.csv(match_data, file = match_file, row.names = TRUE)
if (!all(is.na(score_data))) {
  write.csv(score_data_combined, file = score_file, row.names = TRUE)
}
