# To run this make sure these packages have been installed (to install them run
# file with the next line of code uncommented, you only need to do this once per
# machine)

# install.packages(c("httr", "jsonlite", "dplyr", "tidyr"))

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Sets up information for TBA API
tba_auth <- " EdSrcK5eTwGAwsnfIuBbS8HUTe4nK4GlMIYC8AwPNDaPSgNmKPaGTrZwDLhqVzfR "
base <- "www.thebluealliance.com/api/v3/"


# Change these values
year <- 2018
team <- 2557
event <- "tur"


get_opr <- function(year_opr, event_opr) {
  # Queries TBA for match data for 2557 at an event
  query <- paste0("event/", year_opr, event_opr, "/oprs")
  url <- paste0(base, query)
  API_KEY <- " EdSrcK5eTwGAwsnfIuBbS8HUTe4nK4GlMIYC8AwPNDaPSgNmKPaGTrZwDLhqVzfR "
  httpResponse <- GET(url, add_headers("X-TBA-Auth-Key" = API_KEY), accept_json())
  fromJSON(content(httpResponse, "text"))
}

get_matches <- function(year_matches, team_matches, event_matches) {
  query <- paste0(
    "team/frc", team_matches, "/event/", year_matches,
    event_matches, "/matches"
  )
  url <- paste0(base, query)
  API_KEY <- " EdSrcK5eTwGAwsnfIuBbS8HUTe4nK4GlMIYC8AwPNDaPSgNmKPaGTrZwDLhqVzfR "
  httpResponse <- GET(url, add_headers("X-TBA-Auth-Key" = API_KEY), accept_json())
  fromJSON(content(httpResponse, "text"))
}

get_team_opr <- function(team) {
  oprs[[team]]
}

get_team_dpr <- function(team) {
  dprs[[team]]
}

opr_results <- get_opr(year, event)
match_results <- get_matches(year, team, event)

# ccwm is "Calculated Contribution to Winning Margin"
ccwms <- opr_results[["ccwms"]]
oprs <- opr_results[["oprs"]]
dprs <- opr_results[["dprs"]]

both_alliances <- match_results %>% select(alliances)

red_alliance <- both_alliances[, 1]$red
red_score <- red_alliance %>% select(score)
red_score <- as.numeric(red_score[, 1])
red_alliance_teams <- do.call(rbind, red_alliance[["team_keys"]])
red_alliance_teams <- as.data.frame(red_alliance_teams, stringsAsFactors = FALSE)
colnames(red_alliance_teams) <- c("robot1", "robot2", "robot3")
red_robot1_opr <- signif(sapply(red_alliance_teams[["robot1"]], get_team_opr,
  USE.NAMES = FALSE
), 4)
red_robot2_opr <- signif(sapply(red_alliance_teams[["robot2"]], get_team_opr,
  USE.NAMES = FALSE
), 4)
red_robot3_opr <- signif(sapply(red_alliance_teams[["robot3"]], get_team_opr,
  USE.NAMES = FALSE
), 4)
red_robot1_dpr <- signif(sapply(red_alliance_teams[["robot1"]], get_team_dpr,
  USE.NAMES = FALSE
), 4)
red_robot2_dpr <- signif(sapply(red_alliance_teams[["robot2"]], get_team_dpr,
  USE.NAMES = FALSE
), 4)
red_robot3_dpr <- signif(sapply(red_alliance_teams[["robot3"]], get_team_dpr,
  USE.NAMES = FALSE
), 4)

blue_alliance <- both_alliances[, 1]$blue
blue_score <- blue_alliance %>% select(score)
blue_score <- as.numeric(blue_score[, 1])
blue_alliance_teams <- do.call(rbind, blue_alliance[["team_keys"]])
blue_alliance_teams <- as.data.frame(blue_alliance_teams, stringsAsFactors = FALSE)
colnames(blue_alliance_teams) <- c("robot1", "robot2", "robot3")
blue_robot1_opr <- signif(sapply(blue_alliance_teams[["robot1"]], get_team_opr,
  USE.NAMES = FALSE
), 4)
blue_robot2_opr <- signif(sapply(blue_alliance_teams[["robot2"]], get_team_opr,
  USE.NAMES = FALSE
), 4)
blue_robot3_opr <- signif(sapply(blue_alliance_teams[["robot3"]], get_team_opr,
  USE.NAMES = FALSE
), 4)
blue_robot1_dpr <- signif(sapply(blue_alliance_teams[["robot1"]], get_team_dpr,
  USE.NAMES = FALSE
), 4)
blue_robot2_dpr <- signif(sapply(blue_alliance_teams[["robot2"]], get_team_dpr,
  USE.NAMES = FALSE
), 4)
blue_robot3_dpr <- signif(sapply(blue_alliance_teams[["robot3"]], get_team_dpr,
  USE.NAMES = FALSE
), 4)

comp_levels <- match_results[["comp_level"]]
levels_num <- gsub("qm", 1, comp_levels)
levels_num <- gsub("qf", 2, levels_num)
levels_num <- gsub("sf", 3, levels_num)
levels_num <- gsub("f", 4, levels_num)
result_df <- match_results %>% select(match_number, winning_alliance)
result_df <- result_df %>% mutate(
  "comp_level" = comp_levels,
  "level_num" = levels_num
)
result_df <- result_df %>% arrange(levels_num, match_number)

result_df <- result_df %>% mutate(
  "red_average_opr" = signif((red_robot1_opr + red_robot2_opr + red_robot3_opr)
  / 3, 4),
  "blue_average_opr" = signif((blue_robot1_opr + blue_robot2_opr + blue_robot3_opr)
  / 3, 4),
  "red_average_dpr" = signif((red_robot1_dpr + red_robot2_dpr + red_robot3_dpr)
  / 3, 4),
  "blue_average_dpr" = signif((blue_robot1_dpr + blue_robot2_dpr + blue_robot3_dpr)
  / 3, 4),
  "red_robot1" = paste(red_alliance_teams[["robot1"]], paste0(
    "opr =", red_robot1_opr,
    " dpr =", red_robot1_dpr
  )),
  "red_robot2" = paste(red_alliance_teams[["robot2"]], paste0(
    "opr =", red_robot2_opr,
    " dpr =", red_robot2_dpr
  )),
  "red_robot3" = paste(red_alliance_teams[["robot3"]], paste0(
    "opr =", red_robot3_opr,
    " dpr =", red_robot3_dpr
  )),

  "blue_robot1" = paste(blue_alliance_teams[["robot1"]], paste0(
    "opr =", blue_robot1_opr,
    " dpr =", blue_robot1_dpr
  )),
  "blue_robot2" = paste(blue_alliance_teams[["robot2"]], paste0(
    "opr =", blue_robot2_opr,
    " dpr =", blue_robot2_dpr
  )),
  "blue_robot3" = paste(blue_alliance_teams[["robot3"]], paste0(
    "opr =", blue_robot3_opr,
    " dpr =", blue_robot3_dpr
  ))
)

result_df <- result_df %>% mutate(
  "red_adjusted_score" = 2 /3 * red_average_opr + 1 / 2 * red_average_dpr,
  "blue_adjusted_score" = 2 / 3 * blue_average_opr + 1 / 2 * blue_average_dpr
)

prediction <- result_df$red_adjusted_score > result_df$blue_adjusted_score
prediction <- gsub(TRUE, "red", prediction)
prediction <- gsub(FALSE, "blue", prediction)

result_df <- result_df %>% mutate("predicted_winner" = prediction)
rownames(result_df) <- paste0(result_df$comp_level, result_df$match_number)
# result_df <- result_df[, c(1, 2, 13, 5, 6, 7, 8, 9, 10, 11, 12, 3, 4)]
# result_df <- result_df[, c(-1, -12, -13)]

print_df<- result_df %>% subset(select = c(
  winning_alliance, predicted_winner, red_average_opr, red_average_dpr, 
  red_robot1, red_robot2, red_robot3, blue_robot1, blue_robot2, blue_robot3
))

result_file <- paste0(team, event, year, "_oprs.csv")
write.csv(print_df, file = result_file, row.names = TRUE)
