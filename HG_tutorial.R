# Hockey Graphs: Intro to R with Hockey Data
# Tutorial website: https://hockey-graphs.com/2019/12/11/an-introduction-to-r-with-hockey-data/

# Load tidyverse
library(tidyverse)

# Load in data from GitHub (4 Flyers games from Nov. 2019)
PHI_tutorial_data <- 
  read_csv("https://github.com/hockey-graphs/HG_intro_tutorial/blob/master/PHI_tutorial_data.csv?raw=true")

# Data Exploration

# Create a Data frame that contains Goals
goals <- PHI_tutorial_data %>% filter(event_type == "GOAL")

# Even Strength Goals Data Frame
goals_5v5 <- PHI_tutorial_data %>% filter(event_type == "GOAL" & game_strength_state == "5v5")

# Special Teams Goals Data Frame
goals_special_teams <- PHI_tutorial_data %>% filter(event_type == "GOAL" & 
                        (game_strength_state == "5v4" | game_strength_state == "4v5"))

# Goals at any Strength Data Frame
goals_5v5_ST <- PHI_tutorial_data %>% filter(event_type == "GOAL" & game_strength_state %in% c("5v5", "5v4", "4v5"))

# Keep selected variables for Goal Data Frame
goals_small <- goals %>% select(game_id, game_date, event_type, event_detail, event_team, event_player_1)

# Remove Description Variable 
goals_drop <- goals %>% select(-c(event_description))

# Pull Score to the front
goals <- goals %>% select(home_score, away_score, everything())

# Create a new data frame with Goal Variable
goal_variable <- PHI_tutorial_data %>% mutate(goal = ifelse(event_type == "GOAL", 1, 0))

# Check to ensure that Variable was Created
sum(goal_variable$goal)

# Another check to ensure variabe was created
count(goal_variable, event_type)

# Find Total Games per Game
goals_by_game <- goal_variable %>% group_by(game_id) %>% summarize(total_goals = sum(goal))

# Add event_team so we have total goals per game per team
# But if you look at the resulting data frame, there are NA values
goals_by_game_team <- goal_variable %>% group_by(game_id, event_team) %>% summarize(goals = sum(goal))

# Let's try that again and remove the NAs first
# ! means "not" in R language
# is.na() identifies the null values
goals_by_game_team <- goal_variable %>% filter(!is.na(event_team)) %>%
  group_by(game_id, event_team) %>% summarize(goals = sum(goal))

# Arrange by number of goals
goals_by_game_team <- goals_by_game_team %>% arrange(desc(goals))

# Making a graph----

# Make a bar chart!
ggplot(data = PHI_tutorial_data) + geom_bar(aes(x = event_zone))

# Make a bar chart with some color!
ggplot(data = PHI_tutorial_data) + geom_bar(aes(x = event_zone, fill = event_zone))

# Add a label to the y-axis
ggplot(data = PHI_tutorial_data) + geom_bar(aes(x = event_zone, fill = event_zone)) + labs(y = "Number of Events")

# Exercises
# 1. What was the 5v5 shooting percentage for each team in each game?

# Create variables for Shots on Goal and for Goals
# Filter down to 5v5 play only, group by game and team, then summarize
# Finish by calculating shooting percentage
sh_perc <- PHI_tutorial_data %>%
  mutate(SOG = ifelse(event_type %in% c("SHOT", "GOAL"), 1, 0),
         goal = ifelse(event_type == "GOAL", 1, 0)) %>%
  filter(game_strength_state == "5v5" & !is.na(event_team)) %>%
  group_by(game_id, event_team) %>%
  summarize(SOG = sum(SOG),
            goal = sum(goal)) %>%
  mutate(sh_perc = goal / SOG)

# 2. Which team won each game? How many points did PHI get in each game?

# Group by game, find the highest home score, road score, and period
# Find the winning team, create variable to assign points based on who won
PHI_results <- PHI_tutorial_data %>%
  group_by(game_id, home_team, away_team) %>%
  summarize(max_home_score = max(home_score),
            max_away_score = max(away_score),
            max_period = max(game_period)) %>%
  mutate(winning_team = ifelse(max_home_score > max_away_score, home_team, away_team),
         PHI_points = ifelse(winning_team == "PHI", 2,
                             ifelse(max_period > 3, 1, 0)))

# 3. Which player generated the most shot attempts among all of these games?
indiv_corsi <- PHI_tutorial_data %>%
  mutate(shot_attempts = ifelse(event_type %in% c("SHOT", "BLOCK", "MISS", "GOAL"), 1, 0)) %>%
  filter(!is.na(event_player_1)) %>%
  group_by(event_player_1, event_team) %>%
  summarize(shot_attempts = sum(shot_attempts)) %>%
  arrange(desc(shot_attempts))

# 4. Create a bar chart showing the top 5 players with the most shot attempts

# Use top_n function to show top 5 in shot attempts
indiv_corsi_top5 <- indiv_corsi %>%
  ungroup() %>%
  top_n(5, shot_attempts)

# Create a horizontal bar chart 
ggplot(data = indiv_corsi_top5) + 
  geom_bar(aes(x = event_player_1, y = shot_attempts), stat = "identity") +
  labs(y = "Number of Shot Attempts", x = "Player") +
  coord_flip()

# Sort the player names by shot attempts
ggplot(data = indiv_corsi_top5) + 
  geom_bar(aes(x = reorder(event_player_1, shot_attempts), y = shot_attempts), stat = "identity") +
  labs(y = "Number of Shot Attempts", x = "Player") +
  coord_flip()

# 5. Which players drew the most penalties?

# By examining the data, the player who drew the most penalties is player 2
penl <- PHI_tutorial_data %>%
  filter(event_type == "PENL" & !is.na(event_player_2)) %>%
  count(event_player_2, sort = TRUE)

# 6. What was the faceoff win percentage for PHI in each game?

# Filter data to faceoffs and create a variable for Flyers faceoff wins
# Group and summarize number of faceoff wins and total faceoffs by game
# Calculate Faceoff Win Percentage
faceoffs <- PHI_tutorial_data %>%
  filter(event_type == "FAC") %>%
  mutate(PHI_FO_win = ifelse(event_team == "PHI", 1, 0)) %>%
  group_by(game_id) %>%
  summarize(FO_wins = sum(PHI_FO_win),
            FO_total = n()) %>%
  mutate(FO_win_perc = FO_wins / FO_total)