# Hockey Graphs tutorial: Exploring Data Analysis using tidyverse
# https://hockey-graphs.com/2019/10/08/exploratory-data-analysis-using-tidyverse/

# Load tidyverse library
library(tidyverse)

# Load the sample game data (TBL @ CBJ & VGK @ COL both games Feb 18 2019)
game_data <- read_csv("https://github.com/hockey-graphs/hg-data-cleaning/blob/master/game_data.csv?raw=true")

#PP example
# Filter the data to Power Play events
PP <- game_data %>%
  filter(game_strength_state %in% c("5v4", "4v5") & 
           (event_length > 0 | event_type %in% c("SHOT", "GOAL", "MISS", "BLOCK")))

# Create Power Play Variable Sets to have data in same place
PP <- PP %>%
  mutate(PP_1 = ifelse(game_strength_state == "5v4", home_on_1, away_on_1),
         PP_2 = ifelse(game_strength_state == "5v4", home_on_2, away_on_2),
         PP_3 = ifelse(game_strength_state == "5v4", home_on_3, away_on_3),
         PP_4 = ifelse(game_strength_state == "5v4", home_on_4, away_on_4),
         PP_5 = ifelse(game_strength_state == "5v4", home_on_5, away_on_5),
         PP_6 = ifelse(game_strength_state == "5v4", home_on_6, away_on_6),
         PP_goalie = ifelse(game_strength_state == "5v4", home_goalie, away_goalie),
         PP_team = ifelse(game_strength_state == "5v4", home_team, away_team))

# Create a Power Play line variable by grouping players on-ice 
PP <- PP %>% unite(PP_line, PP_1:PP_6, sep = "-", remove = FALSE)

# Function ro remove the goalie from Power Play Line 
create_line <- function(line, goalie) {
  line <- str_replace_all(line, goalie, "")
  line <- str_replace_all(line, c("--" = "-", "^-" = "", "-$" = ""))
}

#Run the function
PP <- PP %>% mutate(PP_line = create_line(PP_line, PP_goalie))

# Corsi For (CF) variable used to identify shot attempts
PP <- PP %>% mutate(CF = ifelse(event_type %in% c("GOAL","SHOT","MISS","BLOCK") & event_team == PP_team, 1, 0))

# Group by Power Play Line, summarize Time on Ice and Corsi For
# We then Filter down to the lines that played at least 2 minutes and sort by CF/60
PP_group <- PP %>%
  group_by(PP_team, PP_line) %>%
  summarize(TOI = sum(event_length) / 60, CF = sum(CF)) %>%
  filter(TOI >= 2) %>%
  mutate(CF_60 = CF * 60 / TOI) %>%
  arrange(desc(CF_60))

#Defenseman example

# Filter to even strength events using only game time and select variables
D <- game_data %>%
  filter(game_strength_state %in% c("5v5") & event_length > 0) %>%
  select(game_id, event_length, home_team, away_team, home_on_1:home_on_6, away_on_1:away_on_6)

# Pivot player names to create a new team variable
D_pivot <- D %>%
  pivot_longer(cols = home_on_1:away_on_6, names_to = "on_ice", values_to = "player") %>%
  mutate(team = ifelse(str_detect(on_ice, "home"), home_team, away_team))

# Group and summarize game and player by Time On Ice
D_player <- D_pivot %>% group_by(game_id, team, player) %>% summarize(TOI = sum(event_length) / 60)

# Load in position data provided from NaturalStatTrick
position_data_NST <- read_csv("https://github.com/hockey-graphs/hg-data-cleaning/blob/master/position_data_NST.csv?raw=true")

# Modify the format of Player variable to match event data
position_data_NST <- position_data_NST %>%
  mutate(Player = str_to_upper(Player),
         Player = str_replace(Player, " ", "."),
         Position = ifelse(Position == "D", "D", ifelse(Position == "G", "G", "F")))

# Join and sort the position data by na to see if there are any problems  
D_player_join <- D_player %>%
  left_join(select(position_data_NST, Player, Position), by = c("player" = "Player")) %>%
  arrange(desc(is.na(Position)))

# Edit two names in the data to match our event data
position_data_NST_edited <- position_data_NST %>%
  mutate(Player = ifelse(Player == "ALEXANDER.KERFOOT", "ALEX.KERFOOT",
                         ifelse(Player == "ALEXANDER.WENNBERG", "ALEX.WENNBERG",
                                Player)))

# Join once more
D_player_join <- D_player %>%
  left_join(select(position_data_NST_edited, Player, Position), by = c("player" = "Player")) %>%
  arrange(desc(is.na(Position)))

# Filter and sort defensemen by time on ice
D_player_result <- D_player_join %>%
  filter(Position == "D") %>%
  arrange(game_id, team, desc(TOI))

# Repeat above, but also create a running count variable
# Filter down to the top player on each team for each game
D_player_result <- D_player_join %>%
  filter(Position == "D") %>%
  arrange(game_id, team, desc(TOI)) %>%
  group_by(game_id, team) %>%
  mutate(count = row_number()) %>%
  filter(count == 1)