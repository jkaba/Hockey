# Taking a look at the 2018 NHL Draft
# Looking at Height and Weight for players on the Canucks and Predators

# Load in the tidyverse library
library(tidyverse)

# Load in draft data, .rds file will be in repo
draft_data <- read_rds("2018_nhl_draft_data.rds")

# Snapshot of draft_data
draft_data

# View the draft data
View(draft_data)

# Creating a height and weight plot with a trend line for the drafted players
draft_data %>% 
  ggplot(aes(x = height, y = weight)) +
  geom_point() +
  labs(x = "Height", 
       y = "Weight", 
       title = "Height & Weight for 2018 Drafted Players",
       subtitle = "Data from EliteProspects") +
  theme_bw() +
  geom_smooth()

# Let's take a look at the height and weight for players drafted by the Canucks and Predators
draft_data %>% 
  filter(draft_team == "Vancouver Canucks" | draft_team == "Nashville Predators") %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point() +
  labs(x = "Height", 
       y = "Weight", 
       title = "Height & Weight for 2018 Drafted Canucks and Predators",
       subtitle = "Data from EliteProspects") +
  theme_bw() +
  geom_smooth()

# Let's take a look at the picks, when they were taken, and who they were taken by
selected_data <- select(draft_data, pick_number, draft_team, name)
View(selected_data)

# Let's look at all the forwards who were drafted
draft_data %>%
  filter(position == "F") %>%
  select(pick_number, round, draft_team, name)

# Now Let's look at the defenseman
draft_data %>%
  filter(position == "D") %>%
  select(pick_number, round, draft_team, name)

# And finally the Goalies
draft_data %>%
  filter(position == "G") %>%
  select(pick_number, round, draft_team, name)

# Let's look at who the Canucks and Predators selected
draft_data %>%
  filter(draft_team == "Vancouver Canucks" | draft_team == "Nashville Predators") %>%
  select(pick_number, round, draft_team, name)

# Let's calculate the density of these players that the Canucks and Predators selected
draft_data %>% 
  filter(draft_team == "Vancouver Canucks" | draft_team == "Nashville Predators") %>%
  mutate(density = weight/height) %>%
  select(pick_number, round, draft_team, name, height, weight, density)

# Now let's arrange these players 
draft_data %>% 
  mutate(density = weight/height) %>% 
  filter(draft_team == "Vancouver Canucks" | draft_team == "Nashville Predators") %>%
  select(pick_number, round, draft_team, name, height, weight, density) %>%
  arrange(desc(density))


