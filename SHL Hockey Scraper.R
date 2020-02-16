# SHL Hockey Scraper
# This Scraper will scrape box score data for SHL and Allsvensken season specified
# Author = Jameel Kaba

# Import Statements
library(tidyverse)
library(rvest)
library(progress)

# Function to get URLs for every fame for the league + season requested 
get_schedule <- function(league, season, ..., progress = TRUE) {
  
  mydata <- tidyr::crossing(league, season)
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_schedule() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    pb$tick(0)
    
  }
  
  .get_schedule <- function(league, season, ...) {
    
    if (league == "SHL") {
      
      if (season == "2019-20") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/10371"}
      
      else if (season == "2018-19") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9171"}
      else if (season == "2017-18") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/8121"}
      else if (season == "2016-17") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/7132"}
      else if (season == "2015-16") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/6052"}
      else if (season == "2014-15") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/5056"}
      
      else {stop("Season not available. Sorry!")}
      
    }
    
    else if (league == "Allsvenskan") {
      
      if (season == "2019-20") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/10333"}
      
      else if (season == "2018-19") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/9168"}
      else if (season == "2017-18") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/8122"}
      else if (season == "2016-17") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/7157"}
      else if (season == "2015-16") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/6053"}
      else if (season == "2014-15") {url <- "http://stats.swehockey.se/ScheduleAndResults/Schedule/5057"}
      
      else {stop("Season not available. Sorry!")}
      
    }  
    
    else {stop("League not available. Sorry!")}
    
    schedule_clean <- function(url, ...) {
      
      clean_schedule <- url %>%
        read_html() %>%
        html_nodes("#groupStandingResultContent a") %>%
        html_attr("href") %>%
        str_replace_all("[\r\n]", "") %>%
        str_squish() %>%
        str_extract("/Game/Events/[[:digit:]]{2,}") %>%
        str_c("http://stats.swehockey.se", .) %>%
        as_tibble() %>%
        set_names("url") %>%
        mutate(league = league) %>%
        mutate(season = season) %>%
        drop_na()
      
      return(clean_schedule)
      
    }
    
    schedule_data <- map_dfr(url, schedule_clean)
    
    if (progress) {pb$tick()}
    
    return(schedule_data)
    
  }
  
  all_schedule_data <- map2_dfr(mydata[["league"]], mydata[["season"]], .get_schedule)
  
  return(all_schedule_data)
  
}

# Gets the URL for SHL or Allsvenskan season(s) requested
games <- get_schedule(c("SHL", "Allsvenskan"), c("2019-20"))

# Function gets box score data via the URLs from the get schedule function
get_box_score <- function(..., progress = TRUE) {
  
  if (progress) {
    pb <- progress::progress_bar$new(format = "get_box_score() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(...), show_after = 0) 
    pb$tick(0)}
  
  .get_box_score <- function(url, league, season, ...) {
    
    seq(0, 0.5, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    messy_data <- url %>%
      read_html() %>%
      html_nodes("tr+ tr .tblContent .tdOdd") %>%
      html_text() %>%
      as_tibble() %>%
      filter(lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 0) | 
               lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 1) |
               lag(str_detect(value, "\\(") & str_detect(value, "[0-9]-[0-9]"), 2))
    
    team <- messy_data %>%
      filter(row_number() %% 3 == 2) %>%
      set_names("team")
    
    game_strength <- messy_data %>%
      filter(row_number() %% 3 == 1) %>%
      set_names("game_strength")
    
    players <- messy_data %>%
      filter(row_number() %% 3 == 0) %>%
      set_names("players")
    
    if (nrow(players) != 0) {
      
      box_score_data <- bind_cols(team, game_strength, players) %>%
        mutate(goal_state = str_split(game_strength, "\\(", simplify = TRUE, n = 2)[,1]) %>%
        mutate(game_strength = str_split(game_strength, "\\(", simplify = TRUE, n = 2)[,2]) %>%
        mutate(game_strength = str_replace_all(game_strength, "\\)", "")) %>%
        mutate(players = str_replace_all(players, "[\r\n]", "")) %>%
        mutate(goal = str_split(players, "[0-9]+\\.", simplify = TRUE, n = 4)[,2]) %>%
        mutate(goal = str_split(goal, "\\(", simplify = TRUE)[,1]) %>%
        mutate(goal = str_c(str_squish(str_split(goal, ",", simplify = TRUE, n = 2)[,2]), str_squish(str_split(goal, ",", simplify = TRUE, n = 2)[,1]), sep = " ")) %>%
        mutate(goal = str_replace_all(goal, "[0-9]", "")) %>%
        mutate(primary_assist = str_split(players, "[0-9]+\\.", simplify = TRUE, n = 4)[,3]) %>%
        mutate(primary_assist = str_c(str_squish(str_split(primary_assist, ",", simplify = TRUE, n = 2)[,2]), str_squish(str_split(primary_assist, ",", simplify = TRUE, n = 2)[,1]), sep = " ")) %>%
        mutate(secondary_assist = str_split(players, "[0-9]+\\.", simplify = TRUE, n = 4)[,4]) %>%
        mutate(secondary_assist = str_c(str_squish(str_split(secondary_assist, ",", simplify = TRUE, n = 2)[,2]), str_squish(str_split(secondary_assist, ",", simplify = TRUE, n = 2)[,1]), sep = " ")) %>%
        mutate(league = league) %>%
        mutate(season = season) %>%
        mutate(game_url = url) %>%
        mutate_all(str_squish) %>%
        select(-players)
      
    }
    if (progress) {pb$tick()}
    return(box_score_data)
  }
  all_box_score_data <- pmap_dfr(..., .get_box_score)
  return(all_box_score_data)
}

# Gets the Box Score data for the games in the season specified
scoring_data <- games %>% get_box_score()