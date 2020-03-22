# KHL Scraper
# This Script can be used to scrape box scores for KHL games
# Author = Jameel Kaba

# Load Libraries
library(tidyverse)
library(rvest)
library(RSelenium)
library(progress)

# Get URLS for each game of each season from 08/09 - 18/19
getSchedule <- function(league, season, ..., progress = TRUE) {
  mydata <- tidyr::crossing(league, season)
  
  if (progress) {
    pb <- progress::progress_bar$new(format = "getSchedule() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    pb$tick(0)
  }
  
  # Get Schedule for each season
  .getSchedule <- function(league, season, ...) {
    if (league == "KHL") {  
      if (season == "2018-19") {url = "https://en.khl.ru/calendar/671/00/"}
      else if (season == "2017-18") {url = "https://en.khl.ru/calendar/468/00/"}
      else if (season == "2016-17") {url = "https://en.khl.ru/calendar/405/00/"}
      else if (season == "2015-16") {url = "https://en.khl.ru/calendar/309/00/"}
      else if (season == "2014-15") {url = "https://en.khl.ru/calendar/266/00/"}
      else if (season == "2013-14") {url = "https://en.khl.ru/calendar/244/00/"}
      else if (season == "2012-13") {url = "https://en.khl.ru/calendar/222/00/"}
      else if (season == "2011-12") {url = "https://en.khl.ru/calendar/202/00/"}
      else if (season == "2010-11") {url = "https://en.khl.ru/calendar/185/00/"}
      else if (season == "2009-10") {url = "https://en.khl.ru/calendar/167/00/"}
      else if (season == "2008-09") {url = "https://en.khl.ru/calendar/160/00/"}
      else {stop("Season not available.")}
    }
    else {stop("Not Available")}
    
    driver <- rsDriver(verbose = FALSE)
    driver$client$navigate(url)
    
    page <- driver$client$getPageSource() %>%
      purrr::pluck(1) %>%
      read_html()
    
    driver$client$close()
    driver$server$stop()
    
    schedule <- page %>%
      html_nodes("ul+ ul li:nth-child(1) a") %>%
      html_attr("href") %>%
      str_c("https://en.khl.ru", .) %>%
      as_tibble() %>%
      set_names("url") %>%
      mutate(season = season) %>%
      mutate(league = league) %>%
      distinct()
    
    return(schedule)
    
  }
  
  scheduleData <- map2_dfr(mydata[["league"]], mydata[["season"]], .getSchedule)
  return(scheduleData)
  
}

# This gets all URLs for the 18/19 season
# Modify to adjust for other seasons
games <- getSchedule("KHL", "2018-19")

# Function to get score summaries for specified games
# Takes as input the URL for the game, league (KHL) and specified season
getBoxScore <- function(..., progress = TRUE) {
  
  if (progress) {
    pb <- progress::progress_bar$new(format = "getBoxScore() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(...), show_after = 0) 
    pb$tick(0)}
  
  driver <- rsDriver(verbose = FALSE)
  .getBoxScore <- function(url, league, season, ...) {
    
    seq(4, 7, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    driver$client$navigate(url)
    
    page <- driver$client$getPageSource() %>%
      purrr::pluck(1) %>%
      read_html()
    
    home_team <- page %>%
      html_nodes(".e-details_img+ .b-details_txt .e-club_name") %>%
      html_text()
    
    away_team <- page %>%
      html_nodes(".m-rightward .e-club_name") %>%
      html_text()
    
    box_score_data <- page %>% 
      html_node("#goals") %>% 
      html_table() %>% 
      set_names("goal_number", "period", "time", "score", "game_strength", "goal", "primary_assist", "secondary_assist", "home_team_on_ice", "away_team_on_ice") %>% 
      as_tibble() %>% 
      mutate_at(vars(goal, primary_assist, secondary_assist), ~str_replace_all(., c("[[:digit:]]" = "", "[[:punct:]]" = ""))) %>%
      mutate_all(str_squish) %>%
      mutate(home_score = str_split(score, "\\:", simplify = TRUE, n = 2)[,1]) %>%
      mutate(away_score = str_split(score, "\\:", simplify = TRUE, n = 2)[,2]) %>%
      mutate(goal = str_c(str_split(goal, " ", simplify = TRUE, n = 2)[,2], str_split(goal, " ", simplify = TRUE, n = 2)[,1], sep = " ")) %>%
      mutate(primary_assist = str_c(str_split(primary_assist, " ", simplify = TRUE, n = 2)[,2], str_split(primary_assist, " ", simplify = TRUE, n = 2)[,1], sep = " ")) %>%
      mutate(secondary_assist = str_c(str_split(secondary_assist, " ", simplify = TRUE, n = 2)[,2], str_split(secondary_assist, " ", simplify = TRUE, n = 2)[,1], sep = " ")) %>%        
      mutate(league = league) %>%
      mutate(season = season) %>%
      mutate(game_url = url) %>%
      mutate(home_team = home_team) %>%
      mutate(away_team = away_team) %>%
      mutate(team = case_when(home_score != lag(home_score, n = 1) ~ home_team,
                              away_score != lag(away_score, n = 1) ~ away_team,                                
                              pull(slice(., 1), home_score) == 1 ~ home_team,
                              pull(slice(., 1), away_score) == 1 ~ away_team)) %>%
      mutate_all(as.character) %>%
      mutate_all(str_squish) %>%
      select(team, game_strength, goal, primary_assist, secondary_assist, season, league, game_url)
    
    if (progress) {pb$tick()}
    return(box_score_data)
  }
  
  persistently_getBoxScore <- elite::persistently(.getBoxScore, max_attempts = 10, wait_seconds = 0.0001)
  try_getBoxScore <- function(url, league, season, ...) {
    tryCatch(persistently_getBoxScore(url, league, season, ...), 
             
             error = function(e) {
               print(e) 
               print(url)
               data_frame()},
             
             warning = function(w) {
               print(w) 
               print(url)
               data_frame()})
  }
  
  all_box_score_data <- pmap_dfr(..., try_getBoxScore)
  driver$client$close()
  driver$server$stop()
  return(all_box_score_data)
  
}

# Gets box score data for games needed
scoring_data <- games %>% getBoxScore()