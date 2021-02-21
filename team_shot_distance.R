library(ncaahoopR)
library(tidyverse)

team_shot_distance <- function(team){
  team = team
  gids <- get_game_ids(team)
  
  data <- get_shot_locs(gids)
  
  data <- data%>%
    filter(team_name == dict$ESPN_PBP[dict$ESPN == team])
  
  
  if(!is.null(data)) {
    side_one <- court %>% filter(side == 1)
    team_shots <- data %>% filter(team_name %in% c(team, dict$ESPN_PBP[dict$ESPN == team]))
    
    ### flip shots if they are on the wrong side
    team_shots[team_shots$y > 47, "x"] <- 50 - team_shots[team_shots$y > 47, "x"]
    team_shots[team_shots$y > 47, "y"] <- 94 - team_shots[team_shots$y > 47, "y"]
    
  }
  
  team_shots <- team_shots%>%
    mutate(distance = sqrt((x - 25)**2 + (y - 6.266)**2))
  
  
  savg_dist<- team_shots%>%
    group_by(shooter)%>%
    summarize(avgDIS = mean(distance), shots = n())%>%
  
  return(avg_dist)
  
}