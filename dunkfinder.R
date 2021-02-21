library(ncaahoopR)
library(tidyverse)

dunk_finder <- function(team){
team = team
data <- get_pbp(team)

dunk <- data%>%
  filter(shot_team == dict$ESPN_PBP[dict$ESPN == team])%>%
  mutate(dunk_att = ifelse(str_detect(description, "Dunk"), TRUE, FALSE))

dunk_dunk <- dunk%>%
  mutate(dunk_made = ifelse(shot_outcome == "made" & dunk_att == TRUE, 1, 0))%>%
  mutate(dunk_attem= ifelse(is.na(shot_outcome) == FALSE & dunk_att == TRUE,1,0))%>%
  group_by(shooter)%>%
  summarize(makes = sum(dunk_made), attempts = sum(dunk_attem), per = (makes/attempts) * 100)
}
