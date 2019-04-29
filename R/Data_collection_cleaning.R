library(XML)
library(RCurl)
library(nbastatR)
library(tidyverse)


# Data collection from basketball-reference and stats.nba.com

team_rating_url <- 'https://www.basketball-reference.com/leagues/NBA_2019_ratings.html'
team_names <- as.character(readHTMLTable(getURL(team_rating_url))$ratings$Team)
for (year in 2004:2019) {
  teams <- teams_tables(teams = team_names, seasons = year, tables = c('splits', 'shooting'),
                        measures = c('Base', 'Advanced'), modes = 'PerGame', season_types = 'Regular Season' )
  players <- cbind(readHTMLTable(getURL(paste0('https://www.basketball-reference.com/leagues/NBA_',year,'_per_game.html', collapse = ''))),
                   readHTMLTable(getURL(paste0('https://www.basketball-reference.com/leagues/NBA_',year,'_advanced.html', collapse = ''))))
  save(teams, file = paste0('data/raw/team_',year,'.RData', collapse = ''))
  save(players, file = paste0('data/raw/player_',year,'.RData', collapse = ''))
}

# Data Cleaning

Team_splits = Team_shooting = Player = c()
for (year in 2004:2019) {
  load(paste0('data/raw/team_',year,'.RData', collapse = ''))
  load(paste0('data/raw/player_',year,'.RData', collapse = ''))

  players[[2]] <- players[[2]][,-c(1,3:7,20,25)]
  P <- left_join(players[[1]], players[[2]], by = 'Player')
  P <- P %>% filter(Rk != 'Rk') %>% select(-Rk) %>%
    mutate_all(as.character) %>%
    mutate_at(vars(Age, G:VORP), as.numeric) %>%
    distinct(Player, Tm, .keep_all = TRUE)
    mutate(Pos = factor(Pos, levels = c('PG', 'PG-SG', 'PG-SF', 'SG-PG', 'SG', 'SG-SF', 'SG-PF', 'SF-SG', 'SF', 'SF-PF', 'PF-SF', 'PF', 'PF-C', 'C-PF', 'C')))
    levels(P$Pos) = c('Guards', 'Guards', 'Guards', 'Guards', 'Guards', 'Wings', 'Wings', 'Wings', 'Wings', 'Wings', 'Wings', 'Bigs', 'Bigs', 'Bigs', 'Bigs')
    P[P$Tm=='BRK',]$Tm = 'BKN'
    P[P$Tm=='CHA' & P$year <= 2014,]$Tm = 'CHB'
  P <- bind_cols(P, year = rep(year, dim(P)[1]))
  
  Player <- bind_rows(Player, P)

  team_names = unique(teams$nameTeam)

  teams_base_split <- teams %>% filter(tableSlugName == 'dataSplitsTeam' & typeMeasure == 'Base' & nameTable == 'LocationTeamDashboard') %>%
    pull(dataTable) %>% bind_rows() %>%
    bind_cols(team = rep(team_names, rep(2, length(team_names)))) %>%
    select(-c(typeFilter, gp, gpRank:pctFTRank, wins:minutes, plusminus:plusminusRank)) %>%
    group_by(team) %>%
    summarise_all(mean)

  teams_advanced_split <- teams %>% filter(tableSlugName == 'dataSplitsTeam' & typeMeasure == 'Advanced' & nameTable == 'LocationTeamDashboard') %>%
    pull(dataTable) %>% bind_rows() %>%
    bind_cols(team = rep(team_names, rep(2, length(team_names)))) %>%
    select(-c(typeFilter:pctWins, gpRank:minutes, winsRank:pieRank)) %>%
    group_by(team) %>%
    summarise_all(mean)

  team_splits <- left_join(teams_base_split, teams_advanced_split, by = 'team')
  team_splits <- bind_cols(team_splits, year = rep(year, dim(team_splits)[1]))

  Team_splits <- bind_rows(Team_splits, team_splits)

  team_shooting <- teams %>% filter(tableSlugName == 'dataShootingTeam' & typeMeasure == 'Base' & nameTable == 'Shot8FTTeamDashboard') %>%
    pull(dataTable) %>% bind_rows() %>%
    bind_cols(team = rep(team_names, rep(5, length(team_names)))) %>%
    mutate(distance = typeFilter) %>%
    select(c(team, distance, fgm:pctFG))
  team_shooting <- bind_cols(team_shooting, year = rep(year, dim(team_shooting)[1]))

  Team_shooting = bind_rows(Team_shooting, team_shooting)
}

save(Player, file = 'data/tidy/Player.RData')
save(Team_splits, file = 'data/tidy/Team_splits.RData')
save(Team_shooting, file = 'data/tidy/Team_shooting.RData')