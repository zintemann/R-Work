#####How much does weakest link influence a soccer match?


rm(list = ls())

##Data
starters = read.csv("starters.csv")
library(dplyr)
library(tidyr)

starters = starters %>% gather(id, id.1, home_player_1: away_player_11)

library(RSQLite)

con <- dbConnect(SQLite(), dbname="database.sqlite")
players = dbReadTable(con, "Player_Attributes")
teams = dbReadTable(con, "Team")
matches = dbReadTable(con, "Match")

players$player_fifa_api_id = !duplicated(players$player_fifa_api_id)
players = subset(players, player_fifa_api_id ==TRUE)

starters$match_id = starters$id 
starters = starters[,-1]

FIFA = merge(x=starters, y=players, by.x = 'id.1', by.y = 'player_api_id', all.y=FALSE)

all= merge(x= FIFA, y=matches, by.x = 'match_id', by.y = 'id')


##select the needed variables
use = all[, c(1,2,3,4,5,6,7,8,10,11,50,51,52,53, 54,55,56,57,58)]
use$id.x = substr(use$id.x, 1, 4)

##score vs avg rating vs min rating by team
games = use %>% group_by(match_id, id.x) %>% summarise(away_team = mean(away_team_api_id.x,na.rm=TRUE),
                                                       home_team = mean(home_team_api_id.x,na.rm=TRUE), 
                                                       away_goals= mean(away_team_goal.x,na.rm=TRUE), 
                                                       home_goals=mean(home_team_goal.x,na.rm=TRUE), 
                                                       avg_rating = mean(overall_rating,na.rm=TRUE), 
                                                       min_rating=min(overall_rating,na.rm=TRUE),
                                                       max_rating=max(overall_rating,na.rm=TRUE),
                                                       sd_rating=sd(overall_rating, na.rm=TRUE))

#home team data
home_games = subset(games, id.x=='home')

home_games = home_games %>% rename( home_avg=avg_rating, home_min=min_rating,
                                    home_sd=sd_rating)

#away team data
away_games = subset(games, id.x=='away')
away_games = away_games %>% rename( away_avg=avg_rating, away_min=min_rating,
                                    away_max=max_rating, away_sd=sd_rating)



#all games
all_results = cbind(away_games, home_games)


all_results$win = ifelse(all_results$home_goals > all_results$away_goals & all_results$id.x=='home',
                         1, 0)
all_results$win[all_results$home_goals < all_results$away_goals & all_results$id.x=='away'] = 1

table(all_results$win)

##compare games
all_results$avwin = ifelse(all_results$home_goals > all_results$away_goals & all_results$home_avg > all_results$away_avg,
                           1, 0)
all_results$avwin[all_results$away_goals > all_results$home_goals & all_results$away_avg > all_results$home_avg] = 1


all_results$minwin = ifelse(all_results$home_goals > all_results$away_goals & all_results$home_min > all_results$away_min,
                           1, 0)
all_results$minwin[all_results$away_goals > all_results$home_goals & all_results$away_min > all_results$home_min] = 1


all_results$maxwin = ifelse(all_results$home_goals > all_results$away_goals & all_results$home_max > all_results$away_max,
                            1, 0)
all_results$maxwin[all_results$away_goals > all_results$home_goals & all_results$away_max > all_results$home_max] = 1


all_results$combo = ifelse(all_results$avwin==1 & all_results$minwin==1, 1, 0)

all_results$betteravg = ifelse(all_results$home_avg < all_results$away_avg & all_results$id.x=='away', 1, 0)
all_results$betteravg[all_results$away_avg < all_results$home_avg & all_results$id.x=='home'] = 1

all_results$bettermin= ifelse(all_results$home_min < all_results$away_min & all_results$id.x=='away', 1, 0)
all_results$bettermin[all_results$away_min < all_results$home_min & all_results$id.x=='home'] = 1

all_results$bettermax= ifelse(all_results$home_max < all_results$away_max & all_results$id.x=='away', 1, 0)
all_results$bettermax[all_results$away_max < all_results$home_max & all_results$id.x=='home'] = 1

all_results$highersd= ifelse(all_results$home_sd < all_results$away_sd & all_results$id.x=='away', 1, 0)
all_results$highersd[all_results$away_sd < all_results$home_sd & all_results$id.x=='home'] = 1




  
all_results$goaldif[all_results$id.x=='away'] = all_results$away_goals - all_results$home_goals
all_results$goaldif[all_results$id.x=='home'] = all_results$home_goals - all_results$away_goals

all_results$mindif[all_results$id.x=='away'] = all_results$away_min - all_results$home_min
all_results$mindif[all_results$id.x=='home'] = all_results$home_min - all_results$away_min

all_results$avgdif[all_results$id.x=='away'] = all_results$away_avg - all_results$home_avg
all_results$avgdif[all_results$id.x=='home'] = all_results$home_avg - all_results$away_avg

all_results$maxdif[all_results$id.x=='away'] = all_results$away_max - all_results$home_max
all_results$maxdif[all_results$id.x=='home'] = all_results$home_max - all_results$away_max

all_results$sddif[all_results$id.x=='away'] = all_results$away_sd - all_results$home_sd
all_results$sddif[all_results$id.x=='home'] = all_results$home_sd - all_results$away_sd


table(all_results$avwin)
table(all_results$minwin)
table(all_results$combo)

#ties
all_results$avwin[all_results$away_goals==all_results$home_goals] = 2
all_results$minwin[all_results$away_goals==all_results$home_goals] = 2
all_results$combo[all_results$away_goals==all_results$home_goals] = 2
all_results$maxwin[all_results$away_goals==all_results$home_goals] = 2

library(memisc)
percent(all_results$avwin)
percent(all_results$minwin)
percent(all_results$combo)
percent(all_results$maxwin)

Result = c('Win', 'Loss', 'Tie', 'Win', 'Loss', 'Tie', 'Win', 'Loss', 'Tie')
var = c('Higher Average Rating', 'Higher Average Rating', 'Higher Average Rating',
        'Higher Lowest Rating', 'Higher Lowest Rating', 'Higher Lowest Rating', 
        'Higher Best Rating', 'Higher Best Rating', 'Higher Best Rating')
Probability = c(49.84, 24.07, 26.087, 44.98, 28.93, 26.087 , 46.3, 27.61, 26.087)
dataf = data.frame(Result, var, Probability)

library(ggplot2)
ggplot(dataf, aes(var, Probability)) + geom_bar(stat='Identity',aes(fill=Result)) + xlab('') + ylab('Outcome Percentage') +
  ggtitle("Teams' Player Ratings and Percentage of Outcomes") + scale_fill_brewer(palette = "Blues") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + labs( caption = 'Source: European Soccer Database & EA Sports')

#############
