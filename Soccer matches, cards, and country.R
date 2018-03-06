##the correlation between Red cards and exposure to civil war in native coutnry

rm(list = ls())


library(dplyr)
library(tidyr)
library(ggplot2)

library(RSQLite)


####Player and game data

con <- dbConnect(SQLite(), dbname="database.sqlite")
players = dbReadTable(con, "Player")

cards = read.csv('Matches and Cards.csv')

cards1 = cards %>% gather(id, player_id, home_player_1: away_player_11)

cards2 = merge(x=cards1, y=players, by.x = 'player_id', by.y='player_api_id')
FIFA = read.csv('PlayerPersonaldata.csv')

cards2 = merge(x=cards2, y=FIFA, by.x= 'player_fifa_api_id', by.y='ID')

cards2$booked = ifelse(cards2$player_id==cards2$card1 | cards2$player_id==cards2$card2 | 
                         cards2$player_id==cards2$card3 | cards2$player_id==cards2$card4 |
                         cards2$player_id==cards2$card5 | cards2$player_id==cards2$card6, 1, 0)

table(cards2$Nationality, cards2$booked)


cards3 = cards2[,-3]
#cards3 = subset(cards3, cards3$country != 15722 | cards3$country != 13274 | cards3$country != 24558 |
#                 cards3$country != 19694)  #Switzerland, Netherlands, Poland, scotland 

cards3 = subset(cards3, cards3$country == 1729 | cards3$country == 4769 | 
                  cards3$country == 10257 | cards3$country == 21518 |
                  cards3$country == 7809)

cards3a = subset(cards3, season=='2008/2009')
cards4 = subset(cards3, season=='2015/2016' |  season=='2014/2015')

countries3a = cards3a %>% group_by(Nationality) %>% 
  summarise(games = n(), cards = sum(booked))

countries4 = cards4 %>% group_by(Nationality) %>% 
  summarise(games = n(), cards = sum(booked))

countries3a= subset(countries3a, games >= 5)
countries4 = subset(countries4, games >= 5)

countries3a$cardspgame = countries3a$cards/countries3a$games
countries4$cardspgame = countries4$cards/countries4$games

##corruption data
corrupt = read.csv('TI-corruption-perception-index.csv')

corrupt = corrupt %>% group_by(Entity) %>% summarise(avg_score = mean(Corruption.Perception.Index))


##civil war data
wars = read.csv('intrastate.csv')
wars = subset(wars, start_year1 >= 1980)

wars$end_year1[wars$end_year1==-7] = 2017
wars$duration = wars$end_year1 - wars$start_year1

waryears = wars %>% group_by(side1_name) %>% summarise(years = sum(duration))
waryears = waryears[-1,]


##merge all data
DF3 = merge(x=countries3a, y  = corrupt, by.x = 'Nationality', by.y = 'Entity')
DF3 = merge(x=DF3, y  = waryears, by.x = 'Nationality', by.y = 'side1_name', all.x=TRUE)
DF3$years[is.na(DF3$years)] = 0

DF4 = merge(x=countries4, y  = corrupt, by.x = 'Nationality', by.y = 'Entity')
DF4 = merge(x=DF4, y  = waryears, by.x = 'Nationality', by.y = 'side1_name', all.x=TRUE)
DF4$years[is.na(DF4$years)] = 0



##Regressions
cardsreg3a = lm(cardspgame ~ avg_score, data=DF3)
summary(cardsreg3a)  
cardsreg4a = lm(cardspgame ~ avg_score, data=DF4)
summary(cardsreg4a) 


cardsreg3b = lm(cardspgame ~  years, data=DF3)
summary(cardsreg3b) 
cardsreg34 = lm(cardspgame ~  years, data=DF4)
summary(cardsreg34)
