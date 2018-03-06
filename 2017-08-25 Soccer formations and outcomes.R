###soccer formations and outcomes

rm(list = ls())

######Data
footie = read.csv('FootballEurope.csv')

footie$homepossession_dif = footie$homePossessionFT/90 - footie$awayPossessionFT/90
footie$awaypossession_dif = footie$awayPossessionFT/90 - footie$homePossessionFT/90
footie$homewin = ifelse(footie$homeGoalHT > footie$awayGoalHT, 1, 0)

#which variables ot keep
home = footie[, c(1:4, 13:16, 91, 21, 25:27, 29, 30, 34,36, 38, 40,  
                  44,45,47,50,51, 53, 58, 61, 64, 66:68, 73, 76,77, 78, 79,
                  80:82, 85,86, 89:91, 94, 95, 97)]
away = footie[,c (1,2,59, 11, 12, 17,18,20,22:24,28, 32,33, 35,37, 39, 41,
                  42, 46, 47, 49, 51, 52, 53, 54, 56, 59,60, 62, 63, 65, 69, 72,
                  75, 78, 79, 84, 88, 92,93, 96, 97 )]


home$win = ifelse(home$homeGoalFT > home$awayGoalFT, 1, 0)
home$win[home$homeGoalFT == home$awayGoalFT] = 2
away$win = ifelse(away$homeGoalFT > away$awayGoalFT, 1, 0)
away$win[away$homeGoalFT == away$awayGoalFT] = 2


library(MASS)
library(dplyr)
library(plyr)

home = home[, c('X', 'win', 'homeShotsOnTargetFT',  'homePossessionFT', 'homeFormation', 'homepossession_dif', "homeGoalFT", 'homewin')]
away = away[, c('X', 'win', 'awayShotsOnTargetFT', 'awayPossessionFT', 'awayFormation', 'awaypossession_dif', "awayGoalFT", 'homewin')]

home = rename(home, c("homeShotsOnTargetFT"="ShotsOnTargetFT", "homePossessionFT"="PossessionFT",
               "homeFormation"="Formation", "homepossession_dif"="possession_dif", "homeGoalFT"="GoalFT", 'homeShotsOnTargetFT'= 'ShotsOnTargetFT'))
away = rename(away, c("awayShotsOnTargetFT"="ShotsOnTargetFT", "awayPossessionFT"="PossessionFT",
                      "awayFormation"="Formation", "awaypossession_dif"="possession_dif", "awayGoalFT"="GoalFT", 'awayShotsOnTargetFT'= 'ShotsOnTargetFT'))

home$home = 1
away$home = 0

games = rbind(away,home)
detach(package:plyr)

games2 = games %>% group_by(X) %>% summarise(max_goal = max(GoalFT, na.rm=TRUE))

games = merge(x=games, y=games2, by='X')

games$win = ifelse(games$GoalFT == games$max_goal & games$win!=2, 1, 0)


games$Formation[games$Formation=='343d'] = '343'
games$Formation[games$Formation=='4240'] = '424'

####Regressions
regression = lm(win~ home + PossessionFT + ShotsOnTargetFT, data= games)
summary(regression)

reg = lm(win ~  home, data=games)
summary(reg)

reg2 = lm(GoalFT ~ PossessionFT, data=games)
summary(reg2)

reg3 = lm(ShotsOnTargetFT ~ PossessionFT, data=games)
summary(reg3)

#difference in possession


#number of shots
regshots = lm(ShotsOnTargetFT ~ Formation, data = games)
summary(regshots)

regposs = lm(possession_dif ~ Formation, data = games)
summary(regposs)

regwin = lm(win ~ Formation, data = games)
summary(regwin)

reggoal = lm(GoalFT ~ Formation, data = games)
summary(reggoal)



formations = data.frame(regshots$xlevels ,regshots$coefficients, regposs$coefficients,
                        reggoal$coefficients, regwin$coefficients)
formations = formations[-1,]

formations2 = rbind(regshots$xlevels ,regshots$coefficients, regposs$coefficients,
                        reggoal$coefficients, regwin$coefficients)


####Plots
library(ggplot2)
games = filter(games, !is.na(games$Formation)) #[!is.na(games$Formation)]

ggplot(games, aes(x=Formation)) + geom_bar(fill='lightgreen') + ylab('Number of games used') + ggtitle("Usage of Different Soccer Formations") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(formations, aes(x=Formation)) + geom_point(aes(y=regshots.coefficients, colour = 'Shots on Target'), size=4) + 
  geom_point(aes(y=regposs.coefficients, colour = 'Ball Possession Percentage'), size=4) +
  geom_point(aes(y=reggoal.coefficients, colour = 'Goals'), size=4) +
  geom_point(aes(y=regwin.coefficients, colour = 'Win'), size=4) +
  ylab("") + ggtitle("Formations, Shots, Possession, Goals, and Wins") + theme_bw() +
  scale_colour_manual("", breaks = c('Shots on Target', 'Ball Possession Percentage', 'Goals', 'Win'),
                      values = c('Shots on Target' = 'lightblue', 'Ball Possession Percentage' =  'yellow', 'Goals' = 'lightgreen', 'Win'= 'red')) +
  theme(plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept =0) 
