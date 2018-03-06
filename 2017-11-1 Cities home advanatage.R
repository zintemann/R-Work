###Seeing which cities have best home advnatages in sports

rm(list = ls())
#data
NBA = read.csv('NBA.csv')

NBA = subset(NBA, Year>=2009)

allnba = NBA %>% group_by(Team) %>%
  summarise(homwinpct = mean(HomePCT))

write.csv(allnba, 'allnba.csv')

nba = read.csv('allnba.csv')
nba = nba %>% arrange(Home) 
nfl = read.csv('allnfl.csv')
nfl = nfl %>% arrange(Home) 
mlb = read.csv('baseball home wins.csv')
nfl = nfl %>% arrange(Home) 

teams = rbind(nba, nfl, mlb)
teams = teams %>% arrange(Home) 


library(dplyr)

teams = teams %>% arrange(Home) 

teams$Home <- factor(teams$Home, levels = c('Arizona', 'Atlanta', 'Baltimore', 'Boston', 'Buffalo', 'Carolina', 'Chicago', 'Cincinnati', 'Cleveland', 'Dallas', 'Denver', 'Detroit', 'Houston', 'Indianapolis', 'Jacksonville', 'Kansas City', 'Los Angeles', 'Miami', 'Minnesota', 'New Jersey', 'New Orleans', 'New York', 'Oakland', 'Oklahoma City', 'Orlando', 'Philadelphia', 'Pittsburgh', 'Portland', 'Sacramento', 'San Antonio', 'San Diego', 'San Francisco', 'Seattle', 'St. Louis', 'Tampa Bay', 'Tennessee', 'Toronto', 'Utah', 'Washington', 'Wisconsin'))
teams$winpct = teams$winpct*100

ggplot(teams, aes(Home, winpct)) + geom_point(aes(color=league), size=3) + 
   theme(axis.text.x = element_text(face = 'bold', angle=110), plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(values=c("NBA"="red", "NFL"="blue", "MLB"="orange"), 
                      labels=c("NBA", "NFL", "MLB")) +
  ylab('Winning Percentage') + xlab('') + ggtitle('Home Winning Percentage in City by League') + theme(legend.title=element_blank())
  

teams1 = teams %>% group_by(Home) %>% summarise(Teams = n(), Win_Pct = mean(winpct)) %>% 
  arrange(desc(Win_Pct)) %>% mutate(Home = factor(Home, Home)) 

teams1$Win_Pct = teams1$Win_Pct*100

ggplot(teams1, aes(Home, Win_Pct)) + geom_point(aes(size=Teams), color='purple') + theme(axis.text.x = element_text(face = 'bold', angle=100), plot.title = element_text(hjust = 0.5)) +
  ylab('Winning Percentage') + ggtitle('Best Overall Home Advantages') + xlab('')



teamsmerge = merge(x=nba, y=nfl, by='Home', all = TRUE)
teamsmerge = merge(x=teamsmerge, y=mlb, by='Home', all = TRUE)
teamsmerge = merge(x=teamsmerge, y=teams1, by='Home', all = TRUE)
teamsmerge = subset(teamsmerge, teams > 1)
teamsmerge = teamsmerge %>% arrange(desc(Win_Pct))


library(ggplot2)

ggplot(teamsmerge, aes(y=Home)) + geom_point(aes(x=Win_Pct), color='black') + 
  geom_point(aes(x=winpct.y), color='lightgreen') + geom_point(aes(x=winpct), color='yellow') + 
  geom_point(aes(x=winpct.x), color='black')
