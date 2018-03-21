### Seinfeld Episodes
rm(list = ls())

library(readr)
library(stringr)

##Data 
episodes = read_csv('episode_info.csv')
scripts = read.csv('scripts.csv', stringsAsFactors = FALSE)


#fix strings
episodes$strfix = str_detect(episodes$Title, "(1)")
episodes$strfix2 = str_detect(episodes$Title, "(2)")
episodes$Title = ifelse(episodes$strfix ==TRUE | episodes$strfix2 ==TRUE, str_sub(episodes$Title, 1, -4), episodes$Title )
episodes$Title[episodes$Title == 'The Reverse Peephole (a.k.a. The Man Fur)'] = 'The Reverse Peephole'

episodes$Title = str_to_lower(episodes$Title)
scripts$Dialogue= str_to_lower(scripts$Dialogue)

##Merge data using dplyr
library(dplyr)

seinfeld = scripts %>% 
  left_join(episodes, by=c('EpisodeNo', 'SEID'))

##find name in scripts
seinfeld$New_Title = ifelse(str_sub(seinfeld$Title, 1, 3) =='the', 
                           str_sub(seinfeld$Title, 4, -1), seinfeld$Title)

seinfeld$count = str_count(seinfeld$Dialogue ,seinfeld$New_Title)


#count by episode

title_count = seinfeld %>% group_by(Title, New_Title, Season.x, Director, Writers) %>%
  summarise(count = sum(count)) 

title_count$Title = str_to_title(title_count$Title)

mean(title_count$count) #8.24

####Plot
top_episodes = subset(title_count, count >= 20)
top_episodes$Season.x = as.factor(top_episodes$Season.x)

top_episodes = top_episodes %>% mutate(Season = Season.x)

library(ggplot2)
library(plotly)


g = ggplot(top_episodes, aes(x=Title, y=count)) + geom_bar(stat = 'identity', aes(fill=Season)) + 
  ggtitle("The Most Times The Seinfeld Episode Title is Mentioned in The Episode") +
  theme_bw() + ylab("Number of Times The Title is Said in The Episode") +
  theme(axis.text.x = element_text(face = 'bold', angle=450), plot.title = element_text(hjust = 0.5))
g

print(ggplotly(g))
