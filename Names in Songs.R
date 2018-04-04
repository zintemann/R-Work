#### How often do names appear in song titles>

rm(list = ls())

library(ggplot2)
library(stringr)
library(dplyr)
library(stringi)

#names data

names = read.csv('Names.csv', stringsAsFactors = FALSE)

names = names %>% select(Name, Gender, Year, Count) %>% group_by(Name, Gender) %>%
  mutate(NewCount = sum(Count)) %>% filter(NewCount  >= 50000)

names$Name = str_to_title(names$Name)
#names$Name = stri_pad_both(names$Name, 5, pad = " ")

# songs

songs = read.csv('Songs.csv', stringsAsFactors = FALSE)
songs = songs %>% select(Song, Artist, Year)
songs$Song = str_to_title(songs$Song)

###merge
newsongs = songs %>% full_join(names, by='Year')


##number of times each name is song


newsongs$name.in.title = str_count(newsongs$Song, newsongs$Name)
newsongs2 = subset(newsongs, name.in.title > 0)
newsongs2 = subset(newsongs2, str_length(Name) >= 4)

namecounts = newsongs2 %>% group_by(Name, Gender) %>%
  summarise(times = sum(name.in.title))

lastdf = namecounts %>% inner_join(newsongs2, by= c('Name', 'Gender')) %>%
  select(Name, Gender, times, Song, Artist, Year) %>% filter(times < 20 & times > 2)


Name = c('Bill', 'Jack', 'John', 'Mary', 'Ruby', 'Sara')
Gender = c('M', 'M', 'M', 'F', 'F', 'F')
Songs = c(5, 3, 3, 3, 3, 3)

final = data.frame(Name, Gender, Songs)



ggplot(final, aes(x=Name, y=Songs)) + geom_bar(stat='identity', aes(fill=Gender), color='black') +
  ggtitle("Most Common Names in Billboard Top 100 Songs since 1964") + 
  ylab('Number of Songs') +
  annotate('text', x='Bill', y=4.5, label="Don't Mess with Bill") +
  annotate('text', x='Bill', y=4.3, label="by The Marvelettes") +
  annotate('text', x='Bill', y=4.1, label="(1966)") +
  
  annotate('text', x='Bill', y=3.7, label="Which Way You") +
  annotate('text', x='Bill', y=3.5, label="Goin Billy") +
  annotate('text', x='Bill', y=3.3, label="by The Poppy") +
  annotate('text', x='Bill', y=3.1, label="Family") +
  annotate('text', x='Bill', y=2.9, label="(1970)") +
  
  annotate('text', x='Bill', y=2.5, label="Ode to Billie Joe") +
  annotate('text', x='Bill', y=2.3, label="by Bobbie Gentry") +
  annotate('text', x='Bill', y=2.1, label="(1967)") +
  
  annotate('text', x='Bill', y=1.7, label="Billy Don't Be a") +
  annotate('text', x='Bill', y=1.5, label="Hero") +
  annotate('text', x='Bill', y=1.3, label="by Bo Donaldson") +
  annotate('text', x='Bill', y=1.1, label="and The Heywoods") +
  annotate('text', x='Bill', y=0.9, label="(1974)") +

  annotate('text', x='Bill', y=0.5, label="My Girl Bill") +
  annotate('text', x='Bill', y=0.3, label="by Jim Stafford") +
  annotate('text', x='Bill', y=0.1, label="(1974)") +
  
  annotate('text', x='Jack', y=2.5, label="Jack and Diane") +
  annotate('text', x='Jack', y=2.3, label="by John Cougar") +
  annotate('text', x='Jack', y=2.1, label="(1982)") +
  
  annotate('text', x='Jack', y=1.5, label="Jack and Jill") +
  annotate('text', x='Jack', y=1.3, label="Raydio") +
  annotate('text', x='Jack', y=1.1, label="(1978)") +
  
  annotate('text', x='Jack', y=0.7, label="Jumpin Jack Flash") +
  annotate('text', x='Jack', y=0.5, label="by The Rollling") +
  annotate('text', x='Jack', y=0.3, label="Stones") +
  annotate('text', x='Jack', y=0.1, label="(1968)") +
  
  annotate('text', x='John', y=2.5, label="Empty Garden") +
  annotate('text', x='John', y=2.3, label="(Hey Hey Johnny)") +
  annotate('text', x='John', y=2.1, label="by Elton John") +
  annotate('text', x='John', y=1.9, label="(1982)") +
  
  annotate('text', x='John', y=1.5, label="Who's Johnny") +
  annotate('text', x='John', y=1.3, label="by El Debarge") +
  annotate('text', x='John', y=1.1, label="(1986)") +
  
  annotate('text', x='John', y=0.7, label="Sloop John B") +
  annotate('text', x='John', y=0.5, label="by The Beach") +
  annotate('text', x='John', y=0.3, label="Boys") +
  annotate('text', x='John', y=0.1, label="(1966)") +
  
  annotate('text', x='Mary', y=2.5, label="Proud Mary") +
  annotate('text', x='Mary', y=2.3, label="by Creedence") +
  annotate('text', x='Mary', y=2.1, label="Clearwater Revival") +
  annotate('text', x='Mary', y=1.9, label="(1969)") +
  
  annotate('text', x='Mary', y=1.5, label="Proud Mary") +
  annotate('text', x='Mary', y=1.3, label="by Tina Turner") +
  annotate('text', x='Mary', y=1.1, label="(1971)") +
  
  annotate('text', x='Mary', y=0.7, label="Sweet Mary") +
  annotate('text', x='Mary', y=0.5, label="by Wadsworth") +
  annotate('text', x='Mary', y=0.3, label="Mansion") +
  annotate('text', x='Mary', y=0.1, label="(1971)") +
  
  annotate('text', x='Ruby', y=2.7, label= "Ruby Tuesday") +
  annotate('text', x='Ruby', y=2.5, label= "by The Rolling") +
  annotate('text', x='Ruby', y=2.3, label= "Stones") +
  annotate('text', x='Ruby', y=2.1, label= "(1967)") +
  
  annotate('text', x='Ruby', y=1.7, label="Ruby Don't Take") +
  annotate('text', x='Ruby', y=1.5, label="Your Love To Town") +
  annotate('text', x='Ruby', y=1.3, label="by Kenny Rogers") +
  annotate('text', x='Ruby', y=1.1, label="(1969)") +
  
  annotate('text', x='Ruby', y=0.7, label="Leave Me Alone") +
  annotate('text', x='Ruby', y=0.5, label="Ruby Red Dress") +
  annotate('text', x='Ruby', y=0.3, label="by Helen Reddy") +
  annotate('text', x='Ruby', y=0.1, label=("1974")) +
  
  annotate('text', x='Sara', y=2.5, label="Sara Smile") +
  annotate('text', x='Sara', y=2.3, label="by Hall & Oates") +
  annotate('text', x='Sara', y=2.1, label="(1976)") +
  
  annotate('text', x='Sara', y=0.5, label="Sara") +
  annotate('text', x='Sara', y=0.3, label="by Starship") +
  annotate('text', x='Sara', y=0.1, label="(1986)") +
  
  annotate('text', x='Sara', y=1.5, label="Sara") +
  annotate('text', x='Sara', y=1.3, label="by Fleetwood Mac") +
  annotate('text', x='Sara', y=1.1, label="(1980)") +
  
  scale_fill_brewer(palette="Spectral") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  
  
