##Which European soccer leagues have the most foreigners

rm(list = ls())



###################Fifa data
FIFA = read.csv("FIFA Full Data.csv")
FIFA$championsleague[FIFA$championsleague==1] = "Champions League"               
FIFA$championsleague[FIFA$championsleague==0] = "Not Champions League"               
FIFA$Level = FIFA$championsleague

champions = c("Real Madrid", "FC Barcelona", "Atl?f?f?,©tico Madrid", "Sevilla Atl?f?f?,©t", "Juventus", "Roma", "Naopil", "Atalanta", "Spurs", "Chelsea", "Machester City", "Liverpool", "FC Bayern", "RB Leipzig", "Bor. Dortmund", "1899 Hoffenheim", "AS Monaco", "PSG", "OGC Nice", "Olym. Lyonnais")


FIFA = subset(FIFA, league!="")
### averages by league
install.packages("doBy")
library(doBy)

collapsed1 = summaryBy(internationalplayer + foreigner + native
                       ~ league, FUN=mean, data = FIFA)
collapsed2 = summaryBy(internationalplayer + foreigner + native
                         ~ league + Level, FUN=mean, data = FIFA)

col1 = data.frame(collapsed1)
col$foreignpct = col$foreign.mean

install.packages("ggplot2")
library(ggplot2)

p1 = ggplot(data=collapsed1, aes(league, foreigner.mean*100))
p1 + geom_col() + ylab("Percentage of players that are foreign") + xlab("League") + ggtitle("Percentage of Foreign Born Players by League") + geom_bar(stat="identity", fill="steelblue") 


p2 = ggplot(data=collapsed2, aes(league, foreigner.mean*100, fill=Level))
p2 + geom_bar(stat="identity", position=position_dodge()) + ylab("Percentage of players that are foreign") + xlab("League") + ggtitle("Percentage of Foreign Born Players by Team Quality") + theme(plot.title = element_text(hjust = 0.5)) 


################################################3
##Comparing Mexican and American Soccer Players using FIFA Data

rm(list = ls())
FIFA = read.csv("FIFA Full Data2.csv")
FIFA$color[FIFA$nationality=="United States"] <- "brown"
FIFA$color[FIFA$nationality=="Mexico"] <- "Green"


############league
MEXUSA = subset(FIFA, league=="Mexico" | league=="MLS")

g1 = ggplot(MEXUSA, aes(rating, fill=league)) +
  geom_density(aes(fill=factor(league)), alpha=0.5) +
  scale_fill_manual( values = c("green","red")) +
  labs(title = "MLS Compared to Mexican League",
       x = "FIFA Rating", y = "Density", fill="League",
       caption = "Source: EA FIFA") +
  theme(plot.title = element_text(hjust = 0.5))
g1

stargazer(subset(MEXUSA["rating"], MEXUSA$league=="MLS"), type = "text")
stargazer(subset(MEXUSA["rating"], MEXUSA$league=="Mexico"), type = "text")



###########league2
MEXUSA2 = subset(MEXUSA, club != "Chicago Fire" & club != "Houston Dynamo")

g2 = ggplot(MEXUSA2, aes(rating, fill=league)) +
  geom_density(aes(fill=factor(league)), alpha=0.5) +
  scale_fill_manual( values = c("green","red")) +
  labs(title = "MLS Compared to Mexican League",
       x = "FIFA Rating", y = "Density", fill="League",
       caption = "Source: EA FIFA") +
  theme(plot.title = element_text(hjust = 0.5))
g2

stargazer(subset(MEXUSA2["rating"], MEXUSA2$league=="MLS"), type = "text")



stargazer(subset(FIFA["rating"], FIFA$nationality=="United States"), type = "text")
stargazer(subset(FIFA["rating"], FIFA$nationality=="Mexico"), type = "text")





#######################all players by birth
table(FIFA$nationality)
MEXUSAbirth = subset(FIFA, nationality=="Mexico" | nationality=="United States")


g3 = ggplot(MEXUSAbirth, aes(rating, fill=nationality)) +
  geom_density(aes(fill=factor(nationality)), alpha=0.5) +
  scale_fill_manual( values = c("green","red")) +
  labs(title = "All Players Born in Mexico vs US",
       x = "FIFA Rating", y = "Density", fill="Nationality",
       caption = "Source: EA FIFA") +
  theme(plot.title = element_text(hjust = 0.5))
g3

stargazer(subset(FIFA["rating"], FIFA$nationality=="United States"), type = "text")
stargazer(subset(FIFA["rating"], FIFA$nationality=="Mexico"), type = "text")


##################### national teams
MEXUSAinternational = subset(MEXUSAbirth, internationalplayer==1)
  
g4 = ggplot(MEXUSAinternational, aes(rating, fill=nationality)) +
  geom_density(aes(fill=factor(nationality)), alpha=0.5) +
  scale_fill_manual( values = c("green","red")) +
  labs(title = "All Players Born in Mexico vs US for National Team Players",
       x = "FIFA Rating", y = "Density", fill="Nationality",
       caption = "Source: EA FIFA") +
  theme(plot.title = element_text(hjust = 0.5))
g4 


stargazer(subset(FIFA["rating"], FIFA$nationality=="United States" & FIFA$internationalplayer==1), type = "text")
stargazer(subset(FIFA["rating"], FIFA$nationality=="Mexico" & FIFA$internationalplayer==1), type = "text")

summary(MEXUSA$rating[MEXUSA$nationality=="United States"])
summary(MEXUSA$rating[MEXUSA$nationality=="Mexico"])
