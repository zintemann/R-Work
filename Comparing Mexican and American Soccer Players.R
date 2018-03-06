################################################3
##Comparing Mexican and American Soccer Players using FIFA Data

library(stargazer)

#data
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


# national teams
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
