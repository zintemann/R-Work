
rm(list = ls())

library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

#######################################################
food = read.csv("FAO.csv")

food = food %>% filter(Item =='Beer' | Item == 'Wine') %>%
  gather(year, consumption, Y1990:Y2013) %>%
  select(Area.Abbreviation, Area.Code, Area, Item, year, consumption) %>%
  filter(consumption > 0)


food = food %>% group_by(Item, year) %>% summarise(worldtotal = sum(consumption))

#food = food %>% group_by(Area.Abbreviation, Area.Code, Area, Item, year) %>%
# summarise(consumption = sum(consumption)) %>% filter(consumption >= 100)

food$year = as.numeric(str_sub(food$year, 2, 5))
100 * (food[24,3] - food[1,3])/food[1,3]
100 * (food[48,3] - food[25,3])/food[25,3]

ggplot(food, aes(x=year, y=worldtotal)) + geom_line(aes(color=Item), size=1.5) +
  scale_color_brewer(palette="Accent") + theme_bw() + xlab("") + ylab('World total (in 1000 tons)') +
  ggtitle("Global Wine and Beer Production") + labs(caption = "Source: The Food and Agriculture Organization of the United Nations") +
  theme(legend.title=element_blank())

###################################################################################
food = read_csv("food.csv")

food = food %>% filter(Item =='Beer' | Item == 'Wine') %>%
  gather(year, consumption, Y1990:Y2013) %>%
  select(Area, Item, Element, year, consumption) 

food$year = as.numeric(str_sub(food$year, 2, 5))
#food$consumption = as.numeric(food$consumption)

food = food %>% filter(Element == "Production" & year > 1989)
food$consumption[is.na(food$consumption)] = 0

food = food %>% group_by(Item, year) %>% summarise(worldtotal = sum(consumption))

100 * (food[24,3] - food[1,3])/food[1,3] ##76
100 * (food[48,3] - food[25,3])/food[25,3] ##1

ggplot(food, aes(x=year, y=worldtotal)) + geom_line(aes(color=Item), size=1.5) +
  scale_color_brewer(palette="Accent") + theme_bw() + xlab("") + ylab('World total (in 1000 tons)') +
  ggtitle("Global Wine and Beer Production") + labs(caption = "Source: The Food and Agriculture Organization of the United Nations") +
  theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust = 0.5))
########################################################################################3



food = read_csv("food.csv")

food1 = food %>% filter(Item =='Beer' | Item == 'Wine') %>%
  select( Area, Item, Element, Y1990, Y2013) %>%
  filter(Area == 'France' | Area == 'United Kingdom' | Area == 'Italy' | Area == 'Spain' |
           Area == 'Australia' | Area == 'Canada'  | Area == 'Germany'  |
           Area == 'United States of America' | Area == 'Denmark' |
           Area == 'Japan' | Area == 'Finland' | Area == 'Norway' | Area == 'Sweden')
food1 = food1 %>% filter(Element == "Domestic supply quantity")

food1$Area[food1$Area=='United States of America'] = "USA"
food1$Area[food1$Area=='United Kingdom'] = "UK"

food1$consumptionchange = food1$Y2013 - food1$Y1990

ggplot(food1, aes(x=Area, y=consumptionchange)) + geom_bar(stat = 'identity', aes(fill=Item), position='dodge') +
  scale_fill_brewer(palette="Accent") + theme_bw() + xlab("") + ylab("Change in National Supply (in 1000 tons)") +
  ggtitle("Change in Domestic Supply of Beer and Wine in Developed Countries") + labs(caption = "Source: The Food and Agriculture Organization of the United Nations") +
  theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust = 0.5))



#food = read_csv("food.csv")

food2 = food %>% filter(Item =='Beer' | Item == 'Wine') %>%
  select(Area, Item, Element, Y1990, Y2013) %>%
  filter(Area == 'Brazil' | Area == 'Colombia' | Area == 'China, mainland' | Area == 'South Africa' |
           Area == 'Nigeria' |  Area == 'Thailand'  |
           Area == 'Mexico' | Area == 'India' | Area == 'Philippines' |
           Area == 'Chile' | Area == 'Turkey')

food2 = food2 %>% filter(Element == "Domestic supply quantity")
food2$consumptionchange = food2$Y2013 - food2$Y1990
#food2$pctchange = 100 * (food2$Y2013 - food2$Y1990)/food2$Y1990

food2$Area[food2$Area=='China, mainland'] = "China"
food2$consumptionchange = food2$Y2013 - food2$Y1990

ggplot(food2, aes(x=Area, y=consumptionchange)) + geom_bar(stat = 'identity', aes(fill=Item), position='dodge') +
  scale_fill_brewer(palette="Accent") + theme_bw() + xlab("") + ylab("Change in National Supply (in 1000 tons)") +
  ggtitle("Change in Domestic Supply of Beer and Wine in Developing Countries") + labs(caption = "Source: The Food and Agriculture Organization of the United Nations") +
  theme(legend.title=element_blank()) + theme(plot.title = element_text(hjust = 0.5))

food2$pctchange = 100 * (food2$Y2013 - food2$Y1990)/food2$Y1990
