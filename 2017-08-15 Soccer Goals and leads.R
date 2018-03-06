#####How safe are 2-0 leads in Soccer

rm(list = ls())

#data 
events = read.csv("events.csv")

goals = subset(events, is_goal==1)


library(dplyr)
score = goals %>% group_by(id_odsp, event_team) %>% arrange(id_odsp, time) %>% mutate(seqnum = 1:length(id_odsp))
scores = goals %>% group_by(id_odsp, event_team) %>% arrange(time, event_team) %>% mutate(seq = rank(id_odsp, event_team))

regexp <- c("[[:digit:]]+", "[[:digit:]]+")

# process string
library(qdap)
goals$g1 = str_extract(goals$text, regexp)

#goals$g2 = strsplit(goals$text, ",", "[[", 1)
goals$score = as.numeric(gsub("\\D", "", goals$text))
goals$g1 = substr(goals$score, 1, 1)
goals$g2 = substr(goals$score, 2, 2)

class(goals$g1)
goals$g1 = as.integer(goals$g1)
goals$g2 = as.integer(goals$g2)
goals$g2[is.na(goals$g2)] = 0


goals = merge(x=goals, y= score1, by= c('id_odsp', 'event_team'))



##### Scores at time of goal
#1-0
goals$s1_0_a = ifelse(goals$g1==1 & goals$g2==0, 1, 0)
goals$s1_0_b[goals$g1==0 & goals$g2==1] = 1
#2-0
goals$s2_0_a = ifelse(goals$g1==2 & goals$g2==0, 1, 0)
goals$s2_0_b[goals$g2==0 & goals$g2==2] = 1
#2-1
goals$s2_1_a = ifelse(goals$g1==2 & goals$g2==1, 1, 0)
goals$s2_1_b[goals$g1==1 & goals$g2==2] = 1
#3-1
goals$s3_1_a = ifelse(goals$g1==3 & goals$g2==1, 1, 0)
goals$s3_1_b[goals$g1==1 & goals$g2==3] = 1
#3-2
goals$s3_2_a = ifelse(goals$g1==3 & goals$g2==2, 1, 0)
goals$s3_2_b[goals$g1==2 & goals$g2==3] = 1
#3-0
goals$s3_0_a = ifelse(goals$g1==3 & goals$g2==0, 1, 0)
goals$s3_0_b[goals$g1==0 & goals$g2==3] = 1


###scores at time of goal in first half
#1-0
goals$s1_0_half_a = ifelse(goals$g1==1 & goals$time <= 45, 1, 0)
goals$s1_0_half_b[goals$g2==0 & goals$g2==1 & goals$time <= 45] = 1
#2-0
goals$s2_0_half_a = ifelse(goals$g1==2 & goals$g2==0 & goals$time <= 45, 1, 0)
goals$s2_0_half_b[goals$g2==0 & goals$g2==2 & goals$time <= 45] = 1
#2-1
goals$s2_1_half_a = ifelse(goals$g1==2 & goals$g2==1 & goals$time <= 45, 1, 0)
goals$s2_1_half_b[goals$g1==1 & goals$g2==2 & goals$time <= 45] = 1
#3-1
goals$s3_1_half_a = ifelse(goals$g1==3 & goals$g2==1 & goals$time <= 45, 1, 0)
goals$s3_1_half_b[goals$g1==1 & goals$g2==3 & goals$time <= 45] = 1
#3-2
goals$s3_2_half_a = ifelse(goals$g1==3 & goals$g2==2 & goals$time < 45, 1, 0)
goals$s3_2_half_b[goals$g1==2 & goals$g2==3 & goals$time <= 45] = 1
#3-0
goals$s3_0_half_a = ifelse(goals$g1==3 & goals$g2==0 & goals$time < 45, 1, 0)
goals$s3_0_half_b[goals$g1==0 & goals$g2==3 & goals$time <= 45] = 1


###Score at lat point of game 2 columns: one for each team
goals$g1 = as.integer(goals$g1)
goals$g2 = as.integer(goals$g2)
goals$g1[is.na(goals$g1)] = 0
goals$g2[is.na(goals$g2)] = 0

goals$g1[goals$g1==8] = 0
goals$g2[goals$g1==8] = 0

## final score
goals = transform(goals, final1 = ave(g1, id_odsp, FUN = max))
goals = transform(goals, final2 = ave(g2, id_odsp, FUN = max))

goals$final1 = as.integer(goals$final1)
goals$final2 = as.integer(goals$final2)

goals$result = as.factor(goals$result)

goals$result[goals$final1 > goals$final2 & goals$g1 > goals$g2] = "W"
goals$result[goals$final1 < goals$final2 & goals$g1 < goals$g2] = "W"


goals$result[goals$final1 == goals$final2] = "D"

goals$result[goals$final1 < goals$final2 & goals$g1 > goals$g2] = "L"
goals$result[goals$final1 > goals$final2 & goals$g1 < goals$g2] = "L"


#2=0 games
game2_0_a = subset(goals, s2_0_a==1 )
game2_0_b = subset(goals, s2_0_b==1 )
g20 = rbind(game2_0_a, game2_0_b)
table(g20$result)

#1=0 games
game1_0_a = subset(goals, s1_0_a==1 )
game1_0_b = subset(goals, s1_0_b==1 )
g10 = rbind(game1_0_a, game1_0_b)
table(g10$result)

#2=1 games
game2_1_a = subset(goals, s2_1_a==1 )
game2_1_b = subset(goals, s2_1_b==1 )
g21 = rbind(game2_1_a, game2_1_b)
table(g21$result)

#3-2
game3_2_a = subset(goals, s3_2_a==1 )
game3_2_b = subset(goals, s3_2_b==1 )
g32 = rbind(game3_2_a, game3_2_b)
table(g32$result)


#3-1
game3_1_a = subset(goals, s3_1_a==1 )
game3_1_b = subset(goals, s3_1_b==1 )
g31 = rbind(game3_1_a, game3_1_b)
table(g31$result)

#3-0
game3_0_a = subset(goals, s3_0_a==1 )
game3_0_b = subset(goals, s3_0_b==1 )
g30 = rbind(game3_0_a, game3_0_b)
table(g30$result)

################before halftime!!!
#2=0 games
game2_0_ah = subset(goals, s2_0_half_a==1 )
game2_0_bh = subset(goals, s2_0_half_b==1 )
g20h = rbind(game2_0_ah, game2_0_bh)
table(g20h$result)

#1=0 games
game1_0_ah = subset(goals, s1_0_half_a==1 )
game1_0_bh = subset(goals, s1_0_half_b==1 )
g10h = rbind(game1_0_ah, game1_0_bh)
table(g10h$result)

#2=1 games
game2_1_ah = subset(goals, s2_1_half_a==1 )
game2_1_bh = subset(goals, s2_1_half_b==1 )
g21h = rbind(game2_1_ah, game2_1_bh)
table(g21h$result)

##3-1
game3_1_ah = subset(goals, s3_1_half_a==1 )
game3_1_bh = subset(goals, s3_1_half_b==1 )
g31h = rbind(game3_1_ah, game3_1_bh)
table(g31h$result)

##3-2
game3_2_ah = subset(goals, s3_2_half_a==1 )
game3_2_bh = subset(goals, s3_2_half_b==1 )

g32h = rbind(game3_2_ah, game3_2_bh)
table(g32h$result)

##3-0
game3_0_ah = subset(goals, s3_0_half_a==1 )
game3_0_bh = subset(goals, s3_0_half_b==1 )
g30h = rbind(game3_0_ah, game3_0_bh)
table(g30h$result)


###percentages data.frame
fun = function(num){percent(num)}
#any point in game
fun(g10$result)
fun(g20$result)
fun(g21$result)
fun(g30$result)
fun(g31$result)
fun(g32$result)

##before half
fun(g10h$result)
fun(g21h$result)
fun(g20h$result)
fun(g30h$result)
fun(g31h$result)
fun(g32h$result)

###########goalscorers and wins!!!!

Prob = c(67.96, 81.61, 89.81, 90.26, 72.62, 80.85,
          10.48, 5.54, 2.98, 1.73, 5.69,  3.19,
         21.57, 12.84,  7.21, 8.01, 21.68, 15.96)

Probhalf = c(60.23, 74.74, 83.01, 86.52, 67.05, 75, 
             11.68, 8.35, 8.49, 2.84, 12.14, 12.5,
           28.09, 16.91,  8.49, 10.64, 20.81, 12.5)


Result = c('Win', 'Win', 'Win', 'Win', 'Win', 'Win', 
           'Loss', 'Loss', 'Loss', 'Loss', 'Loss', 'Loss', 
           'Draw', 'Draw', 'Draw', 'Draw', 'Draw', 'Draw')

score1 = c("1-0", "2-0", "3-0", "3-1", "2-1", "3-2", 
           "1-0", "2-0", "3-0", "3-1", "2-1", "3-2",
           "1-0", "2-0", "3-0", "3-1", "2-1", "3-2")

bars = data.frame(score1, Result, Prob, Probhalf)
bars$color[bars$Result=="Win"]= "deepskyblue"
bars$color[bars$Result=="Loss"]= "drkblue"
bars$color[bars$Result=="Draw"]= "dodgerblue1"


ggplot(bars, aes(score1, Prob)) + geom_bar(stat='Identity',aes(fill=Result)) +
  xlab('Score') + ylab('Probability, %') + scale_fill_brewer(
    palette = "Blues") +
  ggtitle("Probability of Winning with a Particular Lead")

ggplot(bars, aes(score1, Probhalf)) + geom_bar(stat='Identity',aes(fill=Result)) +
  xlab('Score') + ylab('Probability, %') + scale_fill_brewer(
    palette = "Blues") +
  ggtitle("Probability of Winning with a Particluar Lead before Halftime")





