rm(list = ls())

#data
allplays= read.csv('all plays.csv')
table(allplays$PlayType)

fourthdown = subset(allplays, down==4)
fourthdown = fourthdown %>% group_by(GameID) %>% mutate(lastdrive = max(Drive))
fourthd = subset(fourthdown, Drive != lastdrive)


fourthd$conversion = ifelse(fourthd$Yards.Gained >= fourthd$ydstogo, 'Converted', 'Not converted')
allplays$conversion = ifelse(allplays$Yards.Gained >= allplays$ydstogo, 'Converted', 'Not converted')
fourthdown$conversion = ifelse(fourthdown$Yards.Gained >= fourthdown$ydstogo, 'Converted', 'Not converted')

table(fourthd$conversion)
summary(fourthd$ydstogo)

allplays$PlayDown = ifelse(allplays$down==1, 'First Down', 'NA')
allplays$PlayDown[allplays$down==2] = 'Second Down'
allplays$PlayDown[allplays$down==3] = 'Third Down'
allplays$PlayDown[allplays$down==4] = 'Fourth Down'
allplays = subset(allplays, down <=4)
allplays$PlayDown <- factor(allplays$PlayDown, levels = c('First Down', 'Second Down', 'Third Down', 'Fourth Down'))

allplays$success = ifelse(allplays$conversion=='Converted', 1, 0)
fourthd$success = ifelse(fourthd$conversion=='Converted', 1, 0)

###logit models
allplays$time = allplays$TimeSecs/60
rega = glm(success ~ down + time + ydstogo + yrdline100 + 
            ScoreDiff, data = allplays, family = binomial(link = 'logit'))
summary(rega)

fourthd$time = fourthd$TimeSecs/60
regb = glm(success ~ time + yrdline100 + ydstogo +  
            ScoreDiff, data = fourthd, family = binomial(link = 'logit'))
summary(regb)


regamfx = mean(dlogis(predict(rega, type = 'link')))
regamfx * coef(rega)

regbmfx = mean(dlogis(predict(regb, type = 'link')))
regbmfx * coef(regb)

###charts
library(coefplot)
coefplot(rega, newNames=c(down = 'Down', ScoreDiff='Score Differential', yrdline100='Yards to goal line',
                          ydstogo='Yards to go',time = 'Time of Game(minutes)', '(Intercept)'='Constant'), 
         title='Coefficient Plot and Probability of First Down, All Downs', color='darkgreen')
coefplot(regb, newNames=c(ScoreDiff='Score Differential', yrdline100='Yards to goal line',
                          ydstogo='Yards to go',time = 'Time of Game(minutes)', '(Intercept)'='Constant'), 
         title='Coefficient Plot and Probability of First Down, Fourth Down Plays Only', color='darkgreen',
         legendpos = "bottomright")


ggplot(allplays, aes(x=yrdline100, y=ydstogo)) + geom_point(aes(color=conversion)) + 
  facet_grid(~PlayDown) + scale_colour_manual(values=c("Converted"="darkgreen", "Not converted"="lightgreen"), 
                                              labels=c("Converted", "Not converted")) +
   xlab('Distance to the goal line') + ylab('Yards to go') + theme_minimal() + ggtitle('Down, Yards to go, and Distance to the Goal') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank())

ggplot(fourthd, aes(x=yrdline100, y=ydstogo)) + geom_point(aes(color=conversion)) + scale_colour_manual(values=c("Converted"="darkgreen", "Not converted"="lightgreen"), 
                                                                                                        labels=c("Converted", "Not converted")) +
   xlab('Distance to the goal line') + ylab('Yards to go') + theme_minimal() + 
  ggtitle('4th Down, Yards, and Distance to the Goal') + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title=element_blank()) 



################################################################

######Testing the model
table(fourthd$conversion)
more10 = subset(fourthd, fourthd$ydstogo > 10)
table(more10$conversion)
less10 = subset(fourthd, fourthd$ydstogo <= 10 & fourthd$ydstogo > 5)
table(less10$conversion)
less5 = subset(fourthd, fourthd$ydstogo < 5 & fourthd$ydstogo > 1)
table(less5$conversion)
less1 = subset(fourthd, fourthd$ydstogo <= 1)
table(less1$conversion)

plays = c('All plays', 'More than 10 yds', '5-10 yds', '1-5 yds', 'Less than 1 yd')
probs = as.data.frame(plays)
probs$distance= as.numeric(c(51.1, 16.9, 47.9, 49.4, 64.5))
probs$plays = factor(probs$plays, levels = c('All plays', 'More than 10 yds', '5-10 yds', '1-5 yds', 'Less than 1 yd'))
ggplot(probs, aes(plays, distance)) + geom_bar(stat="identity", fill='darkgreen', color='black') +
  theme_classic() + ylab('Percentage of conversions') + xlab('Yards to go') +
  ggtitle('Percentage of Fourth Down Conversions') + theme(plot.title = element_text(hjust = 0.5)) 
  


probs = as.data.frame(cbind(x, as.numeric(y)))

probs$y = as.numeric(levels(probs$y))

exactly2 = subset(fourthd, fourthd$ydstogo ==2  & fourthd$yrdline100==2)
table(exactly2$conversion)
library(caTools)
set.seed(1234)

split = sample.split(fourthd$ydstogo, SplitRatio = 0.7)

train = subset(fourthd, split== TRUE)
test = subset(fourthd, split== FALSE)

regtrain = lm(success ~ TimeSecs + ydstogo + yrdline100 + 
             ScoreDiff + posteam + DefensiveTeam, data = train)
summary(regtrain)
predfourth = predict(regtrain, test)
tetdata = as.data.frame(predfourth, test)

library(e1071)
model <- svm(formula = success ~ ydstogo + yrdline100,data=train)
summary(model)
predicted <- predict(model,test)
table(predicted.values,test$success)
cluster = kmeans(fourthd$success, 2)
cluster
table(cluster$cluster, fourthd$success)
library(cluster) 
clusplot(pam(fourthd$success, fourthd$ydstogo))
install.packages('caret')
library(caret)
featurePlot(x=fourthd$yrdline100, y=fourthd$ydstogo, plot="ellipse")

regc = glm(success ~ ydstogo + yrdline100, 
              data = fourthd, family = binomial(link = 'logit'))
summary(regc)
newdata = data.frame( ydstogo=2, yrdline100=2)
predict(regc, newdata)
