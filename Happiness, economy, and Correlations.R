rm(list = ls())


library(dplyr)
library(ggplot2)
library(plotly)
library(ggpubr)

df = read.csv("happiness 2017.csv")


################## GDP Per Capita and Happiness
ggplot(df, aes(Economy..GDP.per.Capita., Happiness.Score)) + 
  geom_point(size = 3, shape=21, fill='blue2', color='black') +
  ggtitle("Correlation between Countries' GDP per Capita and Happiness Score") +
  xlab("GDP per Capita") + ylab("Happiness Score") + theme_classic() + 
  geom_smooth(method = 'lm', se= FALSE, color='black') +
  theme(plot.title = element_text(hjust = 0.5)) + labs(caption = "Source: World Happiness Report")

# OLS model
hap.gdp = lm(Economy..GDP.per.Capita. ~ Happiness.Score, df)
summary(hap.gdp)


###graphs
library(gridExtra)

# Correlations with Freedom Score
g1 = ggplot(df, aes(Trust..Government.Corruption., Economy..GDP.per.Capita.)) + 
  geom_point(size=3, color='green3') + ylab("GDP per Capita") + xlab('Government Trust Corruption Score') +
  geom_smooth(method = 'lm', se= FALSE, color='black') + theme_classic()
g2 = ggplot(df, aes(Happiness.Score, Trust..Government.Corruption.)) + geom_point(size=3, color='blue3') +
  ylab("Happiness Score") + xlab('Government Trust Corruption Score') + theme_classic() +
  geom_smooth(method = 'lm', se= FALSE, color='black') 

x1 = ggarrange(g1, g2)

annotate_figure(x1,
                top = text_grob("Correlations with Freedom Score", face = "bold", size = 14))

# Correlations with Curruption and Trust Score
g3 = ggplot(df, aes(Trust..Government.Corruption., Economy..GDP.per.Capita.)) + 
  geom_point(size=3, color='green3') + ylab("GDP per Capita") + xlab('Curruption and Trust Score') +
  geom_smooth(method = 'lm', se= FALSE, color='black') + theme_classic()
g4 = ggplot(df, aes(Trust..Government.Corruption., Happiness.Score)) + geom_point(size=3, color='blue3') +
  ylab("Happiness Score") + xlab('Curruption and Trust Score') + theme_classic() +
  geom_smooth(method = 'lm', se= FALSE, color='black') 
grid.arrange(g1, g2, ncol=1)
x2 = ggarrange(g3, g4)
annotate_figure(x2,
                top = text_grob("Correlations with Curruption and Trust Score", face = "bold", size = 14))

# Correlations with Health and Life Expectancy Score
g5 = ggplot(df, aes(Health..Life.Expectancy., Economy..GDP.per.Capita.)) + 
  geom_point(size=3, color='green3') + ylab("GDP per Capita") + xlab('Health and Life Expectancy Score') +
  geom_smooth(method = 'lm', se= FALSE, color='black') + theme_classic()
g6 = ggplot(df, aes(Health..Life.Expectancy., Happiness.Score)) + geom_point(size=3, color='blue3') +
  ylab("Happiness Score") + xlab('Health and Life Expectancy Score') + geom_smooth(method = 'lm', se= FALSE, color='black') +
  theme_classic()
grid.arrange(g1, g2, ncol=1, top="Title")+ theme_classic()
x3 = ggarrange(g5, g6)
x3 
annotate_figure(x3,
                top = text_grob("Correlations with Health and Life Expectancy Score", face = "bold", size = 14))


# Correlations with Generosity Score
g7 = ggplot(df, aes(Generosity, Economy..GDP.per.Capita.)) + 
  geom_point(size=3, color='green3') + ylab("GDP per Capita") + xlab('Generosity Score') +
  geom_smooth(method = 'lm', se= FALSE, color='black') + theme_classic()
g8 = ggplot(df, aes(Generosity, Happiness.Score)) + geom_point(size=3, color='blue3') +
  ylab("Happiness Score") + xlab('Generosity Score') + geom_smooth(method = 'lm', se= FALSE, color='black') +
  theme_classic()
grid.arrange(g1, g2, ncol=1, top="Title")+ theme_classic()
x4 = ggarrange(g7, g8)
x4 
annotate_figure(x4,
                top = text_grob("Correlations with Generosity Score", face = "bold", size = 14))


# IV Analysis
ivreg1 = ivreg(Economy..GDP.per.Capita. ~ Happiness.Score | Generosity, data = df)
summary(ivreg1)

coefdiff = coef(ivreg1) - coef(hap.gdp)
vcdiff = vcov(ivreg1) - vcov(hap.gdp)
x_diff = as.vector(t(coefdiff) %*% solve(vcdiff) %*% coefdiff)
pchisq(x_diff, df = 2, lower.tail = FALSE) #p=0.26 No Endogoenity
