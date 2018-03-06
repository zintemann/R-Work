df = read.csv('payroll and attendance changes.csv')

#library(dplyr)
df = df %>% group_by(yearID) %>% 
  mutate(salary_rank = rank(Payroll), attendance_rank = rank(attendance)) 


df %>% order_by(salary_rank)

##New Variables
df$Payroll_change = df$Payroll - df$Lag_Payroll 
df$attendance_change = df$attendance  - df$Lag_attendance
df$WinPct_change = df$WinPct - df$Lag_WinPct
df$Payroll_pct_change = (df$Payroll - df$Lag_Payroll)/df$Lag_Payroll
df$Pattendance_pct_change = (df$attendance - df$Lag_attendance)/df$Lag_attendance


reg_win_pct = lm(WinPct_change ~ Payroll_pct_change, data = df)
summary(reg_win_pct)

reg_win = lm(WinPct_change ~ Payroll_change, data = df)
summary(reg_win)

reg_att_pct = lm(Pattendance_pct_change ~ Payroll_pct_change, data = df)
summary(reg_att_pct)

df$ln_attendance = log(df$attendance_change)
df$ln_salary = log(df$Payroll_change)

####this one
ln_regs = lm(attendance_change ~ Payroll_change, data = df)
summary(ln_regs)

ln_regs_wins = lm(WinPct_change ~ Payroll_change, data = df)
summary(ln_regs_wins)

ln_regs2 = lm(attendance_change ~ WinPct_change, data = df)
summary(ln_regs2)

############################middle pay team
midteams = subset(df, salary_rank > 5 & salary_rank < 25)

reg_win_pctmid = lm(WinPct_change ~ Payroll_pct_change, data = midteams)
summary(reg_win_pctmid)

reg_winmid = lm(WinPct_change ~ Payroll_change, data = midteams)
summary(reg_winmid)

reg_att_pctmid = lm(Pattendance_pct_change ~ Payroll_pct_change, data = df)
summary(reg_att_pct)


ln_regsmd = lm(attendance_change ~ Payroll_change, data = midteams)
summary(ln_regsmd)

ln_regs_winsmd = lm(WinPct_change ~ Payroll_change, data = midteams)
summary(ln_regs_winsmd)


payreg = lm(attendance ~ Payroll, data = df)
summary(payreg)

winreg = lm(WinPct ~ Payroll, data = df)
summary(winreg)

att_winreg = lm(ln_attendance ~ WinPct, data = df)
summary(att_winreg)

#####IV stadium size on attendance change on wins and on change in payroll





##gplots
library(ggplot2)

ggplot(data=df, aes(y=attendance, x=Payroll)) + geom_point() + geom_smooth(method=lm)

######THIS ONE
df$new_attendance = df$attendance / 1000000
df$new_payroll = df$Payroll / 1000000


ggplot(data=df, aes(y=new_attendance, x=WinPct)) + geom_point(aes(alpha=new_payroll, size=new_payroll), color='blue2') +
  scale_x_continuous(breaks=seq(0.275, 0.775, by = 0.100)) + 
  ggtitle('Baseball Winning Pct, Home Attendance, and Payroll') + ylab('Home Attendance (in millions)') + xlab('Season Winning Percentage') +
  labs(alpha="Team payroll (in millions)", size="Team payroll (in millions)") + theme_bw()




newmil = subset(df, yearID > 2000)

ggplot(data=newmil, aes(y=new_attendance, x=WinPct)) + geom_point(aes(alpha=new_payroll, size=new_payroll), color='blue2') +
  scale_x_continuous(breaks=seq(0.275, 0.775, by = 0.100)) + 
  ggtitle('Baseball Team Winning Percentage, Home Attendance, and Payroll (Since 2001)') + ylab('Home Attendance (in millions)') + xlab('Season Winning Percentage') +
  labs(alpha="Team payroll (in millions)", size="Team payroll (in millions)") + theme_bw()

allbutbig = subset(newmil, franchID !='LAD' & franchID != 'NYY')


ggplot(data=allbutbig, aes(y=new_attendance, x=WinPct)) + geom_point(aes(alpha=new_payroll, size=new_payroll), color='blue2') +
  scale_x_continuous(breaks=seq(0.275, 0.775, by = 0.100)) + 
  ggtitle('Baseball Team Winning Percentage, Home Attendance, and Payroll (Since 2001)') + ylab('Home Attendance (in millions)') + xlab('Season Winning Percentage') +
  labs(alpha="Team payroll (in millions)", size="Team payroll (in millions)") + theme_bw() +
  geom_smooth(method = 'lm')


#
#############+  
  ############geom_smooth(method=lm)

ggplot(data=df, aes(y=attendance_change, x=Payroll_pct_change)) + geom_point(aes(size=Payroll), color='blue3', alpha=0.5) + 
  geom_smooth(method=lm)

ggplot(data=df, aes(y=attendance_change, x=WinPct_change)) + geom_point(aes(size=Payroll), color='blue3', alpha=0.5) + geom_smooth(method=lm)

ggplot(data=df, aes(y=attendance_change, x=Payroll_change)) + geom_point(aes(size=Payroll), color='blue3', alpha=0.5) + geom_smooth(method=lm)

### who brings in the most fans with the lowest payroll

df$wins = df$WinPct * 162
df$fansperpayratio = df$attendance/df$Payroll

df$funindex = df$wins * df$fansperpayratio


newmil = subset(df, yearID > 2000)

teams = newmil %>% group_by(franchID) %>%
  summarise(avgfun = mean(funindex), avgfanperwin = mean(fansperpayratio))


years = df %>% group_by(yearID) %>% summarise(salary = mean(Payroll), stdev = sd(Payroll))
