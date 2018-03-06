#####comparing payroll attendance and winning percentage of major league basebal teams 


#Data
df = read.csv('payroll and attendance changes.csv')

library(dplyr)
df = df %>% group_by(yearID) %>% 
  mutate(salary_rank = rank(Payroll), attendance_rank = rank(attendance)) 


df %>% order_by(salary_rank)

##New Variables
df$Payroll_change = df$Payroll - df$Lag_Payroll 
df$attendance_change = df$attendance  - df$Lag_attendance
df$WinPct_change = df$WinPct - df$Lag_WinPct
df$Payroll_pct_change = (df$Payroll - df$Lag_Payroll)/df$Lag_Payroll
df$Pattendance_pct_change = (df$attendance - df$Lag_attendance)/df$Lag_attendance

#regression analysis
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



##gplots
library(ggplot2)

df$new_attendance = df$attendance / 1000000
df$new_payroll = df$Payroll / 1000000


ggplot(data=df, aes(y=new_attendance, x=WinPct)) + geom_point(aes(alpha=new_payroll, size=new_payroll), color='blue2') +
  scale_x_continuous(breaks=seq(0.275, 0.775, by = 0.100)) + 
  ggtitle('Baseball Winning Pct, Home Attendance, and Payroll') + ylab('Home Attendance (in millions)') + xlab('Season Winning Percentage') +
  labs(alpha="Team payroll (in millions)", size="Team payroll (in millions)") + theme_bw()


#since 2001
newmil = subset(df, yearID > 2000)

ggplot(data=newmil, aes(y=new_attendance, x=WinPct)) + geom_point(aes(alpha=new_payroll, size=new_payroll), color='blue2') +
  scale_x_continuous(breaks=seq(0.275, 0.775, by = 0.100)) + 
  ggtitle('Baseball Team Winning Percentage, Home Attendance, and Payroll (Since 2001)') + ylab('Home Attendance (in millions)') + xlab('Season Winning Percentage') +
  labs(alpha="Team payroll (in millions)", size="Team payroll (in millions)") + theme_bw() +
  labs(caption = 'Source: Lahman Database')

