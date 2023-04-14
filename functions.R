install.packages("rmarkdown")


nba_player_salaries<-read.csv("project-data/2018-19_nba_player-salaries.csv")
head(hr_data)
sum(is.na(nba_player_salaries))

nba_player_statistics<-read.csv("project-data/2018-19_nba_player-statistics.csv")
head(nba_player_statistics)
dim(nba_player_statistics)

sum(is.na(nba_player_statistics))

nba_team_statistics_1<-read.csv("project-data/2018-19_nba_team-statistics_1.csv")
sum(is.na(nba_team_statistics_1))

nba_team_statistics_2<-read.csv("project-data/2018-19_nba_team-statistics_2.csv")
sum(is.na(nba_team_statistics_2))

nba_team_payroll<-read.csv("project-data/2019-20_nba_team-payroll.csv")
sum(is.na(nba_team_payroll))



nba_team_statistics_2
library(ggplot2)
ggplot(nba_team_statistics_1, aes(x= Age, y=PTS)) +
  geom_point()+ theme_dark()

df <- merge(nba_team_statistics_1, nba_team_statistics_2, by = "Team")
dim(df)



mu_x <- mean(df$Age)
mu_y <- mean(df$PTS)
s_x <- sd(df$Age)
s_y <- sd(df$PTS)
r <- cor(df$Age, df$PTS)

ggplot(df, aes(Age, PTS)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x) 

df[ , !(names(df) %in% c('Team','ï..Rk.x','X','X.1','X.2','G'))]


df[ , (names(df) %in% c('Age','FG','X3P','X2P','FT','BLK','STL','AST','TOV','eFG.','PTS'))]



res <- cor(df[ , (names(df) %in% c('Age','FG','X3P','X2P','FT','BLK','STL','AST','TOV','eFG.','PTS'))])
round(res, 2)
res2 <- rcorr(as.matrix(res))
corrplot(res, type = "upper", order = "hclust", 
        tl.col = "black")

flattenCorrMatrix(res2$r, res2$P)


p = corrplot(res, type = "upper", order = "hclust", 
                        tl.col = "black")

ggsave("Figs/correaltion_plot_all_variables.jpeg",plot = p)



flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

library("Hmisc")
res2 <- rcorr(as.matrix(res))
flattenCorrMatrix(res2$r, res2$P)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

install.packages("PerformanceAnalytics")

library("PerformanceAnalytics")

chart.Correlation(df[ , (names(df) %in% c('Age','FG','X3P','X2P','FT','BLK','STL','AST','TOV','eFG.','PTS'))], histogram=TRUE, pch=19)

model <- lm(PTS~Age+FG+X3P+X2P+FT+BLK+STL+AST+TOV+eFG., data = df)
summary(model)











