library(RSQLite)
library(data.table)
library(BradleyTerry2)
library(reshape2)
library(plyr)
library(ggplot2)
library(MASS)
library(ROCR)

#### EXPLORATORY DATA ANALYSIS ####

setwd("~/Desktop/Duke/Fall_2016/Categorical_Data/STA841")
match = fread("match_db.csv")
nrow(match)
ncol(match)
head(match)
new_match = match[,c(1:11,86:118)]
head(new_match,100)

# number of teams
length(unique(match$home_team_api_id))
length(unique(match$away_team_api_id))

# number of leagues
length(unique(match$name))

# just matches, home/away and goals scored
results = data.frame(cbind(match_id = match$match_api_id, 
                           home_team_api = match$home_team_api_id, 
                           away_team_api = match$away_team_api_id,
                           home_team_goal = match$home_team_goal, 
                           away_team_goal = match$away_team_goal))

results$winner = ifelse(results$home_team_goal>results$away_team_goal, 
                        results$home_team_api, 
                        ifelse(results$home_team_goal<results$away_team_goal, results$away_team_api, "tie"))

head(results)

count_winners = data.frame(table(results$winner))
count_winners[,1] = as.numeric(as.character(count_winners[,1]))

names(count_winners) <- c("Team","Winner")
# find top 10 winners
count_winners[order(count_winners[,2], decreasing = T),][1:11,]

# find top 10 losers
count_winners[order(count_winners[,2], decreasing = F),][1:10,]

count_total_temp = data.frame(num_home = table(results$home_team_api), 
                              num_away = table(results$away_team_api))

count_total <- data.frame(cbind(n = as.numeric(as.character(count_total_temp$num_home.Var1)), 
                                count_total_temp$num_home.Freq + count_total_temp$num_away.Freq))

count_winners[,1] = as.numeric(as.character(count_winners[,1]))

names(count_total)<-c("Team","Total_games")

all_counts <- join(count_total, count_winners, by='Team')
all_counts$Proportion = all_counts$Winner/all_counts$Total

# top 10 winning percentage
all_counts[order(all_counts[,4], decreasing = T),][1:10,]

# number of goals scored, home and away
goals_scored = data.frame(cbind(aggregate(results$home_team_goal, by=list(results$home_team_api), FUN=sum),
                                aggregate(results$away_team_goal, by=list(results$away_team_api), FUN=sum)))

goals_scored$Group.1.1 <- NULL
names(goals_scored) <- c('Team','Home_goals','Away_goals')

goals_scored$Total_goals = goals_scored$Home_goals + goals_scored$Away_goals

goals_scored[order(goals_scored$Total_goals, decreasing = T),][1:10,]

goals_wins_df = join(goals_scored, all_counts, by = 'Team')

temp_df = goals_wins_df[order(goals_wins_df$Proportion, decreasing = T),][1:15,]
temp_df$Proportion_home_goals = temp_df$Home_goals/temp_df$Total_goals
temp_df$Proportion_away_goals = temp_df$Away_goals/temp_df$Total_goals
temp_df

prop_df = melt(temp_df, id.vars='Team')[91:120,]
ggplot(prop_df, aes(x = factor(Team), y = value, fill = variable)) + 
  geom_bar(stat = "identity", width=0.75) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  ggtitle("Proportion of goals that are home vs away") + ylab("Proportion of goals") + xlab("Team")

prop_df = melt(temp_df, id.vars='Team')[1:30,]
ggplot(prop_df, aes(x = factor(Team), y = value, fill = variable)) + 
  geom_bar(position = 'dodge', stat = "identity", width=0.75) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  ggtitle("Home vs Away goals by count") + ylab("Number of goals") + xlab("Team")


#### BRADLEY TERRY MODELS ####



results = match[,.("home.team" = home_team_api_id, "away.team" = away_team_api_id, 
                   "home.goal" = home_team_goal, "away.goal" = away_team_goal)]
results = results[home.goal != away.goal]
team.ids = unique(results$home.team)
team1 = t(combn(team.ids,2))
team2 = cbind(team1[,2], team1[,1])
team = rbind(team1,team2)
results.dat = data.table(matrix(0, nrow = nrow(team), ncol = 4))
colnames(results.dat) = c("home.team", "away.team", "home.wins", "away.wins")
results.dat$home.team = team[,1]
results.dat$away.team = team[,2]

#Turn data into appropriate format
for (i in 1:nrow(results)){
  print(i)
  home = results$home.team[i]
  away = results$away.team[i]
  if (results$home.goal[i] > results$away.goal[i]){
    results.dat[home.team == home & away.team == away]$home.wins = 
      results.dat[home.team == home & away.team == away]$home.wins + 1
  }
  if (results$home.goal[i] < results$away.goal[i]){
    results.dat[home.team == home & away.team == away]$away.wins = 
      results.dat[home.team == home & away.team == away]$away.wins + 1
  }
}

#Save since above for loop takes a long time
save(results.dat, file = "~/Desktop/Duke/Fall_2016/Categorical_Data/STA841/results.Rdata")
load("~/Desktop/Duke/Fall_2016/Categorical_Data/STA841/results.Rdata")


#Clean Data
results.dat = results.dat[home.wins != 0 | away.wins != 0]
results.dat$home.team = as.factor(results.dat$home.team)
results.dat$away.team = as.factor(results.dat$away.team)

## Standard Model ##

#Ignoring Home Advantage
btModel1 = BTm(cbind(home.wins, away.wins), home.team, away.team, data = results.dat, id = "team", formula = ~ team)

#Dispersion Estimates
dispersion_p = sum(residuals(btModel1, type = "pearson")^2)/btModel1$df.residual
dispersion_d = sum(residuals(btModel1, type = "deviance")^2)/btModel1$df.residual

## Contest-Specific Predictors ##

#Incorporating "Home Advantage" Effect
results.dat = data.frame(results.dat)
results.dat$home.team <- data.frame(team = results.dat$home.team, at.home = 1)
results.dat$away.team <- data.frame(team = results.dat$away.team, at.home = 0)
btModel2 = update(btModel1, formula = ~ team + at.home)

#Dispersion Estimates
dispersion_p = sum(residuals(btModel2, type = "pearson")^2)/btModel2$df.residual
dispersion_d = sum(residuals(btModel2, type = "deviance")^2)/btModel2$df.residual

#Model Comparison
anova(btModel2, btModel1)

#### Team Specific Predcitors ####

#Get Attributes from SQLITE Database

#Connect
con = dbConnect(SQLite(), dbname="~/Desktop/Duke/Fall_2016/Categorical_Data/STA841/database.sqlite")

#Get Attributes and CLean
attrs = data.table(dbGetQuery(con,"SELECT * FROM Team_Attributes"))
attrs = attrs[,.(buildUpPlaySpeed, buildUpPlaySpeedClass, buildUpPlayDribbling, buildUpPlayDribblingClass, buildUpPlayPassing,
                 buildUpPlayPassingClass, buildUpPlayPositioningClass, chanceCreationPassing, chanceCreationPassingClass,
                 chanceCreationCrossing, chanceCreationCrossingClass, chanceCreationShooting, chanceCreationShootingClass,
                 chanceCreationPositioningClass, defencePressure, defencePressureClass, defenceAggression, defenceAggressionClass,
                 defenceAggressionClass, defenceTeamWidth, defenceTeamWidthClass, defenceDefenderLineClass),
              by = team_api_id, mult = "first"]
attrs = attrs[,.SD[1], by = team_api_id]
attrs = data.frame(attrs)
rownames(attrs) = attrs$team_api_id
attrs$team_api_id = NULL

#EDA on Attributes
hist(attrs$buildUpPlaySpeed, xlab = "Value", main = "Build Up Play Speed", breaks = 20, col = 1)
hist(attrs$buildUpPlayPassing, xlab = "Value", main = "Build Up Play Passing", breaks = 20, col = 1)
hist(attrs$chanceCreationPassing, xlab = "Value", main = "Chance Creation Passing", breaks = 20, col = 1)
hist(attrs$chanceCreationCrossing, xlab = "Value", main = "Chance Creation Crossing", breaks = 20, col = 1)
hist(attrs$chanceCreationShooting, xlab = "Value", main = "Chance Creation Shooting", breaks = 20, col = 1)
hist(attrs$defencePressure, xlab = "Value", main = "Defense Pressure", breaks = 20, col = 1)
hist(attrs$defenceAggression, xlab = "Value", main = "Defence Aggression", breaks = 20, col = 1)
hist(attrs$defenceTeamWidth, xlab = "Value", main = "Defence Team Width", breaks = 20, col = 1)

#Build List Object for BTm
results.eff = list()
results.eff$predictors = attrs
contests1 = data.frame("winner" = results[home.goal > away.goal]$home.team, 
                       "loser" = results[home.goal > away.goal]$away.team)
contests2 = data.frame("winner" = results[home.goal < away.goal]$away.team, 
                       "loser" = results[home.goal < away.goal]$home.team)
contests = rbind(contests1, contests2)
contests$winner = as.factor(contests$winner)
contests$loser = as.factor(contests$loser)
results.eff$contests = contests

#Model with Player Specific Effects
btModel3 = BTm(1, winner, loser, formula = ~ buildUpPlaySpeed[..] + buildUpPlayPassing[..] +
                 + chanceCreationPassing[..] + chanceCreationCrossing[..] + chanceCreationShooting[..] +
                 defencePressure[..] + defenceAggression[..] + defenceTeamWidth[..], data = results.eff)

#### Accuracy ####

#Get Probability for Output
getProb = function(team1, team2, model, alpha = NULL){
  
  coefs = coef(model)
  coefs = coefs[c(paste0("team", team1), paste0("team", team2))]
  diff = coefs[1] - coefs[2]
  if (is.null(alpha)) {
    return(exp(diff)/(1 + exp(diff)))
  } else {
    return(exp(diff + alpha)/(1 + exp(diff + alpha)))
    
  }
  
  
}

#Get Prob for certain teams and 'Home Advantage' estimate
getProb(9772, 9985, btModel1, 0.546)
getProb(9804, 8639, btModel1, 0.546)
getProb(9830, 7819, btModel1, 0.546)
getProb(6631, 9773, btModel1, 0.546)
getProb(9925, 9798, btModel1, 0.546)

#Pearsons and Deviance residuals
hist(residuals(btModel, type = "pearson"), breaks = 20, col = 1, main = "Pearson's Residuals", xlab = "Values")
hist(residuals(btModel1, type = "deviance"), breaks = 20, col = 1, main = "Deviance's Residuals", xlab = "Values")

#Train and Test Set

idx = sample(1:nrow(results.dat), 0.7*nrow(results.dat))
train.dat = results.dat[idx,]
test.dat = results.dat[-idx,]

#Home Advantage
train.dat = data.frame(train.dat)
train.dat$home.team <- data.frame(team = train.dat$home.team, at.home = 1)
train.dat$away.team <- data.frame(team = train.dat$away.team, at.home = 0)
btModel2.train = BTm(cbind(home.wins, away.wins), home.team, away.team, data = train.dat, id = "team", formula = ~ team + at.home)

test.dat = data.frame(test.dat)
test.dat$home.team <- data.frame(team = test.dat$home.team, at.home = 1)
test.dat$away.team <- data.frame(team = test.dat$away.team, at.home = 0)

## Get Vector of Predictions and True Results
coefs = coef(btModel2.train)
at.home = coefs["at.home"]
p.win = rep(0, nrow(test.dat))
for (i in 1:nrow(test.dat)){
  home.team = as.character(test.dat$home.team$team[iin])
  away.team = as.character(test.dat$away.team$team[i])
  beta.home = coefs[paste0("team", home.team)]
  beta.away = coefs[paste0("team", away.team)]
  param = beta.home - beta.away + at.home
  p = exp(param)/(1 + exp(param))
  if (is.na(p)) {
    p.win[i] = NA
    next
  }
  if (p >= 0.5){
    p.win[i] = 1
  } else {
    p.win[i] = 0
  }
}

truth.vec = rep(0, length(p.win))
for (i in 1:nrow(test.dat)){
  if (test.dat$home.wins[i] > test.dat$away.wins[i]){
    truth.vec[i] = 1
  } else {
    truth.vec[i] = 0
  }
}


#Plot ROC Curve
plot(performance(prediction(p.win, truth.vec),'tpr',
                 x.measure = 'fpr'),main="ROC curve")
grid(col='gray')
abline(a=0,b=1,lty=8)

#Get AUC
auc = performance(prediction(p.win, truth.vec),'auc')@y.values[[1]]

#Get Confusion Matrix
confusionMatrix(p.win,truth.vec)$table