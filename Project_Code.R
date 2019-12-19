library(tidyverse)
library(lubridate)
library(glmnet)
library(caret)
setwd("/Users/raghavagovil/Desktop/UCLA Classes/Quarter 7/Stats 101C/Final Project")
test <- read_csv("test.csv")
train <- read_csv("train.csv")

## Feature Engineering
train_new <- train[, -c(1,2)]
train_new$date <- as.character(train_new$date)
train_new$date <- as.Date(train_new$date, format = "%Y%m%d")
train_new$month <- month(train_new$date)
train_new$year <- year(train_new$date)
test_new <- test[, -2]
test_new$date <- as.character(test_new$date)
test_new$date <- as.Date(test_new$date, format = "%Y%m%d")
test_new$month <- month(test_new$date)
test_new$year <- year(test_new$date)


train_new <- train_new %>% mutate(team_group = case_when(HT == "MIND" | HT == "BATA" | HT == "SOCC" | 
                                                           HT == "HZTL" | HT == "LUCA" | HT == "MTLP" |
                                                           HT == "MANL" | HT == "KABO" ~ "Tier 1", 
                                                         HT == "RICE" | HT == "SJNK" | HT == "IMUS" | 
                                                           HT == "CALO" | HT == "QUEZ" | HT == "PASV" |
                                                           HT == "RIZL" | HT == "AJAX" | 
                                                           HT == "NAVO" ~ "Tier 2", 
                                                         HT == "CEBS" | HT == "SPVT" | HT == "BCCS" | 
                                                           HT == "LLOL" | HT == "PARQ" | HT == "BASI" |
                                                           HT == "BOAN" | HT == "PANG" |
                                                           HT == "BATG" ~ "Tier 3"))
train_new <- train_new %>% mutate(within_league = case_when(VTleague == HTleague ~ 1,
                                                            TRUE ~ 0))
test_new <- test_new %>% mutate(team_group = case_when(HT == "MIND" | HT == "BATA" | HT == "SOCC" | 
                                                         HT == "HZTL" | HT == "LUCA" | HT == "MTLP" |
                                                         HT == "MANL" | HT == "KABO" ~ "Tier 1", 
                                                       HT == "RICE" | HT == "SJNK" | HT == "IMUS" | 
                                                         HT == "CALO" | HT == "QUEZ" | HT == "PASV" |
                                                         HT == "RIZL" | HT == "AJAX" | 
                                                         HT == "NAVO" ~ "Tier 2", 
                                                       HT == "CEBS" | HT == "SPVT" | HT == "BCCS" | 
                                                         HT == "LLOL" | HT == "PARQ" | HT == "BASI" |
                                                         HT == "BOAN" | HT == "PANG" | 
                                                         HT == "BATG" ~ "Tier 3"))
test_new <- test_new %>% mutate(within_league = case_when(VTleague == HTleague ~ 1,
                                                          TRUE ~ 0))
 

 
count_20_vt <- function (row){
  count <- 0
  if (row[62] > 20) {count = count + 1}
  if (row[67] > 20) {count = count + 1}
  if (row[72] > 20) {count = count + 1}
  if (row[77] > 20) {count = count + 1}
  if (row[82] > 20) {count = count + 1}
  return (count)
}
train_new$vt_over_20 <- apply(train_new, 1, count_20_vt)
test_new$vt_over_20 <- apply(test_new, 1, count_20_vt)
count_20_ht <- function (row){
  count <- 0
  if (row[166] > 20) {count = count + 1}
  if (row[171] > 20) {count = count + 1}
  if (row[176] > 20) {count = count + 1}
  if (row[181] > 20) {count = count + 1}
  if (row[186] > 20) {count = count + 1}
  return (count)
}
train_new$ht_over_20 <- apply(train_new, 1, count_20_ht)
test_new$ht_over_20 <- apply(test_new, 1, count_20_vt)
 

 
count_30_vt <- function (row){
  count <- 0
  if (row[62] > 30) {count = count + 1}
  if (row[67] > 30) {count = count + 1}
  if (row[72] > 30) {count = count + 1}
  if (row[77] > 30) {count = count + 1}
  if (row[82] > 30) {count = count + 1}
  return (count)
}
train_new$vt_over_30 <- apply(train_new, 1, count_30_vt)
test_new$vt_over_30 <- apply(test_new, 1, count_30_vt)
count_30_ht <- function (row){
  count <- 0
  if (row[166] > 30) {count = count + 1}
  if (row[171] > 30) {count = count + 1}
  if (row[176] > 30) {count = count + 1}
  if (row[181] > 30) {count = count + 1}
  if (row[186] > 30) {count = count + 1}
  return (count)
}
train_new$ht_over_30 <- apply(train_new, 1, count_30_ht)
test_new$ht_over_30 <- apply(test_new, 1, count_30_ht)
 

 
train_new <- train_new %>% mutate(HT_eFG = (HT.TS.fgm + 0.5* HT.TS.tpm)/HT.TS.fga,
                                  HT_TOV_Percent = 100 * HT.TS.to / (HT.TS.fga + 0.475 * 
                                                                       HT.TS.fta + HT.TS.to),
                                  HT_Poss = 0.5 * ( HT.TS.fga+0.475  * 
                                                      HT.TS.fta - HT.TS.oreb + HT.TS.to) + 
                                    0.5 * (HT.TA.fga + 0.475 *  HT.TA.fta - HT.TA.oreb +  HT.TA.to),
                                  HT_ORtg = 100*(HT.TS.pts/HT_Poss),
                                  HT_DRtg = 100* (HT.TA.pts/HT_Poss),
                                  HT_True_Shooting_Att = HT.TS.fga + 0.44*HT.TS.fta,
                                  HT_TS_Percent = HT.TS.pts/(2*HT_True_Shooting_Att))
train_new <- train_new %>% mutate(VT_eFG = (VT.TS.fgm + 0.5* VT.TS.tpm)/VT.TS.fga,
                                  VT_TOV_Percent = 100 * VT.TS.to / 
                                    (VT.TS.fga + 0.475 * VT.TS.fta + VT.TS.to),
                                  VT_Poss = 0.5 * ( VT.TS.fga+0.475  * 
                                                      VT.TS.fta - VT.TS.oreb + VT.TS.to) +
                                    0.5 * (VT.TA.fga + 0.475 *  VT.TA.fta - VT.TA.oreb +  VT.TA.to),
                                  VT_ORtg = 100*(VT.TS.pts/VT_Poss),
                                  VT_DRtg = 100* (VT.TA.pts/VT_Poss),
                                  VT_True_Shooting_Att = VT.TS.fga + 0.44*VT.TS.fta,
                                  VT_TS_Percent = VT.TS.pts/(2*VT_True_Shooting_Att))
test_new <- test_new %>% mutate(HT_eFG = (HT.TS.fgm + 0.5* HT.TS.tpm)/HT.TS.fga,
                                HT_TOV_Percent = 100 * HT.TS.to / 
                                  (HT.TS.fga + 0.475 * HT.TS.fta + HT.TS.to),
                                HT_Poss = 0.5 * ( HT.TS.fga+0.475  * 
                                                    HT.TS.fta - HT.TS.oreb + HT.TS.to) + 
                                  0.5 * (HT.TA.fga + 0.475 *  HT.TA.fta - HT.TA.oreb +  HT.TA.to),
                                HT_ORtg = 100*(HT.TS.pts/HT_Poss),
                                HT_DRtg = 100* (HT.TA.pts/HT_Poss),
                                HT_True_Shooting_Att = HT.TS.fga + 0.44*HT.TS.fta,
                                HT_TS_Percent = HT.TS.pts/(2*HT_True_Shooting_Att))
test_new <- test_new %>% mutate(VT_eFG = (VT.TS.fgm + 0.5* VT.TS.tpm)/VT.TS.fga,
                                VT_TOV_Percent = 100 * VT.TS.to / 
                                  (VT.TS.fga + 0.475 * VT.TS.fta + VT.TS.to),
                                VT_Poss = 0.5 * ( VT.TS.fga+0.475  * 
                                                    VT.TS.fta - VT.TS.oreb + VT.TS.to) +
                                  0.5 * (VT.TA.fga + 0.475 *  VT.TA.fta - VT.TA.oreb +  VT.TA.to),
                                VT_ORtg = 100*(VT.TS.pts/VT_Poss),
                                VT_DRtg = 100* (VT.TA.pts/VT_Poss),
                                VT_True_Shooting_Att = VT.TS.fga + 0.44*VT.TS.fta,
                                VT_TS_Percent = VT.TS.pts/(2*VT_True_Shooting_Att))
 

 
#calculate winning percentage of all teams at home
home_win_percentage <- train_new %>% group_by(HT) %>% 
  summarise(home_win_pct = sum(HTWins == "Yes")/n()) 
#summary(home_win_percentage) #average is 0.5929
#find good home teams (above average win% at home)
#plot(home_win_percentage$HT, home_win_percentage$home_win_pct)
elite_ht <- home_win_percentage %>% filter(home_win_pct >= 0.6)
#good away teams
away_win_percentage <- train_new %>% group_by(VT) %>% 
  summarise(away_win_pct = sum(HTWins == "No")/n()) 
#summary(away_win_percentage) #average is 40%
#filter good away teams
good_vt <- away_win_percentage %>% filter(away_win_pct >= 0.4)
bad_vt <- away_win_percentage %>% filter(away_win_pct < 0.4)
#total winning percentage
home_games <- train_new %>% group_by(HT) %>% 
  summarise(total_home_games = n(), total_home_wins = sum(HTWins == "Yes"))
away_games <- train_new %>% group_by(VT) %>% 
  summarise(total_away_games = n(), total_away_wins = sum(HTWins == "No"))
#merge the two data.frames
total_games <- inner_join(home_games, away_games, by = c("HT" = "VT"))
## ovr winning percentage
teams_win_percent <- total_games %>% 
  mutate(win_pct = (total_home_wins + total_away_wins)/
           (total_home_games + total_away_games)) %>% select(HT, win_pct)
names(teams_win_percent)[1] <- "Team" 
#summary(teams_win_percent) #avg is right about .500
elite_teams <- teams_win_percent %>% filter(win_pct >= .500)
bad_teams <- teams_win_percent %>% filter(win_pct <= .500)
##HT Expected Win%
train_new <- inner_join(train_new, teams_win_percent, by = c("HT" = "Team")) 
names(train_new)[ncol(train_new)] <- "HT_Win_Pct"
train_new <- inner_join(train_new, teams_win_percent, by = c("VT" = "Team"))
names(train_new)[ncol(train_new)] <- "VT_Win_Pct"
train_new <- train_new %>% 
  mutate(HT_Expected_Winpct = HT_Win_Pct*(1-VT_Win_Pct)/
           (HT_Win_Pct*(1-VT_Win_Pct) + (1-HT_Win_Pct)*VT_Win_Pct))
 

 
train_new$ORtg_diff <- train_new$HT_ORtg - train_new$VT_ORtg
train_new$DRtg_diff <- train_new$HT_DRtg - train_new$VT_DRtg
train_new$S1_plmin_diff <- train_new$HT.S1.plmin - train_new$VT.S1.plmin
train_new$S2_plmin_diff <- train_new$HT.S2.plmin - train_new$VT.S2.plmin
train_new$S3_plmin_diff <- train_new$HT.S3.plmin - train_new$VT.S3.plmin
train_new$S4_plmin_diff <- train_new$HT.S4.plmin - train_new$VT.S4.plmin
train_new$S5_plmin_diff <- train_new$HT.S5.plmin - train_new$VT.S5.plmin
train_new$eFG_diff <- train_new$HT_eFG - train_new$VT_eFG
train_new$TS_Percent_diff <- train_new$HT_TS_Percent - train_new$VT_TS_Percent
train_new$Poss_diff <- train_new$HT_Poss - train_new$VT_Poss
train_new$over_20_diff <- train_new$ht_over_20 - train_new$vt_over_20
train_new$over_30_diff <- train_new$ht_over_30 - train_new$vt_over_30
train_new$TOV_Percent_diff <- train_new$HT_TOV_Percent - train_new$VT_TOV_Percent
train_new$cumRest_diff <- train_new$HTcumRest - train_new$VTcumRest
train_new$pmxU_diff <- train_new$HT.pmxU - train_new$VT.pmxU
train_new$pmxW_diff <- train_new$HT.pmxW - train_new$VT.pmxW
train_new$tpm_diff <- train_new$HT.TS.tpm - train_new$VT.TS.tpm


test_new$ORtg_diff <- test_new$HT_ORtg - test_new$VT_ORtg
test_new$DRtg_diff <- test_new$HT_DRtg - test_new$VT_DRtg
test_new$S1_plmin_diff <- test_new$HT.S1.plmin - test_new$VT.S1.plmin
test_new$S2_plmin_diff <- test_new$HT.S2.plmin - test_new$VT.S2.plmin
test_new$S3_plmin_diff <- test_new$HT.S3.plmin - test_new$VT.S3.plmin
test_new$S4_plmin_diff <- test_new$HT.S4.plmin - test_new$VT.S4.plmin
test_new$S5_plmin_diff <- test_new$HT.S5.plmin - test_new$VT.S5.plmin
test_new$eFG_diff <- test_new$HT_eFG - test_new$VT_eFG
test_new$TS_Percent_diff <- test_new$HT_TS_Percent - test_new$VT_TS_Percent
test_new$Poss_diff <- test_new$HT_Poss - test_new$VT_Poss
test_new$over_20_diff <- test_new$ht_over_20 - test_new$vt_over_20
test_new$over_30_diff <- test_new$ht_over_30 - test_new$vt_over_30
test_new$TOV_Percent_diff <- test_new$HT_TOV_Percent - test_new$VT_TOV_Percent
test_new$cumRest_diff <- test_new$HTcumRest - test_new$VTcumRest
test_new$pmxU_diff <- test_new$HT.pmxU - test_new$VT.pmxU
test_new$pmxW_diff <- test_new$HT.pmxW - test_new$VT.pmxW
test_new$tpm_diff <- test_new$HT.TS.tpm - test_new$VT.TS.tpm
 

 
test_new <- inner_join(test_new, teams_win_percent, by = c("HT" = "Team")) 
names(test_new)[ncol(test_new)] <- "HT_Win_Pct"
test_new <- inner_join(test_new, teams_win_percent, by = c("VT" = "Team"))
names(test_new)[ncol(test_new)] <- "VT_Win_Pct"
test_new <- test_new %>% mutate(HT_Expected_Winpct = HT_Win_Pct*(1-VT_Win_Pct)/
                                  (HT_Win_Pct*(1-VT_Win_Pct) + 
                                     (1-HT_Win_Pct)*VT_Win_Pct))
 

 
train_new$winpct_diff <- train_new$HT_Win_Pct - train_new$VT_Win_Pct
test_new$winpct_diff <- test_new$HT_Win_Pct - test_new$VT_Win_Pct
 

 
train_new$HTWins <- ifelse(train_new$HTWins == "Yes", 1, 0)
 

## Variable Selection
set.seed(123)
train_mat <- model.matrix(HTWins ~ ORtg_diff + 
                            DRtg_diff + 
                            S1_plmin_diff + 
                            S2_plmin_diff + 
                            S3_plmin_diff + 
                            S4_plmin_diff + 
                            S5_plmin_diff +
                            eFG_diff + 
                            TS_Percent_diff + 
                            Poss_diff + 
                            over_20_diff + 
                            over_30_diff + 
                            TOV_Percent_diff + 
                            cumRest_diff + 
                            pmxW_diff + 
                            pmxU_diff + 
                            tpm_diff  + 
                            winpct_diff, data = train_new)
lasso <- cv.glmnet(train_mat, train$HTWins, alpha = 1, family = "binomial")
best_lasso <- glmnet(train_mat, train$HTWins, alpha = 1, 
                     lambda = lasso$lambda.min, family = "binomial")

features_df <- coef(best_lasso) %>% as.matrix() %>% as.data.frame() 
features_df %>% mutate (feature = row.names(features_df)) %>% 
  mutate(s0 = abs(s0)) %>%  
  arrange(desc(s0)) %>% 
  filter (s0 > 0) %>% 
  filter(feature != "X.Intercept.") %>% 
  head()
 

## Model
model1 <- glm(HTWins ~ ORtg_diff + 
                DRtg_diff + 
                S1_plmin_diff + 
                S2_plmin_diff + 
                S3_plmin_diff + 
                S4_plmin_diff + 
                S5_plmin_diff + 
                eFG_diff + 
                TS_Percent_diff + 
                Poss_diff + 
                over_20_diff + 
                over_30_diff + 
                TOV_Percent_diff + 
                cumRest_diff + 
                pmxW_diff + 
                pmxU_diff + 
                tpm_diff + 
                winpct_diff, 
              family = "binomial", data = train_new)
summary(model1)
model1_predictions <- predict(model1, train_new, type = "response")
model1_predictions <- ifelse(model1_predictions < .5, 0, 1) 
1 - mean(model1_predictions == train_new$HTWins)
 

 
varImp(model1) %>% mutate (feature = row.names(varImp(model1))) %>% 
  arrange(desc(Overall))
 

### K-Fold Cross Validation
 
set.seed(123)

error_rate <- rep(NA, 500)

for (i in 1:500)
{
  indices <- sample(seq_len(nrow(train_new)), 
                    size = floor(0.75 * nrow(train_new)))
  train_new_split <- train_new[indices,]
  test_split <- train_new[-indices,]
  
  model <- glm(HTWins ~ ORtg_diff + 
                 DRtg_diff + 
                 S1_plmin_diff + 
                 S2_plmin_diff + 
                 S3_plmin_diff + 
                 S4_plmin_diff + 
                 S5_plmin_diff + 
                 eFG_diff + 
                 TS_Percent_diff + 
                 Poss_diff + 
                 over_20_diff + 
                 over_30_diff + 
                 TOV_Percent_diff + 
                 cumRest_diff + 
                 pmxW_diff + 
                 pmxU_diff + 
                 tpm_diff + 
                 winpct_diff, family = "binomial", data = train_new_split)
  
  predictions <- predict(model, test_split, type = "response")
  
  predictions <- ifelse(predictions > 0.5, 1, 0) 
  
  real <- test_split$HTWins %>% as.numeric()
  
  error <- mean(real == predictions)
  
  error_rate[i] <- 1 - error
}

mean(error_rate)
 

## Predictions
predictions <- predict(model1, test_new, type = "response")
predictions <- ifelse(predictions > .5, "No", "Yes")
predictions_df <- data.frame (id = test_new$id, HTWins = predictions)
write.csv(predictions_df, file = "submission.csv", row.names = FALSE)
 


