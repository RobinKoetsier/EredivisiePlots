library(ggtext)
library(tidyverse)
library(xgboost)
library(Matrix)
TeamCodes <- rbind(TeamCodes,c(332,"Czech Republic","Czech Republic"))
url= "https://www.whoscored.com/Matches/1469808/Live/Netherlands-Eredivisie-2020-2021-PEC-Zwolle-Vitesse"

ScrapeGame(url)
ScrapeGameTemp(url)
OneGame <- read_csv("OneGameEvents.csv")
OneGamePlayers <- read_csv("OneGamePlayers.csv")
OneGamePlayers <- OneGamePlayers[,-1]
OneGame <- ConvertIDOneGame(OneGame,OneGamePlayers)


df <- OneGame
df <- PrepAGame2.0(OneGame)
df_for_pred <- df %>% select(Goal,x,y,Bodypart,Type_of_play,isIntentionalAssist,isAssistedShot,isBigChance,Gamestate,Time_in_sec,distance,angle)
#dummy <- makeDummy(test)
dummy[2,1] <- 1 
df_for_pred$Type_of_play[is.na(df_for_pred$Type_of_play)] <- "RegularPlay"
df_for_pred <- rbind(dummy,df_for_pred)
df_for_pred$x <- ifelse(df_for_pred$Type_of_play == "Penalty",88.5,df_for_pred$x)
df_for_pred$y <- ifelse(df_for_pred$Type_of_play == "Penalty",50,df_for_pred$y)
df_for_pred$Goal[is.na(df_for_pred$Goal)] <- 0

pred <- PredictxG(df_for_pred)

xG <- as.data.frame(pred[-c(1:15)])
Wedstrijd <- cbind(xG,df)


colnames(Wedstrijd)[1]<- "xG"
Wedstrijd$xG <- ifelse(Wedstrijd$Type_of_play == "Penalty",0.79,Wedstrijd$xG)
df <- Minutes(OneGame)
df <- df %>% select(1,2,3,5,6,7)
df[is.na(df)] <- 0
xgbuild <- xgChain(OneGame,Wedstrijd) %>% group_by(PlayerId,TeamId) %>%
  summarise(xGC = sum(xG),npxgChain = sum(npxG)) %>% ungroup

Wedstrijd %>% group_by(PlayerId) %>% summarise(xG=sum(xG)) 


substr(Wedstrijd$HomeTeam[1],1,3)
substr(Wedstrijd$AwayTeam[1],1,3)

write.csv(Wedstrijd, "feyemm.csv")
  bind_rows(df %>% add_rownames(), 
          df %>% add_rownames()) %>% 
  # evaluate following calls for each value in the rowname column
  group_by(rowname) %>% 
  # add all non-grouping variables
  summarise_all(sum)

