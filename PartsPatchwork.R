library(ggsoccer)
library(ggtext)
library(tidyverse)

library(ggpattern)
Kleur1 <- "#FC6500"
Kleur2 <- "red"
Wedstrijd$side <- ifelse(Wedstrijd$TeamId==HomeTeam, "Home", "Away")
dfWedstrijd <- Wedstrijd
dfWedstrijd$xG <- ifelse(dfWedstrijd$Type_of_play=="Penalty",0,Wedstrijd$xG)
Home <- dfWedstrijd %>% filter(TeamId == HomeTeam)
Away <- dfWedstrijd %>% filter(TeamId == AwayTeam)
HomexG <- Home$xG
AwayxG <- Away$xG
winner <- calculateChance(HomexG,AwayxG,10000)

# Wedstrijd$xG <- ifelse(Wedstrijd$Type_of_play == "Penalty",0,Wedstrijd$xG)

Wedstrijd$TeamId <- ifelse(Wedstrijd$isOwnGoal == TRUE & Wedstrijd$TeamId == HomeTeam
                            ,Wedstrijd$AwayTeam, 
                           ifelse( Wedstrijd$isOwnGoal == TRUE & Wedstrijd$TeamId == AwayTeam,
                           Wedstrijd$HomeTeam, Wedstrijd$TeamId))

Wedstrijd$side <- ifelse(Wedstrijd$TeamId==HomeTeam, "Home", "Away")

Wedstrijd$PlayerId <- ifelse(Wedstrijd$isOwnGoal == TRUE, glue::glue("{Wedstrijd$PlayerId}  OG"),
                                                                     Wedstrijd$PlayerId)

Wedstrijd$xG <- ifelse(Wedstrijd$isOwnGoal == TRUE,0,Wedstrijd$xG)


HomeTeam <- Wedstrijd$HomeTeam
AwayTeam <- Wedstrijd$AwayTeam

dfWedstrijd <- Wedstrijd %>% mutate(PlayerId = PlayerId %>% 
                                      str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1."),
                                    relatedPlayerId = relatedPlayerId %>% 
                                      str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1.")) 
dfWedstrijd$OT <- ifelse(dfWedstrijd$goalMouthY>45& dfWedstrijd$goalMouthY<55&dfWedstrijd$goalMouthZ<38,1,0)


Home$sumxg <- cumsum(Home$xG)
Away$sumxg <- cumsum(Away$xG)

HomeGoals = sum(Home$Goal)
AwayGoals = sum(Away$Goal)

HomexG = round(sum(Home$xG),2)
AwayxG = round(sum(Away$xG),2)

HomePen <- Home %>% summarise(pen = sum(Type_of_play=="Penalty"))
AwayPen <- Away %>% summarise(pen = sum(Type_of_play=="Penalty"))

xGLabelHome <- ifelse(HomePen == 0, HomexG, glue::glue("(+{HomePen$pen} pen) {HomexG}"))
xGLabelAway <- ifelse(AwayPen == 0, AwayxG, glue::glue("{AwayxG} (+ {AwayPen$pen} pen)"))

dfWedstrijd$xG <- ifelse(dfWedstrijd$Type_of_play=="Penalty",0,Wedstrijd$xG)

shotmap <- ggplot() +annotate_pitch(fill="#f2f4f5")+
  geom_point(data=dfWedstrijd %>% filter(TeamId==HomeTeam & isOwnGoal == FALSE),
            aes(100-x,100-y,size=xG,
            shape=as.factor(Goal),fill=as.factor(Goal))) +
 
  geom_point(data=dfWedstrijd %>% filter(TeamId==AwayTeam & isOwnGoal == FALSE),aes(x,y,size=xG,
shape=as.factor(Goal),fill=as.factor(Goal))) +
  geom_point(data=dfWedstrijd %>% filter(TeamId==AwayTeam & isOwnGoal == TRUE),aes(100-x,100-y,size=xG),
             colour="red",size=3 ,shape=4) +
  geom_point(data=dfWedstrijd %>% filter(TeamId==HomeTeam & isOwnGoal == TRUE),aes(x,y,size=xG),
             colour="red",size=3 ,shape=4) +
 # scale_size_continuous(limits = c(0,10)) +
  scale_shape_manual(name = " ",
                     values=c("1" = 21,
                              "0"= 21),
                     
                     labels = c("Miss", "Goal")) +
  scale_fill_manual(name = " ",
                    values=c("1" = "green4",
                             "0"= "transparent"),
                    
                    labels = c("Mis", "Goal")) +
  ylim(0,122)+
  xlim(0,100)+
 scale_size(range = c(0, 10))+
  annotate(geom="text",x=25,y=122,label=glue::glue("{Wedstrijd$HomeTeam[1]}"), 
           family="Spartan-Bold",colour="#362626",size=5 ) +
  annotate(geom="text",x=25,y=116,label=glue::glue("{HomeGoals}"), 
           family="Spartan-Bold",colour="#362626",size=5 ) +
  annotate(geom="text",x=25,y=110,label=glue::glue("{xGLabelHome}"), 
           family="Spartan-Bold",colour="#362626",size=5 ) +
  annotate(geom="text",x=25,y=104,label=glue::glue("{winner$home}"), 
           family="Spartan-Bold",colour="#362626",size=5 ) +
  annotate(geom="text",x=50,y=116,label=glue::glue("Goals"), 
           family="Spartan-Bold",colour="#362626",size=5 )+
  annotate(geom="text",x=50,y=110,label=glue::glue("xG"), 
           family="Spartan-Bold",colour="#362626",size=5 )+
  annotate(geom="text",x=50,y=104,label=glue::glue("Winning chance"), 
           family="Spartan-Bold",colour="#362626",size=5 )+
  annotate(geom="text",x=75,y=122,label=glue::glue("{Wedstrijd$AwayTeam[1]}"), 
           family="Spartan-Bold",colour="#362626",size=5 ) +
  annotate(geom="text",x=75,y=116,label=glue::glue("{AwayGoals}"), 
           family="Spartan-Bold",colour="#362626",size=5 ) +
  annotate(geom="text",x=75,y=110,label=glue::glue("{xGLabelAway}"), 
           family="Spartan-Bold",colour="#362626",size=5 ) +
  annotate(geom="text",x=75,y=104,label=glue::glue("{winner$away}"), 
           family="Spartan-Bold",colour="#362626",size=5 ) +
  theme(plot.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"), 
        panel.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"),
        
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(family="Spartan-Medium",color="#f2f4f5"),
        plot.subtitle = element_markdown(hjust=0.5,family="Spartan-Light",color="#f2f4f5",size=8),
        plot.caption = element_text(size=8,family="Spartan-Medium",color="Green4"))
shotmap


Home$xG <- ifelse(Home$Type_of_play=="Penalty",0.79,Home$xG)
Home$sumxg <- cumsum(Home$xG)

Home2 <- Home %>% dplyr::select(1,3,4,7,26,27,31,30) 

BeginHome <-data.frame(0,"Player",Home$TeamId[1],0,0,0,0,"Home") %>% set_names(colnames(Home2))
EindHome <- data.frame(0,"Player",Home$TeamId[1],pmax(max(Home$minute),max(Away$minute)),
                       pmax(pmax(max(Home$expandedMinute),max(Away$expandedMinute)),90),0,
                       max(Home$sumxg),"Home")%>% 
  set_names(colnames(Home2))
Home3 <- rbind(BeginHome,Home2,EindHome)

Away$xG <- ifelse(Away$Type_of_play=="Penalty",0.79,Home$xG)
Away$sumxg <- cumsum(Away$xG)
Away2 <- Away %>% dplyr::select(1,3,4,7,26,27,31,30) 
BeginAway <-data.frame(0,"Player",Away$TeamId[1],0,0,0,0,"Away") %>% set_names(colnames(Away2))
EindAway <- data.frame(0,"Player",Away$TeamId[1],pmax(max(Home$minute),max(Away$minute)),
                       pmax(pmax(max(Home$expandedMinute),max(Away$expandedMinute)),90),0,max(Away$sumxg),"Away")%>% 
  set_names(colnames(Away2))

Away3 <- rbind(BeginAway,Away2,EindAway)

both<-rbind(Home3,Away3)
library(ggrepel)
timeline <- ggplot() + 
  geom_step(data=both,aes(expandedMinute,as.numeric(sumxg),colour=side),size=1) +
  scale_color_manual(values =c("Home" = Kleur1,
                               "Away" = Kleur2)) +
  
  geom_point(data=Home %>% filter(Goal==1),aes(expandedMinute,sumxg)) +
  geom_point(data=Away %>% filter(Goal==1),aes(expandedMinute,sumxg)) +
  geom_text_repel(data=both%>%filter(Goal==1),
                  aes(expandedMinute,sumxg,label=PlayerId),family="Spartan-Regular",size=3
                  ,nudge_x=.1,nudge_y = .1,hjust=1,vjust=-1)+
  scale_x_continuous(breaks=seq(0, pmax(pmax(max(Home$expandedMinute),max(Away$expandedMinute)),90),15))+
  labs(y="Sum of xG")+
  theme(plot.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"), 
        panel.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"),
        text = element_text(family="Spartan-Medium",color="black"),
        axis.title.x = element_blank(),
        panel.grid.minor = element_line(size=0.1),
        panel.grid.major = element_line(size=1),
        legend.position = "none",
        plot.title = element_markdown(family="Spartan-Medium",color="#f2f4f5"),
        plot.subtitle = element_markdown(hjust=0.5,family="Spartan-Light",color="#f2f4f5",size=8),
        plot.caption = element_text(size=8,family="Spartan-Medium",color="Green4"))

timeline
library(dplyr)
jaja <- dfWedstrijd %>% group_by(PlayerId,TeamId,side) %>% filter(Type_of_play != "Penalty") %>%
  dplyr::summarise(Shots = n(),ShotsOT = sum(OT),TotalxG=sum(xG),TotalxGOT=sum(xG*OT)) 

jaja <- jaja %>% 
  arrange(desc(TotalxG)) 

barxG <- top_n(ungroup(jaja), 5, TotalxG) %>% ggplot() + 
  geom_bar(stat="identity",aes(x=TotalxG,y=reorder(PlayerId,TotalxG),fill=side,colour=side)) +
 
  geom_bar(stat="identity",aes(x=TotalxGOT,y=reorder(PlayerId,TotalxGOT,colour=side)),
           fill="white",width=0.2) +
  
  scale_fill_manual(values=c("Home" = Kleur1,
                             "Away"=Kleur2))+
  scale_colour_manual(values=c("Home" = Kleur1,
                               "Away"=Kleur2))+
 
 
  labs(title="NPxG",
       subtitle = "Small bar is on target")+
  theme(plot.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"), 
        panel.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"),
        
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(family="Spartan-Medium",color="#273c45"),
        plot.subtitle = element_markdown(family="Spartan-Medium",color="#273c45",size=8),
        plot.caption = element_text(size=8,family="Spartan-Medium",color="Green4"))

barxG
dfbarxA <- dfWedstrijd %>% group_by(relatedPlayerId,TeamId,side) %>%
  summarise(xA=sum(xG),TotalxAOT=sum(xG*OT)) %>% filter(relatedPlayerId != "No")
barxA <- top_n(ungroup(dfbarxA), 5, xA) %>% ggplot() + 
  geom_bar(stat="identity",aes(x=xA,y=reorder(relatedPlayerId,xA),fill=side,colour = side)) +
#  geom_bar(stat="identity",aes(x=TotalxAOT,y=reorder(relatedPlayerId,TotalxAOT)),fill="white",width=0.2) +

  scale_fill_manual(values=c("Home" = Kleur1,
                             "Away"=Kleur2))+
  scale_colour_manual(values=c("Home" = Kleur1,
                               "Away"=Kleur2))+
  
  labs(title="xA",
       subtitle = " ")+
  theme(plot.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"), 
        panel.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"),
        
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(family="Spartan-Medium",color="#273c45"),
        plot.subtitle = element_markdown(family="Spartan-Medium",color="#273c45",size=8),
        plot.caption = element_text(size=8,family="Spartan-Medium",color="Green4"))
xT <- getXt(OneGame)
xT <- top_n(xT,5,total) 
xThreat <- ggplot(xT)+
geom_bar(stat="identity",aes(x=total,y=reorder(PlayerId,total),fill=side,colour = side)) +
  #  geom_bar(stat="identity",aes(x=TotalxAOT,y=reorder(relatedPlayerId,TotalxAOT)),fill="white",width=0.2) +
  
  scale_fill_manual(values=c("Home" = Kleur1,
                             "Away"=Kleur2))+
  scale_colour_manual(values=c("Home" = Kleur1,
                               "Away"=Kleur2))+
  
  labs(title="xThreat",
       subtitle = " ")+
  theme(plot.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"), 
        panel.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"),
        
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(family="Spartan-Medium",color="#273c45"),
        plot.subtitle = element_markdown(family="Spartan-Medium",color="#273c45",size=8),
        plot.caption = element_text(size=8,family="Spartan-Medium",color="Green4"))


OneGame$side <- ifelse(OneGame$TeamId==HomeTeam, "Home", "Away")
passdf <- OneGame %>% mutate(PlayerId = PlayerId %>% 
                               str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1."),
                             relatedPlayerId = relatedPlayerId %>% 
                               str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1.")) %>%
  filter(`type/displayName`=="Pass") %>% group_by(PlayerId,TeamId,side) %>% 
  mutate(Succ = ifelse(`outcomeType/displayName`=="Successful",1,0))%>%
  dplyr::summarise(Passes=n(),PassesSuccessful=sum(Succ))

passes<- top_n(passdf %>% ungroup(), 5, Passes) %>% ggplot() + 
  geom_bar(stat="identity",aes(x=Passes,y=reorder(PlayerId,Passes),fill=side,colour=side)) +
  geom_bar(stat="identity",aes(x=PassesSuccessful,y=reorder(PlayerId,Passes)),fill="white",width=0.2) +
 

  scale_fill_manual(values=c("Home" = Kleur1,
                             "Away"=Kleur2))+
  scale_colour_manual(values=c("Home" = Kleur1,
                               "Away"=Kleur2))+
  labs(title="Passes",
       subtitle = "Small bar is successful")+
  theme(plot.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"), 
        panel.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"),
        
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(family="Spartan-Medium",color="#273c45"),
        plot.subtitle = element_markdown(family="Spartan-Medium",color="#273c45",size=8),
        plot.caption = element_text(size=8,family="Spartan-Medium",color="Green4"))


xgbuild<- top_n(xgbuild, 5, npxgChain) %>% mutate(PlayerId = PlayerId %>% 
  str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1.")) %>%ggplot() + 
  geom_bar(stat="identity",aes(x=npxgChain,y=reorder(PlayerId,npxgChain),fill=TeamId,colour=TeamId)) +
 # geom_bar(stat="identity",aes(x=PassesSuccessful,y=reorder(PlayerId,npxgChain)),fill="white",width=0.2) +
  
  
  scale_fill_manual(values=c(Kleur2,Kleur1))+
  scale_colour_manual(values=c(Kleur2,Kleur1))+
  labs(title="xGBuildUp",
       subtitle = " ")+
  theme(plot.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"), 
        panel.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"),
        
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(family="Spartan-Medium",color="#273c45"),
        plot.subtitle = element_markdown(family="Spartan-Medium",color="#273c45",size=8),
        plot.caption = element_text(size=8,family="Spartan-Medium",color="Green4"))

passes

PassReady <- OneGame %>% mutate(PlayerId = PlayerId %>% 
                                  str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1."),
                                relatedPlayerId = relatedPlayerId %>% 
                                  str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1.")) 
PassReady$xm <- PassReady$x * 106
PassReady$xendm <- PassReady$endX * 106
PassReady$ym <- PassReady$y * 70.4
PassReady$yendm <- PassReady$endY * 70.4
PassReady$startToGoal <- sqrt((10600 - PassReady$xm)^2 + (3520 - PassReady$ym)^2)
PassReady$endToGoal <- sqrt((10600 - PassReady$xendm)^2 + (3520 - PassReady$yendm)^2)
PassReady$progress <- PassReady$startToGoal  - PassReady$endToGoal 
PassReady$progressive <- ifelse(PassReady$progress>3000 & PassReady$xm < 5300 & PassReady$xendm < 5300, "yes",
                                ifelse(PassReady$progress>1500 & PassReady$xm < 5300 & PassReady$xendm > 5300, "yes",
                                       ifelse(PassReady$progress>1000 & PassReady$xm > 5300,"yes","no")) )

keypassdf <- PassReady%>% group_by(PlayerId,TeamId,side) %>% filter(progressive == "yes") %>%
  mutate(Succ = ifelse(`outcomeType/displayName`=="Successful",1,0))%>%
  filter(x > 33.3 & x != 99.5) %>%
  dplyr::summarise(KeyPass=n(),succ = sum(Succ))



keypasses<- top_n(keypassdf %>% ungroup(), 5, KeyPass) %>% ggplot() + 
  geom_bar(stat="identity",aes(x=KeyPass,y=reorder(PlayerId,KeyPass),fill=side,colour=side)) +
  geom_bar(stat="identity",aes(x=succ
                               ,y=reorder(PlayerId,KeyPass),fill=TeamId),fill="white",width=0.2) +
  scale_pattern_color_manual(values=c(Kleur2,Kleur1))+
  scale_pattern_fill_manual(values=c(Kleur2,Kleur1))+
  scale_fill_manual(values=c("Home" = Kleur1,
                             "Away"=Kleur2))+
  scale_colour_manual(values=c("Home" = Kleur1,
                               "Away"=Kleur2))+
  labs(title="Prog. Passes <span style = 'font-size:7pt;'>(final 2/3rd)</span>",
       subtitle = "Small bar is successful") +
  theme(plot.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"), 
        panel.background = element_rect(fill = "#f2f4f5", colour = "#f2f4f5"),
        
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(family="Spartan-Medium",color="#273c45"),
        plot.subtitle = element_markdown(family="Spartan-Medium",color="#273c45",size=8),
        plot.caption = element_text(size=8,family="Spartan-Medium",color="Green4"))
keypasses
#



