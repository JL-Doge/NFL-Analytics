


if(require("tidyverse")==FALSE){install.packages("tidyverse")}
if(require("ggrepel")==FALSE){install.packages("ggrepel")}
if(require("ggimage")==FALSE){install.packages("ggimage")}
if(require("nflfastR")==FALSE){install.packages("nflfastR")}

library(dplyr)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(scatterplot3d)



##Initial PCA, EDA Feature engineering for understanding the response variable, Fantasy points... Position is a factor variable with 3 levels. (WR/RB/TE)
#using PFF Fantasy stats data set"PFF_week_17.csv" which is a *season's worth of data.   (https://www.pff.com/fantasy/stats?scoring=preset_ppr).  
#Scoring format is PPR, ADP is 


Fantasystats <- read.csv("./data_2/PFF_week_17.csv")
str(Fantasystats)
names(Fantasystats)


FFdive <- Fantasystats %>%
  select(player, team, position, fantasyPts, recTarg, depth, ptsPerTouch, rzRecTarg) %>%
  arrange(desc(fantasyPts))

FFdive <- as_tibble(FFdive)
FFdive


#Let's also look at environmental considerations (team v league) for some context to see if there are any insights because there is a considerable amount of noise in 
#any NFL data set, 11 players on the field at one time and it may be short-sighted to look at the individual performances in a vacuum.
#Definition / explanation of EPA:

data <- load_pbp(2021)
rows = dim(data)[[1]]
cols = dim(data)[[2]]


dim(data)
str(data[1:10])
names(data)
view(data)


?`nflfastR-package`
head(teams_colors_logos)
names(teams_colors_logos)
data("teams_colors_logos")


EPA <- read.csv("./data_2/rbsdm.comstats.csv")
head(EPA)

EPA <- EPA %>%
  left_join(teams_colors_logos, by = c('team.1' = 'team_abbr'))

EPAplot <- EPA %>%
  ggplot(aes(x = Dropback.EPA, y = Rush.EPA)) +
  geom_hline(yintercept = mean(EPA$Rush.EPA), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(EPA$Dropback.EPA), color = "red", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_wikipedia)) + 
  labs(x = "Dropback EPA/play",
       y = "Rush EPA per play",
       title = "Passing & Rushing Efficiency, 2021",
       caption = "Data: @nflfastR") +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.4, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#It looks like the league leaders by team in these categories or IND & GB. let's see where the IND RB's lands in terms of Rushing performance & let's look at GB WR's to see if anything stands out.
#Also of note I'd like to better understand what's happening in HOU, NYG and CAR... We'll stick a pin in that and come back to this EPA data
#during exploratory plots


#One of the questions I'd like to answer is how efficient the market (Sports Books) & fantasy players are at predicting fantasy production.  The 
#following ADP (Average Draft Position) reflects where these players were drafted before the season on the major platforms.  Data set was downloaded from,
# https://www.fantasypros.com/nfl/adp/overall.php

FFAdp <- read.csv("./data_2/FantasyPros_ADP.csv")
names(FFAdp)

#make all variable names lower case and mirror for join/merge...
names(FFAdp) <- c("ADP", "player", "team", "bye", "position rank", "ESPN", "RTSports", "Fantrax", "FFC", 'Average')
names(FFAdp)


FF_merge <- left_join(FFdive, FFAdp, by="player")
names(FF_merge)
#[1] "player"        "team.x"        "position"      "fantasyPts"    "recTarg"       "depth"         "ptsPerTouch"  
#[8] "rzRecTarg"     "ADP"           "team.y"        "bye"           "position rank" "ESPN"          "RTSports"     
#[15] "Fantrax"       "FFC"           "Average" 
#Let's create a new table with player, team (removing redundant column, defaulting to the one with more complete data
#in the event we would like to also add EPA data by team for ml algo ) 

FF_cleaned <- FF_merge[, c(1:9, 12,17)]
head((FF_cleaned))
FF_cleaned <- as_tibble(FF_cleaned)
summary(FF_cleaned)
##Action Item:  Remove NA's from pts per touch.  This leaves us with a data set of 259 players which is beyond the realm of what we
#are looking at in order to remove players that will go undrafted during a "normal" fantasy draft or prospects that would represent a relevent sample 
# impactful players

is.na(FF_cleaned)
FF_Omit <- na.omit(FF_cleaned)

##Correlation table to see Fantasy Points / Pts per Touch, Depth, Red Zone Targets, market considerations like ADP
cor(FF_Omit[ ,c(4:9,11)], method = "pearson")
#fantasyPts    recTarg      depth ptsPerTouch  rzRecTarg         ADP     Average
#fantasyPts   1.00000000  0.8209241 0.05698553  0.13234915  0.7110932 -0.63217607 -0.63368069
#recTarg      0.82092414  1.0000000 0.39863121  0.42440155  0.9176394 -0.50640277 -0.50014899
#depth        0.05698553  0.3986312 1.00000000  0.86065618  0.5119631  0.04415814  0.05595817
#ptsPerTouch  0.13234915  0.4244015 0.86065618  1.00000000  0.5499976  0.03316419  0.05036698
#rzRecTarg    0.71109323  0.9176394 0.51196314  0.54999763  1.0000000 -0.39552262 -0.39031965
#ADP         -0.63217607 -0.5064028 0.04415814  0.03316419 -0.3955226  1.00000000  0.98692782
#Average     -0.63368069 -0.5001490 0.05595817  0.05036698 -0.3903196  0.98692782  1.00000000


pairs(FF_Omit[,c(4:9,11)])
#Shows correlation of ADOT And RecTarg

#Let's see if we can use NFLFast web scraping package to incorporate some advanved metrics like EPA
#Pulling data for 2021 so it's also "apples to apples" with our FF Pros and PFF data


names(FF_Omit)
names(EPA) <- c("rank", "team", "team.y", "team.x", "EPA", "Success.Rate.SR", "Dropback.EPA",
                "Dropback.SR", "Rush.EPA", "Rush.SR", "team_name", "team_id", "team_nick",
                "team_color", "team_color2",  "team_color3","team_color4","team_logo_wikipedia",
                "team_logo_espn", "team_wordmark")  

FF_merge2 <- left_join(EPA, FF_Omit, by="team.x")


FF_merge3 <- FF_merge2 %>%
  select(player, team, position, fantasyPts, recTarg, depth, ptsPerTouch, rzRecTarg,
         EPA, Dropback.EPA, Rush.EPA, ADP, Average, `position rank`) %>%
  arrange(desc(fantasyPts))

names(FF_merge3)
names(FF_merge3)
#[1] "player"        "team"          "position"      "fantasyPts"    "recTarg"      
#[6] "depth"         "ptsPerTouch"   "rzRecTarg"     "EPA"           "Dropback.EPA" 
#[11] "Rush.EPA"      "ADP"           "Average"       "position rank"


pairs(FF_merge3[, c(4:13)])
cor(FF_merge3[, c(4:13)], method = "pearson")


#sadly there is no relationship between team efficiency / environment / EPA and Fantasy points for individual players!
#let's run the same numeric variables in PCA / EDA / plots / graphing etc.

#QB performance / Value.  Top players pre-draft by market (Top 479)

QB1 <- read.csv("./data_2/All Positions Points & ADP.csv")
QB2 <- QB1[, c(2,4,5,8,13)]
QB2_Omit <- na.omit(QB2) 
tail(QB2_Omit)
summary(QB2_Omit)

ggplot(data = QB2_Omit) +
  geom_point(mapping = aes(x=expertConsensus, y=Proj.Pts))


QB2_Omit %>%
  ggplot(aes(x = sqrt(expertConsensus), y = sqrt(Proj.Pts))) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA)




hist(Fantasystats$depth, probability = TRUE, breaks = 20)
lines(density(Fantasystats$depth))

boxplot(Fantasystats$depth, col = "blue")
var(Fantasystats$depth)

table(Fantasystats$depth)
barplot(table(Fantasystats$depth), col = "blue")
unique(Fantasystats$depth)
sum(unique(Fantasystats$depth))
summary(Fantasystats$depth)


plot(Fantasystats$depth, Fantasystats$fantasyPts, pch=19, col="blue")
?pch
##Clustering may be a better option for seeing what sort of groups seperate.  There doesn't appear to be a 
##strong relationsip looking at ADOT v Fantasy Points

library(scatterplot3d)

scatterplot3d(Fantasystats$depth, Fantasystats$recTarg, Fantasystats$fantasyPts, 
              highlight.3d = TRUE, col.axis = "blue", col.grid = "lightblue", main = "PFF WR Data Week 8",
              pch = 20, xlab = "ADOT", ylab = "Targets", zlab = "Fantasy Points"
)





scatterplot3d(Fantasystats$depth, Fantasystats$recTarg, Fantasystats$fantasyPts, highlight.3d = TRUE)























library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)


options(scipen = 9999)



data %>% 
  select(posteam, defteam, desc, rush, pass) %>% 
  head()

data %>% select(posteam, defteam, desc, rush, pass) %>% head()

data %>% 
  filter(rush == 1 | pass == 1) %>%
  select(posteam, desc, rush, pass, name, passer, rusher, receiver) %>% 
  head()

pbp_rp <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa))

##In the above, `!is.na(epa)` means to exclude plays with missing (`na`) EPA. The `!` 
##symbol is often used by computer folk to negate something, so `is.na(epa)` means 
##"EPA is missing" and `!is.na(epa)` means "EPA is not missing", which we have used above.

pbp_rp %>%
  filter(posteam == "DAL", rush == 1) %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc = mean(yards_gained), plays = n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 20)


pbp_rp %>%
  filter(posteam == "DAL", down <= 4, play_type == 'run') %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
  ) %>%
  filter(plays > 20)

pbp_rp %>%
  mutate(
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  select(posteam, home_team, home) %>%
  head(10)

pbp_rp %>%
  mutate(
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  group_by(home) %>%
  summarize(epa = mean(epa))

pbp_rp %>%
  filter(!is.na(cp)) %>%
  mutate(
    depth = case_when(
      air_yards < 0 ~ "Negative",
      air_yards >= 0 & air_yards < 10 ~ "Short",
      air_yards >= 10 & air_yards < 20 ~ "Medium",
      air_yards >= 20 ~ "Deep"
    )
  ) %>%
  group_by(depth) %>%
  summarize(cp = mean(cp))


schotty <- pbp_rp %>%
  filter(wp > .20 & wp < .80 & down <= 2 & qtr <= 2 & half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(mean_pass = mean(pass), plays = n()) %>%
  arrange(-mean_pass)
schotty

ggplot(schotty, aes(x=reorder(posteam,-mean_pass), y=mean_pass)) +
  geom_text(aes(label=posteam))

pbp <- load_pbp(2015:2021)
pbp %>%
  group_by(season) %>%
  summarize(n = n())

pbp %>%
  group_by(play_type) %>%
  summarize(n = n())

qbs <- pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 100 & n_plays > 1000)

qbs

head(teams_colors_logos)


qbs <- qbs %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))



qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = qbs$team_color, cex=qbs$n_plays / 350, alpha = .6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2015 - 2019",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))



qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = qbs$n_plays / 45000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2015 - 2019",
       caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

games <- nflreadr::load_schedules()
str(games)

home <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team)
home %>% head(5)

away <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>%
  mutate(result = -result)
away %>% head(5)


results <- bind_rows(home, away) %>%
  arrange(week) %>%
  mutate(
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5
    )
  )
    
    results %>% filter(season == 2019 & team == 'SEA')
        
    
    
team_wins <- results %>%
      group_by(team, season) %>%
      summarize(
        wins = sum(win),
        point_diff = sum(result)) %>%
      ungroup()
  

team_wins %>%
      arrange(-wins) %>%
      head(5)


pbp <- load_pbp(1999:2019) %>%
  filter(rush == 1 | pass == 1, season_type == "REG", !is.na(epa), !is.na(posteam), posteam != "") %>%
  select(season, posteam, pass, defteam, epa)

pbp %>%
  group_by(posteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  head(4)

pbp %>%
  group_by(posteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  head(4)


offense <- pbp %>%
  group_by(posteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(off_pass_epa = `1`, off_rush_epa = `0`)


defense <- pbp %>%
  group_by(defteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(def_pass_epa = `1`, def_rush_epa = `0`)

offense %>%
  arrange(-off_pass_epa) %>%
  head(5)
#top 5 defenses
defense %>%
  arrange(def_pass_epa) %>%
  head(5)

team_wins %>%
  group_by(team) %>%
  summarize(n=n()) %>%
  arrange(n)

team_wins <- team_wins %>%
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )

team_wins %>%
  group_by(team) %>%
  summarize(n=n()) %>%
  arrange(n)

data <- team_wins %>%
  left_join(offense, by = c('team' = 'posteam', 'season')) %>%
  left_join(defense, by = c('team' = 'defteam', 'season'))


data %>%
  filter(team == 'SEA' & season >= 2012)

data <- data %>% 
  arrange(team, season) %>%
  group_by(team) %>% 
  mutate(
    prior_off_rush_epa = lag(off_rush_epa),
    prior_off_pass_epa = lag(off_pass_epa),
    prior_def_rush_epa = lag(def_rush_epa),
    prior_def_pass_epa = lag(def_pass_epa),
    prior_point_diff = lag(point_diff)
  ) %>% 
  ungroup()
data %>%
  head(5)

data %>% 
  select(-team, -season) %>%
  cor(use="complete.obs") %>%
  round(2)

pp = cor(data$off_pass_epa, data$prior_off_pass_epa, use="complete.obs") %>%
  round(2)
rr = cor(data$off_rush_epa, data$prior_off_rush_epa, use="complete.obs") %>%
  round(2)
pd = cor(data$def_pass_epa, data$prior_def_pass_epa, use="complete.obs") %>%
  round(2)
rd = cor(data$def_rush_epa, data$prior_def_rush_epa, use="complete.obs") %>%
  round(2)

message("2009 through 2019")
data %>% 
  filter(season >= 2009) %>%
  select(wins, point_diff, off_pass_epa, off_rush_epa, prior_point_diff, prior_off_pass_epa, prior_off_rush_epa) %>%
  cor(use="complete.obs") %>%
  round(2)


message("1999 through 2008")
data %>% 
  filter(season < 2009) %>%
  select(wins, point_diff, off_pass_epa, off_rush_epa, prior_point_diff, prior_off_pass_epa, prior_off_rush_epa) %>%
  cor(use="complete.obs") %>%
  round(2)

data <- data %>% filter(season >= 2009)
fit <- lm(wins ~ prior_off_pass_epa  + prior_off_rush_epa + prior_def_pass_epa + prior_def_rush_epa, data = data)
summary(fit)


fit2 <- lm(wins ~ prior_point_diff, data = data)
summary(fit2)