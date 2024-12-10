# import necessary packages
library(tidyverse)
library(gt)
library(gtExtras)
library(rvest)
library(reshape2)
library(rmarkdown)

# import dataframe
df <- read.csv("C:/Users/jacks/Desktop/Programming/Syracuse BAC '24/Data/sis_syracuse_case_competition_2024.csv")

# get dimensions
dim(df)

# convert data types of vars as necessary
class(df$player_x) = "numeric"
class(df$player_y) = "numeric"
class(df$result_dist) = "numeric"
class(df$result_pts) = "numeric"
class(df$result_shotclock) = "numeric"
class(df$result_gameclock) = "numeric"

df <- df[order(df$poss_id, df$gameclock), ]

df <- df %>%
  group_by(poss_id) %>%
  mutate(poss_length = gameclock - lag(gameclock, default = first(gameclock))) %>%
  ungroup()

# convert date column manually
df <- df %>% 
  mutate(game_date_new = case_when(
    grepl("10-31", game_date) ~ "2022-10-31",
    grepl("11-01", game_date) ~ "2022-11-01",
    grepl("11-02", game_date) ~ "2022-11-02",
    grepl("11-03", game_date) ~ "2022-11-03",
    grepl("11-04", game_date) ~ "2022-11-04",
    grepl("11-05", game_date) ~ "2022-11-05",
    grepl("11-06", game_date) ~ "2022-11-06",
    grepl("11-07", game_date) ~ "2022-11-07",
    grepl("11-08", game_date) ~ "2022-11-08",
    grepl("11-09", game_date) ~ "2022-11-09",
    grepl("11-10", game_date) ~ "2022-11-10",
    grepl("11-11", game_date) ~ "2022-11-11",
    grepl("11-12", game_date) ~ "2022-11-12", 
    grepl("11-13", game_date) ~ "2022-11-13", 
    grepl("11-14", game_date) ~ "2022-11-14")
  ) %>% 
  select(c(1:2, 'game_date_new', 4:34)) %>% 
  rename(game_date = game_date_new) %>% 
  arrange(game_date, nba_game_id, quarter, desc(gameclock)) 

sequence_points <- df %>%
  group_by(nba_game_id, quarter, sequence_id) %>%
  summarise(seq_pts = sum(result_pts, na.rm = TRUE))

sequence_points <- sequence_points %>% 
  group_by(nba_game_id, quarter) %>% 
  fill(seq_pts)

df <- left_join(df, sequence_points, by = c("nba_game_id", "quarter", "sequence_id"))

# create poss points variable
poss_points <- df %>% 
  group_by(nba_game_id, quarter, poss_id) %>% 
  summarise(poss_pts = sum(result_pts, na.rm = TRUE))

poss_points <- poss_points %>% 
  group_by(nba_game_id, quarter) %>% 
  fill(poss_pts)

df <- left_join(df, poss_points, by = c("nba_game_id", "quarter", "poss_id"))

# remove unnecessary variables + group by date, game, qtr, seq
df <- df %>% 
  group_by(game_date, nba_game_id, quarter, sequence_id) %>%
  select(c(2:17, 19, 21:34))

##   REMOVED:
##   filename
##   playerJersey
##   player_nba_id

# Before diving straight in, it's imperative that we understand the meaning of 
# 'sequences'. To get a feel for this, we can watch the game film for which the
# data is associated while following along with our data. Let's take a look at
# the first quarter of the Hawks/Knicks game from 11/02/2022. Additionally
atl_nyk_nov2_q1 <- new_df %>% 
  filter(nba_game_id == 22200110) %>% 
  filter(quarter == 1) %>% 
  arrange(desc(gameclock)) %>% 
  filter(player_type == 'Ball-Handler') %>% 

# After a few possessions, if you run the code above without the last pipe 
# (%>%), the data is still pretty hard to follow... four rows
# of data just for one P&R? Come on. To combat this, let's further filter
# these results to include only one row per play_id where that row features the
# ball-handler player_type only (excludes screener, BH Def, and SC Def). 
# While we're at it, let's remove all columns that are self-explanatory and/or
# we already have a solid understanding of (this may differ for each user).
  select(c(1:26, 29)) %>% 
  relocate('poss_id', 'sequence_id', 'sbg_id', everything())

# Now that we have a better understanding of how our data translates to the game
# film, Let's address the first part of the prompt. We're allowed to explore the 
# data freely in any way we see fit, but our results/presentation should focus 
# on a few things. 

