# First is utilizing sequencing in the data set; we are to explore the 
# relationship between different actions and play types and the order in which 
# they occur -- what leads to what?

s_df <- df %>%
  filter(player_type == 'Ball-Handler') %>% 
  mutate(sqp_id = row_number()) %>% 
  mutate(preceding_play_type = ifelse(sqp_id == lag(sqp_id, default = last(sqp_id)) + 1, lag(play_type), NA))

non_shots <- c('Jump Ball', 'SC Violation', 'Side Out', 'Take Side', 'TOV', 'NULL')

s_df <- s_df %>% 
  mutate(shot_quality = case_when(
    result_contest == 'NULL' & (!(result_type %in% non_shots)) ~ 1,
    result_contest == 'Open' ~ 1, 
    result_contest == 'Poor' ~ 0.75, 
    result_contest == 'Average' ~ 0.5, 
    result_contest == 'Plus' ~ 0.25, 
    result_contest == 'Alter' ~ 0.125, 
    result_contest == 'Block' ~ 0, 
    result_contest == 'Fouled' ~ NA,
    result_contest == 'NULL' & (result_type %in% non_shots) ~ NA_real_, # Handle NA result_contest with non-NA result_pts
  ))

# create sequence points variable
sequence_points <- s_df %>%
  group_by(nba_game_id, quarter, sequence_id) %>%
  summarise(seq_pts = sum(result_pts, na.rm = TRUE))

sequence_points <- sequence_points %>% 
  group_by(nba_game_id, quarter) %>% 
  fill(seq_pts)

s_df <- left_join(s_df, sequence_points, by = c("nba_game_id", "quarter", "sequence_id"))

# create poss points variable
poss_info <- s_df %>% 
  group_by(nba_game_id, quarter, poss_id) %>% 
  summarise(poss_pts = sum(result_pts, na.rm = TRUE))

poss_info <- poss_info %>% 
  group_by(nba_game_id, quarter) %>% 
  fill(poss_pts)

s_df <- left_join(s_df, poss_info, by = c('nba_game_id', 'quarter', 'poss_id'))

# create shot quality variable per seq
seq_shot_quality <- df %>%
  group_by(nba_game_id, quarter, sequence_id) %>%
  summarise(shot_quality = first(shot_quality[!is.na(result_contest)], default = NA)) %>%
  ungroup()

# Merge the shot quality values back into the original dataset based on sequence identifier
df <- left_join(df, seq_shot_quality, by = c("nba_game_id", "quarter", "sequence_id"))

# Print the resulting dataset
print(df)

# we need to find all possessions in which x play_type is first action of poss
# then, find subsequent actions to discover what x leads to

# play types with sqp_id > 1
s_df %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: totals
s_df %>% 
  group_by(preceding_play_type) %>% 
  count(preceding_play_type) %>% 
  arrange(desc(n))

# preceding play type: ball screen
ppt_ballscreen <- s_df %>% 
  filter(preceding_play_type == 'Ball Screen')

ppt_ballscreen %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: closeout
ppt_closeout <- s_df %>% 
  filter(preceding_play_type == 'Closeout')

ppt_closeout %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: initiation
ppt_initiation <- s_df %>% 
  filter(preceding_play_type == 'Initiation')

ppt_initiation %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: perim gather
ppt_perim_gather <- s_df %>% 
  filter(preceding_play_type == 'Perim Gather')

ppt_perim_gather %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: DHO
ppt_dho <- s_df %>% 
  filter(preceding_play_type == 'DHO')

ppt_dho %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: iso
ppt_iso <- s_df %>% 
  filter(preceding_play_type == 'Iso')

ppt_iso %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: transition
ppt_transition <- s_df %>% 
  filter(preceding_play_type == 'Transition')

ppt_transition %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: off-move
ppt_off_move <- s_df %>% 
  filter(preceding_play_type == 'Off-Move')

ppt_off_move %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: mid gather
ppt_mid_gather <- s_df %>% 
  filter(preceding_play_type == 'Mid Gather')

ppt_mid_gather %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: post
ppt_post <- s_df %>% 
  filter(preceding_play_type == 'Post')

ppt_post %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: off-cut
ppt_off_cut <- s_df %>% 
  filter(preceding_play_type == 'Off-Cut')

ppt_off_cut %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: downhill
ppt_downhill <- s_df %>% 
  filter(preceding_play_type == 'Downhill')

ppt_downhill %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: paint_gather
ppt_paint_gather <- s_df %>% 
  filter(preceding_play_type == 'Paint Gather')

ppt_paint_gather %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: short_roll
ppt_short_roll <- s_df %>% 
  filter(preceding_play_type == 'Short Roll')

ppt_short_roll %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: inbounds
ppt_inbounds <- s_df %>% 
  filter(preceding_play_type == 'Inbounds')

ppt_inbounds %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: rim_gather
ppt_rim_gather <- s_df %>% 
  filter(preceding_play_type == 'Rim Gather')

ppt_rim_gather %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: backcourt
ppt_backcourt <- s_df %>% 
  filter(preceding_play_type == 'Backcourt')

ppt_backcourt %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

# preceding play type: fake DHO
ppt_fake_dho <- s_df %>% 
  filter(preceding_play_type == 'Fake DHO')

ppt_fake_dho %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

write.csv(s_df, "s_df.csv", row.names = FALSE)