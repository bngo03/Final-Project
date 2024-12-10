# Prompt 2

## create shot quality variable

## find specific sequences

### most common sqp_id == 1
s_df %>% 
  filter(sqp_id == 1) %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

### most common sqp_id == 2
s_df %>%
  filter(sqp_id == 2) %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

### most common sqp_id == 3
s_df %>%
  filter(sqp_id == 3) %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

### most common 1-2-3s

three_action_poss <- s_df %>%
  group_by(nba_game_id, quarter, poss_id) %>%
  filter(n() == 3) %>%
  ungroup()

three_action_poss <- three_action_poss %>%
  arrange(nba_game_id, quarter, poss_id, sqp_id) %>%
  group_by(nba_game_id, quarter, poss_id) %>%
  summarise(action_combo = paste(play_type, collapse = " -> ")) %>% 
  group_by(action_combo) %>% 
  filter(nchar(action_combo) > 6)  # Filter out rows with fewer than 3 actions

play_type_combo_counts <- three_action_poss %>%
  count(action_combo, sort = TRUE) %>% 
  group_by(action_combo)

view(play_type_combo_counts)

sum(play_type_combo_counts$n)

### most common second action when first action is ball screen
s_df %>% 
  filter(sqp_id == 2) %>% 
  filter(preceding_play_type == 'Ball Screen') %>% 
  group_by(play_type) %>% 
  count(play_type) %>% 
  arrange(desc(n))

## Combo 1: Ball Screen, 2) Closeout
bs_clout_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'Ball Screen') 

bs_clout_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Closeout')

bs_clout <- inner_join(bs_clout_1, bs_clout_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

bs_clout_12 <- bs_clout %>% 
  filter(!is.na(result_pts.y))
sum(bs_clout_12$poss_pts.y)
dim(bs_clout_12)

bs_clout_12 %>% 
  group_by(play_sis_zone.y) %>% 
  arrange(desc(shot_quality.y)) %>% 
  select(play_sis_zone.y, shot_quality.y)

summary(bs_clout_12$shot_quality.y)

bs_clout_3plus <- bs_clout %>% 
  filter(is.na(result_pts.y))
sum(bs_clout_3plus$poss_pts.y)
dim(bs_clout_3plus)

## Combo 2: Closeout, 2) Closeout
clout_clout_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'Closeout') 

clout_clout_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Closeout')

clout_clout <- inner_join(clout_clout_1, clout_clout_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

clout_clout_12 <- clout_clout %>% 
  filter(!is.na(result_pts.y))
sum(clout_clout_12$seq_pts.y)
dim(clout_clout_12)

summary(clout_clout_12$shot_quality.y)

clout_clout_3plus <- clout_clout %>% 
  filter(is.na(result_pts.y))
sum(clout_clout_3plus$seq_pts.y)
dim(clout_clout_3plus)

## Combo 3: 1) Ball Screen, 2) Iso
bs_iso_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'Ball Screen') 

bs_iso_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Iso')

bs_iso <- inner_join(bs_iso_1, bs_iso_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

bs_iso_12 <- bs_iso %>% 
  filter(!is.na(result_pts.y))
sum(bs_iso_12$poss_pts.y)
dim(bs_iso_12)

summary(bs_iso_12$shot_quality.y)

bs_iso_3plus <- bs_iso %>% 
  filter(is.na(result_pts.y))
sum(bs_iso_3plus$poss_pts.y)
dim(bs_iso_3plus)

## Combo 4: 1) Ball Screen, 2) Off-Cut
bs_off_cut_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'Ball Screen') 

bs_off_cut_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Off-Cut')

bs_off_cut <- inner_join(bs_off_cut_1, bs_off_cut_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

bs_off_cut_12 <- bs_off_cut %>% 
  filter(!is.na(result_pts.y))
sum(bs_off_cut_12$poss_pts.y)
dim(bs_off_cut_12)

summary(bs_off_cut_12$shot_quality.y)

bs_off_cut_3plus <- bs_off_cut %>% 
  filter(is.na(result_pts.y))
sum(bs_off_cut_3plus$poss_pts.y)
dim(bs_off_cut_3plus)


## Combo 5: 1) Iso, 2) Closeout
iso_clout_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'Iso') 

iso_clout_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Closeout')

iso_clout <- inner_join(iso_clout_1, iso_clout_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

iso_clout_12 <- iso_clout %>% 
  filter(!is.na(result_pts.y))
sum(iso_clout_12$poss_pts.y)
dim(iso_clout_12)

summary(iso_clout_12$shot_quality.y)

iso_clout_3plus <- iso_clout %>% 
  filter(is.na(result_pts.y))
sum(iso_clout_3plus$poss_pts.y)
dim(iso_clout_3plus)

## Combo 6: 1) Initiation, 2) Closeout
init_clout_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'Initiation') 

init_clout_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Closeout')

init_clout <- inner_join(init_clout_1, init_clout_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

init_clout_12 <- init_clout %>% 
  filter(!is.na(result_pts.y))
sum(init_clout_12$poss_pts.y)
dim(init_clout_12)

summary(init_clout_12$shot_quality.y)

init_clout_3plus <- init_clout %>% 
  filter(is.na(result_pts.y))
sum(init_clout_3plus$poss_pts.y)
dim(init_clout_3plus)

## Combo 7: 1) Initiation, 2) Downhill
init_downhill_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'Initiation') 

init_downhill_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Downhill')

init_downhill <- inner_join(init_downhill_1, init_downhill_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

init_downhill_12 <- init_downhill %>% 
  filter(!is.na(result_pts.y))
sum(init_downhill_12$poss_pts.y)
dim(init_downhill_12)

summary(init_downhill_12$shot_quality.y)

init_downhill_3plus <- init_downhill %>% 
  filter(is.na(result_pts.y))
sum(init_downhill_3plus$poss_pts.y)
dim(init_downhill_3plus)

## Combo 8: 1) DHO, 2) Closeout
dho_clout_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'DHO') 

dho_clout_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Closeout')

dho_clout <- inner_join(dho_clout_1, dho_clout_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

dho_clout_12 <- dho_clout %>% 
  filter(!is.na(result_pts.y))
sum(dho_clout_12$poss_pts.y)
dim(dho_clout_12)

summary(dho_clout_12$shot_quality.y)

dho_clout_3plus <- dho_clout %>% 
  filter(is.na(result_pts.y))
sum(dho_clout_3plus$poss_pts.y)
dim(dho_clout_3plus)

## Combo 9: 1) Ball Screen, 2) Off-Move
bs_off_move_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'Ball Screen') 

bs_off_move_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Off-Move')

bs_off_move <- inner_join(bs_off_move_1, bs_off_move_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

bs_off_move_12 <- bs_off_move %>% 
  filter(!is.na(result_pts.y))
sum(bs_off_move_12$poss_pts.y)
dim(bs_off_move_12)

summary(bs_off_move_12$shot_quality.y)

bs_off_move_3plus <- bs_off_move %>% 
  filter(is.na(result_pts.y))
sum(bs_off_move_3plus$poss_pts.y)
dim(bs_off_move_3plus)

## Combo 10: Perim Gather, 2) Closeout
perim_gather_clout_1 <- s_df %>% 
  filter(sqp_id == 1, play_type == 'Perim Gather') 

perim_gather_clout_2 <- s_df %>% 
  filter(sqp_id == 2, play_type == 'Closeout')

perim_gather_clout <- inner_join(perim_gather_clout_1, perim_gather_clout_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))

perim_gather_clout_12 <- perim_gather_clout %>% 
  filter(!is.na(result_pts.y))
sum(perim_gather_clout_12$poss_pts.y)
dim(perim_gather_clout_12)

summary(perim_gather_clout_12$shot_quality.y)

perim_gather_clout_3plus <- perim_gather_clout %>% 
  filter(is.na(result_pts.y))
sum(perim_gather_clout_3plus$poss_pts.y)
dim(perim_gather_clout_3plus)



### BS into Closeout w/ only 2 actions
bs_clout_12 <- inner_join(bs_clout_1, bs_clout_2, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id')) %>% 
  filter(!is.na(result_pts.y))



### BS into Closeout w/ 3+ actions

# Join the datasets based on common identifiers
# Filter for sequences where there are more than two actions


# Calculate the sum of result_pts.y for these sequences
sum_points_multiple_actions <- sum(bs_clout_multiple_actions$result_pts.y)

# Print the sum
print(sum_points_multiple_actions)

view(result_data)

### Calculate Points Per Poss. for each
sum(bs_clout_12$result_pts.y)
sum(bs_clout_12_plus$result_pts.y)

bs_clout_4 <- s_df %>% 
  filter(sqp_id == 6, play_type == 'Ball Screen')

bs_clout_5 <- s_df %>% 
  filter(sqp_id == 7, play_type == 'Closeout')

bs_clout_23 <- inner_join(bs_clout_4, bs_clout_5, by = c('game_date', 'nba_game_id', 'quarter', 'sequence_id'))


## 1) Ball Screen, 2) Closeout, 3) x
bs_clout_3 <- s_df %>% 
  filter(sqp_id >= 3) 

bs_clout_123 <- inner_join(bs_clout_12, bs_clout_3, by = c('nba_game_id', 'quarter', 'sequence_id'))



## all shots
bs_clout_12_shots <- bs_clout_12 %>% 
  filter(result_pts.y != 'NULL') %>% 
  filter(result_type.y != 'TOV') %>% 
  filter(result_type.y != 'SC Violation') %>% 
  filter(result_type.y != 'Side Out') %>% 
  filter(result_type.y != 'Take Side') %>% 
  filter(result_type.y != 'Jump Ball') %>% 
  filter(result_type.y != 'Take FT') %>% 
  filter(result_type.y != 'Take Side')

bs_clout_12_non_shots <- bs_clout_12 %>% 
  filter(result_pts.y == 'NULL' |
           result_type.y == 'TOV' |
           result_type.y == 'SC Violation' |
           result_type.y == 'Side Out' |
           result_type.y == 'Take Side' |
           result_type.y == 'Jump Ball' |
           result_type.y == 'Take Side')
sum(bs_clout_12_non_shots$result_pts.y)

non_shots <- c('NULL', 'TOV', 'Take FT', 'Take Side', 'Side Out', 
               'SC Violation', 'Jump Ball')

## makes
bs_clout_12_makes <- bs_clout_12_shots %>% 
  filter(result_pts.y > 0)
sum(bs_clout_12_makes$result_pts.y)

## misses
bs_clout_12_misses <- bs_clout_12_shots %>% 
  filter(result_pts.y == 0)

## fouled
bs_clout_12_fouled <- bs_clout_12_shots %>% 
  filter(result_contest.y == 'Fouled')

bs_clout_12_shots %>% 
  group_by(result_pts.y) %>% 
  count(result_pts.y) %>% 
  arrange(desc(n))

bs_clout_12 %>% 
  group_by(shot_quality.y) %>% 
  count(shot_quality.y) %>% 
  arrange(desc(n))

# all shot types
bs_clout_12 %>% 
  group_by(result_type.y) %>% 
  count(result_type.y) %>% 
  arrange(desc(n))

# sum points
class(bs_clout_12$result_pts.y) = "numeric"
sum(bs_clout_12$result_pts.y)

# sum shot quality
sum(bs_clout_12$shot_quality.y)

## plays where closeouts are second action
second_df <- s_df %>% 
  filter(sqp_id == 2)