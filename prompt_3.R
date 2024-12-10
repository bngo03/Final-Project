### seq. with only one action
singles <- s_df %>% 
  group_by(nba_game_id, quarter, sequence_id) %>% 
  filter(poss_type != 'Transition') %>% 
  filter(poss_type != 'Secondary') %>% 
  filter(max(sqp_id) == 1) %>% 
  ungroup()

singles %>% 
  group_by(play_type) %>% 
  count(play_type) %>%
  arrange(desc(n))

singles_bs <- singles %>%  
  filter(play_type == 'Ball Screen')

mean(singles_bs$shot_quality, na.rm = TRUE)

mean(singles_paint_gather$shot_quality, na.rm = TRUE)

sum(singles_init$poss_pts)

singles_bs <- singles %>%  
  filter(play_type == 'Ball Screen')
mean(singles_bs$shot_quality, na.rm = TRUE)

singles_init <- singles %>%  
  filter(play_type == 'Initiation')
mean(singles_init$shot_quality, na.rm = TRUE)

singles_fake_dho <- singles %>% 
  filter(play_type == 'Fake DHO') 
mean(singles_fake_dho$shot_quality, na.rm = TRUE)

singles_dho <- singles %>% 
  filter(play_type == 'DHO')
mean(singles_dho$shot_quality, na.rm = TRUE)

singles_rim_gather <- singles %>% 
  filter(play_type == 'Rim Gather') 
mean(singles_rim_gather$shot_quality, na.rm = TRUE)

singles_off_cut <- singles %>% 
  filter(play_type == 'Off-Cut') 
mean(singles_off_cut$shot_quality, na.rm = TRUE)

singles_iso <- singles %>% 
  filter(play_type == 'Iso') 
mean(singles_iso$shot_quality, na.rm = TRUE)

singles_closeout <- singles %>% 
  filter(play_type == 'Closeout') 
mean(singles_closeout$shot_quality, na.rm = TRUE)

singles_perim_gather <- singles %>% 
  filter(play_type == 'Perim Gather')
mean(singles_perim_gather$shot_quality, na.rm = TRUE)

singles_paint_gather <- singles %>% 
  filter(play_type == 'Paint Gather')
mean(singles_paint_gather$shot_quality, na.rm = TRUE)

singles_mid_gather <- singles %>% 
  filter(play_type == 'Mid Gather')
mean(singles_mid_gather$shot_quality, na.rm = TRUE)

singles_transition <- singles %>% 
  filter(play_type == 'Transition')
mean(singles_transition$shot_quality, na.rm = TRUE)

singles_downhill <- singles %>% 
  filter(play_type == 'Downhill')
mean(singles_downhill$shot_quality, na.rm = TRUE)

singles_backcourt <- singles %>% 
  filter(play_type == 'Backcourt')
mean(singles_backcourt$shot_quality, na.rm = TRUE)

singles_inbounds <- singles %>% 
  filter(play_type == 'Inbounds')
mean(singles_inbounds$shot_quality, na.rm = TRUE)

singles_off_move <- singles %>% 
  filter(play_type == 'Off-Move')
mean(singles_off_move$shot_quality, na.rm = TRUE)

singles_post <- singles %>% 
  filter(play_type == 'Post')
mean(singles_post$shot_quality, na.rm = TRUE)

s_df_dljf <- s_df %>% 
  filter(sqp_id == 1 & play_type == 'Ball Screen') %>% 
  filter(sqp_id == 2 & play_type == 'Closeout')

df %>%
  group_by(nba_game_id)

ggplot(s_df, aes(x = shot_quality, y = poss_pts)) %>% 
  geom_smoot