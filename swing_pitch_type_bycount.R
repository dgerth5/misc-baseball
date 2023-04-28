library(tidyverse)
library(readr)

statcast22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")

wanted_pitch_types <- c("FF", "SI", "FC", "CH", "CB", "SL", "ST")

# from drew haugen
swing_events <- c(
  "foul_tip", "swinging_strike", "swinging_strike_blocked", 
  "missed_bunt", "foul", "hit_into_play", "foul_bunt", "bunt_foul_tip"
)


mod_df <- statcast22 %>%
  mutate(pitch_type2 = if_else(pitch_type == "CU" | pitch_type == "KC", "CB", pitch_type),
         is_swing = if_else(description %in% swing_events, 1, 0),
         is_pt = if_else(pitch_type2 %in% wanted_pitch_types, 1, 0),
         count = paste0(balls, "-", strikes)) %>%
  filter(is_pt == 1) %>%
  filter(strikes != 3 & balls != 4) %>%
  select(is_swing, pitch_type2, plate_x, plate_z, count) %>%
  rename(pitch_type = pitch_type2) %>%
  drop_na()

unique(mod_df$pitch_type)

mod_df$pitch_type <- as.factor(mod_df$pitch_type)
mod_df$count <- as.factor(mod_df$count)

library(lme4)

m0 <- glmer(is_swing ~ plate_x*plate_z + (1|count/pitch_type), 
            data = mod_df,
            family = binomial(),
            nAGQ=0,
            control=glmerControl(optimizer = "nloptwrap"))

library(mixedup)
m0_re <- extract_random_effects(m0)

# display

strikes <- 0:2
balls <- 0:3

y <- expand.grid(strikes, balls) %>%
  rename(strikes = Var1, balls = Var2) %>%
  select(balls, strikes)

y2 <- y %>%
  mutate(Count = paste0(balls, "-", strikes),
         CB = m0_re$value[1:12],
         CH = m0_re$value[13:24],
         FC = m0_re$value[25:36],
         FF = m0_re$value[37:48],
         SI = m0_re$value[49:60],
         SL = m0_re$value[61:72],
         ST = m0_re$value[73:84]) %>%
  select(-c("balls","strikes"))

library(gt)
library(gtExtras)

y2 %>%
  gt() %>%
  tab_header(title = "Which Pitch Types Get Swung at By Count",
             subtitle = "Change in Log Odds (positive means swung at more)")
