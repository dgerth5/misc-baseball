library(tidyverse)
library(readr)

statcast <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")

ff <- statcast %>%
  filter(pitch_type == "FF") %>%
  mutate(adj_x = if_else(p_throws == "R", pfx_x*12, pfx_x*-1*12),
         adj_relx = if_else(p_throws == "R", release_pos_x, release_pos_x*-1),
         adj_spin_axis = if_else(p_throws == "R", spin_axis, 360 - spin_axis),
         adj_z = pfx_z*12) %>%
  select(adj_x, adj_z, adj_relx, release_pos_z, adj_spin_axis, release_extension, release_spin_rate, release_speed, player_name, home_team) %>%
  mutate_at(c("player_name","home_team"), factor)

sapply(ff, class)

library(mgcv)

mod_hmov <- bam(adj_x ~ s(adj_relx, release_pos_z, k = 100) + adj_spin_axis + release_extension + release_spin_rate + release_speed + 
                  s(player_name, bs = "re") + s(home_team, bs = "re"),
                discrete = TRUE,
                nthreads = 4,
                data = ff)

mod_vmov <- bam(adj_z ~ s(adj_relx, release_pos_z, k = 100) + adj_spin_axis + release_extension + release_spin_rate + release_speed + 
                  s(player_name, bs = "re") + s(home_team, bs = "re"),
                discrete = TRUE,
                nthreads = 4,
                data = ff)

library(mixedup)

home_h <- extract_random_effects(mod_hmov) %>%
  filter(group_var == "home_team") %>%
  mutate(adj_val = value*-1) %>%
  select(group, adj_val) %>%
  rename(Park = group, Stadium_Effect = adj_val)

home_v <- extract_random_effects(mod_vmov) %>%
  filter(group_var == "home_team") %>%
  mutate(adj_val = value) %>%
  select(group, adj_val) %>%
  rename(Park = group, Stadium_Effect = adj_val)

library(ggplot2)
library(scales)

p1 <- ggplot(home_h, aes(x = Park, y = Stadium_Effect, fill = Stadium_Effect)) +
  geom_bar(stat="identity") + 
  scale_fill_gradient2(low = muted("red"), high = muted("green")) +
  ggtitle("Park Effects on Horizontal Break", subtitle = "Season: 2022") +
  ylim(-1.75,1.75) + ylab("Stadium Effect (In.)")

p2 <- ggplot(home_v, aes(x = Park, y = Stadium_Effect,  fill = Stadium_Effect)) +
  geom_bar(stat="identity") + 
  scale_fill_gradient2(low = muted("red"), high = muted("green")) +
  ggtitle("Park Effects on Vertical Break", subtitle = "Season: 2022") +
  ylim(-3.1,3.1) + ylab("Stadium Effect (In.)")

library(gridExtra)

grid.arrange(p1, p2, nrow = 2)
