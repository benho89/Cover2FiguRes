library(tidyverse)
library(nflreadr)
library(nflplotR)
library(ggdist)
library(ggtext)
library(glue)
library(patchwork)
library(showtext)

###############################################################################

# Function inspired by Thomas Mock to add logos/images to plots.
# https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10) {
  
  if (!logo_position %in% c("top right", "top left", "bottom right",
                            "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top right', 'top left', 'bottom right', or 'bottom left'")
    
  }
  
  # Read in raw images.
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # Get dimensions of plot for scaling.
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # Default scale to 1/10th width of plot.
  # Can change with logo_scale.
  logo <- magick::image_scale(logo_raw, as.character(plot_width / logo_scale))
  
  # Get width of logo.
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo.
  # Position starts at top left (0, 0).
  # Using 0.01 for 1% - aesthetic padding.
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay.
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}

###############################################################################

### Prep ###

pbp <- load_pbp(seasons = 2022) %>%
  filter(!is.na(epa)) %>%
  filter(penalty == 0) %>%
  mutate(turnover = case_when(interception == 1 ~ 1,
                              fumble_lost == 1 ~ 1,
                              down == 4 & fourth_down_converted == 0 ~ 1)) %>%
  mutate(turnover = replace(turnover, is.na(turnover), 0))

franchise <- "KC"

team_info <- load_teams() %>%
  select(team_abbr, team_name, team_color, team_color2, team_logo_espn) %>%
  filter(team_abbr == franchise)

main <- team_info$team_color
alt <- team_info$team_color2
team_logo <- team_info$team_logo_espn
team_name <- team_info$team_name

###############################################################################

### Opponent Summary ###

opp_epa <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  group_by(posteam, week, home_team) %>%
  summarise(offense = round(mean(epa), 2)) %>%
  left_join(pbp %>%
              filter(rush == 1 | pass == 1) %>%
              group_by(defteam, week, away_team) %>%
              summarise(defense = round(mean(epa), 2)),
            by = c("week", "posteam" = "defteam"),
            all.x = TRUE) %>%
  ungroup() %>%
  mutate(opponent = ifelse(posteam == home_team, away_team, home_team)) %>%
  select(-c(home_team, away_team)) %>%
  relocate(opponent, .after = week) %>%
  rename(team = posteam) %>%
  arrange(team, week) %>%
  pivot_longer(cols = !c(1:3), names_to = "unit", values_to = "epa") %>%
  mutate(label = ifelse(epa >= 0, paste0("+", epa), epa),
         text_col = ifelse(unit == "offense" & epa >= 0, "darkgreen",
                           ifelse(unit == "defense" & epa < 0, "darkgreen",
                                  "darkred")))
                                  
range(opp_epa$epa) # -0.63, 0.63.

season <- data.frame(week = c(1:18)) %>%
  left_join(opp_epa %>%
              filter(team == franchise),
            by = "week")

font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

week_labels <- unique(season$week)

# Offense.
p1 <- ggplot(season %>%
               filter(unit == "offense" | is.na(unit)) %>%
               mutate(week = factor(week, levels = week_labels)),
             aes(x = week, y = epa, group = 1)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank(),
        axis.line = element_line()) +
  labs(title = "OFFENSE",
       subtitle = "EPA/Play vs. Opponent",
       x = "Week") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 24, hjust = 0.5, vjust = 5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10)) +
  scale_y_continuous(limits = c(-0.8, 0.8),
                     breaks = seq(-0.8, 0.8, 0.4)) +
  annotate("rect", fill = "red", alpha = 0.1,
           xmin = -Inf, xmax = Inf,
           ymin = -Inf, ymax = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(colour = main, size = 2) +
  geom_nfl_logos(aes(team_abbr = opponent),
                 width = 0.05) +
  geom_label(aes(x = week, y = ifelse(epa < 0, epa + 0.3, epa - 0.3),
                 label = label, colour = text_col),
             size = 4, family = "Fjalla One", show.legend = FALSE) +
  scale_colour_identity() +
  theme(plot.margin = unit(c(2, 1, 1, 1), "lines"))

# Defense.
p2 <- ggplot(season %>%
               filter(unit == "defense" | is.na(unit)) %>%
               mutate(week = factor(week, levels = week_labels)),
             aes(x = week, y = epa, group = 1)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank(),
        axis.line = element_line()) +
  labs(title = "DEFENSE",
       subtitle = "EPA/Play vs. Opponent",
       x = "Week") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 24, hjust = 0.5, vjust = 5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10)) +
  scale_y_reverse(limits = c(0.8, -0.8),
                  breaks = seq(-0.8, 0.8, 0.4)) +
  annotate("rect", fill = "red", alpha = 0.1,
           xmin = -Inf, xmax = Inf,
           ymin = Inf, ymax = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(colour = main, size = 2) +
  geom_nfl_logos(aes(team_abbr = opponent),
                 width = 0.05) +
  geom_label(aes(x = week, y = ifelse(epa < 0, epa + 0.3, epa - 0.3),
                 label = label, colour = text_col),
             size = 4, family = "Fjalla One", show.legend = FALSE) +
  scale_colour_identity() +
  theme(plot.margin = unit(c(2, 1, 1, 1), "lines"))


###############################################################################

### Density Plots ###

f1 <- function(x) { x * -1 }

epa_dat <- pbp %>%
  group_by(posteam) %>%
  summarise(off_pass_epa = mean(epa[pass == 1]),
            off_rush_epa = mean(epa[rush == 1])) %>%
  left_join(pbp %>%
              group_by(defteam) %>%
              summarise(def_pass_epa = mean(epa[pass == 1]),
                        def_rush_epa = mean(epa[rush == 1])),
            by = c("posteam" = "defteam"),
            all.x = TRUE) %>%
  ungroup() %>%
  mutate_at(c(4:5), f1) %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(team = posteam)

range(epa_dat$off_pass_epa) # -0.18, 0.28.
range(epa_dat$off_rush_epa) # -0.21, 0.08.
range(epa_dat$def_pass_epa) # -0.19, 0.12.
range(epa_dat$def_rush_epa) # -0.07, 0.20.

team_epa <- epa_dat %>%
  filter(team == franchise)

off_pass_epa_pct <- round(stats::ecdf(epa_dat$off_pass_epa)
                          (team_epa$off_pass_epa) * 100, 1)

off_rush_epa_pct <- round(stats::ecdf(epa_dat$off_rush_epa)
                          (team_epa$off_rush_epa) * 100, 1)

def_pass_epa_pct <- round(stats::ecdf(epa_dat$def_pass_epa)
                          (team_epa$def_pass_epa) * 100, 1)

def_rush_epa_pct <- round(stats::ecdf(epa_dat$def_rush_epa)
                          (team_epa$def_rush_epa) * 100, 1)

# Method for creating the density plot is based on the work by Bruno Mioto.
# https://twitter.com/BrunoHMioto)

# Custom theme to avoid repeating these steps multiple times.
my_theme <- list(
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank(),
        axis.line.x = element_line()),
  theme(text = element_text(family = "Fjalla One"),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()),
  scale_fill_manual(values = c("FALSE" = "grey",
                               "TRUE" = main)),
  coord_flip(expand = TRUE)
)

# Pass offense.
p3 <- ggplot(epa_dat,
             aes(y = off_pass_epa)) +
  labs(subtitle = "EPA/Pass Rank vs. NFL") +
  geom_richtext(aes(x = Inf, y = -Inf,
                    label = glue("EPA/Pass: {round(team_epa$off_pass_epa, 2)}<br><br>Better than {off_pass_epa_pct}%<br>of NFL teams")),
                fill = "#F0F8FF", label.colour = "#F0F8FF",
                size = 5, hjust = 0, vjust = 1, family = "Fjalla One") +
  stat_halfeye(aes(fill = stat(y < team_epa$off_pass_epa)),
               .width = 0,
               point_colour = NA,
               show.legend = FALSE) +
  geom_point(data = team_epa,
             aes(x = 0, y = off_pass_epa),
             fill = main, color = "black", shape = 24, size = 5) +
  scale_y_continuous(limits = c(-0.3, 0.3),
                     breaks = seq(-0.3, 0.3, 0.15)) +
  my_theme

# Rush offense.
p4 <- ggplot(epa_dat,
             aes(y = off_rush_epa)) +
  labs(subtitle = "EPA/Rush Rank vs. NFL") +
  geom_richtext(aes(x = Inf, y = -Inf,
                    label = glue("EPA/Rush: {round(team_epa$off_rush_epa, 2)}<br><br>Better than {off_rush_epa_pct}%<br>of NFL teams")),
                fill = "#F0F8FF", label.colour = "#F0F8FF",
                size = 5, hjust = 0, vjust = 1, family = "Fjalla One") +
  stat_halfeye(aes(fill = stat(y < team_epa$off_rush_epa)),
               .width = 0,
               point_colour = NA,
               show.legend = FALSE) +
  geom_point(data = team_epa,
             aes(x = 0, y = off_rush_epa),
             fill = main, color = "black", shape = 24, size = 5) +
  scale_y_continuous(limits = c(-0.3, 0.1),
                     breaks = c(-0.3, -0.2, -0.1, 0.0, 0.1),
                     labels = scales::number_format(accuracy = 0.01)) +
  my_theme

# Pass defense.
p5 <- ggplot(epa_dat,
             aes(y = def_pass_epa)) +
  labs(subtitle = "EPA/Pass Rank vs. NFL") +
  geom_richtext(aes(x = Inf, y = -Inf,
                    label = glue("EPA/Pass: {round(team_epa$def_pass_epa * -1, 2)}<br><br>Better than {def_pass_epa_pct}%<br>of NFL teams")),
                fill = "#F0F8FF", label.colour = "#F0F8FF",
                size = 5, hjust = 1, vjust = 1, family = "Fjalla One") +
  stat_halfeye(aes(fill = stat(y < team_epa$def_pass_epa)),
               .width = 0,
               point_colour = NA,
               show.legend = FALSE) +
  scale_y_reverse(limits = c(0.2, -0.2),
                  breaks = seq(-0.2, 0.2, 0.1),
                  labels = scales::number_format(accuracy = 0.01)) +
  geom_point(data = team_epa,
             aes(x = 0, y = def_pass_epa * -1),
             fill = main, color = "black", shape = 24, size = 5) +
  my_theme

# Rush defense.
p6 <- ggplot(epa_dat,
             aes(y = def_rush_epa)) +
  labs(subtitle = "EPA/Rush Rank vs. NFL") +
  geom_richtext(aes(x = Inf, y = -Inf,
                    label = glue("EPA/Rush: {round(team_epa$def_rush_epa * -1, 2)}<br><br>Better than {def_rush_epa_pct}%<br>of NFL teams")),
                fill = "#F0F8FF", label.colour = "#F0F8FF",
                size = 5, hjust = 1, vjust = 1, family = "Fjalla One") +
  stat_halfeye(aes(fill = stat(y < team_epa$def_rush_epa)),
               .width = 0,
               point_colour = NA,
               show.legend = FALSE) +
  scale_y_reverse(limits = c(0.2, -0.2),
                  breaks = seq(-0.2, 0.2, 0.1),
                  labels = scales::number_format(accuracy = 0.01)) +
  geom_point(data = team_epa,
             aes(x = 0, y = def_rush_epa * -1),
             fill = main, color = "black", shape = 24, size = 5) +
  my_theme

###############################################################################

### Polar Plots ###

f2 <- function(x) { rank(-x, ties.method = "min") } # Highest value is best (ranked #1).
f3 <- function(x) { rank(x, ties.method = "min") } # Lowest value is best (ranked #1).

polar_dat <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  group_by(posteam) %>%
  summarise(ypp = sum(yards_gained, na.rm = TRUE) / n(),
            success_rate = mean(success),
            third_success = mean(success[down == 3], na.rm = TRUE),
            
            pass_exp = sum(pass == 1 & yards_gained >= 15, na.rm = TRUE),
            rush_exp = sum(rush == 1 & yards_gained >= 10, na.rm = TRUE),
            exp_play = (pass_exp + rush_exp) / n(),
            
            to_rate = sum(turnover) / n()) %>%
  left_join(pbp %>%
              group_by(posteam, week) %>%
              summarise(pts = max(posteam_score_post)) %>%
              group_by(posteam) %>%
              summarise_at(vars(pts), mean),
            by = "posteam",
            all.x = TRUE) %>%
  left_join(pbp %>%
              filter(rush == 1 | pass == 1) %>%
              group_by(defteam) %>%
              summarise(def_ypp = sum(yards_gained, na.rm = TRUE) / n(),
                        def_success_rate = mean(success),
                        def_third_success = mean(success[down == 3],
                                                 na.rm = TRUE),
                        def_pass_exp = sum(pass == 1 & yards_gained >= 15,
                                           na.rm = TRUE),
                        def_rush_exp = sum(rush == 1 & yards_gained >= 10,
                                           na.rm = TRUE),
                        def_exp_play = (def_pass_exp + def_rush_exp) / n(),
                        
                        def_to_rate = sum(turnover) / n()),
            by = c("posteam" = "defteam"),
            all.x = TRUE) %>%
  left_join(pbp %>%
              group_by(defteam, week) %>%
              summarise(def_pts = max(posteam_score_post)) %>%
              group_by(defteam) %>%
              summarise_at(vars(def_pts), mean),
            by = c("posteam" = "defteam"),
            all.x = TRUE) %>%
  ungroup() %>%
  select(-c(5:6, 13:14)) %>%
  mutate_at(c(2:5, 7, 12), f2) %>%
  mutate_at(c(6, 8:11, 13), f3) %>%
  rename(team = posteam)

team_polar <- polar_dat %>%
  filter(team == franchise) %>%
  pivot_longer(cols = !team, names_to = "metric", values_to = "rank") %>%
  mutate(unit = ifelse(str_detect(metric, "def_"), "defense", "offense"),
         y_pos = 33 - rank,
         index = rep(c(1:6), times = 2))

x_labels <- c("Yards/Play", "Success Rate", "3rd Down Success", "Explosive Rate",
              "TO Rate", "Pts/Game")

temp <- (360 / 6) / 2
myAng <- seq(-temp, -360 + temp, length.out = 6)
ang <- ifelse(myAng < -90, myAng + 180, myAng)
ang <- ifelse(ang < -90, ang + 180, ang)

# Offense.
p7 <- ggplot(team_polar %>% filter(unit == "offense"),
             aes(x = reorder(metric, index), y = y_pos, label = rank)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank()) +
  labs(subtitle = "Other Offense Rankings (1-32)") +
  theme(text = element_text(family = "Fjalla One"),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = ang, face = "bold",
                                   colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(limits = c(-5, 32)) +
  geom_col(fill = main, colour = "white", width = 1) +
  coord_polar() +
  geom_col(aes(y = 32),
           fill = main, alpha = 0.3, width = 1) +
  geom_hline(yintercept = seq(0, 32, 32),
             size = 1, colour = "white") +
  geom_vline(xintercept = seq(0.5, 6, 1),
             size = 0.5, colour = "white") +
  geom_label(size = 5, family = "Fjalla One", fontface = "bold")

# Defense.
p8 <- ggplot(team_polar %>% filter(unit == "defense"),
             aes(x = reorder(metric, index), y = y_pos, label = rank)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank()) +
  labs(subtitle = "Other Defense Rankings (1-32)") +
  theme(text = element_text(family = "Fjalla One"),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 14, angle = ang, face = "bold",
                                   colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(limits = c(-5, 32)) +
  geom_col(fill = main, colour = "white", width = 1) +
  coord_polar() +
  geom_col(aes(y = 32),
           fill = main, alpha = 0.3, width = 1) +
  geom_hline(yintercept = seq(0, 32, 32),
             size = 1, colour = "white") +
  geom_vline(xintercept = seq(0.5, 6, 1),
             size = 0.5, colour = "white") +
  geom_label(size = 5, family = "Fjalla One", fontface = "bold")

###############################################################################

### Yardage Leaders ###

player_yards <- pbp %>%
  filter(rush == 1) %>%
  filter(posteam == franchise) %>%
  group_by(rusher_player_name, rusher_player_id, posteam) %>%
  summarise(rush_yds = sum(rushing_yards, na.rm = TRUE)) %>%
  rename(player = rusher_player_name,
         player_id = rusher_player_id,
         team = posteam) %>%
  full_join(pbp %>%
              filter(pass == 1) %>%
              filter(posteam == franchise) %>%
              group_by(receiver_player_name, receiver_player_id, posteam) %>%
              summarise(rec_yds = sum(receiving_yards, na.rm = TRUE)) %>%
              rename(player = receiver_player_name,
                     player_id = receiver_player_id,
                     team = posteam)) %>%
  left_join(pbp %>%
              filter(rush == 1 | pass == 1) %>%
              filter(posteam == franchise) %>%
              group_by(posteam) %>%
              summarise(team_yds = sum(yards_gained, na.rm = TRUE)) %>%
              rename(team = posteam),
            by = "team"
  ) %>%
  mutate_at(c(4:5), ~replace(., is.na(.), 0)) %>% # Zero yards.
  mutate(total_yds = rush_yds + rec_yds) %>%
  mutate(pct_yds = round(total_yds / team_yds * 100, 1)) %>%
  arrange(-pct_yds) %>%
  head(5) %>%
  mutate(player = gsub(".", ". ", player, fixed = TRUE)) %>%
  mutate(label = ifelse(pct_yds < 5, NA, player))

max_yds <- max(player_yards$pct_yds)
xlim <- plyr::round_any(max_yds, 10, f = ceiling) # Get even x-axis breaks.

p9 <- ggplot(player_yards,
             aes(x = pct_yds, y = reorder(player_id, pct_yds))) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(subtitle = "Top 5 Yardage Contributors (Rec & Rush; %)") +
  theme(text = element_text(family = "Fjalla One"),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_nfl_headshot(size = 1.5),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(limits = c(0, xlim),
                     breaks = seq(0, xlim, 10)) +
  geom_col(fill = main) +
  geom_text(aes(label = player),
            colour = "white", size = 4, family = "Fjalla One",
            position = position_stack(vjust = 0.5))

###############################################################################

### Defensive Acts ###

def_acts <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(defteam == franchise) %>%
  group_by(defteam) %>%
  summarise(turnover = sum(epa[turnover == 1]),
            int = sum(epa[interception == 1]),
            sack = sum(epa[sack == 1]),
            fumble = sum(epa[fumble_lost == 1])) %>%
  bind_rows(pbp %>%
              filter(rush == 1 | pass == 1) %>%
              group_by(defteam) %>%
              summarise(turnover = sum(epa[turnover == 1]),
                        int = sum(epa[interception == 1]),
                        sack = sum(epa[sack == 1]),
                        fumble = sum(epa[fumble_lost == 1])) %>%
              summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
              mutate(defteam = "Average")) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(team = defteam) %>%
  pivot_longer(cols = !team, names_to = "play_type", values_to = "epa") %>%
  mutate(team = factor(team, levels = c(franchise, "Average")),
         play_type = factor(play_type, levels = c("turnover", "int", "sack",
                                                  "fumble"),
                            labels = c("Turnover", "INT", "Sack", "Fumble Rec")))

p10 <- ggplot(def_acts,
              aes(x = play_type, y = epa, fill = team)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(subtitle = "EPA By Defensive Act",
       x = "Grey bar: NFL average",
       y = "Total EPA") +
  theme(text = element_text(family = "Fjalla One"),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.text.y = element_text(size = 10),
        axis.ticks.x = element_blank()) +
  scale_y_reverse() +
  geom_col(position = "dodge", show.legend = FALSE) +
  scale_fill_manual(values = c(main, "grey")) +
  geom_hline(yintercept = 0)

###############################################################################

### Combine Plots ###

# t = top; l = left; b = bottom; r = right.
layout <- c(
  area(t = 1, l = 1, b = 2, r = 4), #1.
  area(1, 5, 2, 8), #2.
  
  area(3, 1, 4, 2), #3.
  area(3, 3, 4, 4), #4.
  
  area(3, 5, 4, 6), #5.
  area(3, 7, 4, 8), #6.
  
  area(5, 1, 7, 2), #7.
  area(5, 5, 7, 6), #8.
  
  area(5, 3, 7, 4), #9.
  area(5, 7, 7, 8) #10.
)

plot(layout) # View the layout.

base <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 +
  plot_layout(design = layout) &
  plot_annotation(title = "2022 Season Summary",
                  subtitle = glue("{team_name}"),
                  caption = "Data: nflfastR | Plot: @cover2figuRes",
                  theme = theme(text = element_text(family = "Fjalla One"),
                                plot.title = element_text(size = 30, hjust = 0.5,
                                                          vjust = 2),
                                plot.subtitle = element_text(size = 26,
                                                             hjust = 0.5,
                                                             vjust = 2),
                                plot.caption = element_text(size = 18,
                                                            hjust = 0.5))) &
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF")) &
  theme(plot.margin = unit(c(2, 1, 1, 1), "lines"))

ragg::agg_png("Base.png", width = 22, height = 15,
              units = "in", res = 300)
base
dev.off()

base_img <- magick::image_read("Base.png")

base_raster <- grid::rasterGrob(base_img, width = unit(1, "npc"),
                                height = unit(1, "npc"))

plot_with_personal <- add_logo(
  plot_path = "Base.png",
  logo_path = "Cover2.png",
  logo_position = "bottom left",
  logo_scale = 18
)

magick::image_write(plot_with_personal, "Personal.png")

plot_with_logo <- add_logo(
  plot_path = "Personal.png",
  logo_path = team_logo,
  logo_position = "top right",
  logo_scale = 10
)

magick::image_write(plot_with_logo, "Logo.png")

plot_with_ncaa <- add_logo(
  plot_path = "Logo.png",
  logo_path = "NFL.png",
  logo_position = "top left",
  logo_scale = 25
)

magick::image_write(plot_with_ncaa, "Plots/NFL/NFL_Team_Season_Summary.png")
