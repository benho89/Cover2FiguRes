library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(showtext)
library(tidytext)

###############################################################################

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10) {
  
  if (!logo_position %in% c("top right", "top left", "bottom right",
                            "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
    
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

team_info <- cfbd_team_info(only_fbs = TRUE) %>%
  select(school, color) %>%
  rename(team = school)

p5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")
g5 <- c("American Athletic", "Conference USA", "FBS Independents",
        "Mid-American", "Mountain West", "Sun Belt")

team_yards <- cfbd_stats_season_team(year = 2022) %>%
  select(team, conference, total_yds) %>%
  rename(team_yds = total_yds)

# Top P5 offenses by yards.
p5_top <- team_yards %>%
  arrange(-team_yds) %>%
  filter(conference %in% p5) %>%
  head(30)

g5_top <- team_yards %>%
  arrange(-team_yds) %>%
  filter(conference %in% g5) %>%
  head(30)

rush_stats <- cfbd_stats_season_player(year = 2022, category = "rushing") %>%
  select(player, athlete_id, team, rushing_yds)

rec_stats <- cfbd_stats_season_player(year = 2022, category = "receiving") %>%
  select(player, athlete_id, team, receiving_yds)

player_yards <- left_join(rec_stats, rush_stats,
                          by = c("player", "athlete_id", "team")) %>%
  mutate_at(vars(4:5), ~replace(., is.na(.), 0)) %>% # Zero yards.
  mutate(total_yds = rushing_yds + receiving_yds) %>%
  left_join(team_yards, by = "team") %>%
  relocate(conference, .after = team) %>%
  mutate(pct_yds = round(total_yds / team_yds * 100, 1))

df <- player_yards %>%
  group_by(team) %>%
  arrange(-pct_yds, .by_group = TRUE) %>%
  slice_head(n = 3) %>% # Each team's top three yardage leaders.
  left_join(team_info, by = "team") %>%
  mutate(color = ifelse(team == "Toledo", "#15397f", color)) %>%
  mutate(initial = substr(player, 1, 1),
         surname = sub(".*? ", "", player),
         label = paste(initial, surname)) %>% # Bar chart labels.
  select(-c(initial, surname)) %>%
  mutate(text_col = ifelse(color == "#ffffff", "black", "white")) %>%
  mutate(player = reorder_within(player, pct_yds, team)) # Orders bars highest to lowest in individual facets.

max(df$pct_yds)


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(df %>% filter(team %in% p5_top$team) %>%
                 mutate(team = factor(team, levels = p5_top$team)),
       aes(x = pct_yds, y = player)) +
  facet_wrap(~team, ncol = 5, scales = "free_y") +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(1.5, "lines")) +
  labs(title = "Which Players Are Contributing the Most Yards For Their Team?",
       subtitle = "Power Five | Non-QBs | Top 30 teams for total yards | Top 3 contributors per team",
       caption = "Data: cfbfastR & CollegeFootballData.com | Plot: @cover2figuRes",
       x = "Percent Yardage (%)") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        plot.caption = element_text(size = 10, hjust = 0.5),
        strip.text = element_cfb_logo(size = 0.8)) +
  scale_x_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, 10)) +
  scale_y_reordered() +
  geom_col(aes(fill = color), alpha = 0.8, show.legend = FALSE) +
  scale_fill_identity() +
  geom_text(aes(label = label),
            colour = "white", size = 2.3, family = "Fjalla One",
            position = position_stack(vjust = 0.5), show.legend = FALSE) +
  scale_colour_identity() +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 14, height = 11,
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

plot_with_ncaa <- add_logo(
  plot_path = "Personal.png",
  logo_path = "NCAA.png",
  logo_position = "top left",
  logo_scale = 15
)

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_Player_Yards_P5.png")


base <- ggplot(df %>% filter(team %in% g5_top$team) %>%
                 mutate(team = factor(team, levels = g5_top$team)),
               aes(x = pct_yds, y = player)) +
  facet_wrap(~team, ncol = 5, scales = "free_y") +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(1.5, "lines")) +
  labs(title = "Which Players Are Contributing the Most Yards For Their Team?",
       subtitle = "Group of Five + Independents | Non-QBs | Top 30 teams for total yards | Top 3 contributors per team",
       caption = "Data: cfbfastR & CollegeFootballData.com | Plot: @cover2figuRes",
       x = "Percent Yardage (%)") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        plot.caption = element_text(size = 10, hjust = 0.5),
        strip.text = element_cfb_logo(size = 0.8)) +
  scale_x_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, 10)) +
  scale_y_reordered() +
  geom_col(aes(fill = color), alpha = 0.8, show.legend = FALSE) +
  scale_fill_identity() +
  geom_text(aes(label = label),
            colour = "white", size = 2.3, family = "Fjalla One",
            position = position_stack(vjust = 0.5), show.legend = FALSE) +
  scale_colour_identity() +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 14, height = 11,
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

plot_with_ncaa <- add_logo(
  plot_path = "Personal.png",
  logo_path = "NCAA.png",
  logo_position = "top left",
  logo_scale = 15
)

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_Player_Yards_G5.png")
