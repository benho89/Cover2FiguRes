library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(showtext)

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

team_info <- cfbd_team_info(year = 2022, only_fbs = TRUE) %>%
  select(school, conference, color) %>%
  rename(team = school)

p5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")
g5 <- c("American Athletic", "Conference USA", "FBS Independents",
        "Mid-American", "Mountain West", "Sun Belt")

pbp <- load_cfb_pbp(seasons = 2022)

pbp_r <- pbp %>%
  filter(rush == 1) %>%
  filter(position_rush == "QB") %>%
  mutate(passer_name = rush_player)

pbp_p <- pbp %>%
  filter(pass == 1) %>%
  filter(position_completion == "QB" | position_incompletion == "QB" |
           position_interception_thrown == "QB" |
           position_sack_taken == "QB" | position_fumble == "QB") %>%
  
  # Completions.
  mutate(passer_name = ifelse(!is.na(completion_player), completion_player,
                              ifelse(!is.na(incompletion_player),
                                     incompletion_player, NA))) %>%
  
  # Interceptions.
  mutate(passer_name = ifelse(!is.na(interception_thrown_player),
                              interception_thrown_player, passer_name)) %>%
  
  # Sacks (including fumbles).
  mutate(passer_name = ifelse(!is.na(sack_taken_player),
                              sack_taken_player, passer_name))

pbp_rp <- rbind(pbp_r, pbp_p) %>%
  arrange(week, game_id, game_play_number) %>%
  filter(!is.na(EPA)) %>%
  filter(is.na(penalty_detail))

df <- pbp_rp %>%
  select(passer_name, pos_team, EPA) %>%
  group_by(passer_name, pos_team) %>%
  mutate(play_n = row_number(),
         cum_epa = cumsum(EPA)) %>%
  ungroup() %>%
  rename(team = pos_team) %>%
  arrange(passer_name, play_n) %>%
  left_join(team_info, by = "team")

top_p5 <- df %>%
  filter(conference %in% p5) %>%
  group_by(passer_name, team) %>%
  summarise(n = n(),
            cum_epa = last(cum_epa)) %>%
  arrange(-n) %>%
  head(30) %>%
  arrange(-cum_epa)

p5_levels <- top_p5$passer_name # Order facets by highest cumulative EPA.

p5_df <- df %>%
  filter(passer_name %in% top_p5$passer_name) %>%
  mutate(passer_name2 = passer_name) %>% # Plot ALL lines in each facet.
  mutate(passer_name = factor(passer_name, levels = p5_levels))

max(p5_df$play_n)


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(p5_df,
               aes(x = play_n, y = cum_epa, group = passer_name)) +
  facet_wrap(~passer_name, ncol = 5, scales = "free_x") +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(1.5, "lines")) +
  labs(title = "QB Cumulative EPA: Weeks 0-8",
       subtitle = "Power Five | 240+ plays | Passes and rushes | INTs, sacks and fumbles included",
       caption = "Data: cfbfastR & CollegeFootballData.com | Plot: @cover2figuRes",
       x = "Play Number",
       y = "Cumulative EPA") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        plot.caption = element_text(size = 10, hjust = 0.5),
        strip.text = element_text(size = 12, hjust = 0.15)) +
  scale_x_continuous(limits = c(0, 405),
                     breaks = seq(0, 405, 81)) +
  geom_line(data = p5_df %>% select(c(2:8)),
            aes(x = play_n, y = cum_epa, group = passer_name2),
            colour = "grey", size = 0.3) +
  geom_line(aes(colour = color), size = 1, show.legend = FALSE) +
  scale_colour_identity() +
  geom_cfb_logos(data = p5_df %>%
                   group_by(passer_name) %>% slice_head(n = 1),
                 aes(x = 40, y = 140, team = team), width = 0.1) +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 13, height = 13,
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

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_QB_Cum_EPA_P5.png")
