library(tidyverse)
library(nflreadr)
library(nflplotR)
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

team_info <- load_teams() %>%
  select(team_abbr, team_conf, team_color) %>%
  rename(team = team_abbr)

qbs <- load_players() %>%
  filter(status == "ACT") %>%
  filter(position == "QB")

pbp <- load_pbp(seasons = 2022)

pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(epa)) %>%
  filter(penalty == 0) %>%
  filter(qb_spike == 0 | qb_kneel == 0) %>%
  mutate(player_name = ifelse(!is.na(passer_player_name), passer_player_name,
                              ifelse(!is.na(rusher_player_name),
                                     rusher_player_name,
                                     ifelse(!is.na(fumbled_1_player_name),
                                            fumbled_1_player_name, NA))))

df <- pbp_rp %>%
  select(player_name, posteam, epa) %>%
  filter(player_name %in% qbs$short_name) %>%
  group_by(player_name, posteam) %>%
  mutate(play_n = row_number(),
         cum_epa = cumsum(epa)) %>%
  ungroup() %>%
  rename(team = posteam) %>%
  arrange(player_name, play_n) %>%
  left_join(team_info, by = "team") %>%
  mutate(player_name = gsub(".", ". ", player_name, fixed = TRUE),
         player_name2 = player_name) # To plot individual lines in all facets.

top <- df %>%
  group_by(player_name, team) %>%
  summarise(n = n(),
            cum_epa = last(cum_epa)) %>%
  arrange(-n) %>%
  filter(n >= 150) %>%
  arrange(-cum_epa) %>%
  head(32)

top_levels <- top$player_name

top_df <- df %>%
  filter(player_name %in% top$player_name) %>%
  mutate(player_name = factor(player_name, levels = top_levels))


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(top_df,
               aes(x = play_n, y = cum_epa, group = player_name)) +
  facet_wrap(~player_name, ncol = 8, scales = "free") +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_line(),
        panel.spacing.x = unit(1.5, "lines")) +
  labs(title = "QB Cumulative EPA: Weeks 1-12",
       subtitle = "Top 32 | 150+ plays | Passes and rushes | INTs, sacks and fumbles included",
       caption = "Data: nflfastR | Plot: @cover2figuRes",
       x = "Play Number",
       y = "Cumulative EPA") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5, vjust = -10),
        axis.title = element_text(size = 18),
        axis.title.x = element_text(vjust = -2),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 12, hjust = 0.15)) +
  scale_x_continuous(limits = c(0, max(top_df$play_n)),
                     breaks = seq(0, 500, 100)) +
  ### Red fill ###
  annotate("rect", fill = "red", alpha = 0.1,
           xmin = -Inf, xmax = Inf,
           ymin = -Inf, ymax = 0) +
  ### All lines ###
  geom_line(data = top_df %>% select(c(2:8)), # All lines.
            aes(x = play_n, y = cum_epa, group = player_name2),
            colour = "grey", size = 0.3) +
  ### Individual player lines ###
  geom_line(aes(colour = team_color), size = 1.5, show.legend = FALSE) +
  ### EPA Labels ###
  geom_point(data = top_df %>%
               group_by(player_name) %>% slice_tail(n = 1),
             aes(x = play_n, y = cum_epa, colour = team_color),
             shape = 21, size = 10, stroke = 1.8, fill = "white",
             show.legend = FALSE) +
  scale_colour_identity() +
  geom_text(data = top_df %>%
              group_by(player_name) %>% slice_tail(n = 1) %>%
              mutate_at(vars(cum_epa), round),
            aes(x = play_n, y = cum_epa, label = cum_epa),
            size = 4, family = "Fjalla One") +
  geom_nfl_logos(data = top_df %>%
                   group_by(player_name) %>% slice_head(n = 1),
                 aes(x = 80, y = 150, team_abbr = team), width = 0.2) +
  coord_cartesian(ylim = c(min(top_df$cum_epa), max(top_df$cum_epa)),
                  clip = "off") +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 24, height = 12, units = "in", res = 300)
base
dev.off()

base_img <- magick::image_read("Base.png")

base_raster <- grid::rasterGrob(base_img, width = unit(1, "npc"),
                                height = unit(1, "npc"))

plot_with_personal <- add_logo(
  plot_path = "Base.png",
  logo_path = "Cover2.png",
  logo_position = "bottom left",
  logo_scale = 28
)

magick::image_write(plot_with_personal, "Personal.png")

plot_with_nfl <- add_logo(
  plot_path = "Personal.png",
  logo_path = "NFL.png",
  logo_position = "top left",
  logo_scale = 35
)

magick::image_write(plot_with_nfl, "Plots/NFL/NFL_QB_Cum_EPA.png")
