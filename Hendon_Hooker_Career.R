library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(ggnewscale)
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
    x_pos = plot_width - logo_width - 0.08 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.85 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay.
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}

###############################################################################

team_info <- cfbd_team_info(year = 2022, only_fbs = TRUE) %>%
  filter(school == "Tennessee")

player_info <- cfbd_team_roster(year = 2022, team = "Tennessee") %>%
  filter(first_name == "Hendon")

tenn_col <- team_info$color
vt_col <- "#630031" # Virginia Tech.
tenn_logo <- team_info$logo
headshot <- player_info$headshot_url

pbp <- load_cfb_pbp(seasons = 2019:2022)

# Postseason games as they are absent from load_cfb_pbp().
x <- cfbd_pbp_data(year = 2019, season_type = "postseason", epa_wpa = TRUE,
                   team = "Virginia Tech")
y <- cfbd_pbp_data(year = 2021, season_type = "postseason", epa_wpa = TRUE,
                   team = "Tennessee")

post_pbp <- rbind(x, y)

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
  arrange(game_id, week, game_play_number) %>%
  filter(!is.na(EPA)) %>%
  filter(is.na(penalty_detail))

post_rp <- post_pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(EPA)) %>%
  filter(is.na(penalty_detail)) %>%
  mutate(passer_name = ifelse(!is.na(passer_player_name), passer_player_name,
                              ifelse(!is.na(rusher_player_name),
                                     rusher_player_name, NA)),
         wk = 15)

df1 <- pbp_rp %>%
  select(passer_name, pos_team, season, week, def_pos_team, EPA) %>%
  filter(passer_name == "Hendon Hooker") %>%
  group_by(pos_team, season, week, def_pos_team) %>%
  summarise(epa = round(mean(EPA), 2)) %>%
  ungroup()

df2 <- post_rp %>%
  select(passer_name, pos_team, season, wk, def_pos_team, EPA) %>%
  filter(passer_name == "Hendon Hooker") %>%
  group_by(pos_team, season, wk, def_pos_team) %>%
  summarise(epa = round(mean(EPA), 2)) %>%
  ungroup() %>%
  rename(week = wk)

hooker <- rbind(df1, df2) %>%
  arrange(season, week) %>%
  mutate(text_col = ifelse(epa >= -1.7 & epa <= 0.3, "black", "white"))

hooker_tenn <- hooker %>% filter(pos_team == "Tennessee")
hooker_vt <- hooker %>% filter(pos_team == "Virginia Tech")


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(mapping = aes(x = week, y = season)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank()) +
  labs(title = "Hendon Hooker's College Career",
       subtitle = "2019-2022 | 2018 redshirt year excluded",
       caption = "Data: @CFB_Data with cfbfastR | Plot: @cover2figuRes | Inspired by @BrunoHMioto",
       x = "Week",
       y = "Season") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        plot.caption = element_text(size = 10, hjust = 0.5, vjust = -10)) +
  scale_x_continuous(breaks = seq(1, 15, 1),
                     labels = c(1:14, "Post")) +
  geom_tile(data = hooker_vt,
            aes(fill = epa),
            colour = "white", size = 0.5, show.legend = FALSE) +
  coord_equal() +
  scale_fill_gradientn(colours = c("grey", "white", vt_col),
                       limits = c(-1, 1)) +
  new_scale_fill() +
  geom_tile(data = hooker_tenn,
            aes(fill = epa),
            colour = "white", size = 0.5) +
  scale_fill_gradientn("EPA per Play",
                       colours = c("grey", "white", tenn_col),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, 0.5)) +
  theme(legend.background = element_rect(fill = "#F0F8FF"),
        legend.position = "top",
        legend.key.width = unit(2, "cm")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5)) +
  geom_cfb_logos(data = hooker,
                 aes(x = week, y = season + 0.2, team = def_pos_team),
                 width = 0.03) +
  geom_text(data = hooker,
            aes(x = week, y = season - 0.2, label = epa, colour = text_col),
            size = 4.5, family = "Fjalla One", show.legend = FALSE) +
  scale_colour_identity() +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 11.7, height = 6,
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
  logo_scale = 20
)

magick::image_write(plot_with_personal, "Personal.png")

plot_with_logo <- add_logo(
  plot_path = "Personal.png",
  logo_path = tenn_logo,
  logo_position = "top right",
  logo_scale = 10
)

magick::image_write(plot_with_logo, "Logo.png")

plot_with_headshot <- add_logo(
  plot_path = "Logo.png",
  logo_path = headshot,
  logo_position = "bottom right",
  logo_scale = 8
)

magick::image_write(plot_with_headshot, "Headshot.png")

plot_with_ncaa <- add_logo(
  plot_path = "Headshot.png",
  logo_path = "NCAA.png",
  logo_position = "top left",
  logo_scale = 15
)

magick::image_write(plot_with_ncaa, "Plots/CFB/Hendon_Hooker_Career.png")  
