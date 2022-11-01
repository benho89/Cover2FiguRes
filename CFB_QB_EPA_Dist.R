library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(ggridges)
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

team_info <- cfbd_team_info(only_fbs = TRUE) %>%
  select(school, color) %>%
  rename(team = school)

prospects <- c("C.J. Stroud", "Bryce Young", "Hendon Hooker", "Will Levis",
               "Anthony Richardson", "Jaren Hall", "Cameron Ward",
               "Bo Nix", "Jordan Travis", "Dorian Thompson-Robinson",
               
               "Sam Hartman", "Grayson McCall", "Tanner McKee",
               "Michael Penix Jr.", "Tyler Van Dyke")

pbp <- load_cfb_pbp(seasons = 2022)

pbp_r <- pbp %>%
  filter(rush == 1) %>%
  filter(position_rush == "QB") %>%
  mutate(player_name = rush_player)

pbp_p <- pbp %>%
  filter(pass == 1) %>%
  filter(position_completion == "QB" | position_incompletion == "QB" |
           position_interception_thrown == "QB" |
           position_sack_taken == "QB" | position_fumble == "QB") %>%
  
  # Completions.
  mutate(player_name = ifelse(!is.na(completion_player), completion_player,
                              ifelse(!is.na(incompletion_player),
                                     incompletion_player, NA))) %>%
  
  # Interceptions.
  mutate(player_name = ifelse(!is.na(interception_thrown_player),
                              interception_thrown_player, player_name)) %>%
  
  # Sacks (including fumbles).
  mutate(player_name = ifelse(!is.na(sack_taken_player),
                              sack_taken_player, player_name))

pbp_rp <- rbind(pbp_r, pbp_p) %>%
  arrange(week, game_id, game_play_number) %>%
  filter(!is.na(EPA)) %>%
  filter(is.na(penalty_detail))

df <- pbp_rp %>%
  filter(player_name %in% prospects) %>%
  select(player_name, pos_team, EPA) %>%
  rename(team = pos_team) %>%
  left_join(team_info, by = "team")

levels <- df %>%
  group_by(player_name, team) %>%
  summarise(n = n(),
            epa_m = mean(EPA),
            epa_sd = sd(EPA)) %>%
  arrange(-epa_m) %>%
  head(15) %>%
  arrange(-epa_sd)

df <- df %>%
  mutate(player_name = factor(player_name, levels = levels$player_name)) %>%
  arrange(player_name)

min(df$EPA)
max(df$EPA)


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(df,
               aes(x = EPA, y = player_name, fill = color)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "EPA Distribution of QB Draft Prospects",
       subtitle = "Weeks 0-9 | Vertical white line: mean EPA/play | Ordered by consistency | \n Top: most consistent based on standard deviation | Passes and rushes",
       caption = "Data: cfbfastR & CollegeFootballData.com | Plot: @cover2figuRes",
       x = "EPA per Play") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0.5)) +
  scale_x_continuous(limits = c(min(df$EPA), 10),
                     breaks = seq(-10, 10, 5)) +
  geom_density_ridges(colour = "white", show.legend = FALSE,
                      quantile_lines = TRUE, # Mean lines.
                      quantile_fun = function(x,...) mean(x)) +
  scale_fill_identity() +
  geom_cfb_logos(data = df %>% group_by(player_name, team) %>%
                   slice(n = 1), # So it doesn't plot hundreds of logos.
                 aes(x = min(df$EPA), y = as.numeric(player_name) + 0.5,
                     team = team),
                 width = 0.035) +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 11, height = 12,
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
  logo_scale = 12
)

magick::image_write(plot_with_personal, "Personal.png")

plot_with_ncaa <- add_logo(
  plot_path = "Personal.png",
  logo_path = "NCAA.png",
  logo_position = "top left",
  logo_scale = 10
)

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_QB_EPA_Dist_P5.png")
