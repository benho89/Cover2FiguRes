library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(ggforce)
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
  select(school, conference, color) %>%
  rename(team = school)

p5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")
g5 <- c("American Athletic", "Conference USA", "FBS Independents",
        "Mid-American", "Mountain West", "Sun Belt")

pbp <- load_cfb_pbp(seasons = 2022)

pbp_p <- pbp %>%
  filter(pass == 1) %>%
  filter(!is.na(EPA)) %>%
  filter(is.na(penalty_detail)) %>%
  filter(pos_team %in% team_info$team) %>%
  mutate(passer = ifelse(!is.na(completion_player), completion_player,
                              ifelse(!is.na(incompletion_player),
                                     incompletion_player, NA)))

df <- pbp_p %>%
  filter(!is.na(passer)) %>%
  group_by(passer, receiver_player_name, pos_team) %>%
  summarise(comp = sum(completion == 1),
            epa = sum(EPA)) %>%
  ungroup() %>%
  rename(receiver = receiver_player_name,
         team = pos_team) %>%
  left_join(team_info, by = "team") %>%
  arrange(-comp) %>%
  filter(comp >= 35) %>%
  arrange(-epa) %>%
  mutate(text_label = paste(paste(passer, receiver, sep = " to "),
                                       paste0("(", comp, ")")))

p5_df <- df %>%
  filter(conference %in% p5) %>%
  head(15) %>%
  arrange(epa) %>%
  mutate(y_label = row_number())

g5_df <- df %>%
  filter(conference %in% g5) %>%
  head(15) %>%
  arrange(epa) %>%
  mutate(y_label = row_number())

p5_df$y_label <- factor(p5_df$y_label)
g5_df$y_label <- factor(g5_df$y_label)


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(p5_df,
               aes(x = 0, xend = epa,
                   y = y_label, yend = y_label)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Top Quarterback/Wide Receiver Connections in CFB",
       subtitle = "Season 2022 | Power Five | Top 15 | 35+ connections (brackets)",
       caption = "Data: cfbfastR & CollegeFootballData.com | Plot: @cover2figuRes",
       x = "Total EPA") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 70),
                     breaks = seq(0, 70, 10)) +
  geom_link(aes(colour = color, alpha = stat(index)),
            size = 9, n = 500, show.legend = FALSE) +
  scale_colour_identity() +
  geom_vline(xintercept = 0) +
  geom_cfb_logos(aes(x = epa, y = y_label, team = team), width = 0.04) +
  annotate("text", x = p5_df$epa - 2, y = p5_df$y_label,
           label = p5_df$text_label, colour = "white", size = 3.8,
           family = "Fjalla One", hjust = 1) +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 14, height = 8,
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

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_QB_WR_Pairs_P5.png")


base <- ggplot(g5_df,
               aes(x = 0, xend = epa,
                   y = y_label, yend = y_label)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Top Quarterback/Wide Receiver Connections in CFB",
       subtitle = "Season 2022 | Group of Five + Independents | Top 15 | 35+ connections (brackets)",
       caption = "Data: cfbfastR & CollegeFootballData.com | Plot: @cover2figuRes",
       x = "Total EPA") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 70),
                     breaks = seq(0, 70, 10)) +
  geom_link(aes(colour = color, alpha = stat(index)),
            size = 9, n = 500, show.legend = FALSE) +
  scale_colour_identity() +
  geom_vline(xintercept = 0) +
  geom_cfb_logos(aes(x = epa, y = y_label, team = team), width = 0.04) +
  annotate("text", x = g5_df$epa - 2, y = g5_df$y_label,
           label = g5_df$text_label, colour = "white", size = 3.8,
           family = "Fjalla One", hjust = 1) +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 14, height = 8,
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

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_QB_WR_Pairs_G5.png")
