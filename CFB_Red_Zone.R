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

team_info <- cfbd_team_info(only_fbs = TRUE) %>%
  select(school, conference, color) %>%
  rename(team = school)

p5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")
g5 <- c("American Athletic", "Conference USA", "FBS Independents",
        "Mid-American", "Mountain West", "Sun Belt")

pbp <- read.csv("cfb_pbp.csv", header = TRUE)

df <- pbp %>%
  select(pos_team, game_id, drive_number, drive_result, play_number, down,
         yards_to_goal)

f <- function(x) { round(x * 100, 1) }

red_zone <- df %>%
  filter(down == 1 & yards_to_goal <= 20) %>%
  group_by(pos_team, game_id, drive_number) %>%
  slice_min(play_number, n = 1) %>%
  mutate(td = ifelse(drive_result == "TD", 1, 0)) %>%
  group_by(pos_team) %>%
  summarise(n = n(),
            td_rate = sum(td) / n()) %>%
  mutate_at(vars(td_rate), f) %>%
  mutate(label = as.character(td_rate)) %>%
  rename(team = pos_team) %>%
  filter(team %in% team_info$team) %>%
  left_join(team_info, by = "team") %>%
  mutate(color = case_when(team == "Charlotte" ~ "#046a38",
                           team == "Toledo" ~ "#15397f",
                           TRUE ~ as.character(color))) %>%
  arrange(-td_rate)

p5_top <- red_zone %>% filter(conference %in% p5) %>% head(25)
g5_top <- red_zone %>% filter(conference %in% g5) %>% head(25)


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(p5_top, aes(x = reorder(team, -td_rate), y = td_rate,
                           label = label)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Converting Red Zone Trips to Touchdowns: Weeks 0-8",
       subtitle = "Season 2022 | Power Five | Top 25 | At least one first down in red zone",
       caption = "Data: cfbfastR & CollegeFootballData.com | Plot: @cover2figuRes",
       y = "Touchdown Conversion %") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(size = 10, hjust = 0.5)) +
  scale_x_cfb(size = 25) +
  theme_x_cfb() +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20)) +
  geom_segment(aes(x = reorder(team, -td_rate), xend = reorder(team, -td_rate),
                   y = 0, yend = td_rate,
                   colour = color), size = 3, show.legend = FALSE) +
  geom_point(aes(colour = color), size = 10, show.legend = FALSE) +
  geom_text(colour = "white", size = 3, family = "Fjalla One") +
  scale_colour_identity() +
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

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_Red_Zone_P5_Top.png")


base <- ggplot(g5_top, aes(x = reorder(team, -td_rate), y = td_rate,
                           label = label)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Converting Red Zone Trips to Touchdowns: Weeks 0-8",
       subtitle = "Season 2022 | Group of Five + Independents | Top 25 | At least one first down in red zone",
       caption = "Data: cfbfastR & CollegeFootballData.com | Plot: @cover2figuRes",
       y = "Touchdown Conversion %") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(size = 10, hjust = 0.5)) +
  scale_x_cfb(size = 25) +
  theme_x_cfb() +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20)) +
  geom_segment(aes(x = reorder(team, -td_rate), xend = reorder(team, -td_rate),
                   y = 0, yend = td_rate,
                   colour = color), size = 3, show.legend = FALSE) +
  geom_point(aes(colour = color), size = 10, show.legend = FALSE) +
  geom_text(colour = "white", size = 3, family = "Fjalla One") +
  scale_colour_identity() +
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

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_Red_Zone_G5_Top.png")
