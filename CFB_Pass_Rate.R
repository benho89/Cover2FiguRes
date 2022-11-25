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

p5_teams <- team_info %>% filter(conference %in% p5 | team == "Notre Dame")

top_p5 <- p5_teams %>% slice_head(n = 32) # These teams in separate plots.
bot_p5 <- p5_teams %>% slice_tail(n = 33)

pbp <- load_cfb_pbp(seasons = 2022)

# Get Week 0.
schedule <- load_cfb_schedules(seasons = 2022) %>%
  mutate(week = ifelse(start_date < "2022-09-01", 0, week)) %>%
  select(game_id, week) %>%
  rename(actual_week = week)

pbp_fbs <- pbp %>% # Only FBS teams.
  filter(home %in% team_info$team | away %in% team_info$team) %>%
  left_join(schedule, by = "game_id") %>%
  mutate(week = actual_week)

pbp_rp <- pbp_fbs %>%
  filter(rush == 1 | pass == 1) %>%
  filter(week > 0)

df <- pbp_rp %>%
  group_by(pos_team, week, def_pos_team) %>%
  summarise(n_plays = n(),
            pass_plays = sum(pass),
            pass_rate = round(pass_plays / n_plays * 100, 1)) %>%
  ungroup() %>%
  rename(team = pos_team,
         opponent = def_pos_team) %>%
  filter(team %in% team_info$team) %>%
  left_join(team_info, by = "team")

p5_df <- df %>%
  filter(conference %in% p5 | team == "Notre Dame")

range(p5_df$pass_rate) # 17 and 86 (use for y-axis limits).


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(p5_df %>% filter(team %in% top_p5$team),
               aes(x = week, y = pass_rate, group = team)) +
  facet_wrap(~team, ncol = 8, scales = "free") +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.line.x = element_line(),
        panel.spacing.x = unit(1.5, "lines")) +
  labs(title = "CFB Team Pass/Rush Rate By Opponent",
       subtitle = "Weeks 1-12 | Power Five (ALA - NCST) | Shaded area: higher rush rate | Missing values: bye week or PBP data NA",
       caption = "Data: @CFB_Data with cfbfastR | Plot: @cover2figuRes",
       x = "Week",
       y = "Pass Rate (%)") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5, vjust = -10),
        axis.title = element_text(size = 18),
        strip.text = element_cfb_logo(size = 1)) +
  scale_x_continuous(limits = c(1, 12),
                     breaks = seq(1, 12, 1)) +
  scale_y_continuous(limits = c(10, 90),
                     breaks = seq(10, 90, 20)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 50,
           fill = "grey", alpha = 0.2) +
  geom_line(aes(colour = color),
            size = 2, show.legend = FALSE) +
  geom_point(aes(colour = color),
             fill = "white", shape = 21, size = 7, show.legend = FALSE) +
  scale_colour_identity() +
  geom_cfb_logos(aes(team = opponent), width = 0.045) +
  coord_cartesian(ylim = c(10, 90), clip = "off") +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 24, height = 12,
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
  logo_scale = 25
)

magick::image_write(plot_with_personal, "Personal.png")

plot_with_ncaa <- add_logo(
  plot_path = "Personal.png",
  logo_path = "NCAA.png",
  logo_position = "top left",
  logo_scale = 18
)

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_Pass_Rate_P5_V1.png")
