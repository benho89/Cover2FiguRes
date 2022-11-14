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

team_stats <- cfbd_stats_season_advanced(year = 2022) %>%
  select(team, conference, off_success_rate)

top_success <- team_stats %>%
  filter(conference %in% p5) %>%
  arrange(-off_success_rate) %>%
  head(30)

pbp <- load_cfb_pbp(seasons = 2022)

pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(is.na(penalty_detail)) %>%
  filter(!is.na(down))

f <- function(x) { round(x * 100, 1) }

x_levels <- c("first", "second", "third", "fourth")
x_labels <- c("1st Down", "2nd Down", "3rd Down", "4th Down")

df <- pbp_rp %>%
  filter(pos_team %in% team_info$team) %>%
  group_by(pos_team) %>%
  summarise(first_pass = sum(success[down == 1 & pass == 1]) / sum(down == 1),
            first_rush = sum(success[down == 1 & rush == 1]) / sum(down == 1),
            
            second_pass = sum(success[down == 2 & pass == 1]) / sum(down == 2),
            second_rush = sum(success[down == 2 & rush == 1]) / sum(down == 2),
            
            third_pass = sum(success[down == 3 & pass == 1]) / sum(down == 3),
            third_rush = sum(success[down == 3 & rush == 1]) / sum(down == 3),
            
            fourth_pass = sum(success[down == 4 & pass == 1]) / sum(down == 4),
            fourth_rush = sum(success[down == 4 & rush == 1]) /
              sum(down == 4)) %>%
  mutate_at(vars(2:9), f) %>%
  rename(team = pos_team) %>%
  pivot_longer(cols = !team,
               names_to = c("down", "play_type"),
               names_sep = "_",
               values_to = "success") %>%
  left_join(team_info, by = "team") %>%
  mutate(down = factor(down, levels = x_levels, labels = x_labels),
         alpha_type = ifelse(play_type == "pass", 1, 0.5),
         label = ifelse(success >= 15, paste0(success, "%"), NA)) %>%
  group_by(team) %>%
  arrange(down, .by_group = TRUE)

p5_df <- df %>%
  filter(team %in% top_success$team) %>%
  mutate(team = factor(team, levels = top_success$team))

font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(p5_df,
               aes(x = down, y = success)) +
  facet_wrap(~team, ncol = 5, scales = "free_x") +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(1.5, "lines")) +
  labs(title = "CFB Success Rate Per Down & Play Type",
       subtitle = "Weeks 0-11 | Top 30 Power Five teams for OFF success rate | Dark colours: pass success | Transparent colours: rush success",
       caption = "Data: @CFB_Data with cfbfastR | Plot: @cover2figuRes",
       x = "Down",
       y = "Success Rate (%)") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0.5, vjust = -10),
        axis.title = element_text(size = 14),
        strip.text = element_cfb_logo(size = 0.8)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 25)) +
  geom_col(aes(fill = color, alpha = alpha_type),
           show.legend = FALSE) +
  scale_alpha_identity() +
  scale_fill_identity() +
  geom_text(aes(label = label),
            colour = "white", size = 2, family = "Fjalla One",
            position = position_stack(vjust = 0.5)) +
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

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_OFF_Success_P5.png")
