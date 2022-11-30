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
  select(school, conference, color, alt_color) %>%
  rename(team = school)

# Only looking at Power Five (+ Notre Dame).
p5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")

pbp <- load_cfb_pbp(seasons = 2022)

off <- pbp %>%
  filter(period <= 4) %>%
  select(pos_team, game_id, week, period, def_pos_team, pos_score_diff) %>%
  rename(team = pos_team,
         opponent = def_pos_team,
         pt_diff = pos_score_diff) %>%
  group_by(game_id, week, period) %>%
  slice_tail(n = 1) %>%
  ungroup()

# slice_tail() extracts the final row of data from each quarter.

def <- pbp %>%
  filter(period <= 4) %>%
  select(def_pos_team, game_id, week, period, pos_team, pos_score_diff) %>%
  rename(team = def_pos_team,
         opponent = pos_team,
         pt_diff = pos_score_diff) %>%
  mutate(pt_diff = pt_diff * -1) %>% # Flip the value for defense.
  group_by(game_id, week, period) %>%
  slice_tail(n = 1) %>%
  ungroup()

df <- rbind(off, def) %>%
  arrange(team, week, game_id, period) %>%
  select(-game_id) %>%
  group_by(team, week) %>%
  mutate(pt_diff = pt_diff - lag(pt_diff, default = 0)) %>%
  group_by(team, period) %>%
  summarise(pt_diff = sum(pt_diff)) %>% # Calculate total for season.
  ungroup() %>%
  mutate(period = factor(period, labels = c("Q1", "Q2", "Q3", "Q4")),
         text_label = ifelse(pt_diff > 0, paste0("+", pt_diff),
                             as.character(pt_diff)),
         label_pos = ifelse(pt_diff >= 0, pt_diff + 20, pt_diff - 20)) %>%
  filter(team %in% team_info$team) %>% # Only FBS teams.
  left_join(team_info, by = "team")

# Lines 93 and 94 obtain point differential by quarter.
# default = 0 in lag() maintains the value for first quarter point differential.

p5_df <- df %>% # Power Five (+ Notre Dame) only.
  filter(conference %in% p5 | team == "Notre Dame") %>%
  group_by(period) %>%
  mutate(rank = paste0("#", rank(desc(pt_diff))), # Ranks teams for each quarter.
         rank = gsub(".5", "", rank, fixed = TRUE), # Remove .5 for rank ties.
         rank_pos = ifelse(pt_diff >= 0, -40, 40)) %>%
  ungroup()

range(p5_df$pt_diff) # -101 and 130.


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# Alabama to NC State.
base <- ggplot(p5_df %>% head(128), # First 32 teams (32 * 4 quarters).
               aes(x = period, y = pt_diff)) +
  facet_wrap(~team, ncol = 8) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(1.5, "lines")) +
  labs(title = "Cumulative Point Differential By Quarter",
       subtitle = "Power Five (+ ND) | Through Week 13 | #: rank within Power Five",
       caption = "Data: @CFB_Data with cfbfastR | Plot: @cover2figuRes",
       x = "Quarter",
       y = "Cumulative Point Differential") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5, vjust = -10),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 10),
        strip.text = element_cfb_logo(size = 1)) +
  scale_y_continuous(limits = c(-150, 150),
                     breaks = seq(-150, 150, 75)) +
  geom_col(aes(fill = color, colour = alt_color),
           show.legend = FALSE) +
  scale_fill_identity() +
  scale_colour_identity() +
  geom_hline(yintercept = 0) +
  new_scale_colour() + # New scale to use each team's primary colour for text.
  coord_cartesian(ylim = c(-150, 150), clip = "off") +
  geom_text(aes(x = period, y = label_pos, label = text_label, colour = color),
            size = 5, family = "Fjalla One", show.legend = FALSE) +
  scale_colour_identity() +
  geom_point(aes(x = period, y = rank_pos),
             colour = "#444444", size = 9) +
  geom_text(aes(x = period, y = rank_pos, label = rank),
            colour = "white", size = 3, family = "Fjalla One") +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 22, height = 11, units = "in", res = 300)
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

plot_with_ncaa <- add_logo(
  plot_path = "Personal.png",
  logo_path = "NCAA.png",
  logo_position = "top left",
  logo_scale = 20
)

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_Pt_Diff_P5_V1.png")


# Nebraska to Wisconsin.
base <- ggplot(p5_df %>% tail(132), # Last 33 teams (33 * 4 quarters).
               aes(x = period, y = pt_diff)) +
  facet_wrap(~team, ncol = 9) + # Extra column to account for Notre Dame.
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(1.5, "lines")) +
  labs(title = "Cumulative Point Differential By Quarter",
       subtitle = "Power Five (+ ND) | Through Week 13 | #: rank within Power Five",
       caption = "Data: @CFB_Data with cfbfastR | Plot: @cover2figuRes",
       x = "Quarter",
       y = "Cumulative Point Differential") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        plot.caption = element_text(size = 14, hjust = 0.5, vjust = -10),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 10),
        strip.text = element_cfb_logo(size = 1)) +
  scale_y_continuous(limits = c(-150, 150),
                     breaks = seq(-150, 150, 75)) +
  geom_col(aes(fill = color, colour = alt_color),
           show.legend = FALSE) +
  scale_fill_identity() +
  scale_colour_identity() +
  geom_hline(yintercept = 0) +
  new_scale_colour() +
  coord_cartesian(ylim = c(-150, 150), clip = "off") +
  geom_text(aes(x = period, y = label_pos, label = text_label, colour = color),
            size = 5, family = "Fjalla One", show.legend = FALSE) +
  scale_colour_identity() +
  geom_point(aes(x = period, y = rank_pos),
             colour = "#444444", size = 9) +
  geom_text(aes(x = period, y = rank_pos, label = rank),
            colour = "white", size = 3, family = "Fjalla One") +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 23, height = 12, units = "in", res = 300)
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

plot_with_ncaa <- add_logo(
  plot_path = "Personal.png",
  logo_path = "NCAA.png",
  logo_position = "top left",
  logo_scale = 20
)

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_Pt_Diff_P5_V2.png")
