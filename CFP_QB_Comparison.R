library(tidyverse)
library(cfbfastR)
library(geomtextpath)
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

team_info <- cfbd_team_info(only_fbs = TRUE) %>%
  select(school, color) %>%
  rename(team = school)

cfp_qbs <- c("Stetson Bennett", "J.J. McCarthy", "C.J. Stroud", "Max Duggan")

pbp <- load_cfb_pbp(seasons = 2022)

# PFF subscription data.
pff_pass <- read.csv("season_passing_summary.csv", header = TRUE) %>%
  select(player, completion_percent, ypa, btt_rate, twp_rate, grades_offense)

qbs <- cfbd_team_roster(year = 2022) %>% # Using this to filter only QBs in PBP data.
  filter(position == "QB") %>%
  mutate(full_name = paste(first_name, last_name)) %>%
  select(full_name, team, position) %>%
  rename(player_position = position)

pbp_rp <- pbp %>%
  filter(home %in% team_info$team | away %in% team_info$team) %>% # Only FBS teams.
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(EPA)) %>%
  filter(is.na(penalty_detail)) %>%
  mutate(player_name = ifelse(!is.na(rusher_player_name), rusher_player_name,
                              ifelse(!is.na(passer_player_name), passer_player_name,
                                     ifelse(!is.na(fumble_player_name),
                                            fumble_player_name, NA)))) %>%
  left_join(qbs, by = c("player_name" = "full_name", "pos_team" = "team"),
            all.x = TRUE)

f1 <- function(x) { round(x * 100) }
f2 <- function(x) { 100 - x }

df <- pbp_rp %>%
  filter(home %in% team_info$team & away %in% team_info$team) %>% # FBS vs. FBS.
  filter(player_position == "QB") %>% # Only QBs analysed.
  group_by(player_name, pos_team) %>%
  summarise(n = n(),
            pass_epa = mean(EPA[pass == 1]),
            rush_epa = mean(EPA[rush == 1])) %>%
  ungroup() %>%
  filter(n >= 100) %>% # Minimum of 100 plays.
  left_join(pff_pass, by = c("player_name" = "player")) %>%
  mutate_at(c(4:10), percent_rank) %>%
  mutate_at(c(4:10), f1) %>%
  mutate_at(vars(twp_rate), f2) %>% # Subtract from 100 as lower is better.
  rename(team = pos_team)

plot_df <- df %>%
  filter(player_name %in% cfp_qbs) %>%
  pivot_longer(cols = !c(1:3), names_to = "metric", values_to = "pct_rank") %>%
  mutate(player_name = factor(player_name, levels = cfp_qbs),
         index = rep(c(1:7), times = 4)) %>%
  left_join(team_info, by = "team")

x_labels <- c("EPA/Pass", "EPA/Rush", "Comp %", "Yards/Attempt",
              "BTT Rate", "TWP Rate", "OFF Grade")


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(plot_df,
             aes(x = reorder(metric, index), y = pct_rank, label = pct_rank)) +
  facet_wrap(~player_name, ncol = 2) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines")) +
  labs(title = "How Do the CFP QBs Stack Up?",
       subtitle = "Label: Percent rank versus FBS QBs",
       caption = "Data: @CFB_Data with cfbfastR & PFF | Plot: @cover2figuRes | Polar: @DSamangy") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5, vjust = 5),
        plot.subtitle = element_text(size = 16, hjust = 0.5, vjust = 5),
        plot.caption = element_text(size = 10, hjust = 0.5, vjust = -10),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 10, vjust = -0.1, face = "bold",
                                   colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12, margin = margin(t = 12),
                                  vjust = 5)) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(limits = c(-10, 100)) +
  geom_col(aes(fill = color),
           colour = "white", width = 1, show.legend = FALSE) +
  coord_curvedpolar(clip = "off") +
  geom_col(aes(y = 100, fill = color),
           alpha = 0.3, width = 1, show.legend = FALSE) +
  scale_fill_identity() +
  geom_hline(yintercept = seq(0, 100, 100),
             size = 1, colour = "white") +
  geom_vline(xintercept = seq(0.5, 6, 1),
             size = 0.5, colour = "white") +
  geom_label(size = 4, family = "Fjalla One", fontface = "bold") +
  theme(plot.margin = unit(c(2, 1, 3, 1), "lines"))

ragg::agg_png("Base.png", width = 6.5, height = 8.7,
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
  logo_scale = 10
)

magick::image_write(plot_with_personal, "Personal.png")

plot_with_ncaa <- add_logo(
  plot_path = "Personal.png",
  logo_path = "NCAA.png",
  logo_position = "top left",
  logo_scale = 10
)

magick::image_write(plot_with_ncaa, "Plots/CFB/CFP_QB_Ranks.png")
