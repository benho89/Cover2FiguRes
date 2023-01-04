library(tidyverse)
library(cfbfastR)
library(cfbplotR)
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

heisman <- c("Stetson Bennett", "Max Duggan", "C.J. Stroud", "Caleb Williams")

pbp <- load_cfb_pbp(seasons = 2022)

heisman_ids <- cfbd_team_roster(year = 2022) %>%
  filter(position == "QB") %>%
  mutate(full_name = paste(first_name, last_name)) %>%
  select(full_name, athlete_id) %>%
  filter(full_name %in% heisman)

pbp_rp <- pbp %>%
  filter(home %in% team_info$team | away %in% team_info$team) %>% # Only FBS teams.
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(EPA)) %>%
  filter(is.na(penalty_detail)) %>%
  mutate(player_name = ifelse(!is.na(rusher_player_name), rusher_player_name,
                              ifelse(!is.na(passer_player_name), passer_player_name,
                                     ifelse(!is.na(fumble_player_name),
                                            fumble_player_name, NA))))

df <- pbp_rp %>%
  filter(player_name %in% heisman) %>%
  group_by(player_name, pos_team) %>%
  summarise(total = sum(EPA),
            pass = sum(EPA[pass == 1 & int == 0 & sack == 0]),
            rush = sum(EPA[rush == 1]),
            int = sum(EPA[int == 1]),
            sack = sum(EPA[sack == 1])) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(team = pos_team) %>%
  pivot_longer(cols = !c(1:2), names_to = "play_type", values_to = "epa") %>%
  left_join(heisman_ids, by = c("player_name" = "full_name")) %>%
  relocate(athlete_id, .after = player_name) %>%
  left_join(team_info, by = "team") %>%
  mutate(player_name = factor(player_name, levels = heisman),
         play_type = factor(play_type,
                            levels = c("total", "pass", "rush", "int", "sack"),
                            labels = c("Overall", "Pass\n(excl. INT & Sack)",
                                       "Rush/Scramble", "INT", "Sack")))

colours <- df %>% # Subset to order colour of individual bars correctly.
  group_by(team) %>%
  slice(n = 1) %>%
  arrange(player_name)


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(df,
               aes(x = play_type, y = epa, fill = player_name,
                   group = player_name)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "How Have the Heisman Finalists Produced Their Efficiency?",
       subtitle = "Season 2022",
       caption = "Data: @CFB_Data with cfbfastR | Plot: @cover2figuRes | Inspired by @KevinCole__",
       y = "Total Expected Points Added") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0.5, vjust = -10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, face = "bold", colour = "black"),
        axis.ticks.x = element_blank()) +
  geom_col(position = "dodge", show.legend = FALSE) +
  scale_fill_manual(values = colours$color) +
  geom_hline(yintercept = 0) +
  geom_cfb_headshots(aes(y = ifelse(epa > 0, -15, 15),
                         player_id = athlete_id),
                     height = 0.06, position = position_dodge(width = 0.9)) +
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

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_Heisman_Efficiency.png")
