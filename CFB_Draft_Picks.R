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

team_info <- cfbd_team_info() %>%
  select(school, color) %>%
  rename(team = school)

year <- c(1970:2022) # Draft years to loop through.
picks <- data.frame()

for (i in year) {
  
  x <- cfbd_draft_picks(year = i)
  picks <- rbind(picks, x)
  
}

unique(picks$position) # See position labels.

# Presetting desired position levels and labels for plot.

pos_levels <- c("Defensive Back", "Defensive End", "Defensive Tackle",
                "Linebacker", "Offensive Lineman", "Quarterback",
                "Running Back", "Tight End", "Wide Receiver")

pos_labels <- c("DB", "DE", "DT", "LB", "OL", "QB", "RB", "TE", "WR")

df <- picks %>%
  filter(round == 1) %>%
  filter(!position %in% c("Fullback", "Kick Returner", "Long Snapper",
                          "Place Kicker", "Punter")) %>%
  mutate(position = case_when(position %in% c("Center", "Offensive Guard",
                                              "Offensive Tackle") ~
                                "Offensive Lineman",
                              position %in% c("Inside Linebacker",
                                              "Outside Linebacker") ~
                                "Linebacker",
                              position %in% c("Cornerback", "Safety") ~
                                "Defensive Back",
                              TRUE ~ as.character(position))) %>%
  group_by(college_team, position) %>%
  summarise(n = n()) %>%
  group_by(position) %>%
  arrange(-n, .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  rename(team = college_team) %>%
  left_join(team_info, by = "team") %>%
  mutate(position = factor(position, levels = pos_levels,
                           labels = pos_labels))

max(df$n) # 20.

# Lines 113-119 are used to create gaps between positional groups on the plot.
# https://r-graph-gallery.com/297-circular-barplot-with-groups.html

empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar * nlevels(df$position), ncol(df)) )
colnames(to_add) <- colnames(df)
to_add$position <- rep(levels(df$position), each = empty_bar)
df <- rbind(df, to_add)
df <- df %>% arrange(position)
df$id <- seq(1, nrow(df))

# Lines 125-130 creates another data frame that's used for adding segments
# (essentially imitating axis lines) and position labels underneath each group.
# https://r-graph-gallery.com/297-circular-barplot-with-groups.html

z <- df %>%
  group_by(position) %>%
  summarize(start = min(id),
            end = max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end)))


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggplot(df,
               aes(x = as.factor(id), y = n, fill = reorder(color, -n))) +
  
  # Manual grid lines.
  geom_hline(aes(yintercept = y),
             data.frame(y = c(0, 5, 10, 15, 20)), colour = "white") +
  
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank()) +
  labs(title = "Number of First Round Draftees By Position Group",
       subtitle = "1970-2022",
       caption = "Data: @CFB_Data with cfbfastR | Plot: @cover2figuRes") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = -16.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, vjust = -22),
        plot.caption = element_text(size = 8, hjust = 0.5, vjust = 35),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_y_continuous(limits = c(-10, 25)) + # Negative value makes inner circle larger.
  geom_col(position = position_dodge(width = 0.9), show.legend = FALSE) +
  scale_fill_identity() +
  geom_text(aes(y = n - 1.5, label = n),
            colour = "white", size = 2, family = "Fjalla One",
            position = position_dodge(width = 0.9)) +
  geom_cfb_logos(aes(y = n + 3, team = team),
                 width = 0.03, position = position_dodge(width = 0.9)) +
  coord_polar() +
  
  # Manual y-axis labels.
  geom_text(aes(x = 44.5, y = 6), label = "5", size = 2.5,
            family = "Fjalla One") +
  geom_text(aes(x = 44.5, y = 11), label = "10", size = 2.5,
            family = "Fjalla One") +
  geom_text(aes(x = 44.5, y = 16), label = "15", size = 2.5,
            family = "Fjalla One") +
  geom_text(aes(x = 44.5, y = 21), label = "20", size = 2.5,
            family = "Fjalla One") +
  
  # Segments replicating axis lines for each group.
  geom_segment(data = z,
               aes(x = start - 0.5, xend = end + 0.5, y = -0.5, yend = -0.5),
               inherit.aes = FALSE)  +
  
  # Position labels underneath each group.
  geom_text(data = z,
            aes(x = title, y = -3, label = position),
            size = 5, family = "Fjalla One", inherit.aes = FALSE) +
  
  # Reducing margin so plot appears larger (note vjust of titles/subtitles/captions
  # is adjusted above).
  theme(plot.margin = unit(c(-2, -5, -2, -5), "cm"))

ragg::agg_png("Base.png", width = 6.5, height = 6.8,
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

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_Draft_Picks.png")
