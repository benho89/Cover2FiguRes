library(tidyverse)
library(cfbfastR)
library(ggimage)
library(ggraph)
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
  select(school, conference, color, logo) %>%
  rename(team = school)

p5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")

qbs <- cfbd_team_roster(year = 2022) %>%
  mutate(full_name = paste(first_name, last_name)) %>%
  filter(position == "QB") %>%
  select(full_name, team)

# PFF subscription data.
pass_summary <- read.csv("passing_summary.csv", header = TRUE)

adot <- pass_summary %>%
  filter(attempts >= 200) %>%
  select(player, attempts, completions, avg_depth_of_target) %>%
  rename(adot = avg_depth_of_target) %>%
  mutate(origin = "QB") %>% # Origin of each arc in the plot.
  relocate(origin, .before = player) %>%
  left_join(qbs, by = c("player" = "full_name")) %>%
  distinct() %>% # Jack Plummer and Chandler Rogers were duplicated.
  left_join(team_info, by = "team") %>%
  filter(conference %in% p5) %>%
  mutate(player = sub(".*? ", "", player)) %>%
  arrange(-adot) %>%
  head(20) %>%
  arrange(adot) # Highest ADOT last.

adot_graph <- adot %>%
  tidygraph::as_tbl_graph() %>%
  tidygraph::activate(nodes) %>%
  tidygraph::activate(edges)

x <- adot$adot[1]
z <- nrow(adot)

# Name labels.
name_vert <- c(NA, adot$player)
adot_graph <- igraph::set_vertex_attr(adot_graph, "name", value = name_vert)

# Value labels.
adot_vert <- c(NA, paste(x, "ADOT"), adot$adot[2:z])
adot_graph <- igraph::set_vertex_attr(adot_graph, "adot", value = adot_vert)

# Team logos.
logo_vert <- c(NA, adot$logo)
adot_graph <- igraph::set_vertex_attr(adot_graph, "logo", value = logo_vert)


font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

base <- ggraph(adot_graph, "linear") +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF")) +
  labs(title = "CFB Average Depth of Target Leaders: Weeks 0-8",
       subtitle = "Power Five | Top 20 | 200+ attempts | Arc width: n attempts",
       caption = "Data: PFF | Plot: @cover2figuRes") +
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0.5)) +
  geom_edge_arc(aes(edge_width = attempts,
                    edge_colour = color),
                lineend = "round",
                alpha = 0.8,
                fontface = "bold",
                vjust = -1,
                show.legend = FALSE) +
  scale_edge_colour_identity() +
  geom_node_text(aes(label = adot),
                 size = 4.2,
                 nudge_y = -0.3,
                 family = "Fjalla One") +
  geom_image(aes(x = x, y = y - 0.9, image = logo),
             size = 0.035, asp = 16/9) +
  geom_node_text(aes(label = name),
                 size = 3.5,
                 nudge_y = -1.5,
                 family = "Fjalla One") +
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

magick::image_write(plot_with_ncaa, "Plots/CFB/CFB_QB_ADOT_P5.png")
