library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(ggdist)
library(ggtext)
library(glue)
library(gt)
library(gtExtras)
library(patchwork)
library(showtext)

###############################################################################

# Function inspired by Thomas Mock to add logos/images to plots.
# https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10) {
  
  if (!logo_position %in% c("top right", "top left", "top left 2",
                            "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top right', 'top left', 'top left 2', 'bottom right', or 'bottom left'")
    
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
    x_pos = plot_width - logo_width - 0.08 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left 2") {
    x_pos = 0.01 * plot_width
    y_pos = 0.13 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.85 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay.
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}

###############################################################################

# Prep.

week <- 13

pbp <- read.csv("cfb_pbp.csv", header = TRUE) # Prior weeks' data.

pbp_new <- cfbd_pbp_data(year = 2022, week = week, epa_wpa = TRUE) # Current.

pbp <- rbind(pbp, pbp_new)

passer <- "Michael Penix Jr."
team <- "Washington"

fbs <- cfbd_team_info(only_fbs = TRUE)

team_info <- fbs %>%
  select(school, color, alt_color, logo) %>%
  filter(school == team)

qbs <- cfbd_team_roster(year = 2022) %>%
  filter(position == "QB") %>%
  mutate(full_name = paste(first_name, last_name))

player_info <- qbs %>%
  filter(full_name == passer)

main <- team_info$color
alt <- team_info$alt_color
team_logo <- team_info$logo
headshot <- player_info$headshot_url

# PFF subscription data.
pff_pass <- read.csv("passing_summary.csv", header = TRUE) # Pass stats.
pff_depth <- read.csv("passing_depth.csv", header = TRUE) # Pass depth.

pbp_rp <- pbp %>%
  filter(home %in% fbs$school | away %in% fbs$school) %>% # Only FBS teams.
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(EPA)) %>%
  filter(is.na(penalty_detail)) %>%
  mutate(player_name = ifelse(!is.na(passer_player_name), passer_player_name,
                              ifelse(!is.na(rusher_player_name),
                                     rusher_player_name,
                                     ifelse(!is.na(fumble_player_name),
                                            fumble_player_name, NA)))) %>%
  filter(player_name %in% qbs$full_name) %>%
  mutate(fumble_vec = case_when(fumble_player_name != player_name ~ 0,
                                TRUE ~ as.numeric(fumble_vec)))

###############################################################################

### Box Score Table ###

player_pass <- pff_pass %>%
  filter(player == passer) %>%
  select(player, qb_rating, grades_offense, completion_percent, yards, ypa)

box <- pbp_rp %>%
  filter(player_name == passer & wk == week) %>%
  group_by(player_name, def_pos_team) %>%
  summarise(tot_epa = round(sum(EPA), 2),
            pass_epa = round(mean(EPA[pass == 1]), 2),
            epa_lost = round(sum(EPA[int == 1 | sack == 1 |
                                       fumble_vec == 1]), 2)) %>%
  ungroup() %>%
  left_join(player_pass, by = c("player_name" = "player")) %>%
  select(-player_name) %>%
  gt() %>%
  gt_fmt_cfb_logo(columns = def_pos_team) %>%
  cols_label(def_pos_team = "Opponent",
             tot_epa = html("Total EPA<br>(Pass/Rush)"),
             pass_epa = html("EPA/<br>Pass"),
             epa_lost = html("EPA Lost<br>(Scks/Fum/INTs)"),
             qb_rating = "Rating",
             grades_offense = html("PFF OFF<br>Grade"),
             completion_percent = "Comp %",
             yards = html("Total<br>Yards"),
             ypa = html("Yards/<br>Attempt")) %>%
  cols_align(columns = everything(), align = "center") %>%
  cols_width(c(2:9) ~ px(80)) %>%
  tab_header(title = "QB Box Score") %>%
  tab_style(style = list(cell_fill(color = "#070F6C"),
                         cell_text(color = "white")),
            locations = cells_title()) %>%
  tab_style(style = cell_fill(color = "#F0F8FF"),
            locations = list(cells_column_labels(),
                             cells_body(rows = everything(),
                                        columns = everything()))) %>%
  tab_style(style = cell_text(v_align = "middle"),
            locations = cells_column_labels()) %>%
  tab_options(table.border.top.color = "black",
              table.border.top.width = 3,
              heading.border.bottom.color = "black",
              heading.border.bottom.width = 3,
              table_body.border.top.color = "black",
              table_body.border.top.width = 3,
              table_body.border.bottom.color = "black",
              table_body.border.bottom.width = 3,
              table.margin.left = px(0)) %>%
  gt_highlight_rows(columns = c(2:9), fill = "#F0F8FF") %>%
  opt_table_font(font = google_font("Fjalla One"))

gtsave(box, filename = "Box.png", expand = 0)

# expand = 0 in gtsave() removes white border around table.

###############################################################################

### Field and Pass Zones ###

pass_zones <- pff_depth %>%
  select(player, ends_with("attempts"), ends_with("accuracy_percent")) %>%
  select(player, starts_with(c("left", "center", "right"))) %>%
  filter(player == passer)

att <- pass_zones %>%
  select(player, ends_with("attempts"))

names(att) <- gsub("_attempts", "", names(att))

att_long <- att %>%
  pivot_longer(cols = !player, names_to = "area", values_to = "att") %>%
  na_if(0) # Replace O with NA so those zones aren't plotted.

acc <- pass_zones %>%
  select(player, ends_with("accuracy_percent"))

names(acc) <- gsub("_accuracy_percent", "", names(acc))

acc_long <- acc %>%
  pivot_longer(cols = !player, names_to = "area", values_to = "acc")

increment <- 17.8 # Used to plot zones within field that has a width of 53.33.

plot_zones <- left_join(att_long, acc_long, by = c("player", "area")) %>%
  mutate(x_start = case_when(grepl("beh", area) ~ 30,
                             grepl("sho", area) ~ 30,
                             grepl("med", area) ~ 40,
                             grepl("dee", area) ~ 50),
         x_end = case_when(grepl("beh", area) ~ 22,
                           grepl("sho", area) ~ 40,
                           grepl("med", area) ~ 50,
                           grepl("dee", area) ~ 80),
         y_start = case_when(grepl("lef", area) ~ 0,
                             grepl("cen", area) ~ increment,
                             grepl("rig", area) ~ increment * 2),
         y_end = case_when(grepl("lef", area) ~ increment,
                           grepl("cen", area) ~ increment * 2,
                           grepl("rig", area) ~ increment * 3)) %>%
  mutate(colour = main,
         alt_colour = alt) %>%
  mutate(zone_alpha = case_when(att <= 2 ~ 0.1,
                                att >= 3 & att <= 4 ~ 0.2,
                                att >= 5 & att <= 6 ~ 0.4,
                                att >= 7 & att <= 8 ~ 0.6,
                                att >= 9 & att <= 10 ~ 0.8,
                                att > 10 ~ 1)) %>%
  mutate(label = paste(att, paste0(acc, "%"), sep = "\n")) %>%
  filter(!is.na(att))

font_add_google("Fjalla One", "Fjalla One")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

source("gg_field.R") # This retrieves the function for the field from my directory. See https://github.com/mlfurman3/gg_field for more info.

p1 <- ggplot(plot_zones) +
  gg_field(direction = "vert",
           yardmin = 10,
           yardmax = 80,
           buffer = 0,
           line_color = "black",
           field_color = "white",
           endzone_color = "white") +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF")) +
  labs(subtitle = "Pass Zones (via PFF)",
       y = "LoS: 20 yard line | Labels: attempts + accuracy %") +
  theme(text = element_text(family = "Fjalla One"),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 10, hjust = 0.5)) +
  geom_rect(aes(xmin = x_start, xmax = x_end,
                ymin = y_start, ymax = y_end,
                fill = colour, colour = alt_colour, alpha = zone_alpha),
            size = 1, show.legend = FALSE) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_alpha_identity() +
  geom_label(aes(x = (x_start + x_end) / 2, y = (y_start + y_end) / 2,
                 label = label),
             size = 3, family = "Fjalla One") +
  geom_vline(aes(xintercept = 30), size = 1.5) # LoS line.

###############################################################################

### Cumulative EPA (line graph) ###

cum_epa <- pbp_rp %>%
  select(player_name, pos_team, wk, EPA, touchdown, int, fumble_vec,
         fumble_player_name) %>%
  filter(!is.na(player_name)) %>%
  filter(player_name != "incomplete") %>%
  select(-fumble_player_name) %>%
  group_by(player_name, pos_team, wk) %>%
  filter(n() >= 20) %>% # Minimum 20 plays (passes and rushes).
  mutate(play_number = row_number(),
         EPA = cumsum(EPA)) %>%
  ungroup() %>%
  arrange(player_name, wk) %>%
  mutate(id = paste(player_name, wk)) # To plot individual lines.

player_cum <- cum_epa %>%
  filter(player_name == passer & wk == week) %>%
  mutate(txt_label = case_when(int == 1 ~ "INT",
                               fumble_vec == 1 ~ "F",
                               touchdown == 1 ~ "TD"),
         label_col = case_when(int == 1 | fumble_vec == 1 ~ "darkred",
                               touchdown == 1 ~ "darkgreen")) %>%
  mutate(xpos = case_when(int == 1 | fumble_vec == 1 | touchdown == 1 ~
                            play_number - 0.1),
         ystart = case_when(int == 1 | fumble_vec == 1 | touchdown == 1 ~ EPA),
         yend = case_when(int == 1 | fumble_vec == 1 | touchdown == 1 ~
                            ifelse(EPA >= 0, EPA - 20, EPA + 20)))

plays <- nrow(player_cum)
xlim <- plyr::round_any(plays, 10, f = ceiling) + 1 # Get even x-axis breaks.

cum_epa <- cum_epa %>% filter(play_number <= xlim)

p2 <- ggplot(cum_epa,
             aes(x = play_number, y = EPA, group = id)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(subtitle = "Game Usage & Cumulative EPA",
       x = "Play Number") +
  theme(text = element_text(family = "Fjalla One"),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.line = element_line()) +
  scale_x_continuous(limits = c(1, xlim),
                     breaks = seq(1, xlim, 10)) +
  annotate("rect", fill = "red", alpha = 0.1,
           xmin = -Inf, xmax = Inf,
           ymin = -Inf, ymax = 0) +
  geom_line(colour = "grey", size = 0.5, alpha = 0.3) +
  geom_segment(data = player_cum,
               aes(x = xpos, xend = xpos, y = ystart, yend = yend,
                   colour = label_col),
               show.legend = FALSE) +
  geom_point(data = player_cum,
             aes(x = xpos, y = yend, colour = label_col),
             size = 6, show.legend = FALSE) +
  scale_colour_identity() +
  geom_text(data = player_cum,
            aes(x = xpos, y = yend, label = txt_label),
            size = 2.5, colour = "white", family = "Fjalla One") +
  geom_line(data = player_cum,
            aes(x = play_number, y = EPA),
            colour = main, size = 2)

###############################################################################

### Density Plot ###

qb_epa <- pbp_rp %>%
  group_by(player_name, wk, def_pos_team) %>%
  summarise(n = n(),
            epa = mean(EPA)) %>%
  ungroup() %>%
  filter(n >= 20)

passer_epa <- qb_epa %>%
  filter(player_name == passer & wk == week)

opponent <- passer_epa$def_pos_team

pct_epa_value <- quantile(qb_epa$epa,
                          stats::ecdf(qb_epa$epa)(passer_epa$epa),
                          type = 1)

# Percentile rank relative to 2022 season.
pct_epa_pct <- round(stats::ecdf(qb_epa$epa)(passer_epa$epa) * 100)

# Method for creating the density plot is based on the work by Bruno Mioto.
# https://twitter.com/BrunoHMioto)

p3 <- ggplot(qb_epa,
             aes(y = epa)) +
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF"),
        panel.background = element_rect(fill = "#F0F8FF"),
        panel.grid = element_blank()) +
  labs(subtitle = "EPA/Play Rank vs. 2022 Distribution") +
  theme(text = element_text(family = "Fjalla One"),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line()) +
  stat_halfeye(aes(fill = stat(y < pct_epa_value)),
               adjust = 0.5,
               width = 0.4,
               .width = 0,
               point_colour = NA,
               alpha = 0.7,
               show.legend = FALSE) +
  scale_fill_manual(values = c("FALSE" = "grey",
                               "TRUE" = main)) +
  geom_point(data = passer_epa,
             aes(x = 0.01, y = epa),
             fill = main, color = "black", shape = 24, size = 5) +
  geom_richtext(aes(x = 0.3, y = -0.7,
                    label = glue("EPA/Play: {round(passer_epa$epa, 2)}<br><br>Better than {pct_epa_pct}% of QBs<br>through Week {week}")),
                fill = "#F0F8FF", label.colour = "#F0F8FF",
                family = "Fjalla One") +
  coord_flip(expand = TRUE) +
  scale_y_continuous(limits = c(-1, max(qb_epa$epa)),
                     breaks = seq(-1, 1, 0.5))

###############################################################################

# Combine plots.

base <- (plot_spacer() / p2 / p3 | p1) &
  plot_annotation(title = glue("QB Game Summary: {passer}"),
                  subtitle = glue("Week {week} versus {opponent}"),
                  caption = "Data: @CFB_Data with cfbfastR & PFF | Plot: @cover2figuRes") &
  theme(text = element_text(family = "Fjalla One"),
        plot.title = element_text(size = 24, hjust = 0.5, vjust = 5),
        plot.subtitle = element_text(size = 16, hjust = 0.5, vjust = 5),
        plot.caption = element_text(size = 12, hjust = 0.5)) &
  theme(plot.background = element_rect(fill = "#F0F8FF", colour = "#F0F8FF")) &
  theme(plot.margin = unit(c(2, 1, 1, 1), "lines"))

ragg::agg_png("Base.png", width = 15, height = 10,
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
  logo_scale = 20
)

magick::image_write(plot_with_personal, "Personal.png")

plot_with_logo <- add_logo(
  plot_path = "Personal.png",
  logo_path = team_logo,
  logo_position = "top right",
  logo_scale = 10
)

magick::image_write(plot_with_logo, "Logo.png")

plot_with_headshot <- add_logo(
  plot_path = "Logo.png",
  logo_path = headshot,
  logo_position = "bottom right",
  logo_scale = 8
)

magick::image_write(plot_with_headshot, "Headshot.png")

plot_with_ncaa <- add_logo(
  plot_path = "Headshot.png",
  logo_path = "NCAA.png",
  logo_position = "top left",
  logo_scale = 15
)

magick::image_write(plot_with_ncaa, "CFB.png")

plot_with_box <- add_logo(
  plot_path = "CFB.png",
  logo_path = "Box.png",
  logo_position = "top left 2",
  logo_scale = 2
)

magick::image_write(plot_with_box, "Plots/CFB/CFB_QB_Game_Summary.png")
