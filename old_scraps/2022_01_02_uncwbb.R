# add libraries
library(tidyverse)
library(gt)
library(gtExtras)

# get data from stats.ncaa.org
uncwbb_data <- read.csv('uncwbb.csv')

# theme
theme_me <- function () {
  theme_minimal(base_size = 15, base_family = "RobotoCondensed-Regular") %+replace%
    theme (
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(
        hjust = 0.5,
        vjust = -2,
        lineheight = 0.9,
        size = 10
      ),
      plot.caption = element_text(size = 8, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "floral white", color = "floral white")
    )
}

# plot it
uncwbb_plot <- uncwbb_data %>%
  ggplot() +
  geom_point(aes(
    SCR.MAR,
    Margin,
    color = label,
    size = 1.5,
    alpha = 0.75
  )) +
  scale_color_manual(values = c('#acacac', '#56a0d3')) +
  scale_x_continuous(breaks = seq(-40, 40, 10)) +
  scale_y_continuous(breaks = seq(-450, 450, 100)) +
  labs(
    x = "Scoring Margin",
    y = "Raw Point Differential",
    title = "NCAA Women's Hoops: Scoring margin + point differential through January 2, 2022",
    caption = "data via stats.ncaa.org"
  ) +
  theme_me() +
  theme(legend.position = "none") +
  annotate(
    geom = "curve",
    color = "#56a0d3",
    x = 12,
    y = -175,
    xend = 32,
    yend = 410,
    curvature = .3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "text",
    x = 10,
    y = -250,
    label = "UNC best scoring margin (32.3) \n best point differential (+420) \n through 13 games this season",
    family = "Chalkboard Bold",
    size = 4,
    color = "#56a0d3"
  )

ggsave(
  "uncwbb.png",
  uncwbb + plot,
  w = 10,
  h = 6,
  dpi = 300,
  type = 'cairo'
)

# Show a table too

# remove parentheses
uncwbb_data$Team <-
  gsub("\\s*\\([^\\)]+\\)", "", as.character(uncwbb_data$Team))

# make a table
uncwbb_data %>%
  filter(Margin > 300) %>%
  select(Team, W.L, PTS, OPP.PTS, Margin) %>%
  gt() %>%
  cols_label(
    W.L = "Record",
    PTS = "Scored",
    OPP.PTS = "Allowed",
    Margin = "Differential"
  ) %>%
  gtExtras::gt_theme_dot_matrix(color = "#e5eff8") %>%
  tab_source_note(source_note = "@dadgumboxscores | data via stats.ncaa.org")  %>%
  tab_style(style = list(
    cell_fill(color = "#56a0d3"),
    cell_text(weight = "bold", color = "white")
  ),
  locations = cells_body(rows = 1)) %>%
  fmt(
    columns = c(Margin),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) %>%
  # fix font of source note
  tab_options (
    source_notes.font.size = px(10),
    source_notes.padding = px(5),
    column_labels.text_transform = "uppercase",
  )
