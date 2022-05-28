# load libraries
library(tidyverse)

# add theme
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

# recreate play-by-play scoring - https://s3.amazonaws.com/sidearm.sites/unc.sidearmsports.com/documents/2022/5/27/UNC_NU.pdf
wlax <- read.csv('uncwlax_pbp_manual.csv')

# colors for schools
school_color <- c("#4E2A84", "#4B9CD3")

# make chart
heels_win <- wlax  %>%
  ggplot(aes(x = Time, y = Score, group = Team)) +
  geom_step(aes(color = Team, size = 0.5)) +
  scale_color_manual(values = school_color) +
  theme_me() +
  scale_y_continuous(breaks = seq(0, 15, 1)) +
  scale_x_continuous(breaks = seq(0, 60, 5)) +
  theme(legend.position = "none") +
  annotate(
    "text",
    x = 20,
    y = 12,
    label = "Carolina scored \n nine goals on 15 shots \n in the final period",
    family = "Chalkboard Bold",
    size = 5,
    color = "#4B9CD3"
  ) +
  annotate(
    geom = "curve",
    color = "#4B9CD3",
    x = 20,
    y = 10.5,
    xend = 54.2,
    yend = 11,
    curvature = .3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  labs(
    x = "Game Time",
    y = "Goals",
    title = "North Carolina 15, Northwestern 14 \n NCAA Women's Lacrosse National Semifinal",
    subtitle = "Carolina scored nine goals in the final 15 minutes to advance to the championship game",
    caption = "@dadgumboxscores"
  )

# save the chart
ggsave(
  "uncwlax.png",
  heels_win,
  w = 10,
  h = 6,
  dpi = 300,
  type = 'cairo'
)
