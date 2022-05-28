library(dplyr)
library(patchwork)

#theme
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
      plot.background = element_rect(fill = "#c9e1f1", color = "#c9e1f1")
    )
}


turn <- ggplot(turns, aes(x = Season, y = DTO, label = DTO)) +
  geom_line(color = "#e6cab1") +
  geom_point() +
  geom_text(
    aes(label = sprintf("%0.1f", round(DTO, digits = 1))),
    color = "#13294B",
    size = 4,
    fontface = "bold",
    vjust = -2.5
  ) +
  geom_vline(xintercept = 2021.5, lty = 2) +
  scale_x_continuous(breaks = seq(2013, 2022, 1)) +
  ylim(0, 25) +
  theme_me() +
  theme(legend.position = "none") +
  annotate(
    "text",
    x = 2018,
    y = 12.5,
    label = "As of January 20, 2022 \n UNC ranks 353 out of 358 teams \n in defensive turnover rate",
    family = "Chalkboard Bold",
    size = 4,
    color = "#EF426F"
  ) +
  annotate(
    geom = "curve",
    color = "#EF426F",
    x = 2019.2,
    y = 10.8,
    xend = 2022,
    yend = 13,
    curvature = .3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  labs(
    x = "Season",
    y = "Defensive Turnover Rate",
    title = "Carolina Defense: Opponent Turnover Percentage",
    subtitle = "Compares the last 10 seasons of data, the 2021-22 season is only a sample of 17 games.",
    caption = "@dadgumboxscores | data via @kenpom"
  )


ggsave(
  "turn.png",
  turn,
  w = 10,
  h = 6,
  dpi = 300,
  type = 'cairo'
)

## boards
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

boards <- turns %>%
  ggplot(aes(x = Season, y = D, group = Type)) +
  geom_line(aes(color = Type)) +
  geom_point(
    color = 'black',
    fill = "#acacac",
    shape = 21,
    size = 2.5,
    alpha = .75,
    aes(color = Type)
  ) +
  scale_color_manual(values = c("#acacac", "#56a0d3")) +
  theme_me() +
  geom_vline(xintercept = 2021.5, lty = 2) +
  scale_x_continuous(breaks = seq(2013, 2022, 1)) +
  ylim(0, 45) +
  labs(
    x = "Season",
    y = "Offensive Rebounding Rate",
    title = "Carolina Offense: Rebounding Rate Compared to D-I Median Over Last 10 Seasons",
    subtitle = "Compares the last 10 seasons of data, the 2021-22 season is only a sample of 17 games.",
    caption = "@dadgumboxscores | data via kenpom.com"
  ) +
  theme(legend.position = "none") +
  annotate(
    "text",
    x = 2018,
    y = 12.5,
    label = "As of January 20, 2022 \n UNC ranks 141 out of 358 teams \n in offensive rebounding rate",
    family = "Chalkboard Bold",
    size = 4,
    color = "#EF426F"
  ) +
  annotate(
    geom = "curve",
    color = "#EF426F",
    x = 2019.2,
    y = 14.0,
    xend = 2022,
    yend = 27.7,
    curvature = .3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = 'label',
    x = 2022,
    y = 33,
    hjust = 0.75,
    label = "UNC",
    fill = "#56a0d3",
    fontface = 'bold',
    alpha = .5
  ) +
  annotate(
    geom = 'label',
    x = 2022,
    y = 25,
    hjust = 0.75,
    label = "D-I Median",
    fill = "#acacac",
    fontface = 'bold',
    alpha = .5
  )

ggsave(
  "boards.png",
  boards,
  w = 10,
  h = 6,
  dpi = 300,
  type = 'cairo'
)
