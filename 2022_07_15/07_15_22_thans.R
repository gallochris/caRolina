# load libraries
library(tidyverse)
library(png)
require(cowplot)
require(magick)

# load data from https://247sports.com/college/north-carolina/LongFormArticle/UNC-Tar-Heels-Basketball-Top-100-Players-1-10-189851846/#189851846_1
thans <- read.csv('sportsref_download_thans.csv')

# image
img <- readPNG("th.png")


# add theme
theme_me <- function () {
  theme_minimal(base_size = 10, base_family = "RobotoCondensed-Regular") %+replace%
    theme (
      plot.title = element_text(
        hjust = 0.5,
        size = 24,
        face = "bold"
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 10,
        lineheight = 0.25,
        vjust = -0.5
      ),
      plot.caption = element_text(
        hjust = 1,
        size = 6,
        lineheight = 0.35,
        margin = margin(t = 20)
      ),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = "#ffffff")
    )
}


# scatter plot text repel
tp <- thans %>%
  slice(1:131) %>%
  group_by(Season) %>%
  ungroup() %>%
  ggplot(aes(Season, PTS, color = Season)) +
  geom_jitter(
    show.legend = FALSE,
    height = 0,
    width = 0.10,
    size = 4,
    alpha = 0.5
  ) +
  scale_color_brewer(palette = 'RdBu') +
  theme_me() +
  theme(legend.position = "none") +
  geom_hline(yintercept = c(10, 20, 30), linetype = 'dashed') +
  labs(
    y = "Points",
    x = "Season",
    title = "Points Party",
    caption = "@dadgumboxscores | July 15, 2022 | data via sports-reference.com"
  ) +
  annotate(
    "text",
    x = 4,
    y = 37,
    label = "Tyler Hansbrough records \n 2,872 career points \n 79 games with 20+ points",
    family = "Chalkboard Bold",
    size = 3.3,
    color = "red"
  ) +
  annotate(
    "text",
    x = 0.7,
    y = 9,
    label = "9 games",
    family = "Chalkboard Bold",
    size = 3.3,
    color = "red"
  ) +
  annotate(
    "text",
    x = 0.7,
    y = 19,
    label = "54 games",
    family = "Chalkboard Bold",
    size = 3.3,
    color = "red"
  ) +
  annotate(
    "text",
    x = 0.7,
    y = 29,
    label = "72 games",
    family = "Chalkboard Bold",
    size = 3.3,
    color = "red"
  ) +
  annotate(
    "text",
    x = 0.7,
    y = 39,
    label = "7 games",
    family = "Chalkboard Bold",
    size = 3.3,
    color = "red"
  )



# add image
with_image <- ggdraw() +
  draw_plot(tp) +
  draw_image(
    img,
    scale = .10,
    x = 1,
    y = 1,
    hjust = 1,
    vjust = 1,
    halign = .9,
    valign = 1
  )

# save the chart
ggsave(
  "phans.png",
  with_image,
  w = 6.5,
  h = 6.5,
  dpi = 300,
  type = 'cairo'
)

# find point totals
less_than_10 <- thans %>%
  filter(PTS < 10) %>%
  summarise(count = n())

# 9

b_10_20 <- thans %>%
  filter(PTS > 9) %>%
  filter(PTS < 20) %>%
  summarise(count = n())

# 54

b_20_30 <- thans %>%
  filter(PTS > 19) %>%
  filter(PTS < 30) %>%
  summarise(count = n())

# 72

over_30 <- thans %>%
  filter(PTS > 29) %>%
  summarise(count = n())
# 7

# check season counts for games
thans %>%
  group_by(Season) %>%
  count()
