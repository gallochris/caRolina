# load libraries
library(ggplot2)
library(dplyr)
library(ggrepel)

# load data from https://247sports.com/college/north-carolina/LongFormArticle/UNC-Tar-Heels-Basketball-Top-100-Players-1-10-189851846/#189851846_1
unc <- read.csv('bestunc.csv')

# add theme
theme_me <- function () {
  theme_minimal(base_size = 10, base_family = "Arial") %+replace%
    theme (
      plot.title = element_text(
        hjust = 0.5,
        size = 24,
        face = "bold"
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 12,
        lineheight = 0.15,
        vjust = -0.5
      ),
      plot.caption = element_text(
        hjust = 1,
        size = 8,
        lineheight = 0.35,
        margin = margin(t = 20)
      ),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "floral white", color = "floral white")
    )
}

# set colors
colors = c("#d67400", "#898989", "#0089d6")

# scatter plot text repel
gg <- unc %>%
  ggplot(aes(
    x = Year,
    y = Rank,
    label = Player,
    color = Jersey
  )) +
  geom_point(alpha = 0.25) +
  geom_text_repel() +
  scale_y_reverse(breaks = seq(0, 100, 5)) +
  scale_x_continuous(breaks = seq(1952, 2022, 10),
                     limits = c(1952, 2022)) +
  geom_hline(yintercept = 50,
             linetype = 'dashed',
             color = "#dddddd") +
  scale_color_manual(values = colors) +
  theme_me() +
  theme(legend.position = "none") +
  labs(
    x = "Year (by last season played)",
    y = "Ranking",
    title = "Top 100 Players in Carolina History",
    subtitle = "Players ranked since 1953-54 season by Adrian Atkinson and Inside Carolina.",
    caption = "@dadgumboxscores | July 10, 2022 | data via Adrian Atkinson"
  )

# save the chart
ggsave(
  "gg.png",
  gg,
  w = 10,
  h = 8,
  dpi = 300,
  type = 'cairo'
)


# find highest not retired or honored
unc %>%
    filter(Jersey == 'None') %>%
    arrange(Rank) %>%
    slice(1:5)

# find lowest retired or honored
unc %>%
    filter(Jersey != 'None') %>%
    arrange(-Rank) %>%
    slice(1:5)
