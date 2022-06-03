# libraries
library(tidyverse)
library(treemapify)

# scrape data from unc stats: https://goheels.com/documents/2022/2/20/OverallStats.pdf
dingers <- tibble::tribble(
     ~Player,~homers, 
     "Serretti",8,
     "Zarate",6,
     "Madej",3,
     "Stokely",2,
     "Honeycutt",22,
     "Castagnozzi",9,
     "Horvath",18,
     "Osuna",19,
     "Frick",2,
     "Grintz",1,
     "Alvarez",1,
    )

# make a plot
theme_me <- function () {
  theme_minimal(base_size = 10, base_family = "RobotoCondensed-Regular") %+replace%
    theme (
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(
        hjust = 0.5,
        vjust = -2,
        lineheight = 0.9,
        size = 8
      ),
      plot.caption = element_text(size = 6, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#c9e1f1", color = "#c9e1f1")
    )
}

# create bar chart
dings <- dingers %>%
  ggplot(aes(x = fct_rev(fct_reorder(Player,-homers)), y = homers)) +
  geom_bar(
    stat = "identity",
    fill = "#13294B",
    width = .25,
    colour = '#13294B'
  ) +
  ylim(0, 25) +
  labs(caption = "@dadgumboxscores | June 3, 2022") +
  xlab("Player") +
  ylab("Home Runs") +
  geom_text(
    aes(label = round(homers)),
    size = 3,
    colour = '#13294B',
    hjust = -0.5
  ) +
  theme_me() +
  coord_flip() +
  labs(
    x = "",
    y = "Home runs",
    title = "Carolina: Home runs by player \n through 58 games in the 2022 season",
    caption = "@dadgumboxscores | June 3, 2022"
  )

# save the chart
ggsave(
  "dingers.png",
  dings,
  w = 4,
  h = 6,
  dpi = 300,
  type = 'cairo'
)

