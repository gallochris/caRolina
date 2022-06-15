
 
# compile data from sports-reference player pages, i.e. https://www.sports-reference.com/cbb/players/armando-bacot-1.html
 rebound_data <- read.csv(text="year,player,start,end,
                       1,Tyler Hansbrough,0,243,
                       2,Tyler Hansbrough,243,544,
                       3,Tyler Hansbrough,544,943,
                       4,Tyler Hansbrough,943,1219,
                       1,Sam Perkins,0,289
                       2,Sam Perkins,289,539
                       3,Sam Perkins,539,869
                       4,Sam Perkins,869,1167
                       1,George Lynch,0,183
                       2,George Lynch,183,441
                       3,George Lynch,441,732
                       4,George Lynch,732,1097
                       1,Billy Cunningham,0,339
                       2,Billy Cunningham,339,718
                       3,Billy Cunningham,718,1062
                       1,Kennedy Meeks,0,207
                       2,Kennedy Meeks,207,478
                       3,Kennedy Meeks,478,674
                       4,Kennedy Meeks,674,1052
                       1,Brice Johnson,0,115
                       2,Brice Johnson,115,324
                       3,Brice Johnson,324,619
                       4,Brice Johnson,619,1035
                       1,Antawn Jamison,0,309
                       2,Antawn Jamison,309,638
                       3,Antawn Jamison,638,1027
                       1,Mitch Kupchak,0,165,
                       2,Mitch Kupchak,165,356
                       3,Mitch Kupchak,356,690
                       4,Mitch Kupchak,690,1006
                       1,Brad Daugherty,0,181
                       2,Brad Daugherty,181,348
                       3,Brad Daugherty,348,697
                       4,Brad Daugherty,697,1003
                       1,Armando Bacot,0,264
                       2,Armando Bacot,264,491
                       3,Armando Bacot,491,1002")

# load libraries
library(tidyverse)
library(png)
require(cowplot)
require(magick)

# load mando img
img <- readPNG("mando.png")

# add theme
theme_me <- function () {
  theme_minimal(base_size = 15, base_family = "RobotoCondensed-Regular") %+replace%
    theme (
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(size = 6, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 8),
      plot.background = element_rect(fill = "floral white", color = "floral white")
    )
}

# make chart
chart <- rebound_data %>%  # first, re-arrange the order of the rows
  mutate(player = fct_relevel(
    player,
    c(
      'Armando Bacot',
      'Brad Daugherty',
      'Mitch Kupchak',
      'Antawn Jamison',
      'Brice Johnson',
      'Kennedy Meeks',
      'Billy Cunningham',
      'George Lynch',
      'Sam Perkins',
      'Tyler Hansbrough'
    )
  )) %>%
  ggplot() +
  coord_flip() +
  geom_linerange(aes(
    x = player,
    ymin = start,
    ymax = end,
    colour = as.factor(year)
  ), size = 4) +
  scale_colour_manual(
    values = c('#c3ddef', '#94c3e3', '#d38956', '#378fcb'),
    labels = c('Freshman', 'Sophomore', 'Junior', 'Senior')
  ) +
  labs(
    x = '',
    y = 'Rebounds',
    colour = '',
    caption = '@dadgumboxscores | June 15, 2022 | data via sports-reference.com',
    title = 'Carolina: Rebounds by season \n compares only players with 1,000 career rebounds'
  ) +
  theme_me() +
  theme(legend.position = 'bottom') +
  annotate(
    "text",
    x = 4,
    y = 1150,
    label = "Bacot set the \n single season record \n with 511 total rebounds",
    family = "Chalkboard Bold",
    size = 3.3,
    color = "#4B9CD3"
  ) +
  annotate(
    geom = "curve",
    color = "#4B9CD3",
    x = 3.5,
    y = 1025,
    xend = 1,
    yend = 750,
    curvature = .3,
    arrow = arrow(length = unit(2, "mm"))
  )

# add funny mando image
with_image <- ggdraw(chart) +
  draw_image(img,
             x = 0.42,
             y = -0.19,
             scale = 0.2)

# save big mando image
ggsave(
  "big_mando.png",
  with_image,
  w = 10.2,
  h = 6,
  dpi = 300,
  type = 'cairo'
)