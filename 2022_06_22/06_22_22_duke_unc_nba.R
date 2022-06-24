# load libraries
library(tidyverse)
library(ggpmisc)

# reload data from https://www.nba.com/stats/draft/history/
all <- read.csv('raw_nba.csv')

# manually build table of decades
decades <- tibble::tribble(
     ~Decade,~UNC,~Duke, 
    "1960s",0,1,
    "1970s",7,2,
    "1980s",9,4,
    "1990s",9,10,
    "2000s",10,8,
    "2010s",12,20,
    "2020s",2,1,
    "totals",49,46,
    )

data.dec <- tibble(x = 1978, y = 40, decades = list(decades))


# add theme 
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
      plot.background = element_rect(fill = "floral white", color = "floral white")
    )
}


# make sphaghetti plot with table
nba <- all %>% 
  ggplot(aes(x = YEAR, y = totals, colour = factor(COLLEGE))) +
  geom_line(size = .9) +
  scale_colour_manual(values = c("North Carolina" = "#56a0d3","Duke" = "#003087")) +
  theme_me() +
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = seq(1966, 2022, 8)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  labs(
    x = "",
    y = "1st round picks",
    title = "Carolina and Duke: First Round NBA Draft Picks Since 1966",
    subtitle = "Only first round picks since the start of the modern NBA draft (1996).",
    caption = "@dadgumboxscores | June 23, 2022 | data via nba.com"
  ) +
  geom_table(data = data.dec, aes(x, y, label = decades)) 

# save the chart
ggsave(
  "nba_duke_unc.png",
  nba,
  w = 6,
  h = 6,
  dpi = 300,
  type = 'cairo'
)



