# load libraries
library(dplyr)
library(waffle)

# scrap data from https://goheels.com/sports/baseball/roster/vance-honeycutt/23146 and add as csv, scope to May and June
vance <- read.csv('bagger_vance.csv')

# calculate more numbers
may_june <- vance %>%
  filter(Month == 'May' | Month == 'June') %>%
  mutate(
    total_abs = sum(AB),
    total_hits = sum(H),
    total_hrs = sum(HR),
    total_rbi = sum(RBI),
    total_bb = sum(BB),
    total_hbp = sum(HBP),
    total_sf = sum(SF),
    total_k = sum(K),
    batting_avg = (total_hits / total_abs),
    obp = ((total_hits + total_bb + total_hbp) / (total_abs + total_bb + total_hbp + total_sf)
    ),
    slugging = ((10 + (2 * 3) + (3 * 1) + (4 * 14)) / total_abs)
  )

# check not may june
not_may_june <- vance %>%
  filter(Month != 'May' & Month != 'June') %>%
  mutate(
    total_abs = sum(AB),
    total_hits = sum(H),
    total_hrs = sum(HR)
  )


# theme
theme_me <- function () {
  theme_minimal(base_size = 12, base_family = "RobotoCondensed-Regular") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

# waffle chart of last 75 abs
honeycutt <-
  c(
    `Home Runs` = 14,
    Singles = 10,
    Doubles = 3,
    Triples = 1,
    Ks = 23,
    Outs = 24
  )


# Waffle chart
vh <- waffle(honeycutt, rows = 5) +
  theme_me() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  theme(legend.position = 'top') +
  scale_fill_manual(
    values = c(
      "#93c2e3",
      "#93e3b4",
      "#e393c2",
      "#e3b493",
      "#e3939a",
      "#acacac"
    ),
    labels = c("Home Runs", "Singles", "Doubles", "Triples", "Ks", "Outs")
  ) +
  labs(
    fill = "",
    title = "Vance Honeycutt: results of last 75 at-bats",
    subtitle = "Each square represents one outcome of the at-bat (ex. 14 home runs).",
    caption = "@dadgumboxscores | June 6, 2022"
  ) +
  theme(strip.text.x = element_text(size = 10)) +
  theme(
    plot.title = element_text(face = 'bold', size = 15, hjust = .5),
    plot.subtitle = element_text(size = 10, hjust = .5),
    plot.caption = element_text(color = 'black')
  ) +
  theme(
    plot.margin = unit(c(.5, .5, .5, .5), "cm"),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = .25
    )
  )

# save the chart
ggsave("vh.png",
       vh,
       w = 6,
       h = 4,
       dpi = 300)
