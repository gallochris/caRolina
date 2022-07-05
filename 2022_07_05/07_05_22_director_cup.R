# load libraries
library(ggridges)
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(cfbplotR)

# load data from https://nacda.com/sports/2018/7/17/directorscup-nacda-directorscup-current-scoring-html.aspx
dc <- read.csv('dc.csv')

# just filter by power five conferences
powerfive <- dc %>%
  filter(
    Conference == 'ACC' | Conference == 'Big Ten'
    | Conference == 'Pac-12' | Conference == 'SEC' | Conference == 'Big 12'
  )

# find avg and median rankings
mm <- powerfive %>%
  group_by(Conference) %>%
  summarize(Median = median(Rank), Avg = mean(Rank)) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  arrange(Avg)

# save coordinates for table
mm <- tibble(x = 110, y = 3.5, mm = list(mm))

# build density chart
by_conf <- powerfive %>%
  ggplot(aes(y = Conference, x = Rank, fill = Conference)) +
  geom_density_ridges(
    alpha = 0.6,
    stat = "binline",
    bins = 14,
    scale = 0.95
  ) +
  theme_me() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  scale_x_continuous(breaks = seq(0, 125, 25)) +
  labs(
    x = "Rank",
    y = "",
    title = "2021-22 Directors' Cup Rankings by Conference",
    subtitle = "Shows distribution of rankings for teams in power five conference",
    caption = "@dadgumboxscores | July 5, 2022 | data via nacda.com"
  ) +
  geom_table(data = mm, aes(x, y, label = mm))

# save the chart
ggsave(
  "cup.png",
  by_conf,
  w = 9,
  h = 8,
  dpi = 300,
  type = 'cairo'
)

# make scatter plot by school
top <- powerfive %>%
  mutate(
    # change names to be abbreviated
    School = case_when(
      School == 'North Carolina State' ~ 'NC State',
      School == 'Miami (FL)' ~ 'Miami',
      TRUE ~ School
    )
  ) %>%
  ggplot(aes(x = Total, y = Rank)) +
  geom_cfb_logos(aes(team = School), width = 0.035, position = 'jitter') +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(0, 1500, 250)) +
  geom_vline(xintercept = 750, linetype = 'dashed') +
  geom_hline(yintercept = 50, linetype = 'dashed') +
  theme_me() +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  labs(
    x = "Total Points",
    y = "Rank",
    title = "2021-22 Directors' Cup Points and Rankings",
    subtitle = "Shows points and rankings for schools in a Power 5 conference.",
    caption = "@dadgumboxscores | July 5, 2022 | data via nacda.com"
  ) +
  annotate(
    geom = 'label',
    x = 1250,
    y = 20,
    hjust = 0.75,
    label = "Higher ranking \n more points",
    fill = "#6df876",
    fontface = 'bold',
    alpha = .5
  ) +
  annotate(
    geom = 'label',
    x = 500,
    y = 90,
    hjust = 0.75,
    label = "Lower ranking \n fewer points",
    fill = "#f8766d",
    fontface = 'bold',
    alpha = .5
  )

# save the chart
ggsave(
  "top.png",
  top,
  w = 7,
  h = 5,
  dpi = 300,
  type = 'cairo'
)

# find non-power fives
npf <- dc %>%
  filter(
    Conference != 'ACC',
    Conference != 'Big Ten',
    Conference != 'Pac-12',
    Conference != 'SEC',
    Conference != 'Big 12'
  ) %>%
  arrange(Rank) %>%
  select(Rank, School, Conference) %>%
  filter(Rank < 100) %>%
  slice(1:10)
