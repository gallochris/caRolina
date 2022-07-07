# load libraries
library(tidyverse)
library(cfbplotR)
library(gt)
library(gtExtras)

# load data from https://nacda.com/sports/2018/7/17/directorscup-nacda-directorscup-current-scoring-html.aspx
varsity <- read.csv('vsports.csv')

# pivot it wide from the group
v <- varsity %>%
  pivot_wider(names_from = group, values_from = value)

# add theme
theme_me <- function () {
  theme_minimal(base_size = 15, base_family = "Arial") %+replace%
    theme (
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(
        hjust = 0.5,
        lineheight = 0.9,
        size = 10
      ),
      plot.caption = element_text(size = 8, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#fdf5e6", color = "#fdf5e6")
    )
}

# make scatter plot by school
vs <- v %>%
  ggplot(aes(x = w, y = m)) +
  geom_cfb_logos(aes(team = school), width = 0.035, position = 'jitter') +
  scale_y_continuous(breaks = seq(5, 20, 1), limits = c(5, 20)) +
  scale_x_continuous(breaks = seq(5, 25, 1), limits = c(5, 25)) +
  geom_vline(xintercept = 15, linetype = 'dashed') +
  geom_hline(yintercept = 15, linetype = 'dashed') +
  theme_me() +
  labs(
    x = "Women's sports",
    y = "Men's sports",
    title = "2021-22 Sponsored Sports by School",
    subtitle = "Shows sponsored sports for 2021-22 season of Power 5 conference schools.",
    caption = "@dadgumboxscores | July 7, 2022 | data via web3.ncaa.org/directory"
  )


# save the chart
ggsave(
  "vs.png",
  vs,
  w = 8,
  h = 6,
  dpi = 300,
  type = 'cairo'
)


# find schools with more than 25 sponsored sports
big <- v %>%
  mutate(total = w + m, logo = school) %>%
  filter(total > 24) %>%
  arrange(-total)

table <- big %>%
  select(logo, school, w, m, total) %>%
  gt() %>%
  # add trophy emojis
  cols_label(
    # rename columns
    school = "School",
    w = "Women",
    m = "Men",
    total = "Total",
    logo = "",
  ) %>%
  gt_fmt_cfb_logo(columns = 'logo') %>%
  opt_row_striping() %>% # stripe rows
  # set title + caption + subtitle
  tab_header(title = "2021-22 Sponsored Sports by School",
             subtitle = "Only shcools with 25 or more sponsored sports shown.") %>%
  tab_source_note(source_note = "@dadgumboxscores | July 7, 2022 | data via web3.ncaa.org/directory")  %>%
  tab_footnote(footnote = "Counts mixed sports twice (i.e. sailing)",
               locations = cells_column_labels(columns = total)) %>%
  # adjusts row striping color and font sizes
  tab_options (
    source_notes.font.size = px(10),
    table.font.size = px(12),
    row.striping.background_color = '#fdf5e6'
  ) %>%
  # add theme using 538
  gt_theme_538()


# save as image
gtsave(table,
       'vsports.png',
       vwidth = 500,
       vheight = 430)
