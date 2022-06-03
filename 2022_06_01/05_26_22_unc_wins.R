# load libraries
library(dplyr)
library(gt)
library(gtExtras)

# load csv from sports-reference
games <- read.csv('unc_2000.csv')


# count games by seasons
S_counts <- games %>%
  group_by(Year) %>%
  count(Year, name = "total games")

# count wins
BW_counts <- games %>%
  filter(MOV > 19) %>%
  group_by(Year) %>%
  count(Year, name = "games_won_by_20_or_more") %>%
  ungroup() %>%
  complete(Year = unique(games$Year)) %>% # include any NAs
  replace(is.na(.), 0) # replace NAs with 0s

# count losses
BL_counts <- games %>%
  filter(MOV < -19) %>%
  group_by(Year) %>%
  count(Year, name = "games_lost_by_20_or_more") %>%
  ungroup() %>%
  complete(Year = unique(games$Year)) %>% # include any NAs
  replace(is.na(.), 0) # replace NAs with 0s

# combine blowouts by year
blowouts <- merge(BW_counts, BL_counts, by = "Year")

# combine blowouts with total games
totals <- merge(blowouts, S_counts, by = "Year")

# make table
chart <- totals %>%  # first, re-arrange the order of the rows
  arrange(-games_lost_by_20_or_more) %>%
  gt() %>%
  opt_row_striping() %>% # stripe rows
  tab_header(title = "Carolina Hoops: Blowout wins and losses by season",
             subtitle = "Number of games won or lost by 20 or more points in a single season") %>%
  summary_rows(
    # summarize the rows
    columns = c(games_lost_by_20_or_more, games_won_by_20_or_more),
    fns = list("sum"),
    formatter = fmt_number,
    decimals = 0,
    use_seps = FALSE
  ) %>%
  cols_label(
    games_lost_by_20_or_more = "# lost",
    games_won_by_20_or_more = "# won",
    `total games` = "Total Games",
    Year = "Season"
  ) %>%
  gt_theme_dot_matrix() %>%
  tab_source_note(source_note = "@dadgumboxscores | June 1, 2022")  %>%
  # fix font of source note and set stripe color
  tab_options (
    source_notes.font.size = px(10),
    row.striping.background_color = "#dbebf6",
  )

# save image
gtsave_extra(chart, "blowout_chart.png", vwidth = 400, vheight = 430)
