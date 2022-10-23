# libraries
library(tidyverse)
library(gt)
library(gtExtras)
library(cfbplotR)

# load odds from espn
acc <- read_csv('acc.csv')

# make the table
acc <- acc %>%
  mutate(logo = team) %>%
  select(logo, team, div, W, L, win_div, `win_ conf`, overall_w, overall_l)

table <- acc %>%
  gt(groupname_col = "div") %>%
  cols_label(
    # rename columns
    logo = "",
    team = "",
    win_div = "Win Div %",
    `win_ conf` = "Win Conf %",
    overall_w = "W",
    overall_l = "L"
  ) %>%
  tab_spanner(label = "ACC",
              columns = c(W, L, win_div, `win_ conf`)) %>%
  tab_spanner(label = "Overall",
              columns = c(overall_w, overall_l)) %>%
  gt_highlight_rows(
    rows = 1,
    fill = "#ffc093",
    bold_target_only = TRUE,
    target_col = win_div
  ) %>%
  gt_highlight_rows(
    rows = 7,
    fill = "#ddecf6",
    bold_target_only = TRUE,
    target_col = win_div
  ) %>%
  gt_fmt_cfb_logo(columns = "logo") %>%
  tab_header(
    title = "2022 ACC standings and odds of winning league",
    subtitle = md("Odds of winning division and conference from **ESPN's FPI**.")
  ) %>%
  tab_source_note(source_note = "@dadgumboxscores | October 23, 2022 | data via ESPN")  %>%
  # adjust font sizes
  tab_options (source_notes.font.size = px(10),
               table.font.size = px(12),) %>%
  # add theme using 538
  gt_theme_538()

gtsave(table, 'acc.png', vwidth = 900, vheight = 800)
