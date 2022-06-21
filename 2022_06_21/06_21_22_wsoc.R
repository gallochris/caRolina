# load libraries
library(tidyverse)
library(gt)
library(gtExtras)


# create table from results in goheels record book: https://s3.amazonaws.com/sidearm.sites/unc.sidearmsports.com/documents/2021/8/13/2021WSOCRecordBook.pdf
wow <- tibble::tribble(
~Year,~W,~L,~T,~Title,~GF,~GA,
1982,19,2,0,1,112,8,
1983,19,1,0,1,95,11,
1984,24,0,1,1,120,6,
1985,18,2,1,0,98,13,
1986,24,0,1,1,113,10,
1987,23,0,1,1,96,2,
1988,18,0,3,1,58,9,
1989,24,0,1,1,99,9,
1990,20,1,1,1,87,12,
1991,24,0,0,1,101,9,
1992,25,0,0,1,132,11,
1993,23,0,0,1,92,15,
1994,25,1,1,1,114,12,
1995,25,1,0,0,108,6,
1996,25,1,0,1,109,11,
1997,27,0,1,1,117,8,
1998,25,1,0,0,98,7,
1999,24,2,0,1,91,12,
2000,21,3,0,1,97,17,
)

# make table + calculate diff
full_table <- wow %>%
  # calculate diff
  mutate(diff = (GF - GA)) %>%
  select(Year, Title, W, L, T, GF, GA, diff) %>% # adjust order of columns
  gt() %>%
  # add trophy emojis
  gt_fa_repeats(
    column = Title,
    palette = "orange",
    name = "trophy",
    align = 'left'
  ) %>%
  fmt(
    # format differential column
    columns = c(diff),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) %>%
  summary_rows(
    # summarize the rows
    columns = c(Title, W, L, T, diff),
    fns = list("sum"),
    formatter = fmt_number,
    decimals = 0,
    use_seps = FALSE
  ) %>%
  cols_label(
    # rename columns
    diff = "+/-",
    Title = "",
    GF = "Scored",
    GA = "Allowed"
  ) %>%
  opt_row_striping() %>% # stripe rows
  # set title + caption + subtitle
  tab_header(title = "Carolina Women's Soccer Dominance",
             subtitle = "Season results from 1982 to 2000 under Anson Dorrance.") %>%
  tab_source_note(source_note = "@dadgumboxscores | June 21, 2022")  %>%
  # adjusts row striping color and font sizes
  tab_options (
    source_notes.font.size = px(10),
    table.font.size = px(12),
    row.striping.background_color = '#dbebf6'
  ) %>%
  # add theme using 538
  gt_theme_538()


# save as image
gtsave(full_table,
       'wsoc.png',
       vwidth = 500,
       vheight = 430)