# libraries
library(tidyverse)
library(gt)
library(gtExtras)


# make table from sports-ref data: https://www.sports-reference.com/cbb/schools/north-carolina/2022.html
minutes <- tibble::tribble(
~Name, ~Seasons, ~MP, ~TM,
"Raymond Felton","2002-2004",2279,2635,
"Sam Perkins","1980-1982",2256,2845,
"Caleb Love","2020-2022",2134,2735,
"Marcus Paige","2012-2014",2232,2815,
"Ed Cota","1996-1998",2183,2950,
"Tyler Hansbrough","2005-2007",2079,2770,
"Michael Jordan","1982-1984",2192,2825,
"Joseph Forte","1999-2001",2340,2770,
"Harrison Barnes","2010-2012",2198, 3010,
"J.R. Reid","1987-1989",2072,2770,
"Antawn Jamison","1995-1997",2251,2690,
)


# create table with batting averages
table <- minutes %>%
  mutate(pct = (MP / TM)) %>%
  arrange(-pct) %>%
  mutate(row_number = 1:n()) %>%
  select(row_number, Name, Seasons, MP, TM, pct) %>%
  gt() %>%
  opt_row_striping() %>% # stripe rows
  tab_header(title = 'Carolina: Players with at least 2,000 minutes played in first two collegiate seasons',
             subtitle = 'Shows percentage of minutes played in first two seasons.') %>%
  cols_label(
    pct = '',
    MP = 'Minutes',
    TM = 'Total',
    Name = 'Player',
    row_number = '',
  ) %>%
  cols_align(# align columns left
    align = "right",
    columns = pct) %>%
  fmt_percent(columns = pct,
              decimals = 1) %>%
  gt_merge_stack(col1 = Name, col2 = Seasons) %>%
  gt_highlight_rows(
    rows = 6,
    fill = '#ffffd4',
    bold_target_only = TRUE,
    target_col = pct
  ) %>%
  gt_theme_nytimes() %>%
  tab_source_note(source_note = '@dadgumboxscores | June 16, 2022 | data via sports-reference.com')  %>%
  # fix font of source note and set stripe color
  tab_options (
    source_notes.font.size = px(10),
    row.striping.background_color = '#dbebf6',
    column_labels.text_transform = 'capitalize',
  )

# save as image
gtsave_extra(table,
             'minutes.png',
             vwidth = 500,
             vheight = 430)
