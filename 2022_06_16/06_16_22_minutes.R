# libraries
library(tidyverse)
library(gt)
library(gtExtras)


# make table from sports-ref data: https://www.sports-reference.com/cbb/schools/north-carolina/2022.html
minutes <- tibble::tribble(
~Name, ~Seasons, ~MP, ~TM,~NBA_G,
"Raymond Felton","2002-2004",2279,2635,"971",
"Sam Perkins","1980-1982",2256,2845,"1286",
"Caleb Love","2020-2022",2134,2735,"??",
"Marcus Paige","2012-2014",2232,2815,"5",
"Ed Cota","1996-1998",2183,2950,"0",
"Tyler Hansbrough","2005-2007",2079,2770,"428",
"Michael Jordan","1982-1984",2192,2825,"1072",
"Joseph Forte","1999-2001",2340,2770,"25",
"Harrison Barnes","2010-2012",2198, 3010,"747",
"J.R. Reid","1987-1989",2072,2770,"672",
"Antawn Jamison","1995-1997",2251,2690,"1083",
"Rashad McCants","2002-2004",2007,2635,"249",
"Kenny Smith","1983-1985",2017,2655,"737",
)


# create table with batting averages
table <- minutes %>%
  mutate(pct = (MP / TM)) %>%
  arrange(-pct) %>%
  select(Name, Seasons, MP, TM, pct, NBA_G) %>%
  gt() %>%
  opt_row_striping() %>% # stripe rows
  tab_header(title = 'Carolina: Players with at least 70% of possible minutes played in first two collegiate seasons',
             subtitle = 'Shows percentage of possible minutes played in first two seasons dating back to 1980-81 season, plus career NBA games played.') %>%
  cols_label(
    pct = '',
    MP = 'Minutes',
    TM = 'Total',
    Name = 'Player',
    NBA_G = img_header(
      '',
      'https://andscape.com/wp-content/uploads/2017/06/nbalogo.jpg?w=700',
      height = 30,
      palette = c('white", "white')
    ),
  ) %>% 
  cols_align(# align columns left
    align = 'right',
    columns = c(pct, NBA_G)) %>%
  fmt_percent(columns = pct,
              decimals = 1) %>%
  gt_merge_stack(col1 = Name, col2 = Seasons) %>%
  gt_highlight_rows(
    rows = 6,
    fill = '#ffffd4',
    bold_target_only = TRUE,
    target_col = pct
  ) %>%
  gt_theme_538() %>%
  tab_source_note(source_note = '@dadgumboxscores | June 17, 2022 | data via sports-reference.com')  %>%
  # fix font of source note and set stripe color
  tab_options (
    source_notes.font.size = px(10),
    row.striping.background_color = '#dbebf6',
  )

# save as image
gtsave(table,
             'minutes.png',
             vwidth = 500,
             vheight = 430)
