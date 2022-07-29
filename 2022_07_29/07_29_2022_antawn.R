# libraries
library(tidyverse)
library(gt)
library(gtExtras)
library(webshot2)


# make table from stats from the acc
tawn <- tibble::tribble(
~T, ~P, ~R,
"Middle Tenn St",0,0,
"at Richmond",17,6,
"California",26,7,
"UCLA",23,11,
"Seton Hall",25,5,
"Purdue",23,9,
"Louisville",21,12,
"Chattanooga",21,13,
"Virginia Tech",19,11,
"Princeton",6,9,
"Hampton",28,15,
"at Florida State",22,9,
"at Georgia",26,11,
"at Bethune-Cookman",22,5,
"at Clemson",19,11,
"Georgia Tech",20,8,
"Virginia",26,10,
"at Maryland",27,13,
"Appalachian State",25,9,
"at NC State",36,14,
"Florida State",24,9,
"Clemson",14,5,
"at Wake Forest",21,14,
"Duke",35,11,
"at Georgia Tech",31,7,
"at Virginia",19,14,
"Maryland",36,16,
"NC State",20,8,
"Wake Forest",15,7,
"at Duke",23,13,
"NC State",25,7,
"Maryland",15,9,
"Duke",22,18,
"Navy",17,14,
"Charlotte",19,12,
"Michigan State",20,14,
"Connecticut",20,11,
"Utah",14,12,
)

# create table
table <- tawn %>%
  mutate(row_number = 1:n()) %>%
  select(T, P, R) 


# add tabs
tab1 = table %>%
  slice(2:20) %>%
  gt() %>%
  opt_row_striping() %>% # stripe rows
  cols_label(
    T = "Opponent",
    P = 'Points',
    R = 'Rebounds',
  ) %>% 
  cols_align(# align columns left
    align = "left",
    columns = c(P, R)) %>%
  tab_header(title = "Antawn Jamison: 1997-98 Points and Rebounds", ) %>%
  gt_theme_538() %>%
  # fix font of source note and set stripe color
  tab_options (
    source_notes.font.size = px(10),
    row.striping.background_color = '#dbebf6',
    column_labels.text_transform = 'capitalize',
  ) 

tab2 = table %>%
  slice(21:38) %>%
  gt() %>%
  opt_row_striping() %>% # stripe rows
  cols_label(
    T = "Opponent",
    P = 'Points',
    R = 'Rebounds',
  ) %>% 
  cols_align(# align columns left
    align = "left",
    columns = c(P,R)) %>%
  tab_header(title = "Game-by-Game (37 games played)", ) %>%
  gt_theme_538() %>%
  tab_source_note(source_note = "@dadgumboxscores | July 29, 2022 | data via theacc.com")  %>%
  # fix font of source note and set stripe color
  tab_options (
    source_notes.font.size = px(10),
    row.striping.background_color = '#dbebf6',
    column_labels.text_transform = 'capitalize',
  ) 

# combine tabs
listed_tables <- list(tab1, tab2)


# save as image
gt_two_column_layout(listed_tables)