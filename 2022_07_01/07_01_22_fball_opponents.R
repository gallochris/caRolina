library(tidyverse)
library(gt)
library(gtExtras)

# load csv using play index from sports-reference
fball <- read.csv('unc_results.csv')

# find records
wins <- fball %>%
  count(Opponent, Result) %>%
  filter(Result == 'W') %>%
  arrange(-n)

losses <- fball %>%
  count(Opponent, Result) %>%
  filter(Result == 'L') %>%
  arrange(-n)

# combine the records + include NAs
all <- merge(wins, losses, by = 'Opponent', all = TRUE)

# change NAs to 0s
all[is.na(all)] <- 0

table <- all %>%
  mutate(T = n.x + n.y) %>%
  arrange(-T) %>%
  mutate(row_number = 1:n()) %>%
  select(row_number, Opponent, n.x, n.y, T) %>%
  filter(T > 3) %>% # only opponents with at least 4
  mutate(   # change names to be abbreviated
    Opponent = case_when(
      Opponent == 'North Carolina State' ~ 'NC State',
      Opponent == 'Miami (FL)' ~ 'Miami',
      TRUE ~ Opponent,
    )
  ) %>%
  gt() %>%
  cols_label(#rename columns
    row_number = "",
    n.x = "W",
    n.y = "L",
    T = "Games") %>%
  summary_rows(
    # summarize the rows
    columns = c(n.x, n.y, T),
    fns = list("sum"),
    formatter = fmt_number,
    decimals = 0,
    use_seps = FALSE
  ) %>%
  tab_header(title = "Carolina: Record by opponent since 2000",
             subtitle = "Only shows opponents with at least 4 games played.") %>%
  tab_source_note(source_note = "@dadgumboxscores | July 1, 2022 | data via sports-reference.com")  %>%
  # add theme
  gt_theme_dot_matrix() %>%
  tab_options (   # adjusts row striping color and font sizes, and text transform
    source_notes.font.size = px(10),
    table.font.size = px(12),
    row.striping.background_color = '#d0e4f3',
    column_labels.text_transform = 'capitalize',
  )


# save as image
gtsave(table,
       'fball.png',
       vwidth = 600,
       vheight = 430)
