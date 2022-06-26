library(gt)
library(gtExtras)

# scrape nba dot com and build csv: https://www.nba.com/stats/draft/history/
nba <- read.csv('nba.csv')

# find top picks
top <- nba %>%
  count(College, Year, name = "total_picks") %>%
  arrange(-total_picks) %>%
  filter(total_picks > 3)

# write csv to edit offline
write.csv(top, 'top.csv')

# add results from sports-reference for results: https://www.sports-reference.com/cbb/
results <- read.csv('top.csv')

# make a table of those results
table <- results %>%
  arrange(-Year) %>%
  select(title, Year, College, finish, total_picks) %>%
  gt() %>%
  tab_header(title = 'NBA Draft: Schools with at least four picks in the same draft',
             subtitle = 'Dates back to the 1989 NBA draft or when the draft changed to two rounds.') %>%
  cols_label(
    title = '',
    College = 'School',
    total_picks = 'Picks',
    finish = 'Tourney Result'
  ) %>%
  # add trophy emojis
  gt_fa_repeats(
    column = title,
    palette = "orange",
    name = "trophy",
    align = 'left'
  ) %>%
  gt_theme_dot_matrix() %>%
  tab_source_note(source_note = '@dadgumboxscores | June 26, 2022 | data via nba.com & sports-reference.com')  %>%
  # fix font of source note and set stripe color
  tab_options (
    source_notes.font.size = px(10),
    row.striping.background_color = '#E3E3E4',
    column_labels.text_transform = 'capitalize'
  )

# save image
gtsave(table,
       'picks.png',
       vwidth = 475,
       vheight = 730)

# lottery picks
lottery <- nba %>%
  filter(PICK < 14) %>%
  count(College, Year, name = "total_picks") %>%
  arrange(-total_picks) %>%
  filter(total_picks > 2) 