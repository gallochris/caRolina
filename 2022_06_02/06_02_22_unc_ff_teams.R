# load libraries
library(tidyverse)
library(gt)
library(gtExtras)

# load csv from sports-reference
ff <- read.csv('unc_ff_teams_movs.csv')

# change seasons to only use years
ff$Year <-
  str_replace_all(
    ff$Year,
    c(
      "2004-05" = "2005",
      "2007-08" = "2008",
      "2008-09" = "2009",
      "2015-16" = "2016",
      "2016-17" = "2017",
      "2021-22" = "2022"
    )
  )

# add clutch minutes + create data set from old box scores (holy manual work!)
clutch <- tibble::tribble(
~Year,~Clutch_Mins,~Gs,~Titles,
"2005","~11:30",6,1,
"2008","~1:00",5,0,
"2009","None!",6,1,
"2016","~4:00",6,0,
"2017","~17:00",6,1,
"2022","~21:30",6,0,
)

# calculate point differential + median margin of victory
movs <- ff %>%
  group_by(Year) %>%
  summarize(mean = mean(MOV),
            diff = sum(MOV))

# combine total games + movs
totals <- merge(movs, clutch, by = "Year")

# make table
ff_table <- totals %>%
  arrange(-diff) %>% # sort by best differential
  select(Year, Titles, Clutch_Mins, Gs, mean, diff) %>% # adjust order of columns
  gt() %>%
  # set title + caption + subtitle
  tab_header(title = "Carolina: Comparing Final Four Teams Since 2005",
             subtitle = "Average margin of victory, total clutch minutes in NCAA Tournament, and tourney point differential.") %>%
  tab_source_note(source_note = "@dadgumboxscores | June 2, 2022")  %>%
  # fix font of source note
  tab_options (source_notes.font.size = px(10),
               table.font.size = px(12)) %>%
  # add theme
  gt_theme_nytimes() %>%
  # add trophy emojis
  gt_fa_repeats(
    column = Titles,
    palette = "orange",
    name = "trophy",
    align = 'left'
  ) %>%
  fmt_number(# make this avg include 1 decimal
    columns = c(mean),
    decimals = 1,) %>%
  fmt(
    # format differential + median mov columns
    columns = c(diff),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) %>%
  fmt_number(# make these numbers without decimals
    columns = c(diff, Gs),
    decimals = 0,) %>%
  cols_label(
    # rename columns
    mean = "Avg. Win Margin",
    diff = "+/-",
    Titles = "",
    Gs = "Total games",
    Clutch_Mins = "Clutch Time"
  )


# save as image
gtsave_extra(ff_table, "ff_table.png", vwidth = 500, vheight = 430)
