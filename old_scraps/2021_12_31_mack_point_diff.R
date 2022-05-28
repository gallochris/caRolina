library(rvest)
library(dplyr)
library(janitor)
library(stringr)

# load data
url <-
  'https://www.sports-reference.com/cfb/play-index/sgl_finder.cgi?request=1&match=game&year_min=2019&year_max=2021&school_id=north-carolina&c1stat=points&c1comp=gt&c1val=1&c2stat=opp_points&c2comp=gt&c2val=1&c3comp=gt&c4comp=gt&order_by=date_game#results'

mack_raw <- url %>%
  read_html() %>%
  html_elements('table') %>%
  html_table() %>%
  .[1] %>%
  as.data.frame() %>%
  clean_names()

# make first row the column headers
first_pass <- mack_raw %>%
  row_to_names(row_number = 1)

# remove other column header row
mack <- subset(first_pass, Rk != 'Rk')

# fix unamed columns
names(mack)[5] <- paste0('Location')
names(mack)[7] <- paste0('Result')

# change date to only use year
mack$Date <- str_extract(mack$Date, "[^-]+")

# remove non-power 5 opponents
mack <- mack %>%
  filter(
    Opponent != 'Mercer',
    Opponent != 'Georgia State',
    Opponent != 'Western Carolina',
    Opponent != 'Wofford',
    Opponent != 'Appalachian State'
  )

# change texas a&m bowl game to 2020 season
mack <- mack %>%
  mutate(Date = replace(Date, Opponent == 'Texas A&M', 2020))

# summarize point differential
mack_all <- mack %>%
  mutate(diff_raw = as.numeric(Pts) - as.numeric(Opp)) %>%
  group_by(Date) %>%
  mutate(PD = sum(diff_raw)) %>%
  select(Date, PD)

# show unique point differential totals
diff <- unique(mack_all)


# print it out
print(diff)

# A tibble: 3 × 2
# Groups:   Date [3]
  Date     PD
  <chr> <dbl>
1 2021    -21
2 2020    107
3 2019     76