library(tidyverse)
library(gt)
library(gtExtras)
library(cfbplotR)
library(janitor)

# scrape data from unc stats https://s3.amazonaws.com/sidearm.sites/unc.sidearmsports.com/documents/2022/9/28/FHnotes_2022_Liberty.pdf
fockey <- tibble::tribble(
  ~Opponent,~Result,~G,~GA,~SOG,~SOGA,
  "Michigan","W",5,1,14,5,
  "Iowa","W",3,2,14,2,
  "Princeton","W",4,3,6,6,
  "Penn","W",4,0,13,2,
  "California","W",7,0,15,1,
  "Stanford","W",6,0,12,1,
  "Louisville","W",3,0,12,3,
  "Wake Forest","W",2,0,14,2,
  "Liberty","W",6,3,11,6
)


true <- fockey %>%
  adorn_totals("row", "col") %>%
  mutate(# change names to be abbreviated
    Result = case_when(Result == 'col' ~ 'Record: 9-0',
                       TRUE ~ Result)) %>%
  mutate(logo = Opponent) %>%
  select(logo, Opponent, Result, G, GA, SOG, SOGA) %>%
  mutate(
    logo = if_else(str_detect(logo, 'Total'), 'North Carolina', logo),
    Opponent = if_else(str_detect(Opponent, 'Total'), '', Opponent)
  )

table <- true %>%
  gt() %>%
  tab_header(
    title = md(
      "🏑 Carolina has scored **40 goals** this season. Opponents have _28 shots on goal_"
    ),
    subtitle = "Table shows goals scored, goals allowed, shots on goals, and shots on goal allowed."
  ) %>%
  tab_source_note(source_note = "@dadgumboxscores | October 5, 2022 | data via goheels.com")  %>%
  # add theme
  gt_theme_538() %>%
  opt_row_striping() %>% # stripe rows
  gt_fmt_cfb_logo(columns = "logo") %>%
  gt_highlight_rows(
    rows = 10,
    fill = '#EDF5FA',
    bold_target_only = TRUE,
    target_col = c(G, SOGA)
  ) %>%
  tab_options (
    # adjusts row striping color and font sizes, and text transform
    source_notes.font.size = px(10),
    table.font.size = px(12),
    row.striping.background_color = '#EDF5FA'
  )

#
# save image
gtsave_extra(table,
             "fockey_chart.png",
             vwidth = 600,
             vheight = 600)