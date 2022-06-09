# libraries
library(tidyverse)
library(gt)
library(gtExtras)
library(numform)
library(ggrepel)


# make table from stats + record book: https://s3.amazonaws.com/sidearm.sites/unc.sidearmsports.com/documents/2022/2/9/2022BSBRecordBook.pdf
# stats archive: https://goheels.com/sports/2012/8/31/205672102.aspx
hitters <- tibble::tribble(
~Name, ~Year, ~Hits, ~G, ~AB, ~BB,
"Dustin Ackley",2007, 119, 73, 296, 30, 
"Dustin Ackley",2008, 116, 68, 278, 53,
"Tim Fedroff",2008, 115, 67, 285, 35, 
"Chad Flack",2006, 112, 67, 292, 11, 
"Dustin Ackley",2009, 111, 66, 266, 50,
"Josh Horton",2006, 107, 68, 271, 33,
"Kyle Seager",2009, 103, 66, 262, 45, 
"Jeremy Cleveland",2003, 103, 65, 251, 37,  
"Brian Roberts",1997, 102, 60, 239, 38,  
"Cody Stubbs", 2013, 101, 71, 276, 38,
"Angel Zarate", 2022, 99, 62, 275, 30,   
"Danny Serretti", 2022, 93, 62, 255, 32,
)

# find avg and remove leading zeroes
with_avg <- hitters %>%
  mutate(avg = f_num((Hits / AB), retain.leading.zero = FALSE, digits = 3))


# create table with batting averages
table <- with_avg %>%
  mutate(row_number = 1:n()) %>%
  select(row_number, Name, Year, G, Hits, AB, avg) %>%
  arrange(-Hits) %>%
  gt() %>%
  opt_row_striping() %>% # stripe rows
  tab_header(title = "Carolina: Players with 100 hits in a single season", ) %>%
  cols_label(
    avg = "AVG",
    G = 'Games',
    Year = 'Season',
    Name = 'Player',
    row_number = '',
  ) %>%
  cols_align(# align columns left
    align = "right",
    columns = avg) %>%
  gt_merge_stack(col1 = Name, col2 = Year) %>%
  gt_highlight_rows(
    rows = 11:12,
    fill = '#ffffd4',
    bold_target_only = TRUE,
    target_col = Hits
  ) %>%
  gt_theme_538() %>%
  tab_source_note(source_note = "@dadgumboxscores | June 9, 2022")  %>%
  # fix font of source note and set stripe color
  tab_options (
    source_notes.font.size = px(10),
    row.striping.background_color = '#dbebf6',
    column_labels.text_transform = 'capitalize',
  )

# save as image
gtsave_extra(table,
             "hits_table.png",
             vwidth = 500,
             vheight = 430)


# create scatterplot of at-bats and hits
# first, add the theme
theme_me <- function () {
  theme_minimal(base_size = 15, base_family = "RobotoCondensed-Regular") %+replace%
    theme (
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(
        hjust = 0.5,
        vjust = -2,
        lineheight = 0.9,
        size = 10
      ),
      plot.caption = element_text(size = 8, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#c9e1f1", color = "#c9e1f1")
    )
}


# make the plot
hits <- hitters %>%
  mutate(color = case_when(Year == '2022' ~ "color")) %>%
  ggplot(aes(
    x = AB,
    y = Hits,
    label = paste0(Name, ', ', Year)
  )) +
  geom_point(
    color = 'black',
    fill = '#c4ced4',
    shape = 21,
    size = 2,
    alpha = .75,
    aes(color = color)
  ) +
  geom_text_repel(aes(colour = color), size = 4.5) +
  scale_fill_manual(values = c("#acacac",
                               "#e18c95")) +
  xlim(230, 300) +
  ylim(90, 120) +
  theme_me() +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 100,
             lty = 2,
             color = '#acacac') +
  labs(
    x = 'At-Bats',
    y = 'Hits',
    title = 'Carolina: Players with 100 hits or more in a single season',
    subtitle = 'Shows the total at-bats in the season and total hits.',
    caption = '@dadgumboxscores | June 9, 2022'
  )

# save the chart
ggsave(
  "hits.png",
  hits,
  w = 8,
  h = 10,
  dpi = 300,
  type = 'cairo'
)