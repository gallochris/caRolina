# load packages 
library(cfbfastR)
library(cfbplotR)
library(tidyverse)

# load defensive drives 
def_drives <- cfbd_drives(
  2022,
  season_type = "regular",
  defense_team = "North Carolina"
) 

# fix bad data 
def_drives$end_offense_score[def_drives$end_offense_score == 111] <- 61

# manipulate data
def <- def_drives %>%
       mutate(points = (end_offense_score - start_offense_score),
              time = paste(sprintf('%02d', time_minutes_elapsed), ":",
                           sprintf('%02d', time_seconds_elapsed),
                            sep = ""),
              yds_play = round(yards/plays, 1)) %>%
        mutate(yds_play = replace_na(yds_play, 0)) %>%
        rename(opponent = offense) %>%
        select(opponent, plays, yards, points, time, yds_play) 

# add theme
 theme_me <- function () {
  theme_minimal(base_size = 10, base_family = "RobotoCondensed-Regular") %+replace%
    theme (
      plot.title = element_text(
        hjust = 0.5,
        size = 18,
        face = "bold"
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 10,
        lineheight = 0.25,
        vjust = -0.5
      ),
      plot.caption = element_text(
        hjust = 1,
        size = 6,
        lineheight = 0.35,
        margin = margin(t = 20)
      ),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "floral white", color = "floral white")
    )
}

# save image
ggsave(
  "um.png",
  um,
  w = 10,
  h = 8,
  dpi = 300,
  type = 'cairo'
)

# reorder levels 
def$opponent <- factor(def$opponent, levels=c("Florida A&M", "Appalachian State", "Georgia State", "Notre Dame", "Virginia Tech", "Miami"))

# build plot 
um <- def %>%
ggplot(aes(opponent, yds_play, color = factor(points))) +
geom_point(size = 3) +
coord_flip() +
scale_color_brewer(palette = "Paired", name = "Points") +
theme_me() +
theme(legend.position = "bottom") +
theme(axis.text.y = element_cfb_logo()) +
labs(
    x = "",
    y = "Yards per Play",
    title = "Carolina Defense: points allowed and yards per play for all drives",
    caption = "@dadgumboxscores | October 10, 2022 | data via cfbfastR"
  ) +
  annotate(
    "text",
    x = "Virginia Tech",
    y = 17,
    label = "Miami's 7.7 yards per play drive \n (12 plays, 92 yards) is longest by \n an opponent without scoring points",
    family = "Chalkboard Bold",
    size = 3.3,
    color = "red"
  ) +
  annotate(
    geom = "curve",
    y = 13.8,
    x = "Virginia Tech",
    yend = 7.5,
    xend = 5.8,
    curvature = -.3,
    arrow = arrow(length = unit(2, "mm")),
    color = "red"
  )