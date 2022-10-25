
# load libraries
library(cfbfastR)
library(tidyverse)
library(grid)
library(ggtext)
library(gt)
library(gtExtras)
library(cfbplotR)

# grad SP rankings
try <- cfbd_ratings_sp(year = 2022) %>% 
       filter(conference == "ACC") %>%
       select(team, ranking, offense_ranking, defense_ranking)

# set max and min
offense_ranking_min <- 0
offense_ranking_max <- 120
defense_ranking_min <- 0
defense_ranking_max <- 120

# make the plot, inspired from https://gist.github.com/Henryjean/9d6c624ce1768f479c6046cb08803a56
sp_plot <- try %>%
        ggplot(aes(x = offense_ranking, y = defense_ranking)) +
        annotate("rect", xmin = (offense_ranking_max + offense_ranking_min) / 2, xmax = offense_ranking_max, 
           ymin = defense_ranking_min, ymax = (defense_ranking_max + defense_ranking_min) / 2, fill= "#f7f7f7", alpha = .5, color = 'transparent') + 
  annotate("rect", xmin = offense_ranking_min, xmax = (offense_ranking_max + offense_ranking_min) / 2, 
           ymin = (defense_ranking_max + defense_ranking_min) / 2, ymax = defense_ranking_max, fill= "#f7f7f7", alpha = .5, color = 'transparent') +
  annotate("rect", xmin = (offense_ranking_max + offense_ranking_min) / 2, xmax = offense_ranking_max,
           ymin = (defense_ranking_max + defense_ranking_min) / 2, ymax = defense_ranking_max, fill= "#ffb3de", alpha = .5, color = 'transparent')  +
  annotate("rect", xmin = offense_ranking_min, xmax = (offense_ranking_max + offense_ranking_min) / 2,
           ymin = defense_ranking_min, ymax = (defense_ranking_max + defense_ranking_min) / 2, fill= "#addfad", alpha = .5, color = 'transparent') +
geom_cfb_logos(aes(team = team), width = 0.08, alpha = 0.75, angle = -1*45) +
  scale_y_continuous(limits = c(defense_ranking_min, defense_ranking_max)) +  
  scale_x_continuous(limits = c(offense_ranking_min, offense_ranking_max)) +
  coord_equal() +
    labs(
    x = "Offensive Ranking",
    y = "Defensive Ranking"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=(-1 *45), hjust = 0.5, margin = margin(t = -9.5)),
        axis.text.y = element_text(angle=(-1 *45), hjust = 0.5, margin = margin(r = -5)),
        axis.title.x = element_text(size = 12,
                                    #angle=(-1 * 45 + 45),
                                    vjust = 0.5, 
                                    margin = margin(t = 10),
                                    face = 'bold',
                                    color = "black"),
        axis.title.y = element_text(size = 12,
                                    angle=(-1 * 45 - 45),
                                    hjust = 0.5,
                                    margin = margin(r = 10),
                                    color = "black", 
                                    face = 'bold'), 
        plot.margin = margin(1.15, .5, .5, -.25, unit = 'in'), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")) +
  annotate(geom = 'text', x =120, y = 120, label = "2022 SP+ Rankings for ACC teams", angle = -1 * 45, vjust = -3.5, fontface = 'bold', size = 4, ) +
  annotate(geom = 'text', x = 119, y = 119, label = paste0("inspiration via @owenlhjphillips and data as of ", format.Date(Sys.Date(), "%B %d, %Y"), ""), angle = -1 * 45, vjust = -2.5, size = 2.5, )  +
  annotate(geom = 'text', x = 5, y = 5, label = "data via cfbfastR | @dadgumboxscores", angle = -1 * 45, hjust = -0.55, vjust = 5, size = 2.5, )  +
  geom_richtext(aes(x = 25, y = 110, label = "Good O, Bad D"), angle = -1 * 45, size = 2.75, , fontface = 'bold', color = 'black', fill = "#f7f7f7")  +
  geom_richtext(aes(x = 100, y = 20, label = "Bad O, Good D"), angle = -1 * 45,  size = 2.75, ,  fontface = 'bold', color = 'black', fill = "#f7f7f7")   +
  geom_richtext(aes(x = 100, y = 100, label = "Bad O, Bad D"), angle = -1 * 45, size = 2.75, , fontface = 'bold', color = 'black',  fill = "#f7f7f7")  +
  geom_richtext(aes(x = 10, y = 10, label = "Good O, Good D"), angle = -1 * 45,  size = 2.75, , fontface = 'bold', color = 'black', fill = "#f7f7f7") 


# save plot
png("sp_plot.png", res = 300, width = 6, height = 6, units = 'in', bg = 'floralwhite')

print(sp_plot, vp=viewport(angle=45,  
                          width = unit(6, "in"), 
                          height = unit(6, "in")))

dev.off()


# make a gt table
table <- try %>%
  mutate(diff = offense_ranking - defense_ranking) %>%
  arrange(ranking) %>%
  gt() %>%
  tab_header(title = '2022: SP+ Rankings for ACC Teams',
             subtitle = 'Rankings through games of October 20.') %>%
  cols_label(
    team = 'Team',
    ranking = "Rank",
    offense_ranking = 'Offense',
    defense_ranking = 'Defense',
    diff = "delta"
  ) %>%
  gt_theme_dot_matrix() %>%
  tab_source_note(source_note = '@dadgumboxscores | October 20, 2022 | data via cfbfastR')  %>%
  # fix font of source note and set stripe color
  tab_options (
    source_notes.font.size = px(10),
    row.striping.background_color = '#E3E3E4',
  )

gtsave(table, 'sp_rank.png', vwidth = 650)