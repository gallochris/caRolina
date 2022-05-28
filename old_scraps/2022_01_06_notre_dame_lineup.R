library(bigballR)

# theme 
theme_me <- function () {
  theme_minimal(base_size = 12, base_family = "RobotoCondensed-Regular") %+replace%
    theme (
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#c9e1f1", color = "#c9e1f1")
    )
}

# schedule
schedule <- get_team_schedule(season = "2021-22", team.name = "North Carolina")


# nd
nd_pbp <- get_play_by_play(schedule$Game_ID[16])

  game_data <- nd_pbp %>%
    dplyr::filter(Game_Seconds <= 2400) %>%
    dplyr::select(Game_Seconds, Home_Score, Away_Score, Home.1:Away.5) %>%
    tidyr::complete(Game_Seconds = 0:2400) %>%
    dplyr::mutate(First = Game_Seconds <= 1200) %>%
    dplyr::group_by(First) %>%
    tidyr::fill(-First, .direction = "up") %>%
    dplyr::ungroup() %>%
    dplyr::select(-First) %>%
    dplyr::distinct() %>%
    dplyr::select(Game_Seconds, Home_Score, Away_Score, dplyr::everything())

players <- unique(unlist(game_data[,4:13]))
players <- players[!is.na(players)]

get_player_stints <- function(game_data, player_name) {
  home <- player_name %in% unlist(game_data[,4:8])

  cols = if(home) 4:8 else 9:13

  is_on <- apply(game_data[,cols], 1, function(x){player_name %in% x})

  subs <- (is_on) & (lag(is_on) == F) | (is_on == T) & (lead(is_on) == F)
  subs[c(1,length(subs))] <- is_on[c(1,length(subs))]

  stint_data <- game_data %>%
    dplyr::select(Game_Seconds, Home_Score, Away_Score) %>%
    dplyr::mutate(Sub = cumsum(subs)) %>%
    dplyr::group_by(Sub) %>%
    dplyr::summarise(Start = min(Game_Seconds),
              Stop = max(Game_Seconds),
              Score_Dif = max(Home_Score) - min(Home_Score) - max(Away_Score) + min(Away_Score)
    ) %>%
    dplyr::mutate(Score_Dif = if(home) Score_Dif else -Score_Dif,
           Player = player_name,
           Team = home
    ) %>%
    dplyr::filter(Start != Stop, Sub %%2 == 1)

  return(stint_data)
}


  all_stints <- lapply(players, get_player_stints, game_data = game_data) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      Player = paste0(
        substr(Player,1,1),
        tolower(substr(Player,2,  sapply(Player, function(x){gregexpr("\\.", x)[[1]]})-1)),
        " ",
        substr(Player,sapply(Player, function(x){gregexpr("\\.", x)[[1]]})+1,sapply(Player, function(x){gregexpr("\\.", x)[[1]]})+1),
        tolower(substr(Player,sapply(Player, function(x){gregexpr("\\.", x)[[1]]})+2, nchar(Player)))
      ),
      Player = reorder(Player, -Start, FUN = max)
    )

nd_data <- all_stints %>% 
        filter(Team == FALSE) %>%
        mutate(Player = str_replace(Player, "^\\S* ", "")) %>%
        mutate(Player = replace(Player, Player == 'Mckoy', 'McKoy')) %>%
        mutate(Stint = (Stop - Start)) %>%
        group_by(Player) %>%
        mutate(Time = sum(Stint)) %>%
        ungroup() %>% 
        arrange(-Time) %>% 
        mutate(Opponent = 'Notre Dame')

nd <- ggplot2::ggplot(nd_data) +
    ggplot2::geom_linerange(ggplot2::aes(x= fct_reorder(Player, Time), ymin = Start, ymax = Stop), size = 2.4) +
    coord_flip() + 
    ggplot2::scale_y_continuous(breaks = c(0,600, 1200, 1800, 2400), 
    labels = c("1 minute", "10 minutes", "Halftime", "30 Minutes", "40 minutes")) +
    ggplot2::geom_hline(yintercept = c(0,1200,2400), color = "gray30", size = 1.5) +
    theme_me() + 
  labs(
    x = "",
    y = "",
    title = "Notre Dame 78, North Carolina 73",
  ) 
ggsave("nd.png", nd, w = 6, h = 4, dpi = 300, type = 'cairo')


## find lineups

unc <- get_lineups(play_by_play_data = nd_pbp)


lines <- unc %>%
  filter(Team == 'North Carolina') %>%
  arrange(-Mins)



lines <- tibble::tribble(
~one,~two,~three,~four,~five,~Mins,~O,~D,
"Love"," Davis"," Black"," Manek"," Bacot", 18.40, 34, 32,
"Davis"," Dunn"," Black"," Manek"," Bacot", 4.42, 7, 7,
"Love"," Davis"," Harris"," Manek"," Bacot", 4.05, 7, 7,
"Love"," Davis"," Harris"," Black"," Bacot", 3.70, 6, 8,
"Davis"," Dunn"," Harris"," Manek"," Bacot", 3.15, 6, 5,
"Love"," Davis"," Harris"," Black"," Manek", 2.47, 4, 5,
"Love"," Davis"," Harris"," Styles"," Bacot", 1.88, 4, 3,
"Love"," Davis"," Harris"," Styles"," Manek", 1.22, 2, 3,
"Love"," Dunn"," Harris"," Black"," Bacot", 0.73, 2, 1,
)

lines %>%
 mutate(r = 1:n()) %>%
  gt() %>%
  cols_move_to_start(
    columns = "r"
  ) %>%
  cols_label(
    r = "",
    one = "",
    two = "", 
    three = "",
    four = "",
    five = "",
    Mins = "Minutes",
    O = "O Poss",
    D = "D Poss",
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "#acacac", size = px(10))
      ),
    locations = cells_body(
      columns = r
    )
  ) %>%
  cols_width(
    r ~ px(2)
    ) %>%
  gtExtras::gt_theme_dot_matrix(color = "#e5eff8") %>%
  tab_header(
    title = "Carolina: Lineups against Notre Dame",
    subtitle = "Shows total minutes, offensive and defensive possession counts"
  ) %>%
  tab_source_note(source_note = "@dadgumboxscores | data via bigballR")  %>%
  # fix font of source note
  tab_options (
    source_notes.font.size = px(10),
    source_notes.padding = px(5),
    column_labels.text_transform = "uppercase",
  )