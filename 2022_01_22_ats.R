library(tidyverse)
library(gt)
library(gtExtras)

hubie<-tibble::tribble(
~Opponent,~Result,~Location,~Pts,~Opp,~Spread,~ATS,~Total,~OU,
"Loyola (MD)","W","H",83,67,-24,"L",153,"U",
"Brown","W","H",94,87,-24.5,"L",147,"O",
"College of Charleston","W","A",94,83,-12.5,"L",163,"O",
"Purdue","L","N",84,93,7.5,"L",152,"O",
"Tennessee","L","N",72,89,2.5,"L",154.5,"O",
"UNC-Asheville","W","H",72,53,-23.5,"L",158,"U",
"Michigan","W","H",72,51,2.5,"W",151.5,"U",
"Georgia Tech","W","A",79,62,-3.5,"W",150,"U",
"Elon","W","H",80,63,-21,"L",151,"U",
"Furman","W","H",74,61,-10.5,"W",156,"U",
"Kentucky","L","N",69,98,3,"L",145,"O",
"App State","W","H",70,50,-16,"W",141,"U",
"Boston College","W","A",91,65,-8,"W",138.5,"O",
"Notre Dame","L","A",73,78,-1.5,"L",144.5,"O",
"Virginia","W","H",74,58,-5,"W",132,"-",
"Georgia Tech","W","H",88,65,-12,"W",145.5,"O",
"Miami","L","A",57,85,-2,"L",153.5,"U",
)


hubie %>%
  gt() %>%
  cols_label(OU = "O/U") %>%
  data_color(columns = c(Pts),
             colors = scales::col_numeric(
               palette = c("#e7fff5", "#ceffeb", "#b3ffe1", "#94ffd7"),
               domain = NULL
             )) %>%
  data_color(columns = c(Opp),
             colors = scales::col_numeric(
               palette = c("#ffe3e3", "#ffc7c8", "#feaaae", "#fa8d94"),
               domain = NULL
             )) %>%
  summary_rows(
    groups = NULL,
    columns = c(Pts, Opp),
    fns = list("sum"),
    formatter = fmt_number,
    decimals = 0
  ) %>%
  data_color(#Updatecellcolors...
    columns = c(ATS),
    #...forsuppcolumn!
    colors = scales::col_factor(
      #<-bcit'safactor
      palette = c("#ffffff", "#daeaf5"),
      #Twofactorlevels,twocolors
      domain = c("L", "W")#Levels
    )) %>%
  data_color(#Updatecellcolors...
    columns = c(Result),
    #...forsuppcolumn!
    colors = scales::col_factor(
      #<-bcit'safactor
      palette = c("#ffffff", "#daeaf5"),
      #Twofactorlevels,twocolors
      domain = c("L", "W")#Levels
    )) %>%
  data_color(#Updatecellcolors...
    columns = c(OU),
    #...forsuppcolumn!
    colors = scales::col_factor(
      #<-bcit'safactor
      palette = c("#ffc6bb", "#ffffff"),
      #Twofactorlevels,twocolors
      domain = c("O", "U", "-")#Levels
    )) %>%
  fmt(
    columns = c(Spread),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) %>%
  tab_header(title = "Carolina 2021-22: Record Against the Spread",
             subtitle = "Shows result, point-spread, and total for each game this season.") %>%
  tab_source_note(source_note = "Source:data via covers.com | @dadgumboxscores") %>%
  gt_theme_538()