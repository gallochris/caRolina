# scrape results from acc website: https://theacc.com/schedule.aspx?schedule=3437

# make a csv and read it
base <- read.csv('uncbaseball_scores.csv')

# load libraries
library(gt)
library(gtExtras)
library(dplyr)


# find wins
wins <- base %>%
  group_by(month) %>%
  filter(result == "W") %>%
  count(result, name = "W") %>%
  ungroup()

# count losses by month
losses <- base %>%
  group_by(month) %>%
  filter(result == "L") %>%
  count(result, name = "L") %>%
  ungroup()

# count differential
diff <- base %>%
  group_by(month) %>%
  summarise(Diff = sum(runs))

# combine wins and losses
results <- merge(wins, losses, by = "month")

# combine results with differential
totals <- merge(results, diff, by = "month")


# make table
totals %>%  # first, re-arrange the order of the rows
  mutate(Month = fct_relevel(month, c('February', 'March', 'April', 'May'))) %>%
  arrange(Month) %>%
  select(Month, W, L, Diff) %>%
  gt() %>%
  fmt_number(
    columns = c(W, L, Diff),
    # use commas
    use_seps = TRUE,
    decimals = 0,
  ) %>%
  opt_row_striping() %>% # stripe rows
  tab_header(title = "Carolina Baseball: Record and Run Differential by Month",) %>%
  cols_label(Diff = "+/-",) %>%
  summary_rows(
    # summarize the rows
    columns = c(W, L, Diff),
    fns = list("sum"),
    formatter = fmt_number,
    decimals = 0,
    use_seps = FALSE
  ) %>%
  fmt(
    # format differential columns
    columns = c(Diff),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) %>%
  cols_align(# align columns left
    align = "left",
    columns = Month) %>%
  gt_theme_538() %>%
  tab_source_note(source_note = "@dadgumboxscores | May 29, 2022")  %>%
  # fix font of source note and set stripe color
  tab_options (
    source_notes.font.size = px(10),
    row.striping.background_color = "#dbebf6",
  )
