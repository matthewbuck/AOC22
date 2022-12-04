library('tidyverse')

x = 'path/to/input'
df <- read_delim(x, delim = '\n', skip_empty_rows = FALSE, col_names = FALSE, col_types = 'd') %>%
  mutate(group = cumsum(is.na(X1))) %>%
  drop_na(X1) %>%
  group_by(group) %>%
  summarize(cals = sum(X1)) %>%
  arrange(desc(cals))

answer <- max(df$cals)

# Part 2

top_three <- head(df, 3)
answer_two <- sum(top_three$cals)
