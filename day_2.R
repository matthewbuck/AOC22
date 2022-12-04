library('tidyverse')

input <- 'c:/users/mbbuc/documents/aoc22/day 2/input.txt'

df <- read_delim(input, delim = '\n', col_names = F) %>%
  separate(X1, c('opp', 'move')) %>%
  mutate(move = case_when(
    move == 'X' ~ 'A',
    move == 'Y' ~ 'B', 
    move == 'Z' ~ 'C'
  )) %>%
  mutate(pts = case_when(
    move == 'A' ~ 1,
    move == 'B' ~ 2,
    move == 'C' ~ 3
  )) %>%
  mutate(win_move = case_when(
    opp == 'A' ~ 'B',
    opp == 'B' ~ 'C',
    opp == 'C' ~ 'A'
  )) %>%
  mutate(outcome = case_when(
    move == win_move ~ 6,
    move == opp ~ 3,
    TRUE ~ 0
  )) %>%
  rowwise() %>%
  mutate(score = sum(pts, outcome)) %>%
  ungroup()

answer <- sum(df$score)

# Part 2

df2 <- df %>%
  mutate(move = case_when(
    move == 'A' ~ 0,
    move == 'B' ~ 3,
    move == 'C' ~ 6
  )) %>%
  mutate(lose_move = case_when(
    opp == 'A' ~ 'C',
    opp == 'B' ~ 'A',
    opp == 'C' ~ 'B'
  )) %>%
  select(-c('pts','outcome','score')) %>%
  mutate(play = case_when(
    move == 0 ~ lose_move,
    move == 3 ~ opp,
    move == 6 ~ win_move
  )) %>%
  mutate(pts = case_when(
    play == 'A' ~ 1,
    play == 'B' ~ 2,
    play == 'C' ~ 3
  )) %>%
  rowwise() %>%
  mutate(score = sum(move,pts)) %>%
  ungroup()

answer2 <- sum(df2$score)
