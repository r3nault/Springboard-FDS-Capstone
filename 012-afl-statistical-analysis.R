library(dplyr)

load(file=".\\afl_by_match_idx.Rda")



# Re-randomization
AFL.by.match.idx.random <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% mutate(team_result = sample(0:1, nrow(AFL.by.match.idx.random), replace = TRUE))
AFL.by.match.idx.random.res <- AFL.by.match.idx.random %>% group_by(team_result) %>%  summarise_all(funs(mean))

for(i in 2:1000){
  AFL.by.match.idx.random$team_result <- sample(0:1, nrow(AFL.by.match.idx.random), replace = TRUE)
  AFL.by.match.idx.random.res <- rbind(AFL.by.match.idx.random.res, AFL.by.match.idx.random %>% group_by(team_result) %>% summarise_all(funs(mean)))
}

Xbar <- AFL.by.match.idx %>% select(team_result, starts_with("idx_")) %>% group_by(team_result) %>% summarise(mean(idx_win_aerial_ball))
print(as.list(AFL.by.match.idx.random.res %>% filter(team_result == 1 & (idx_win_ground_ball <= Xbar[[2]][[1]] | idx_win_ground_ball >= Xbar[[2]][[2]])) %>% count()))

save(AFL.by.match.idx.random.res, file = ".\\AFL_by_match_idx_random_res.Rda")


# Sample of 1000
AFL.by.match.idx.sample <- AFL.by.match.idx %>% sample_n(1000)
save(AFL.by.match.idx.sample, file=".\\AFL_by_match_idx_sample.Rda")

  # idx_win_ground_ball
  AFL.by.match.idx.sample %>% group_by(team_result) %>% summarise(mean(idx_win_ground_ball), sd(idx_win_ground_ball), n())
  
  # idx_win_aerial_ball
  AFL.by.match.idx.sample %>% group_by(team_result) %>% summarise(mean(idx_win_aerial_ball), sd(idx_win_aerial_ball), n())