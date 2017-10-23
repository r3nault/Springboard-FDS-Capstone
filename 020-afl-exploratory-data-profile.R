library(ggplot2)

# player data
load(file=".\\player_metrics.Rda")

# player metrics: Contested_possessions, Disposals, Handballs, Kicks, Marks, Tackles
ggplot(player.metrics, aes(x=Contested_possessions)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Year ~ .)

ggplot(player.metrics, aes(x=Disposals)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Year ~ .)

ggplot(player.metrics, aes(x=Handballs)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Year ~ .)

ggplot(player.metrics, aes(x=Kicks)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Year ~ .)

ggplot(player.metrics, aes(x=Marks)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Year ~ .)

ggplot(player.metrics, aes(x=Tackles)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Year ~ .)


# team round data
load(file=".\\team_metrics_by_round.Rda")

# team averages per match: Bounces, Clangers, Clearances, Contested_marks, Contested_possessions
# Disposals, Frees_against, Goal_assists, Handballs, Inside_50s, Kicks, Marks, Marks_inside_50, One_pct, Tackles
ggplot(team.metrics.by.round, aes(x=Bounces)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Clangers)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Clearances)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Contested_marks)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Contested_possessions)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Disposals)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Frees_against)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Goal_assists)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Handballs)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Inside_50s)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Kicks)) +
  geom_histogram(binwidth = 1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Marks)) +
  geom_histogram(binwidth = .5) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Marks_inside_50)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=One_pct)) +
  geom_histogram(binwidth = .1) +
  facet_grid(Year ~ .)

ggplot(team.metrics.by.round, aes(x=Tackles)) +
  geom_histogram(binwidth = .5) +
  facet_grid(Year ~ .)