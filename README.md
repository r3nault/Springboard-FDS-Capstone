# Springboard-FDS-Capstone
Springboard Foundations of Data Science Capstone project

# Data source
afltables.com

# Data preparation
Scraping player data
- scrape-afltables-playermatch-stats.R
- Raw data saved as: playerstats_1998to2005.Rda, playerstats_2006to2011.Rda, playerstats_2012to2017.Rda
Scraping match results data
- Copied into Excel match-results-prep.xlsx, formulas used to identify required data
- prepare-afltable-match-results.R
- Raw data saved as: teamresults_1998to2017.Rda
Data wrangling and initial analysis
- 010-afl-exploratory-players-vs-wins.R
- Prepared player data (raw): player_metrics.Rda
- Season averages of player data: mean_metrics_by_year.Rda
- Prepared team results (raw): team_results.Rda
- Prepared match-wise view of indices: team_indices_by_match.Rda
- Prepared team-wise view of indices (long): team_indices_by_round_tidy.Rda
- Prepared team-wise view of indices (wide): team_indices_by_round.Rda
