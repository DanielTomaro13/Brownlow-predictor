#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
library(xgboost)
library(Matrix)
library(elo)
library(data.table)
library(caret)
library(stringr)
library(nnet)
#####################################################
afl_tables_stats <- fetch_player_stats_afltables(2005:2025)

colSums(is.na(afl_tables_stats))
unique(afl_tables_stats$Round)

# Remove finals games - Brownlow is regular season only
afl_tables_stats <- afl_tables_stats %>%
  filter(!Round %in% c("QF", "EF", "SF", "PF", "GF"))

# Replacing NA votes with 0 - removing rows with ID na (debut players) - removing all other columns
afl_tables_stats <- afl_tables_stats %>%
  mutate(Brownlow.Votes = ifelse(is.na(Brownlow.Votes), 0, Brownlow.Votes))
afl_tables_stats <- afl_tables_stats %>%
  filter(!is.na(ID))
afl_tables_stats <- afl_tables_stats %>%
  select(where(~ !any(is.na(.))))

team_name_mapping <- c(
  "Kangaroos" = "North Melbourne",
  "Western Bulldogs" = "Footscray",
  "Greater Western Sydney" = "GWS"
)

afl_tables_stats <- afl_tables_stats %>%
  mutate(
    Home.team = recode(Home.team, !!!team_name_mapping),
    Away.team = recode(Away.team, !!!team_name_mapping) 
  )

# Select Neccessary Columns
afl_tables_stats <- afl_tables_stats %>% mutate(
  Name = paste(First.name, Surname),
  Game_Id = paste0(Date, "_", Home.team, "_vs_", Away.team)
) %>% select(
  Date, Season, Round, Game_Id, Name, ID, Team, Kicks, Marks, Handballs, Disposals, Goals, Behinds, Hit.Outs, Tackles,
  Rebounds, Inside.50s, Clearances, Clangers, Frees.For, Frees.Against, Contested.Possessions, Uncontested.Possessions,
  Contested.Marks, Marks.Inside.50, One.Percenters, Goal.Assists, Age, Career.Games, Coach, Home.team, Away.team,
  Brownlow.Votes
) %>% mutate(
  Round = as.numeric(Round),
  Date = as.Date(Date, format = "%d/%m/%Y")
)

# Features and Modelling

# Adding is_winning_team, spread and SC and coaches votes

match_results <- fetch_results_afltables(2005:2025)
colnames(match_results)

match_results <- match_results %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Game_Id = paste0(Date, "_", Home.Team, "_vs_", Away.Team),
    Home_Win = Home.Points > Away.Points,
    Away_Win = Away.Points > Home.Points,
    Draw = Home.Points == Away.Points
  )

afl_tables_stats <- afl_tables_stats %>%
  left_join(
    match_results %>% 
      select(Game_Id, Home.Team, Away.Team, Home_Win, Away_Win, Draw, Margin),
    by = "Game_Id"
  ) %>%
  mutate(
    is_winning_team = case_when(
      Team == Home.Team & Home_Win ~ TRUE,
      Team == Away.Team & Away_Win ~ TRUE,
      TRUE ~ FALSE
    ),
    is_draw = Draw == TRUE
  )

# coaches_votes <- fetch_coaches_votes(2005:2025)
# colnames(coaches_votes)
# saveRDS(coaches_votes, "coaches_votes.RDS")

coaches_votes <- readRDS("coaches_votes.RDS")
team_name_mapping <- c(
  "BL" = "Brisbane Lions",
  "STK" = "St Kilda",
  "NMFC" = "North Melbourne",
  "CARL" = "Carlton",
  "MELB" = "Melbourne",
  "ESS" = "Essendon",
  "FRE" = "Fremantle",
  "PORT" = "Port Adelaide",
  "SYD" = "Sydney",
  "HAW" = "Hawthorn",
  "RICH" = "Richmond",
  "GEEL" = "Geelong",
  "ADEL" = "Adelaide",
  "WCE" = "West Coast"
)


coaches_votes <- coaches_votes %>%
  mutate(
    Team = str_extract(Player.Name, "\\(([^()]+)\\)"),
    Team = str_remove_all(Team, "[()]"),
    Name = str_trim(str_remove(Player.Name, "\\s*\\([^()]+\\)"))
  ) %>%
  select(Season, Round, Name, Team, Coaches.Votes) %>%
  mutate(
    Team = recode(Team, !!!team_name_mapping) 
  )

afl_tables_stats <- afl_tables_stats %>%
  left_join(coaches_votes, by = c("Season", "Round", "Name", "Team")) %>%
  mutate(Coaches.Votes = ifelse(is.na(Coaches.Votes), 0, Coaches.Votes))

sc <- fetch_player_stats_footywire(2009:2025) # SC started in 09
colnames(sc)

sc <- sc %>%
  mutate(
    Round = str_remove(Round, "Round "),
    Round = as.numeric(Round)
  )

sc <- sc %>%
  select(Season, Round, Player, Team, SC)

colSums(is.na(sc))
sc <- na.omit(sc)

afl_tables_stats <- afl_tables_stats %>%
  left_join(
    sc,
    by = c("Season", "Round", "Name" = "Player", "Team")
  ) %>%
  mutate(SC = ifelse(is.na(SC), 0, SC)) 

# Visualise 

# Convert to long data frame
afl_tables_stats_viz <- afl_tables_stats %>%
  mutate(
    is_winning_team_numeric = as.integer(is_winning_team)
  )

afl_tables_stats_viz <- afl_tables_stats %>%
  mutate(
    is_winning_team_numeric = as.integer(is_winning_team),
    Coaches.Votes = as.numeric(Coaches.Votes),
    SC = as.numeric(SC)  
  )

long_stats <- afl_tables_stats_viz %>%
  pivot_longer(
    cols = c(
      Kicks, Marks, Handballs, Disposals, Goals, Behinds, Hit.Outs, Tackles,
      Rebounds, Inside.50s, Clearances, Clangers, Frees.For, Frees.Against,
      Contested.Possessions, Uncontested.Possessions, Contested.Marks,
      Marks.Inside.50, One.Percenters, Goal.Assists,
      is_winning_team_numeric, Margin, Coaches.Votes, SC
    ),
    names_to = "Stat",
    values_to = "Value"
  )

# Plot
ggplot(long_stats, aes(x = Value, y = Brownlow.Votes)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ Stat, scales = "free_x") +
  labs(
    title = "Player Stats, Team Success & Coaches Votes vs Brownlow Votes",
    x = "Feature Value",
    y = "Brownlow Votes"
  ) +
  theme_minimal()

# Correlations
correlation_data <- afl_tables_stats %>%
  mutate(
    is_winning_team_numeric = as.integer(is_winning_team),
    Margin = as.numeric(Margin),
    Coaches.Votes = as.numeric(Coaches.Votes)
  ) %>%
  select(
    Kicks, Marks, Handballs, Disposals, Goals, Behinds, Hit.Outs, Tackles,
    Rebounds, Inside.50s, Clearances, Clangers, Frees.For, Frees.Against,
    Contested.Possessions, Uncontested.Possessions, Contested.Marks,
    Marks.Inside.50, One.Percenters, Goal.Assists,
    is_winning_team_numeric, Margin, Coaches.Votes, SC,
    Brownlow.Votes
  )

correlations <- cor(correlation_data, use = "complete.obs")

cor_brownlow <- correlations[, "Brownlow.Votes", drop = FALSE] %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Stat") %>%
  arrange(desc(Brownlow.Votes))

cor_brownlow

features <- c("Kicks", "Marks", "Handballs", "Goals", "Behinds", "Hit.Outs", "Tackles",
              "Rebounds", "Inside.50s", "Clearances", "Clangers", "Frees.For", "Frees.Against",
              "Contested.Possessions", "Uncontested.Possessions", "Contested.Marks",
              "Marks.Inside.50", "One.Percenters", "Goal.Assists", "Age", "Career.Games", "is_winning_team", "Margin", "SC", "Coaches.Votes")

model_data <- afl_tables_stats %>%
  select(all_of(features), Brownlow.Votes)

set.seed(123)
train_data <- afl_tables_stats %>%
  filter(Season >= 2003, Season <= 2023) %>%
  select(all_of(features), Brownlow.Votes)

test_data <- afl_tables_stats %>%
  filter(Season == 2024) %>%
  select(all_of(features), Brownlow.Votes)

# Regression - continuous values
regression_lm <- lm(Brownlow.Votes ~ ., data = train_data)
summary(regression_lm)

# Predict on test set
pred_lm <- predict(regression_lm, newdata = test_data)

# Calculate RMSE
rmse_lm <- sqrt(mean((pred_lm - test_data$Brownlow.Votes)^2))
rmse_lm

# Predictions using linear
afl_2025_data <- afl_tables_stats %>%
  filter(Season == 2025) %>%
  select(all_of(features)) %>%
  na.omit()

predicted_votes_2025 <- predict(regression_lm, newdata = afl_2025_data)

afl_2025_predictions <- afl_tables_stats %>%
  filter(Season == 2025) %>%
  mutate(Predicted_Brownlow_Votes = predicted_votes_2025)

predicted_brownlow_totals <- afl_2025_predictions %>%
  group_by(Name, Team) %>%
  summarise(
    Total_Predicted_Brownlow_Votes = sum(Predicted_Brownlow_Votes),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Predicted_Brownlow_Votes))

# Make it more realistic 
afl_2025_predictions <- afl_tables_stats %>%
  filter(Season == 2025) %>%
  select(Date, Game_Id, Round, Name, Team, all_of(features), Brownlow.Votes) %>%
  na.omit() %>%
  mutate(
    Raw_Vote_Pred = predict(regression_lm, newdata = .),
    Actual_Brownlow_Votes = Brownlow.Votes
  )


afl_2025_predictions_fractional <- afl_2025_predictions %>%
  group_by(Game_Id) %>%
  mutate(
    Sum_Pred = sum(pmax(Raw_Vote_Pred, 0)),
    Scaled_Votes = ifelse(Sum_Pred > 0, (Raw_Vote_Pred / Sum_Pred) * 6, 0)
  ) %>%
  ungroup()

afl_2025_predictions_strict <- afl_2025_predictions %>%
  group_by(Game_Id) %>%
  arrange(desc(Raw_Vote_Pred)) %>%
  mutate(
    Allocated_Votes = case_when(
      row_number() == 1 ~ 3,
      row_number() == 2 ~ 2,
      row_number() == 3 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

predicted_brownlow_totals_fractional <- afl_2025_predictions_fractional %>%
  group_by(Name, Team) %>%
  summarise(
    Total_Predicted_Brownlow_Votes = sum(Scaled_Votes),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Predicted_Brownlow_Votes))

predicted_brownlow_totals_strict <- afl_2025_predictions_strict %>%
  group_by(Name, Team) %>%
  summarise(
    Total_Predicted_Brownlow_Votes = sum(Allocated_Votes),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Predicted_Brownlow_Votes))

# Round by Round
round_by_round_votes <- afl_2025_predictions_strict %>%
  select(Name, Team, Round, Allocated_Votes) %>%
  filter(!is.na(Round)) %>%
  group_by(Name, Team, Round) %>%
  summarise(Round_Votes = sum(Allocated_Votes), .groups = "drop")

round_by_round_wide <- round_by_round_votes %>%
  pivot_wider(
    names_from = Round,
    values_from = Round_Votes,
    names_prefix = "R"
  )

round_by_round_wide <- round_by_round_votes %>%
  pivot_wider(
    names_from = Round,
    values_from = Round_Votes,
    names_prefix = "R"
  )

round_columns <- paste0("R", 1:8)

leaderboard <- round_by_round_wide %>%
  mutate(Total_Votes = rowSums(select(., all_of(round_columns)), na.rm = TRUE)) %>%
  arrange(desc(Total_Votes)) %>%
  select(Total_Votes, Name, Team, all_of(round_columns))


leaderboard_clean <- leaderboard %>%
  mutate(across(starts_with("R"), ~ ifelse(is.na(.), "-", round(., 1))))
leaderboard_clean

