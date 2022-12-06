return_team_df <- function(schedule_df) {
  team_df <- schedule_df %>%
    group_by(team) %>%
    mutate(
      oRatio = mean(PointsFor / avg_points),
      dRatio = mean(PointsAgainst / avg_points),
      pointsFor = mean(PointsFor),
      pointsAgainst = mean(PointsAgainst),
      netPF = mean(PointsFor / dRatio),
      netPA = mean(PointsAgainst / oRatio),
      wins = sum(win),
      losses = sum(win == 0),
      winPct = wins / (wins + losses),
      pythWin = ((pointsFor^2.37)/((pointsFor^2.37)+(pointsAgainst^2.37)))*17
    ) %>%
    summarize(team,
              pointsFor,
              pointsAgainst,
              netPF,
              netPA,
              wins,
              pythWin,
              losses,
              winPct,
              oRatio,
              dRatio) %>%
    unique()
  
  return(team_df)
}

return_predictor_df <- function(current_schedule) {
  predictor_DF <- current_schedule %>%
    inner_join(
      team_df, by = c('home_team' = 'team')
    ) %>%
    inner_join(
      team_df, by = c('away_team' = 'team')
    ) %>%
    mutate(
      homeAvg = pointsFor.x,
      homeavgRatio = pointsFor.x*dRatio.y,
      homeNetRatio = netPF.x * dRatio.y,
      awayAvg = pointsFor.y,
      awayAvgRatio = pointsFor.y*dRatio.x,
      awayNetRatio = netPF.y * dRatio.x,
      avgWinner = if_else(homeAvg > awayAvg, home_team, if_else(awayAvg > homeAvg, away_team, "TIE")),
      avgRatioWinner = if_else(homeavgRatio > awayAvgRatio, home_team, if_else(awayAvgRatio > homeavgRatio, away_team, "TIE")),
      netRatioWinner = if_else(homeNetRatio > awayNetRatio, home_team, if_else(awayNetRatio > homeNetRatio, away_team, "TIE")),
      actualWinner = if_else(home_score > away_score, home_team, if_else(away_score > home_score, away_team, "TIE"))
    ) %>%
    summarize(
      game_id, 
      home_team,
      away_team,
      homeAvg,
      homeavgRatio,
      homeNetRatio, 
      home_score,
      awayAvg,
      awayAvgRatio,
      awayNetRatio,
      away_score,
      avgWinner,
      avgRatioWinner,
      netRatioWinner,
      actualWinner
    )
}