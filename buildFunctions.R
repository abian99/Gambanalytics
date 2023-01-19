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

return_predictor_df <- function(current_schedule, team_df, avgPts) {
    
  predictor_DF <- current_schedule %>%
    inner_join(
      team_df, by = c('home_team' = 'team')
    ) %>%
    inner_join(
      team_df, by = c('away_team' = 'team')
    ) %>%
    mutate(
      homeAvg = pointsFor.x,
      homeAvgRatio = pointsFor.x*dRatio.y,
      homeNetRatio = netPF.x * dRatio.y,
      homeAvgRatio2 = oRatio.x * pointsAgainst.y,
      homeNetRatio2 = netPA.y * oRatio.x,
      awayAvg = pointsFor.y,
      awayAvgRatio = pointsFor.y*dRatio.x,
      awayNetRatio = netPF.y * dRatio.x,
      awayAvgRatio2 = oRatio.y * pointsAgainst.x,
      awayNetRatio2 = netPA.x * oRatio.y,
      homeJuccula = (pointsFor.x/avgPts)*(pointsAgainst.y/avgPts)*avgPts, 
      awayJuccula = (pointsFor.y/avgPts)*(pointsAgainst.x/avgPts)*avgPts, 
      avgWinner = if_else(homeAvg > awayAvg, home_team, if_else(awayAvg > homeAvg, away_team, "TIE")),
      avgRatioWinner = if_else(homeAvgRatio > awayAvgRatio, home_team, if_else(awayAvgRatio > homeAvgRatio, away_team, "TIE")),
      netRatioWinner = if_else(homeNetRatio > awayNetRatio, home_team, if_else(awayNetRatio > homeNetRatio, away_team, "TIE")),
      avgRatioWinner2 = if_else(homeAvgRatio2 > awayAvgRatio2, home_team, if_else(awayAvgRatio2 > homeAvgRatio2, away_team, "TIE")),
      netRatioWinner2 = if_else(homeNetRatio2 > awayNetRatio2, home_team, if_else(awayNetRatio2 > homeNetRatio2, away_team, "TIE")),
      pythWinner = if_else(pythWin.x > pythWin.y, home_team, if_else(pythWin.y > pythWin.x, away_team, "TIE")),
      jucculaWinner = if_else(homeJuccula > awayJuccula, home_team, if_else(awayJuccula > homeJuccula, away_team, "TIE")),
      actualWinner = if_else(home_score > away_score, home_team, if_else(away_score > home_score, away_team, "TIE"))
    ) %>%
    summarize(
      game_id, 
      home_team,
      away_team,
      homeAvg,
      homeAvgRatio,
      homeNetRatio, 
      home_score,
      awayAvg,
      awayAvgRatio,
      awayNetRatio,
      away_score,
      homeAvgRatio2,
      homeNetRatio2,
      awayAvgRatio2,
      awayNetRatio2,
      homeJuccula,
      awayJuccula,
      avgWinner,
      avgRatioWinner,
      netRatioWinner,
      avgRatioWinner2,
      netRatioWinner2,
      jucculaWinner,
      actualWinner,
      total_line,
      spread_line
    )
}

totalReturnCalc <- function(x, output) {
  unit_value = 5
  odds = as.numeric(x[24])
  win = x[25]
  if (win == TRUE) {
    if (odds > 0){
      payout = (unit_value*odds/100)
    }
    else {
      payout = unit_value/(-1*(odds/100))
    }
  }
  else {
    payout = unit_value * -1
  }
  return(payout)
}