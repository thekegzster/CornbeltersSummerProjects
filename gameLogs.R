calculateWhiffs <- function(data){
  whiffs <- data |> 
    filter(!is.na(PitchCall)) |> 
    filter(IsSwing == TRUE) |> 
    mutate(isWhiff = ifelse(PitchCall == "StrikeSwinging", 1, 0))
  
  whiffPct <- whiffs |> 
    summarise(
      pct = mean(isWhiff, na.rm = TRUE)
    ) |> 
    pull(pct)
  
  numWhiffs <- whiffs |> 
    summarise(
      num = sum(isWhiff, na.rm = TRUE)
    ) |> 
    pull(num)
  
  return(
    list(
      whiffPct = whiffPct,
      numWhiffs = numWhiffs
    )
  )
}

calculateChases <- function(data){
  chases <- data |> 
    filter(!is.na(PitchCall)) |> 
    filter(IsStrike == FALSE) |> 
    mutate(isChase = ifelse(IsSwing == TRUE, 1, 0)) 
  
  chasePct <- chases |> 
    summarise(
      pct = mean(isChase, na.rm = TRUE)
    ) |> 
    pull(pct)
  
  numChases <- chases |> 
    summarise(
      num = sum(isChase, na.rm = TRUE)
    ) |> 
    pull(num)
  
  return(
    list(
      chasePct = chasePct,
      numChases = numChases
    )
  )
}

calculateIZTakes <- function(data){
  takes <- data |> 
    filter(!is.na(PitchCall)) |> 
    filter(IsStrike == TRUE) |> 
    mutate(isTake = ifelse(IsSwing == FALSE, 1, 0)) 
  
  takePct <- takes |> 
    summarise(
      pct = mean(isTake, na.rm = TRUE)
    ) |> 
    pull(pct)
  
  numTakes <- takes |> 
    summarise(
      num = sum(isTake, na.rm = TRUE)
    ) |> 
    pull(num)
  
  return(
    list(
      takePct = takePct,
      numTakes = numTakes
    )
  )
}

calculateOZTakes <- function(data){
  takes <- data |> 
    filter(!is.na(PitchCall)) |> 
    filter(IsStrike == FALSE) |> 
    mutate(isTake = ifelse(IsSwing == FALSE, 1, 0)) 
  
  takePct <- takes |> 
    summarise(
      pct = mean(isTake, na.rm = TRUE)
    ) |> 
    pull(pct)
  
  numTakes <- takes |> 
    summarise(
      num = sum(isTake, na.rm = TRUE)
    ) |> 
    pull(num)
  
  return(
    list(
      takePct = takePct,
      numTakes = numTakes
    )
  )
}

gameLogHittingStats <- function(df, date, stats_df) {
  df <- df |> 
    fill(PlayerPA, .direction = "up")
  
  df <- df |> 
    arrange(Date, Time) |> 
    mutate(
      endPA     = !is.na(PlayResult),                             
      cumEnds   = cumsum(endPA),                                   
      BatterPA  = cumEnds - endPA + 1                              
    ) |> 
    select(-endPA, -cumEnds)
  
  seasonStats <- tibble(
    AB      = "Season",
    PlayResult = NA,
    Whiffs  = calculateWhiffs(df)$whiffPct,
    Chases  = calculateChases(df)$chasePct,
    IZTakes = calculateIZTakes(df)$takePct,
    OZTakes = calculateOZTakes(df)$takePct,
    ExitVelo = stats_df$avgExitVelo,
    LA = NA,
    Distance = NA,
    xBA = stats_df$xBA,
    xSLG = stats_df$xSLG,
    xWOBA = stats_df$xWOBA
  )
  
  game_df <- df |> 
    filter(Date == date) |> 
    mutate(AtBat = dense_rank(BatterPA))
  
  gameStats <- tibble(
    AB      = "Game",
    PlayResult = NA,
    Whiffs  = calculateWhiffs(game_df)$whiffPct,
    Chases  = calculateChases(game_df)$chasePct,
    IZTakes = calculateIZTakes(game_df)$takePct,
    OZTakes = calculateOZTakes(game_df)$takePct,
    ExitVelo = NA,
    LA = NA,
    Distance = NA,
    xBA = NA,
    xSLG = NA,
    xWOBA = NA
  )
  
  atBatStats <- game_df |> 
    nest(data = -AtBat) |> 
    mutate(
      Whiffs  = map_dbl(data, ~ calculateWhiffs(.x)$numWhiffs),
      Chases  = map_dbl(data, ~ calculateChases(.x)$numChases),
      IZTakes = map_dbl(data, ~ calculateIZTakes(.x)$numTakes),
      OZTakes = map_dbl(data, ~ calculateOZTakes(.x)$numTakes),
      AB      = as.character(AtBat),
      PlayResult = map_chr(data, ~ last(na.omit(.x$PlayResult))),
      ExitVelo = map_dbl(data, ~ last(na.omit(.x$ExitSpeed))),
      LA = map_dbl(data, ~ last(na.omit(.x$Angle))),
      Distance = map_dbl(data, ~ last(na.omit(.x$Distance))),
      xBA = map_dbl(data, ~ max(.x$predicted_xba, na.rm = TRUE)),
      xSLG = map_dbl(data, ~ max(.x$predicted_xslg, na.rm = TRUE)),
      xWOBA = map_dbl(data, ~ max(.x$predicted_xwoba, na.rm = TRUE))
    ) |> 
    select(
      AB,
      PlayResult,
      Whiffs,
      Chases,
      IZTakes,
      OZTakes,
      ExitVelo,
      LA,
      Distance,
      xBA,
      xSLG,
      xWOBA
    )
  
  bind_rows(atBatStats, gameStats, seasonStats)
}

# --------------------------------------------------
# UI: Game Hitting Logs For Player
# --------------------------------------------------
gameLogHittingUI <- function(id) {
  ns <- NS(id)
  div(
    class = "basic-info-card",
    style = "
      display: flex;
      flex-direction: column;
      background: #fff;
      border: 1px solid #ccc;
      border-radius: 8px;
      overflow: hidden;
      box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      width: 100%;
    ",
    
    div(
      style = "text-align: center; margin-bottom: 5px;",
      h3("Game Logs")
    ),
    
    div(
      style = "padding: 12px;",
      tableOutput(ns("gameLogHittingTable"))
    )
  )
}

# --------------------------------------------------
# Server: Game Hitting Logs For Player
# --------------------------------------------------
gameLogHittingServer <- function(id, data, date, stats_df) {
  moduleServer(id, function(input, output, session) {
    output$gameLogHittingTable <- renderTable({
      req(date())
      df <- gameLogHittingStats(data(), date(), stats_df())
      
      df_formatted <- df |> 
        mutate(
          Whiffs  = if_else(
            AB %in% c("Game","Season"),
            sprintf("%.1f%%", Whiffs  * 100),
            as.character(Whiffs)
          ),
          Chases  = if_else(
            AB %in% c("Game","Season"),
            sprintf("%.1f%%", Chases  * 100),
            as.character(Chases)
          ),
          IZTakes = if_else(
            AB %in% c("Game","Season"),
            sprintf("%.1f%%", IZTakes * 100),
            as.character(IZTakes)
          ),
          OZTakes = if_else(
            AB %in% c("Game","Season"),
            sprintf("%.1f%%", OZTakes * 100),
            as.character(OZTakes)
          ),
          
          xBA   = sprintf("%.3f", xBA),
          xSLG  = sprintf("%.3f", xSLG),
          xWOBA = sprintf("%.3f", xWOBA),
          
          Distance = ifelse(
            is.na(Distance),
            "",
            as.character(as.integer(round(Distance)))
          ),
          
          ExitVelo = ifelse(
            is.na(ExitVelo),
            "",
            sprintf("%.2f", ExitVelo)
          ),
          
          LA = ifelse(
            is.na(LA),
            "",
            sprintf("%.2f", LA)
          ),
          
          PlayResult = ifelse(
            is.na(PlayResult),
            "",
            PlayResult
          )
        )
      
      df_formatted
    }, rownames = FALSE)
  })
}


#####################################################################################
######################### PITCHING
#####################################################################################

calculateWhiffPct <- function(data){
  whiffs <- data |> 
    filter(!is.na(PitchCall)) |> 
    filter(IsSwing == TRUE) |> 
    mutate(isWhiff = ifelse(PitchCall == "StrikeSwinging", 1, 0))
  
  whiffPct <- whiffs |> 
    summarise(
      pct = mean(isWhiff, na.rm = TRUE)
    ) |> 
    pull(pct)
  
  return(whiffPct)
}

calculateChasePct <- function(data){
  chases <- data |> 
    filter(!is.na(PitchCall)) |> 
    filter(IsStrike == FALSE) |> 
    mutate(isChase = ifelse(IsSwing == TRUE, 1, 0)) 
  
  chasePct <- chases |> 
    summarise(
      pct = mean(isChase, na.rm = TRUE)
    ) |> 
    pull(pct)
  
  return(chasePct)
}

calculateZonePct <- function(data){
  zone <- data |> 
    filter(!is.na(PitchCall)) |> 
    filter(!is.na(IsStrike)) |> 
    mutate(isZone = ifelse(IsStrike == TRUE, 1, 0))
  
  zonePct <- zone |> 
    filter(!is.na(isZone)) |> 
    summarise(
      pct = mean(isZone, na.rm = TRUE)
    ) |> 
    pull(pct)
  
  return(zonePct)
}

calculateAvgVelo <- function(data){
  avgVelo <- data |> 
    summarise(
      mean = mean(RelSpeed, na.rm = TRUE)
    ) |> 
    pull(mean)
  
  return(avgVelo)
}

calculateMaxVelo <- function(data){
  maxVelo <- data |> 
    summarise(
      max = max(RelSpeed, na.rm = TRUE)
    ) |> 
    pull(max)
  
  return(maxVelo)
}

calculateAvgExitVelo <- function(data){
  avgVelo <- data |> 
    summarise(
      mean = mean(ExitSpeed, na.rm = TRUE)
    ) |> 
    pull(mean)
  
  return(avgVelo)
}

calculateIVB <- function(data) {
  ivb <- data |> 
    summarise(
      avg = mean(InducedVertBreak, na.rm = TRUE)
    ) |> 
    pull(avg)
  
  return(ivb)
}

calculateHB <- function(data) {
  hb <- data |> 
    summarise(
      avg = mean(HorzBreak, na.rm = TRUE)
    ) |> 
    pull(avg)
  
  return(hb)
}

calculateSpinRate <- function(data){
  spin <- data |> 
    summarise(
      avg = mean(SpinRate, na.rm = TRUE)
    ) |> 
    pull(avg)
  
  return(spin)
}

calculateNum <- function(data){
  num <- data |> 
    filter(!is.na(PitchCall)) |> 
    summarise(
      count = n()
    ) |> 
    pull(count)
  
  return(num)
}

gameLogPitchingStats <- function(df, date) {
  df_date <- df |> 
    filter(Date == date) |> 
    filter(!is.na(AutoTaggedPitchType))
  
  seasonStats <- tibble(
    Pitch      = "Season",
    `#` = NA,
    `%` = NA,
    avgVelo = NA,
    maxVelo = calculateMaxVelo(df),
    exitVelo = calculateAvgExitVelo(df),
    iVB = NA,
    hb = NA,
    spinRate = NA,
    WhiffPct  = calculateWhiffPct(df),
    ChasePct  = calculateChasePct(df),
    ZonePct   = calculateZonePct(df)
  )
  
  pitchStats <- df_date |> 
    group_by(AutoTaggedPitchType) |> 
    group_modify(
      ~ {
        dat <- .x
        tibble(
          Pitch    = .y$AutoTaggedPitchType,
          `#`      = nrow(dat),
          `%`      = nrow(dat) / nrow(df_date),
          avgVelo  = calculateAvgVelo(dat),
          maxVelo  = calculateMaxVelo(dat),
          exitVelo = calculateAvgExitVelo(dat),
          iVB      = calculateIVB(dat),
          hb       = calculateHB(dat),
          spinRate = calculateSpinRate(dat),
          WhiffPct = calculateWhiffPct(dat),
          ChasePct = calculateChasePct(dat),
          ZonePct  = calculateZonePct(dat)
        )
      },
      keep = FALSE
    ) |> 
    ungroup()
  
  bind_rows(pitchStats, seasonStats) |> 
    select(-AutoTaggedPitchType) |> 
    arrange(desc(`#`))
}

# --------------------------------------------------
# UI: Game Hitting Logs For Player
# --------------------------------------------------
gameLogPitchingUI <- function(id) {
  ns <- NS(id)
  div(
    class = "basic-info-card",
    style = "
      display: flex;
      flex-direction: column;
      background: #fff;
      border: 1px solid #ccc;
      border-radius: 8px;
      overflow: hidden;
      box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      width: 100%;
    ",
    
    div(
      style = "text-align: center; margin-bottom: 5px;",
      h3("Game Logs")
    ),
    
    div(
      style = "padding: 12px;",
      tableOutput(ns("gameLogPitchingTable"))
    )
  )
}

# --------------------------------------------------
# Server: Game Pitching Logs For Player
# --------------------------------------------------
gameLogPitchingServer <- function(id, data, date) {
  moduleServer(id, function(input, output, session) {
    output$gameLogPitchingTable <- renderTable({
      req(date())
      df <- gameLogPitchingStats(data(), date())
      
      df_formatted <- df |> 
        mutate(
          `%` = ifelse(
            is.na(`%`),
            "",
            sprintf("%.1f%%", `%` * 100)
          ),
          `#` = ifelse(
            is.na(`#`),
            "",
            `#`
          ),
          iVB = ifelse(
            is.na(iVB),
            "",
            sprintf("%.1f", iVB)
          ),
          hb = ifelse(
            is.na(hb),
            "",
            sprintf("%.1f", hb)
          ),
          avgVelo = ifelse(
            is.na(avgVelo),
            "",
            sprintf("%.1f", avgVelo)
          ),
          maxVelo = ifelse(
            is.na(maxVelo),
            "",
            sprintf("%.1f", maxVelo)
          ),
          exitVelo = ifelse(
            is.na(exitVelo),
            "",
            sprintf("%.1f", exitVelo)
          ),
          spinRate = ifelse(
            is.na(spinRate),
            "",
            sprintf("%.1f", spinRate)
          ),
          WhiffPct = ifelse(
            is.na(WhiffPct),
            "",
            sprintf("%.1f%%", WhiffPct * 100)
          ),
          ChasePct = ifelse(
            is.na(ChasePct),
            "",
            sprintf("%.1f%%", ChasePct * 100)
          ),
          ZonePct = ifelse(
            is.na(ZonePct),
            "",
            sprintf("%.1f%%", ZonePct * 100)
          )
          
        )
      
      df_formatted
    }, rownames = FALSE)
  })
}



