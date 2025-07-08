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

