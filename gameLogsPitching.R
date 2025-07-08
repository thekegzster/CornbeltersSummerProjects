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
    mutate(isZone = ifelse(IsStrike == TRUE, 1, 0))
  
  zonePct <- zone |> 
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

