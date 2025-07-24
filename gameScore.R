calculateGameScoreHitting <- function(df, date, batter){
  date <- as.Date(date)
  
  gameScoreDF <- df |> 
    group_by(Batter, Date) |> 
    summarize(
      bases = sum(bases),
      rbi = sum(RunsScored, na.rm = TRUE),
      k = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking")),
      bb = sum(PlayResult %in% c("Walk", "HitByPitch")),
      out = sum(PlayResult %in% c("Out", "Error", "FieldersChoice")),
      ab = sum(!is.na(PlayResult)),
      gameScore = 100 + bases + .75*rbi + .9*bb - .8*k - .25*out + .05*ab,
      .groups = "drop"
    ) |> 
    mutate(
      percentile = percent_rank(gameScore),
      realGameScore = round(percentile * 100, 0)
    )

  gameScoreDF |> 
    filter(Date == date) |> 
    filter(Batter == batter) |> 
    pull(realGameScore)
}

#UI
gameScoreHittingUI <- function(id) {
  ns <- NS(id)
  uiOutput(
    outputId = ns("gameScoreHitting"),
    style    = "height:100%; width:100%;"
  )
}

#Server
gameScoreHittingServer <- function(id, data, date, batter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$gameScoreHitting <- renderUI({
      req(data(), date(), batter())
      
      # compute the 0–100 score
      cat(date())
      cat(batter())
      score <- calculateGameScoreHitting(data(), date(), batter())
      
      # build two ramps and pick the right one
      ramp1 <- grDevices::colorRamp(c("darkblue","grey90"))
      ramp2 <- grDevices::colorRamp(c("grey90","darkred"))
      rgbcol <- function(col) grDevices::rgb(col[1], col[2], col[3], maxColorValue = 255)
      
      bgcol <- if (score <= 50) {
        rgbcol(ramp1(score/50))
      } else {
        rgbcol(ramp2((score - 50) / 50))
      }
      
      div(
        style = paste0(
          "width: 100%;",
          "height: 100%;",
          "background-color: ", bgcol, ";",
          "color: white;",
          "display: flex;",
          "flex-direction: column;",
          "align-items: center;",
          "justify-content: center;",
          "border-radius: 12px;"
        ),
        div(style="font-size: 2.5em; margin-bottom: 0.2em;", "Game Score"),
        div(style="font-size: 5.5em;", score)
      )
    })
  })
}

##########################################################################################################
############### PITCHING
##########################################################################################################

calculateGameScorePitching <- function(df, date, pitcher){
  date <- as.Date(date)
  
  gameScoreDF <- df |> 
    group_by(Pitcher, Date) |> 
    summarize(
      k = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking")),
      bb = sum(PlayResult %in% c("Walk", "HitByPitch")),
      out = sum(OutsOnPlay, na.rm = TRUE),
      run = sum(RunsScored, na.rm = TRUE),
      hit = sum(PlayResult %in% c("Single" ,"Double", "Triple", "HomeRun")),
      hr = sum(PlayResult %in% c("HomeRun")),
      gameScore = 100 + out + .51*k - 1.52*run - .99*hit - .74*bb - 2.9*hr,
      .groups = "drop"
    ) |> 
    mutate(
      percentile = percent_rank(gameScore),
      realGameScore = round(percentile * 100, 0)
    )
  
  gameScoreDF |> 
    filter(Date == date) |> 
    filter(Pitcher == pitcher) |> 
    pull(realGameScore)
}

#UI
gameScorePitchingUI <- function(id) {
  ns <- NS(id)
  uiOutput(
    outputId = ns("gameScorePitching"),
    style    = "height:100%; width:100%;"
  )
}

#Server
gameScorePitchingServer <- function(id, data, date, pitcher) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$gameScorePitching <- renderUI({
      req(data(), date(), pitcher())
      
      # compute the 0–100 score
      score <- calculateGameScorePitching(data(), date(), pitcher())
      
      # build two ramps and pick the right one
      ramp1 <- grDevices::colorRamp(c("darkblue","grey90"))
      ramp2 <- grDevices::colorRamp(c("grey90","darkred"))
      rgbcol <- function(col) grDevices::rgb(col[1], col[2], col[3], maxColorValue = 255)
      
      bgcol <- if (score <= 50) {
        rgbcol(ramp1(score/50))
      } else {
        rgbcol(ramp2((score - 50) / 50))
      }
      
      div(
        style = paste0(
          "width: 100%;",
          "height: 100%;",
          "background-color: ", bgcol, ";",
          "color: white;",
          "display: flex;",
          "flex-direction: column;",
          "align-items: center;",
          "justify-content: center;",
          "border-radius: 12px;"
        ),
        div(style="font-size: 2.5em; margin-bottom: 0.2em;", "Game Score"),
        div(style="font-size: 5.5em;", score)
      )
    })
  })
}