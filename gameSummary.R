gameSummaryHitting <- function(df, date, batter) {
  game_df <- df |> 
    filter(Date == date) |> 
    filter(Batter == batter)
  
  gameStats <- game_df |> 
    summarize(
      AB = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Error", "StrikeoutSwinging", "StrikeoutLooking", "FieldersChoice")),
      H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
      `2B` = sum(PlayResult %in% c("Double")),
      `3B` = sum(PlayResult %in% c("Triple")),
      HR = sum(PlayResult %in% c("HomeRun")),
      RBI = int(sum(RunsScored, na.rm = TRUE)),
      BB = sum(PlayResult %in% c("Walk")),
      HBP = sum(PlayResult %in% c("HitByPitch")),
      SO = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking"))
    )
  
  gameStats
  
}

# --------------------------------------------------
# UI: Game Hitting Summaries For Player
# --------------------------------------------------
gameSummaryHittingUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Scoped CSS with escaped percent sign (%%)
    tags$head(
      tags$style(HTML(sprintf("
        /* target only the table inside our #%s */
        #%s table {
          font-size: 18px;
          width: 100%%;           /* <- escaped %% */
        }
        #%s th, #%s td {
          padding: 12px;
          text-align: center;
        }
      ",
                              ns("summaryCard"),    # 1st %s
                              ns("summaryCard"),    # 2nd %s
                              ns("summaryCard"),    # 3rd %s
                              ns("summaryCard")     # 4th %s
      )))
    ),
    # Your existing wrapper, with id for scoping
    div(
      id    = ns("summaryCard"),
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
        style = "padding: 12px;",
        tableOutput(ns("gameSummaryHittingTable"))
      )
    )
  )
}




# --------------------------------------------------
# Server: Game Summary Logs For Player
# --------------------------------------------------
gameSummaryHittingServer <- function(id, data, date, batter) {
  moduleServer(id, function(input, output, session) {
    output$gameSummaryHittingTable <- renderTable({
      req(date())
      gameSummaryHitting(data(), date(), batter())
    }, rownames = FALSE)
  })
}

#############################################################################################################
############################### PITCHING
#############################################################################################################

gameSummaryPitching <- function(df, date, pitcher) {
  game_df <- df |>
    filter(Date == date) |>
    filter(Pitcher == pitcher)

  gameStats <- game_df |>
    summarize(
      outs = sum(OutsOnPlay, na.rm = TRUE),
      H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
      R = int(sum(RunsScored, na.rm = TRUE)),
      HR = sum(PlayResult %in% c("HomeRun")),
      BB = sum(PlayResult %in% c("Walk")),
      K = sum(PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking")),
      GB = sum(HitType %in% c("GroundBall")),
      FB = sum(HitType %in% c("LineDrive", "FlyBall", "Popup")),
      `#P` = sum(!is.na(PitchCall)),
      `S%` = sum(IsStrike == TRUE, na.rm = TRUE) / (sum(IsStrike == TRUE, na.rm = TRUE) + sum(IsStrike == FALSE, na.rm = TRUE))
    ) |>
    mutate(
      full_innings = outs %/% 3,
      leftover     = outs %% 3,
      IP           = paste0(full_innings, ".", leftover),
      `S%` = sprintf("%.1f%%", `S%` * 100)
    ) |>
    select(-outs, -full_innings, -leftover) |> 
    select(IP, H, R, HR, BB, K, GB, FB, `#P`, `S%`)

  gameStats

}

# --------------------------------------------------
# UI: Game Hitting Summaries For Player
# --------------------------------------------------
gameSummaryPitchingUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Scoped CSS with escaped percent sign (%%)
    tags$head(
      tags$style(HTML(sprintf("
        /* target only the table inside our #%s */
        #%s table {
          font-size: 18px;
          width: 100%%;           /* <- escaped %% */
        }
        #%s th, #%s td {
          padding: 12px;
          text-align: center;
        }
      ",
                              ns("summaryCard"),    # 1st %s
                              ns("summaryCard"),    # 2nd %s
                              ns("summaryCard"),    # 3rd %s
                              ns("summaryCard")     # 4th %s
      )))
    ),
    # Your existing wrapper, with id for scoping
    div(
      id    = ns("summaryCard"),
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
        style = "padding: 12px;",
        tableOutput(ns("gameSummaryPitchingTable"))
      )
    )
  )
}


# --------------------------------------------------
# Server: Game Summary Logs For Player
# --------------------------------------------------
gameSummaryPitchingServer <- function(id, data, date, pitcher) {
  moduleServer(id, function(input, output, session) {
    output$gameSummaryPitchingTable <- renderTable({
      req(date())
      gameSummaryPitching(data(), date(), pitcher())
    }, rownames = FALSE)
  })
}