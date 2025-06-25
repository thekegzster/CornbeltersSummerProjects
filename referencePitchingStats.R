library(tidyverse)

# --------------------------------------------------
# UI: Reference Stats For Player
# --------------------------------------------------
referencePitchingStatsUI <- function(id) {
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
      h3("Standard Pitching")
    ),
    
    div(
      style = "padding: 12px;",
      tableOutput(ns("referencePitching"))
    )
  )
}

# --------------------------------------------------
# SERVER: Reference Stats For Player
# --------------------------------------------------
referencePitchingStatsServer <- function(id, stats_df) {
  moduleServer(id, function(input, output, session) {
    
    # Reference stats table
    output$referencePitching <- renderTable({
      stats_df() %>% select(W, L, ERA, G, GS, SV, IP, H, R, ER, HR, BB, SO, HBP, WP, `ERA+`, FIP2, WHIP, H9, HR9, BB9, SO9, `SO/BB`, WAR_r) |> 
        transmute(
          W  = as.integer(round(W, 0)),
          L  = as.integer(round(L, 0)),
          ERA = sprintf("%.2f", ERA),
          G  = as.integer(round(G, 0)),
          GS   = as.integer(round(GS, 0)),
          SV = as.integer(round(SV, 0)),
          IP = sprintf("%.1f", IP),
          H  = as.integer(round(H, 0)),
          R  = as.integer(round(R, 0)),
          ER  = as.integer(round(ER, 0)),
          HR  = as.integer(round(HR, 0)),
          BB = as.integer(round(BB, 0)),
          SO  = as.integer(round(SO, 0)),
          HBP  = as.integer(round(HBP, 0)),
          WP  = as.integer(round(WP, 0)),
          `ERA+`  = as.integer(round(`ERA+`, 0)),
          FIP = sprintf("%.2f", FIP2),
          WHIP = sprintf("%.2f", WHIP),
          H9 = sprintf("%.1f", H9),
          HR9 = sprintf("%.1f", HR9),
          BB9 = sprintf("%.1f", BB9),
          SO9 = sprintf("%.1f", SO9),
          `SO/BB` = sprintf("%.1f", `SO/BB`),
          WAR = sprintf("%.1f", WAR_r)
        )
    }, rownames = FALSE, spacing = "l", align = "c")
  })
}