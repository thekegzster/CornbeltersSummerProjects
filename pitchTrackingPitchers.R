library(tidyverse)

# --------------------------------------------------
# UI: Reference Stats For Player
# --------------------------------------------------
pitchTrackingPitcherUI <- function(id) {
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
      h3("Pitch Tracking")
    ),
    
    div(
      style = "padding: 12px;",
      tableOutput(ns("pitchTrackingPitcher"))
    )
  )
}

#–– Server function ––
pitchTrackingPitcherServer <- function(id, data, pitcherName) {
  moduleServer(id, function(input, output, session) {
    output$pitchTrackingPitcher <- renderTable({
      data() |> 
      filter(Pitcher == pitcherName()) |> 
        select(-Pitcher) |> 
        transmute(
          Pitch = PitchType,
          `#` = as.integer(round(`#`, 0)),
          `# RHB` = as.integer(round(`# RHB`, 0)),
          `# LHB` = as.integer(round(`# LHB`, 0)),
          `%` = sprintf("%.1f", `%`),
          MPH = sprintf("%.2f", MPH),
          H = as.integer(round(H, 0)),
          `1B` = as.integer(round(`1B`, 0)),
          `2B` = as.integer(round(`2B`, 0)),
          `3B` = as.integer(round(`3B`, 0)),
          HR = as.integer(round(HR, 0)),
          SO = as.integer(round(SO, 0)),
          BBE = as.integer(round(BBE, 0)),
          BA = sprintf("%.3f", BA),
          xBA = sprintf("%.3f", XBA),
          SLG = sprintf("%.3f", SLG),
          xSLG = sprintf("%.3f", XSLG),
          WOBA = sprintf("%.3f", WOBA),
          xWOBA = sprintf("%.3f", XWOBA),
          EV = sprintf("%.2f", EV),
          LA = sprintf("%.2f", LA),
          Spin = as.integer(round(Spin, 0)),
          Ext = sprintf("%.1f", Ext.),
          `Whiff %` = sprintf("%.2f", `Whiff %`),
          `Put Away %` = sprintf("%.0f", `Put Away %`)
        )
    }, rownames = FALSE)
  })
}