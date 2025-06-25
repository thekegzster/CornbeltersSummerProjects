library(knitr)

plotPitchDetailsTable <- function(pitcherData, pitcherName){
  #Get a pitch summary of the usage and speed of every pitch
  pitch_summary <- pitcherData |> 
    filter(!is.na(RelSpeed), !is.na(HorzBreak)) |> 
    group_by(Pitcher, TaggedPitchType) |> 
    summarise(
      meanSpeed = mean(RelSpeed, na.rm = TRUE),
      count     = n(),
      .groups   = "drop"
    ) |> 
    
    group_by(Pitcher) |> 
    mutate(
      usageRate = count / sum(count)
    ) |> 
    ungroup()
  
  # Create the table from the pitch summaries
  table_df <- pitch_summary |> 
    filter(Pitcher == pitcherName) |> 
    arrange(desc(usageRate)) |> 
    transmute(
      PitchType = TaggedPitchType,
      Usage     = sprintf("%.0f%%", usageRate*100),
      Speed     = sprintf("%.1f", meanSpeed),
      Color     = paste0(
        "<span style='display:inline-block;",
        "width:12px;height:12px;background:", 
        pitch_colors[TaggedPitchType],
        ";border:1px solid #333;'></span>"
      )
    )
  
  #Pull out vectors from the table
  colors <- table_df$Color
  ptypes <- table_df$PitchType
  usage  <- table_df$Usage
  speed  <- table_df$Speed
  
  m <- rbind(
    colors,
    ptypes,
    usage,
    speed
  )
  
  #Turn into dataframe
  df_key <- as.data.frame(m, stringsAsFactors = FALSE)
  rownames(df_key) <- c("", " ", "Usage", "Avg Speed")
  colnames(df_key)  <- rep("", ncol(df_key))
  
  kable(
    df_key,
    format     = "html",
    escape     = FALSE,
    row.names  = TRUE,
    align      = rep("c", ncol(df_key)),
    table.attr = '
    style="
      background-color: white;
      border-collapse: separate;
      border-spacing: 25px 0;
    "
  '
  )
}

# UI ---------------------------------------------------------
pitchDetailsTableUI <- function(id) {
  ns <- NS(id)
  div(
    style = "
      height: 100%;
      padding: 2px;
      border: 1px solid #ccc;
      border-radius: 8px;
      box-sizing: border-box;
    ",
    htmlOutput(ns("pitchDetailsTable"))
  )
}

# SERVER -----------------------------------------------------
pitchDetailsTableServer <- function(id, data_source, player_name) {
  moduleServer(id, function(input, output, session) {
    output$pitchDetailsTable <- renderUI({
      req(player_name())
      HTML(
        plotPitchDetailsTable(
          pitcherData = data_source(),
          pitcherName = player_name()
        )
      )
    })
  })
}
