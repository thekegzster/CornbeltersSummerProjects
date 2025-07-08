

plotPitchDetails <- function(pitcherData, pitcherName){
  pitch_colors <- c(
    "Fastball" = "#c66471",
    "Curveball" = "#90d8ed",
    "Cutter" = "#9c695b",
    "Changeup" = "#85c873",
    "Sinker" = "#edb462",
    "Knuckleball" = "#6c6cd1",
    "Splitter" = "#87bbbc",
    "Slider" = "#edeb75"
  )
  
  circle_data <- tibble(
    radius = rep(c(6,12,18,24), each=200),
    theta  = rep(seq(0, 2*pi, length.out=200), times=4)
  ) %>%
    mutate(
      x = radius * cos(theta),
      y = radius * sin(theta)
    )
  
  circle24 <- tibble(
    theta = seq(0, 2*pi, length.out = 360)
  ) %>%
    mutate(
      x = 25 * cos(theta),
      y = 25 * sin(theta)
    )
  
  x_label_df <- tibble(
    radius = c(12, 24),
    x      = radius, 
    y      = 0,        
    label  = paste0(radius, "\"") 
  )
  
  y_label_df <- tibble(
    radius = c(12, 24),
    x      = 0, 
    y      = radius,        
    label  = paste0(radius, "\"") 
  )
  
  p <- pitcherData %>% 
    filter(Pitcher == pitcherName, !is.na(HorzBreak)) %>% 
    ggplot(aes(x=HorzBreak, y=InducedVertBreak)) +
    coord_fixed(xlim=c(-25,25), ylim=c(-25,25)) +
    geom_polygon(
      data        = circle24,
      aes(x=x, y=y),
      inherit.aes = FALSE,
      fill        = "#e8f2f5",
      color       = NA
    ) +
    geom_path(
      data        = circle_data,
      aes(x = x, y = y, group = radius),
      inherit.aes = FALSE,
      linetype    = "dashed",
      color       = "black",
      alpha = .5
    ) +
    geom_vline(xintercept = 0, alpha = .3) +
    geom_hline(yintercept = 0, alpha = .3) +
    geom_point(
      aes(fill = AutoTaggedPitchType),
      shape  = 21,     
      color  = "black",         
      size   = 4,                
      stroke = 0.5,          
      alpha  = 0.85            
    ) +
    scale_fill_manual(
      name   = "Pitch Type",
      values = pitch_colors,
      na.value = "grey50"
    ) +
    geom_text(
      data        = x_label_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      vjust       = -0.5,
      hjust       =  1.2,
      size        = 3,
      color       = "grey40"
    ) +
    geom_text(
      data        = y_label_df,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      vjust       = 1.4,
      hjust       =  0,
      size        = 3,
      color       = "grey40"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "none"
    ) +
    labs(
      x = element_blank(),
      y = element_blank(),
      title = element_blank()
    )
  print(p)
}

# UI ---------------------------------------------------------
pitchDetailsUI <- function(id) {
  ns <- NS(id)
  div(
    style = "
      height: 100%;
      background: #f9f9f9;
      padding: 2px;
      border: 1px solid #ccc;
      border-radius: 8px;
      box-sizing: border-box;
    ",
    plotOutput(ns("pitchDetailsPlot"), height = "100%")
  )
}

# SERVER -----------------------------------------------------
pitchDetailsServer <- function(id, data_source, player_name) {
  moduleServer(id, function(input, output, session) {
    
    output$pitchDetailsPlot <- renderPlot({
      req(player_name())
      
      plotPitchDetails(
        pitcherData = data_source(),
        pitcherName = player_name()
      )
    }, res = 96)
  })
}