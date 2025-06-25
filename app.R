library(shiny)
library(tidyverse)
library(rlang)

# Put all the R files here

#Hitting Files
source("zoneChartModule.R")
source("SprayChart.R")
source("advancedStatsHittingModule.R")
source("basicStatsModuleHitting.R")
source("referenceHittingStats.R")
source("rollingXWOBAModule.R")
source("Batted_Ball_Profiles.R")
source("plateDisciplineModule.R")
source("pitchTrackingBatters.R")
source("Catcher_Defense.R")

#Pitching Files
source("advancedStatsPitchingModule.R")
source("basicStatsModulePitching.R")
source("referencePitchingStats.R")
source("pitchDetailsModule.R")
source("pitchDetailsTableModule.R")
source("pitchTrackingPitchers.R")


# Load data
KCLYakkertechData <- read_csv("KCLYakkertechData.csv")
BeltersHitterYakkertechData <- read_csv("BeltersHitterYakkertechData.csv")
BeltersPitcherYakkertechData <- read_csv("BeltersPitcherYakkertechData.csv")

YakkertechHitterData <- bind_rows(KCLYakkertechData, BeltersHitterYakkertechData)
YakkertechPitcherData <- bind_rows(KCLYakkertechData, BeltersPitcherYakkertechData)

hitters  <- unique(YakkertechHitterData$Batter)
pitchers <- unique(YakkertechPitcherData$Pitcher)
roster <- sort(union(hitters, pitchers))

#Savant Data
AdvancedHittingData <- read_csv("HitterSavantData.csv")
AdvancedPitchingData <- read_csv("PitcherSavantData.csv")

playerDetails <- read_csv("playerDetails.csv")
pitchTrackingBatter <- read_csv("YakkertechData/BaseballSavant/batters (pitch tracking).csv")
pitchTrackingPitcher <- read_csv("YakkertechData/Baseballsavant_Pitcher/pitchers (pitch tracking).csv")

# UI
ui <- fluidPage(
  
  conditionalPanel(
    condition = "!output.login_successful",
    div(style = "
          position: absolute;
          top: 30%;
          left: 35%;
          width: 400px
        ",
        wellPanel(
          h2("Baseball Savant Login", style = "text-align: center;"),
          textInput("user", "Username", value = ""),
          textInput("password", "Password", value = ""),
          actionButton("go_to_main_page", "Enter")
        )
    )
  ),
  
  style = "padding: 0; margin: 0; overflow-x: hidden;",
  
  conditionalPanel(
    condition = "output.login_successful",
    div(
      style = "min-height: 100vh; position: relative;",
      
      # Fixed player selector
      absolutePanel(
        top   = "5%", left = "2%", width = "250px",
        selectInput(
          inputId  = "selected_player",
          label    = "Choose a player:",
          choices  = NULL,
          selected = NULL
        )
      ),
      
      # Team Logo
      absolutePanel(
        top   = "0%",
        right = "2%",
        width = "230px",
        uiOutput("teamDisplay")
      ),
      
      uiOutput("mainContent")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Process Login Info
  permissions <- c("bobcats8729", "groundsloths4528", "merchants9253", "bluecaps0827", "admin1928")
  
  login_status <- reactiveVal(FALSE)
  
  observeEvent(input$go_to_main_page, {
    creds <- paste0(tolower(input$user), input$password)
    login_status(creds %in% permissions)
  })
  
  output$login_successful <- reactive({
    login_status()
  })
  
  #Filter Dataset
  filtered_data <- reactive({
    req(input$user)
    if(tolower(input$user) != "admin"){
      YakkertechData %>%
        filter(BatterTeam == paste("Kcl", tolower(input$user), "2025")) |> 
        arrange(Batter)
    }
    else{
      YakkertechData |> 
        arrange(Batter)
    }
  })
  
  observeEvent(filtered_data(), {
    updateSelectInput(
      session,
      "selected_player",
      choices  = roster,
      selected = roster[1]
    )
  })
  
  outputOptions(output, "login_successful", suspendWhenHidden = FALSE)
  
  #Get the player
  selected_player <- reactive({
    req(input$selected_player)
    input$selected_player
  })
  
  #Get player details
  player_details <- reactive({
    playerDetails |> 
      filter(Name == selected_player()) |> 
      select(
        name       = Name,
        position   = Position,
        college    = College,
        side       = Side,
        team       = Team
      ) %>%
      slice(1)
  })
  
  #Get basic hitting stats
  player_hitting_stats <- reactive({
    req(selected_player())
    AdvancedHittingData %>%
      filter(Batter == selected_player()) %>%
      select(AB, H, HR, SB, AVG, OBP, SLG, OPS)
  })
  
  #Get basic pitching stats
  player_pitching_stats <- reactive({
    req(selected_player())
    AdvancedPitchingData %>%
      filter(Pitcher == selected_player()) %>%
      select(W, L, ERA, G, GS, SV, IP, SO, WHIP)
  })
  
  #Get player photos
  action_photo <- reactive({
    normalizePath(
      file.path("www", "actions", paste0(selected_player(), "_action.jpg")),
      mustWork = FALSE
    )
  })
  
  headshot_photo <- reactive({
    normalizePath(
      file.path("www", "headshots", paste0(selected_player(), "_headshot.jpg")),
      mustWork = FALSE
    )
  })
  
  #Get Logo
  output$teamDisplay <- renderUI({
    team_name <- player_details()$team
    logo_src  <- paste0(team_name, ".png")
    
    div(
      style = "
      display: flex;
      align-items: center;
      justify-content: flex-start;
      gap: 20px;
      padding: 5px;
    ",
      
      # Logo
      tags$img(
        src   = logo_src,
        style = "height: 130px; max-width: 130px; object-fit: contain;"
      )
    )
  })
  
  
  output$mainContent <- renderUI({
    req(input$selected_player)
    player <- input$selected_player
    isHit    <- player %in% hitters
    isPitch  <- player %in% pitchers
    
    tabs <- list()
    
    if (isHit) {
      tabs <- append(tabs, list(
        tabPanel("Hitting",
                 # Basic Stats Panel
                 absolutePanel(
                   top    = "13%", 
                   left   = "2%",
                   width  = "27%",
                   basicStatsHittingUI("profileHitting")
                 ),
                 # Advanced Stats
                 absolutePanel(
                   top    = "5%", 
                   bottom = "2%",
                   left   = "30%",   
                   width  = "35%", 
                   advancedStatsHittingUI("advancedHitting")
                 ),
                 # Rolling XWOBA
                 absolutePanel(
                   top = "61%",
                   bottom = "2%",
                   left   = "2%",   
                   width  = "27%", 
                   rollingXWOBAUI("rollingChart")
                 ),
                 # Zone & Spray Chart / Catcher Defense
                 absolutePanel(
                   bottom    = "2%",
                   right     = "2%",
                   width     = "32%",
                   `max-width` = "500px",
                   uiOutput("defenseTabsUI")
                 ),
                 # Scrollable section below fixed panels
                 absolutePanel(
                   top    = "100%",
                   left   = "2%",
                   right  = "2%",
                   style  = "overflow-y: auto; padding: 20px 5%;",
                   referenceHittingStatsUI("refHitting"),
                   br(),
                   battedBallProfileUI("battedProfile"),
                   br(),
                   plateDisciplineUI("plateDiscipline"),
                   br(),
                   pitchTrackingBatterUI("pitchTrackingBatter")
                 )
        )
      ))
    }
    
    # — Pitching tab —
    if (isPitch) {
      tabs <- append(tabs, list(
        tabPanel("Pitching",
                 # Basic Stats Panel
                 absolutePanel(
                   top    = "13%", 
                   left   = "2%",
                   width  = "27%",
                   basicStatsPitchingUI("profilePitching")
                 ),
                 # Advanced Stats
                 absolutePanel(
                   top    = "5%", 
                   bottom = "2%",
                   left   = "30%",   
                   width  = "35%", 
                   advancedStatsPitchingUI("advancedPitching")
                 ),
                 #Pitch Details Plot
                 absolutePanel(
                   right = "2%",
                   top = "15%",
                   height = "65%",
                   width = "32%",
                   pitchDetailsUI("pitchDetails")
                 ),
                 #Pitch Details Table
                 absolutePanel(
                   right = "2%",
                   top = "82%",
                   height = "10%",
                   width = "32%",
                   pitchDetailsTableUI("pitchDetailsTable")
                 ),
                 # Scrollable section below fixed panels
                 absolutePanel(
                   top    = "100%",
                   left   = "2%",
                   right  = "2%",
                   style  = "overflow-y: auto; padding: 10px 0%;",
                   referencePitchingStatsUI("refPitching"),
                   br(),
                   pitchTrackingPitcherUI("pitchTrackingPitcher")
                 )
        )
      ))
    }
    
    # If no tabs, show a placeholder
    if (length(tabs) == 0) {
      div("No data available for this player.")
    } else {
      do.call(tabsetPanel, c(
        list(id = "main_tabs", type = "tabs"),
        tabs
      ))
    }
  })
  
  
  #Get basic hitting stats server
  basicHittingStatsServer(
    id           = "profileHitting",
    details_df   = player_details,
    stats_df     = player_hitting_stats,
    action_img   = action_photo,
    headshot_img = headshot_photo
  )
  
  #Get basic stats server
  basicPitchingStatsServer(
    id           = "profilePitching",
    details_df   = player_details,
    stats_df     = player_pitching_stats,
    action_img   = action_photo,
    headshot_img = headshot_photo
  )
  
  #Reference Hitting Stats Server
  referenceHittingStatsServer(
    id = "refHitting",
    stats_df = reactive({
      AdvancedHittingData %>%
        filter(Batter == selected_player())
    })
  )
  
  #Reference Pitching Stats Server
  referencePitchingStatsServer(
    id = "refPitching",
    stats_df = reactive({
      AdvancedPitchingData %>%
        filter(Pitcher == selected_player())
    })
  )
  
  #Batted Ball Profile Server
  battedBallProfileServer(
    id = "battedProfile",
    data = reactive({
      YakkertechHitterData |> 
        filter(Batter == selected_player())
    })
  )
  
  #Plate Discipline Server
  plateDisciplineServer(
    id = "plateDiscipline",
    data = reactive({
      YakkertechHitterData |> 
        filter(Batter == selected_player())
    })
  )
  
  #Pitch Tracking Server (Hitters)
  pitchTrackingBatterServer(
    id = "pitchTrackingBatter",
    data = reactive({
      pitchTrackingBatter |> 
        filter(Batter == selected_player())
    })
  )
  
  #Pitch Tracking Server (Pitchers)
  pitchTrackingPitcherServer(
    id = "pitchTrackingPitcher",
    data = reactive(pitchTrackingPitcher),
    pitcherName = selected_player
  )
  
  #Get Rolling XWOBA Plot
  rollingXWOBAServer(
    id          = "rollingChart",
    data_source = reactive(YakkertechHitterData),
    player_name = selected_player
  )
  
  #Get advanced hitting stats plot
  advancedStatsHittingServer(
    id          = "advancedHitting",
    data_source = reactive(AdvancedHittingData),
    player_name = selected_player,
    stat_cols = c("avgExitVelo", "maxExitVelo", "LASweetSpot", "hardHitPct", "squaredUpPct",
                  "kPct", "bbPct", "whiffPct", "chasePct", "xBA", "xSLG", "xWOBA", "xWOBA_2",
                  "xBABIP", "babip")
  )
  
  #Get advanced pitching stats plot
  advancedStatsPitchingServer(
    id          = "advancedPitching",
    data_source = reactive(AdvancedPitchingData),
    player_name = selected_player,
    stat_cols = c("xERA", "xBA", "FastballVelo", "avgExitVelo", "chasePct", "whiffPct",
                  "kPct", "bbPct", "hardHitPct", "groundBallPct")
  )
  
  #Get zone chart plot
  zoneChartServer(
    id          = "zoneChart",
    data        = reactive(YakkertechHitterData),
    player_name = selected_player
  )
  
  #Get spray chart plot
  sprayChartServer(
    id          = "sprayChart",
    data        = reactive(YakkertechHitterData),
    player_name = selected_player
  )
  
  #Get pitch details plot
  pitchDetailsServer(
    id = "pitchDetails",
    data_source = reactive(YakkertechPitcherData),
    player_name = selected_player
  )
  
  #Get pitch details table
  pitchDetailsTableServer(
    id = "pitchDetailsTable",
    data_source = reactive(YakkertechPitcherData),
    player_name = selected_player
  )
  
  #Get catcher framing plot
  CatcherFramingServer(
    id          = "catcherFramingChart",
    data        = reactive(YakkertechPitcherData),
    player_name = selected_player,
    league = reactive({
      team <- YakkertechPitcherData |> 
        filter(Catcher == input$selected_player) |> 
        pull(CatcherTeam) |> 
        unique()
      paste(team)
      if (startsWith(team, "Kcl")) {
        "Kcl"
      } else {
        "Prospect"
      }
    })
  )
  
  # 2) Dynamically build the tabs:
  output$defenseTabsUI <- renderUI({
    req(input$selected_player)
    
    # base tab: Zone & Spray
    tabs <- list(
      tabPanel(
        "Zone & Spray",
        tags$div(
          style = "display: flex; flex-direction: column; gap: 15px;",
          sprayChartUI("sprayChart"),
          zoneChartUI("zoneChart")
        )
      )
    )
    
    catchers <- unique(YakkertechPitcherData$Catcher)
    if (input$selected_player %in% catchers) {
      tabs <- append(tabs,
                     list(
                       tabPanel(
                         "Catcher Framing",
                         CatcherFramingUI("catcherFramingChart")
                       )
                     )
      )
    }
    
    do.call(tabsetPanel, c(
      list(id = "defense_tabs", type = "tabs"),
      tabs
    ))
  })
}
    


# Launch the app
shinyApp(ui = ui, server = server)