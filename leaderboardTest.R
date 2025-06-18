library(shiny)
library(tidyverse)
library(rlang)
library(DT)

# Put all the R files here
source("zoneChartModule.R")
source("SprayChart.R")
source("advancedStatsModule.R")
source("basicStatsModule.R")
source("referenceStats.R")
source("rollingXWOBAModule.R")


# Load data
YakkertechData <- read_csv("yakkertechData.csv")
AdvancedData <- read_csv("SavantData.csv")

playerDetails <- read_csv("playerDetails.csv")

#UI
ui <- fluidPage(
  ## LOGIN PANEL (shown until login_successful == TRUE) ##
  conditionalPanel(
    condition = "!output.login_successful",
    div(
      style = "position: absolute; top: 30%; left: 35%; width: 400px;",
      wellPanel(
        h2("Baseball Savant Login", style = "text-align: center;"),
        textInput("user", "Username", value = ""),
        passwordInput("password", "Password", value = ""),
        actionButton("go_to_main_page", "Enter")
      )
    )
  ),
  
  ## MAIN APP (shown once login_successful == TRUE) ##
  conditionalPanel(
    condition = "output.login_successful",
    
    navbarPage(
      title = "Yakkertech Baseball App",
      
      ### Player Search Tab ###
      tabPanel(
        "Player Search",
        
        # Player selector
        absolutePanel(
          top    = "5%",
          left   = "2%",
          width  = "250px",
          selectInput(
            inputId  = "selected_player",
            label    = "Choose a player:",
            choices  = NULL
          )
        ),
        
        # Team logo
        absolutePanel(
          top    = "0%",
          right  = "2%",
          width  = "230px",
          uiOutput("teamDisplay")
        ),
        
        # Basic stats
        absolutePanel(
          top    = "13%",
          left   = "2%",
          width  = "26%",
          basicStatsUI("profileChart")
        ),
        
        # Advanced stats
        absolutePanel(
          top    = "5%",
          bottom = "2%",
          left   = "30%",
          width  = "35%",
          advancedStatsUI("advancedChart")
        ),
        
        # Rolling xWOBA
        absolutePanel(
          top    = "61%",
          bottom = "2%",
          left   = "2%",
          width  = "26%",
          rollingXWOBAUI("rollingChart")
        ),
        
        # Spray & Zone charts
        tags$div(
          style = "
            position: absolute;
            bottom: 2%;
            right: 2%;
            width: 34%;
            max-width: 500px;
            display: flex;
            flex-direction: column;
            gap: 15px;
          ",
          sprayChartUI("sprayChart"),
          zoneChartUI("zoneChart")
        ),
        
        # Scrollable reference stats below
        div(
          style = "margin-top: 0; padding: 20px 5%;",
          referenceStatsUI("refStats")
        )
      ),
      
      
      ### Leaderboards Tab ###
      tabPanel(
        "Leaderboards",
        sidebarLayout(
          sidebarPanel(
            checkboxInput(
              inputId = "qualified",
              label   = "Only Show Qualified Hitters (≥ 10 PA)",
              value   = FALSE
            )
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Basic",    DTOutput("basic_table")),
              tabPanel("Advanced", DTOutput("advanced_table"))
            )
          )
        )
      )
      
    )  # end navbarPage
    
  )  # end main conditionalPanel
  
)  # end fluidPage


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
  
  # KEEP the login_successful output alive even when its UI is hidden:
  outputOptions(output, "login_successful", suspendWhenHidden = FALSE)
  
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
      choices = unique(filtered_data()$Batter)
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
  
  #Get basic stats
  player_stats <- reactive({
    req(selected_player())
    AdvancedData %>%
      filter(Batter == selected_player()) %>%
      select(AB, H, HR, SB, AVG, OBP, SLG, OPS)
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
  
  #Get Team Display and Logo
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
  
  #Get basic stats server
  basicStatsServer(
    id           = "profileChart",
    details_df   = player_details,
    stats_df     = player_stats,
    action_img   = action_photo,
    headshot_img = headshot_photo
  )
  
  #Reference Stats Server
  referenceStatsServer(
    id = "refStats",
    stats_df = reactive({
      AdvancedData %>%
        filter(Batter == selected_player())
    })
  )
  
  #Get Rolling XWOBA Plot
  rollingXWOBAServer(
    id          = "rollingChart",
    data_source = reactive(YakkertechData),
    player_name = selected_player
  )
  
  #Get advanced stats plot
  advancedStatsServer(
    id          = "advancedChart",
    data_source = reactive(AdvancedData),
    player_name = selected_player,
    stat_cols = c("avgExitVelo", "maxExitVelo", "LASweetSpot", "hardHitPct", "squaredUpPct",
                  "kPct", "bbPct", "whiffPct", "chasePct", "xBA", "xSLG", "xWOBA", "xWOBA_2",
                  "xBABIP", "babip")
  )
  
  #Get zone chart plot
  zoneChartServer(
    id          = "zoneChart",
    data        = reactive(YakkertechData),
    player_name = selected_player
  )
  
  #Get spray chart plot
  sprayChartServer(
    id          = "sprayChart",
    data        = reactive(YakkertechData),
    player_name = selected_player
  )
  
  # ─────────────────────────────────
  # LEADERBOARDS LOGIC
  # ─────────────────────────────────
  filteredLeaderboardData <- reactive({
    df <- AdvancedData
    if (input$qualified) {
      df <- df %>% filter(PA >= 10)
    }
    df
  })
  
  output$basic_table <- renderDT({
    filteredLeaderboardData() %>%
      select(
        Batter, PA, AB, H, `2B`, `3B`, HR, SB,
        AVG, OBP, SLG, OPS, wOBA, `wRC+`, oWAR
      )
  }, options = list(pageLength = 25, autoWidth = TRUE))
  
  output$advanced_table <- renderDT({
    filteredLeaderboardData() %>%
      select(
        Batter, PA,
        avgExitVelo, maxExitVelo, LASweetSpot,
        hardHitPct, squaredUpPct, kPct, bbPct,
        whiffPct, chasePct,
        xBA, xSLG, xWOBA, xBABIP
      )
  }, options = list(pageLength = 25, autoWidth = TRUE))
}

# Launch the app
shinyApp(ui = ui, server = server)