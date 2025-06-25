library(tidyverse)

getBattedBallData <- function(gamedata){
  #Filtering for the teams project pertains to
  batted_balls <- gamedata %>%
    filter(!is.na(HitType),BatterTeam %in% c("Normal cornbelters","Kcl groundsloths 2025",
                                             "Kcl merchants 2025","Kcl bluecaps 2025","Kcl bobcats 2025","Kcl merchants 2026", "Kcl groundsloths 2026", "Kcl bluecaps 2026", "Kcl bobcats 2026")) %>%
    filter(HitType!="Throwdown",HitType!="Foul")
  
  # Finding the Batted Ball Data Profiles
  batted_ball_summary <- batted_balls %>%
    mutate(
      Valid_Bearing = !is.na(Bearing),
      # Spray Direction Percentage
      Spray_Direction = case_when(
        BatterSide == "Right" & Bearing <= -15 ~ "Pull",
        BatterSide == "Right" & Bearing >= 15 ~ "Opposite",
        BatterSide =="Left" & Bearing >= 15 ~ "Pull",
        BatterSide == 'Left' & Bearing <=-15 ~ "Opposite",
        !is.na(Bearing) & Bearing > -15 & Bearing < 15 ~ "Straight",
        TRUE ~ NA_character_
      ),
      BallType = case_when(
        HitType == "GroundBall" ~ "GB",
        HitType %in% c("FlyBall","LineDrive","Popup") ~ "AIR",
        TRUE ~ NA_character_
      )
    )
  
  batted_ball_summary <- batted_ball_summary %>%
    filter(!is.na(Bearing)) %>%
    group_by(Batter) %>%
    summarize(
      BBE=n(),
      Valid_Bearing =sum(Valid_Bearing),
      GB=sum(HitType=="GroundBall"),
      GB_Percent=round(100*GB/BBE,1),
      AIR = sum(HitType %in% c("FlyBall","LineDrive","Popup")),
      AIR_Percent=(100-GB_Percent),
      FB=sum(HitType=="FlyBall"),
      FB_Percent=round(100*FB/BBE,1),
      LD=sum(HitType=="LineDrive"),
      LD_Percent=round(100*LD/BBE,1),
      PU=sum(HitType=="Popup"),
      PU_Percent=round(100*PU/BBE,1),
      #Pull Percent 
      Pull_Percent = ifelse(
        Valid_Bearing > 0,
        round(100*sum((BatterSide=="Right" & Bearing <= -15)|
                        (BatterSide=="Left" & Bearing >= 15), na.rm = TRUE) /Valid_Bearing, 1),0),
      #Straight Percent
      Straight_Percent = ifelse(
        Valid_Bearing > 0, 
        round(100*sum(Bearing > -15 & Bearing < 15, na.rm = TRUE)/Valid_Bearing,1),0),
      # Opposite Percent
      Opposite_Percent =ifelse(
        Valid_Bearing > 0, 
        round(100*sum((BatterSide=="Right" & Bearing >=15)|
                        (BatterSide =="Left" & Bearing <= -15),na.rm= TRUE)/Valid_Bearing, 1),0),
      
      
      # Directional GB and AIR Data 
      Pulled_GB_Percent = ifelse(GB >0, round(100* sum(BallType == "GB" & Spray_Direction == "Pull") / BBE, 1), 0),
      Opposite_GB_Percent = ifelse(GB >0, round(100* sum(BallType == "GB" & Spray_Direction == "Opposite")/BBE, 1), 0),
      Straight_GB_Percent = ifelse(GB >0, round(100* sum(BallType == "GB" & Spray_Direction == "Straight")/BBE, 1), 0),
      Pull_AIR_Percent = ifelse(AIR >0, round(100* sum(BallType == "AIR" & Spray_Direction == "Pull")/ BBE, 1), 0),
      Opposite_AIR_Percent = ifelse(AIR >0, round(100* sum(BallType == "AIR" & Spray_Direction == "Opposite")/ BBE,1), 0),
      Straight_AIR_Percent = ifelse(AIR >0, round(100* sum(BallType == "AIR" & Spray_Direction =="Straight")/ BBE, 1), 0)
    ) %>%
    select(-Batter, -GB,-FB,-LD,-PU,-AIR, -Valid_Bearing) |> 
    rename(
      "GB %" = "GB_Percent",
      "AIR %" = "AIR_Percent",
      "FB %" = "FB_Percent",
      "LD %" = "LD_Percent",
      "PU %" = "PU_Percent",
      "Pull %" = "Pull_Percent",
      "Straight %" = "Straight_Percent",
      "Oppo %" = "Opposite_Percent",
      "Pull GB %" = "Pulled_GB_Percent",
      "Straight GB %" = "Straight_GB_Percent",
      "Oppo GB %" = "Opposite_GB_Percent",
      "Pull AIR %" = "Pull_AIR_Percent",
      "Straight AIR %" = "Straight_AIR_Percent",
      "Oppo AIR %" = "Opposite_AIR_Percent"
    )
}

#–– UI function ––
battedBallProfileUI <- function(id) {
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
      h3("Batted Ball Profile")
    ),
    
    div(
      style = "padding: 12px;",
      tableOutput(ns("batted_table"))
    )
  )
}

#–– Server function ––
battedBallProfileServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$batted_table <- renderTable({
      getBattedBallData(data())
    }, rownames = FALSE)
  })
}



   
 

  