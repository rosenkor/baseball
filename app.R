# R-Shiny: Major League Baseball Over the Years
# Theo Deitz-Green and Kory Rosen
# STA 230


#Installing necessary libraries to run program
library(shiny)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(plotly)
library(shinythemes)


#Importing data
Batting2 <- read.csv('Data/Batting2.csv')
Pitching2 <- read.csv('Data/Pitching2.csv')
Fielding2 <- read.csv('Data/Fielding2.csv')
Teams2 <- read.csv('Data/Teams2.csv')


#Next section of code creates three sets of statistics - fielding, 
#pitching,and hitting - and assigning each statistic
#a user-friendly name. Used later as part of code allowing user to
#select type of statistic to explore and then being presented
#with only statistics of that type.

field_choices <-
  c(
    "Put Outs" = "PO",
    "Assists" = "A",
    "Errors" = "E",
    "Double plays" = "DP",
    "Passed Balls" = "PB",
    "Wild Pitches" = "WP",
    "Opponents Caught Stealing" = "CSF",
    "Zone Rating" = "ZR"
  )
pitch_choices <-
  c(
    "Games" = "G",
    "Games Started" = "GS",
    "Complete Games" = "CG",
    "Shutouts" = "SHO",
    "Saves" = "SV",
    "Outs Pitched" = "IPouts",
    "Hits" = "H",
    "Earned runs" = "ER",
    "Home Runs" = "HRP",
    "Walks" = "BB",
    "Strikeouts" = "SO",
    "Opponent's Batting Average" = "BAOpp",
    "Earned Run Average" = "ERA",
    "Intentional Walks" = "IBB",
    "Wild Pitches" = "WP",
    "Batters Hit By Pitch" = "HBP",
    "Balks" = "BK",
    "Batters Faced By Pitcher" = "BFP",
    "Games Finished" = "GF",
    "Runs Allowed" = "RP",
    "Sacrificed Hits Allowed" = "SHP",
    "Sacrifice Flies Allowed" = "SFP",
    "Grounded Into Double Plays" = "GIDPP"
  )
hit_choices <- c(
  "At Bats" = "AB",
  "Runs" = "RB",
  "Hits" = "H",
  "Doubles" = "DoublesB",
  "Triples" = "TriplesB",
  "Home Runs" = "HRB",
  "Runs Batted In" = "RBI",
  "Stolen Bases" = "SB",
  "Caught Stealing" = "CS",
  "Walks" = "BB",
  "Strikeouts" = "SO",
  "Intentional Walks" = "IBB",
  "Hit By Pitch" = "HBPB",
  "Sacrifice Hits" = "SHB",
  "Sacrifice Flies" = "SFB",
  "Double Plays Grounded Into" = "GIDPB"
)
all_choices <- c(field_choices, pitch_choices, hit_choices)

teams_choices <-
  c(
    "Wins" = "WT",
    "Losses" = "LT",
    "Runs scored" = "RT",
    "At Bats" = "ABT",
    "Hits By Batters" = "HT",
    "Doubles" = "DoublesT",
    "Triples" = "TriplesT",
    "Home Runs By Batters" = "HRT",
    "Walk By Batters" = "BBT",
    "Strikeouts By Batters" = "SOT",
    "Stolen Bases" = "SBT",
    "Caught Stealing" = "CST",
    "Batters Hit By Pitch" = "HBPT",
    "Sacrifice Flies" = "SFT",
    "Opponents Runs Scored" = "RAT",
    "Earned Runs Allowed" = "ERT",
    "Earned Run Average" = "ERAT",
    "Complete Games" = "CGT",
    "Shutouts" = "SHOT",
    "Saves" = "SVT",
    "Outs Pitched" = "IPOutsT",
    "Hits Allowed" = "HAT",
    "Home Runs Allowed" = "HRAT",
    "Walks Allowed" = "BBAT",
    "Strikeouts By Pitchers" = "SOAT",
    "Errors" = "ET",
    "Double Plays" = "DPT",
    "Fielding Percentage" = "FPT"
  )

teamss_choices <- c(teams_choices)


#Defining User Interface (UI)
ui <- shiny::fluidPage(theme = shinytheme("flatly"),collapsable = TRUE,
  
  #Establishing app title
  titlePanel("Major League Baseball Over the Years"),
  
  #Creating directory of tabs
  navbarPage(
    "Directory",
    
    #Setting up user interface for tab #1: League-Wide Statistics
    tabPanel(
      "League-Wide Statistics",
      sidebarLayout(
        sidebarPanel(
          h1("Customize Analysis"),
          
          #Creating widget #1 - a radio button that allows users to 
          #select the type of baseball statistic they would like to 
          #analyze
          
          radioButtons(
            "stattypes",
            h3("Type of Statistic"),
            c(
              "Pitching" = "pitch",
              "Hitting" = "hit",
              "Fielding" = "field"
            )
          ),
          
          #Creating widget #2 - a drop down menu that allows users to 
          #select which specific baseball statistic they would like to 
          #analyze. Note that the specific set of statistics they can 
          #choose from changes based on which type of statistic 
          #they choose for widget #1. Therefore, widget #2 is coded as an 
          #output, and its value will be changed by the server 
          #depending on the value of widget #1.  
          
          uiOutput("stat1"),
          
          #Creating widget #3 - a slider input allowing users to change 
          #the year range over which they would like to analyze their 
          #chosen statistic      
          
          sliderInput(
            "years",
            h3("Year Range"),
            1871,
            2020,
            value = c(1871, 2020),
            sep = "",
            animate = FALSE
          ),
          
          #Creating widget #4 - a radio button allowing users to
          #choose to calculate statistics by games-per-season 
          
          radioButtons(
            "bygames",
            h3("Calculate by Games-Per-Season"),
            choices = list("Yes" = "y", "No" = "n"),
            selected = "y"
          )
          
        ),
        mainPanel(
          
          #Creating the graph output, suppressing error messages
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
          ),
          plotlyOutput("stat1Plot"),
        )
      )
    ),
    
    #Setting up user interface for tab #2: Team-Wide Statistics  
    tabPanel(
      "Team-Wide Statistics",
      sidebarLayout(
        sidebarPanel( 
      #Creating widget #1 - a drop down menu that allows users to 
      #select which baseball team they would like to analyze 
          
      selectInput(
        "TeamsA",
        h3("Team"),
        choices = unique(Teams2$nameT),
        selected = Teams2$nameT[1]
      ),
      
      #Creating widget #2 - a drop down menu that allows users to 
      # select which statistic they would like to analyze. 
      #Note that it is coded as an output. 
      #This is simply a function of a change we made 
      #during the process of crafting the code - the values of the 
      #drop down menu do not vary based on user input, but changing 
      # back would have been too time consuming. 
      
      uiOutput("stat2"),
      
      #Creating widget #3 - a slider input that allows users to 
      #determine which year range they wish to consider. 
      #Note that it is coded as an output. 
      #This is because values of this slider input 
      #change based on which team users select for widget #1. 
      #Since different teams existed for different year ranges, 
      #it makes sense to have the year range on the slider reflect 
      #the years for which the team played. 
      #Therefore, widget #3 is coded as an output, and its 
      #value will be changed by the server depending on the 
      #value of widget #1.  
      
      uiOutput("stat3"),
      
      #Creating widget #4 -  a radio button allowing users to 
      #choose to calculate statistics by games-per-season 
      
      radioButtons(
        "bygames2",
        h3("Calculate by Games-Per-Season"),
        choices = list("Yes" = "y", "No" = "n"),
        selected = "y"
      ),
    ),
      mainPanel(
        
        #Creating the graph output, suppressing temporary error messages
        #that were not effecting how the code ran, just it 
        #giving an error message.
        
        tags$style(
          type = "text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
        ),
        plotlyOutput("stat2Plot"),
        
        ),
      ),
      ),
    ),
  )

# Defining server logic
server <- function(input, output) {
  
  #Next sections switch values of League-Wide widget #2 
  #based on user choice for widget #1 to display the set of 
  #statistics corresponding to the statistic type selected by user
  
  output$stat1 <- renderUI({
    switch(
      input$stattypes,
      "field" = selectInput("stat1", label = h3("Fielding Statistic"),
                            choices = field_choices),
      "pitch" = selectInput("stat1", label = h3("Pitching Statistic"),
                            choices = pitch_choices),
      
      "hit" =  selectInput ("stat1", label = h3("Batting Statistic"),
                            choices = hit_choices)
    )
    
  })
  
  #Sets up statistic options user can 
  #chose from in Team-Wide widget #2 
  #(see more detailed explanation in UI) 
  
  output$stat2 <- renderUI({
    selectInput("stat2", label = h3("Statistic"),
                choices = teams_choices)
  })
  
  #Establishes reactive system that automatically changes the 
  #year range available for the user to select from in Team-Wide
  #widget #3 based on the years the team chosen in widget #1 existed
  
  output$stat3 <- renderUI({
    g <- NewData()
    sliderInput(
      "yearsT",
      h3("Year Range"),
      min(g$yearIDT),
      max(g$yearIDT),
      value = c(min(g$yearIDT), max(g$yearIDT)),
      sep = "",
      animate = FALSE
    )
  })
  
#Next sections create code that generates new data set 
#filtered by year range selected by user in League-Wide 
#widget #3 (making the x-axis of the graph - year - reactive 
#to user input). These sections then create a new variable 
#whose values equal the statistic chosen by the user in 
#League-Wide widget #2 divided by the number of games played 
#each year. This is used later to graph statistics-per-game by year.
  
  PreparedData <- reactive ({
    if (input$stattypes == "hit")
    {
      d <- Batting2
      d <- Batting2[Batting2$yearID >= input$years[1] &
                      Batting2$yearID <= input$years[2], ]
      d$bygame = d[, input$stat1] / d$G
      d$year <- as.numeric(d$year)
    }
    if (input$stattypes == "pitch")
    {
      d <- Pitching2
      d <- Pitching2[Pitching2$yearID >= input$years[1] &
                       Pitching2$yearID <= input$years[2], ]
      d$bygame = d[, input$stat1] / d$G
      d$year <- as.numeric(d$year)
    }
    if (input$stattypes == "field")
    {
      d <- Fielding2
      d <- Fielding2[Fielding2$yearID >= input$years[1] &
                       Fielding2$yearID <= input$years[2], ]
      d$bygame = d[, input$stat1] / d$G
      d$year <- as.numeric(d$year)
      
    }
    d
  })
  
  #Next sections create a data set filtered to include only year 
  #range chosen by user in Team-Wide widget #3. 
  #These sections then create a graph showing user's chosen statistic 
  #League-Wide widget #2) over user's chosen time range (League-Wide 
  #widget #3). Graphs the statistic-per-game each 
  #year if this option is selected in League-Wide widget #4, and 
  #graphs total statistics per year if this option is selected 
  
  output$stat1Plot <- renderPlotly({
    d = PreparedData()
    if (input$bygames == "y")
    {
      graph <-
        ggplot(data = d) + geom_point(aes_string(x = "yearID", y = "bygame"), colour =
                                        "navy") + scale_x_continuous(breaks = waiver()) + labs(
                                          title = paste(names(all_choices[which(all_choices == input$stat1)]), "Over Time (By Games-Per-Season)"),
                                          x = "Year",
                                          y = names(all_choices[which(all_choices == input$stat1)])
                                        ) + theme_light() 
      
      ggplotly(graph)
    }
    else {
      graph2 <-
        ggplot(data = d) + geom_point(aes_string(x = "yearID", y = input$stat1), colour =
                                        "navy") + scale_x_continuous(breaks = waiver()) + labs(
                                          title = paste(names(all_choices[which(all_choices == input$stat1)]), "Over Time"),
                                          x = "Year",
                                          y = names(all_choices[which(all_choices == input$stat1)])
                                        ) + theme_light()
      
      ggplotly(graph2)
    }
    
  })
  
  #Next sections create new data set filtered to 
  #include statistics only for the team selected by 
  #the user in Team-Wide widget #3, allowing app to graph 
  #statistics specifically of team chosen by user. 
  #These sections then create a new variable whose values 
  #equal the statistic chosen by the user in League-Wide 
  #widget #2 divided by the number of games 
  #played each year. This is used later to graph 
  #statistics-per-game by year
  
  NewData <- reactive({
    TeamData <- Teams2 %>%
      select(c(1:50)) %>%
      filter(nameT == input$TeamsA)
  
    TeamData$bygame = TeamData[, input$stat2] / TeamData$GT
    TeamData
    
    
  })
  
  #Next sections create a new data set filtered to include only the 
  #year range selected by the user in Team-Wide widget #3. 
  #These sections then create graph showing user's chosen statistic 
  #(Team-Wide widget #1)over user's chosen time range 
  #(Team-Wide widget #2). Graphs the statistic-per-game each year 
  #if this option is selected in Team-Wide widget #4, 
  #and graphs total statistics per year if this option is selected.   
  
  output$stat2Plot <- renderPlotly({
    g = NewData()
    ga <-
      g[g$yearIDT >= input$yearsT[1] &
          g$yearIDT <= input$yearsT[2],]
    if (input$bygames2 == "y")
    {
      ga$bygame <- unlist(ga$bygame)
      graph <-
        ggplot(data = ga) + geom_point(aes_string(x = "yearIDT", y = "bygame"), colour =
                                         "navy") + scale_x_continuous(breaks = waiver()) + labs(
                                           title = paste(
                                             names(teams_choices[which(teams_choices == input$stat2)]),
                                             "Over Time (By Games-Per-Season) for the",
                                             input$TeamsA
                                           ),
                                           x = "Year",
                                           y = names(teams_choices[which(teams_choices == input$stat2)])
                                         ) + theme_light()
      
      ggplotly(graph)
    }
    else {
      graph2 <-
        ggplot(data = ga) + geom_point(aes_string(x = "yearIDT", y = input$stat2),
                                       colour = "navy") +
        scale_x_continuous(breaks = waiver()) + labs(
          title = paste(names(teams_choices[which(teams_choices == input$stat2)]), "Over Time for the", input$TeamsA),
          x = "Year",
          y = names(teams_choices[which(teams_choices == input$stat2)])
        ) + theme_light() 
      ggplotly(graph2, tooltype = "text")
    }
    
  })
  
}



# Runs the app
shinyApp(ui = ui, server = server)