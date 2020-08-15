######################################################################################
# AFL Visual Analysis Shiny App
# Initial deployment - 15/8/2020 RL
######################################################################################

# Load required libraries and attempt to install if not already installed on users system
if(!require(tidyverse)) {
  install.packages("tidyverse")
} else {
  library(tidyverse)
}
if(!require(tidyverse)) {
  install.packages("ggforce")
} else {
  library(ggforce)
}
if(!require(tidyverse)) {
  install.packages("grid")
} else {
  library(grid)
}
if(!require(tidyverse)) {
  install.packages("ggisoband")
} else {
  library(ggisoband)
}
if(!require(tidyverse)) {
  install.packages("shiny")
} else {
  library(shiny)
}
if(!require(tidyverse)) {
  install.packages("shinydashboard")
} else {
  library(shinydashboard)
}
if(!require(tidyverse)) {
  install.packages("shinyjs")
} else {
  library(shinyjs)
}

############################################################
# Global environment
############################################################

# Venues with lengths and widths
venues <- tibble(Venue = c("MCG", "Docklands", "Custom", "Perth Stadium", "Adelaide Oval", 
                           "Gabba", "Carrara", "SCG", "Sydney Showgrounds", "Kardinia Park"),
                 x = c(160, 160, 160, 165, 167, 156, 158, 155, 164, 170),
                 y = c(141, 129, 141, 130, 124, 138, 134, 136, 128, 116))

# Teams and colors for full chain plots
teams <- tibble(Team = c("Adelaide", "Brisbane", "Carlton", "Collingwood", "Essendon", "Fremantle", "Geelong", 
                         "Gold Coast", "GWS", "Hawthorn", "Melbourne", "North Melbourne", "Port Adelaide", 
                         "Richmond", "St Kilda", "Sydney", "West Coast", "western Bulldogs"),
                Color = c("#e41937", "#68003d", "#031e2f", "#000000", "#000000", "#331b55", "#002247", "#f03226", "#ff7a01",
                          "#342015", "#061a33", "#003b9f", "#000000", "#000000", "#000000", "#ffffff", "#013088", "#0038a5"))

# Available analysis types
analysis <- c("Inside 50 Heatmaps", "Inside 50 Kicks", "D50 Exit Kicks", "D1/2 Set Disposals", "Full Chain")

############################################################
# UI section
############################################################

ui <- dashboardPage(
  dashboardHeader(title = "AFL Visual Analysis"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Data Collection", tabName = "data_collection", icon = icon("clipboard-list")),
                menuItem("Visual Output", tabName = "visual_output", icon = icon("project-diagram")),
                menuItem("Data Output", tabName = "data_output", icon = icon("file-excel")),
                tags$style(type="text/css", # Supress error messages if no data available
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_collection",
              useShinyjs(),
              fluidRow(box(title = "Game Information",
                           fluidRow(box(selectInput("venue", "Select Venue:", choices = venues$Venue), width = 4),
                                    box(uiOutput("length"), width = 4),
                                    box(uiOutput("width"), width = 4)),
                           fluidRow(box(uiOutput("home"), width = 3),
                                    box(uiOutput("away"), width = 3),
                                    box(uiOutput("right"), width = 3),
                                    box(selectInput("quarter", "Quarter:", choices = c(1, 2, 3, 4)), width = 3)), width = 12)),
              fluidRow(box(title = textOutput("home_team"), plotOutput("home_collection", click = "plot_click_h"), width = 6),
                       box(title = textOutput("away_team"), plotOutput("away_collection", click = "plot_click_a"), width = 6)),
              fluidRow(box(tags$script(HTML("$(function(){                        
                                            $(document).keyup(function(r_h) {
                                            if (r_h.which == 49) {
                                              $('#retain_h').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("retain_h", "Retained", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(s_h) {
                                            if (s_h.which == 50) {
                                              $('#stoppage_h').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("stoppage_h", "Stoppage", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(t_h) {
                                            if (t_h.which == 51) {
                                              $('#to_h').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("to_h", "TO", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(g_h) {
                                            if (g_h.which == 52) {
                                              $('#goal_h').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("goal_h", "Goal", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(b_h) {
                                            if (b_h.which == 53) {
                                              $('#behind_h').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("behind_h", "Behind", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(i_h) {
                                            if (i_h.which == 54) {
                                              $('#id_h').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("id_h", "End Chain", width = "16%")),
                       box(tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(r_a) {
                                            if (r_a.which == 81) {
                                              $('#retain_a').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("retain_a", "Retained", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(s_a) {
                                            if (s_a.which == 87) {
                                              $('#stoppage_a').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("stoppage_a", "Stoppage", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(t_a) {
                                            if (t_a.which == 69) {
                                              $('#to_a').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("to_a", "TO", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(g_a) {
                                            if (g_a.which == 82) {
                                              $('#goal_a').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("goal_a", "Goal", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(b_a) {
                                            if (b_a.which == 84) {
                                              $('#behind_a').click()
                                            }
                                            });
                                            })")), # JS function to "hotkey" the actionButton
                           actionButton("behind_a", "Behind", width = "16%"),
                           tags$script(HTML("$(function(){ 
                                            $(document).keyup(function(i_a) {
                                            if (i_a.which == 89) {
                                              $('#id_a').click()
                                            }
                                            });
                                            })")),# JS function to "hotkey" the actionButton
                           actionButton("id_a", "End Chain", width = "16%")))
      ),
      tabItem(tabName = "visual_output",
              fluidRow(box(selectInput("analysis_type", "Select Analysis Type:", choices = analysis), width = 3),
                       box(selectInput("time_filter", "Filter output by Quarter/Half/Match:", 
                                       choices = c("Match", "1st Half", "2nd Half", "Q1", "Q2", "Q3", "Q4")), width = 3)),
              fluidRow(box(plotOutput("home_visual"), 
                           downloadButton("png_home", "Save PNG"), width = 12, height = 800)),
              fluidRow(box(plotOutput("away_visual"), 
                           downloadButton("png_away", "Save PNG"), width = 12, height = 800))
      ),
      tabItem(tabName = "data_output",
              fluidRow(box(title = textOutput("home_team_tab"),
                           tableOutput("table_h"),
                           numericInput("delete_h", "ID to delete", value = 0),
                           actionButton("delete_h_button", "Delete"), 
                           actionButton("save_h", "Save to CSV"), width = 6),
                       box(title = textOutput("away_team_tab"),
                           tableOutput("table_a"),
                           numericInput("delete_a", "ID to delete", value = 0),
                           actionButton("delete_a_button", "Delete"), 
                           actionButton("save_a", "Save to CSV"), width = 6))
      )
    )
  )
)

############################################################
# Server section
############################################################

server <- function(input, output){
  
  ##########################################################
  # Data collection tab
  ##########################################################
  
  # Output for title of datacollection plots
  output$home_team <- renderText({
    paste(input$home)
  })
  
  output$away_team <- renderText({
    paste(input$away)
  })
  
  output$home_team_tab <- renderText({
    paste(input$home)
  })
  
  output$away_team_tab <- renderText({
    paste(input$away)
  })
  
  # UI for venue length
  output$length <- renderUI({
    numericInput("length", "Venue Length:", value = filter(venues, Venue == input$venue)$x)
  })
  
  # UI for venue width
  output$width <- renderUI({
    numericInput("width", "Venue Width:", value = filter(venues, Venue == input$venue)$y)
  })
  
  # UI for home team
  output$home <- renderUI({
    selectInput("home", "Select Home Team:", choices = teams$Team)
  })
  
  # UI for away team
  output$away <- renderUI({
    req(input$home)
    selectInput("away", "Select Away Team:", choices = filter(teams, Team != input$home)$Team)
  })
  
  # UI for the team kicking to the right in the 1st quarter
  output$right <- renderUI({
    selectInput("right", "Select Team Kicking to the Right in 1st Quarter:", choices = c(input$home, input$away))
  })
  
  # Reactive object for plotting the oval 
  oval <- reactive({
    # Section required to calculate the intersection of the 50m arc with the boundary of the oval
    size <- tibble(x = input$length, y = input$width)
    z <- (((size$x + size$y) / 4.01)^2 - 2500 + (size$x / 2)^2) / (2 * (size$x / 2))
    h <- ((size$x + size$y) / 4)
    a <- sqrt(h^2 - z^2)
    angle <- acos(a / 50)
    
    # Oval plot
    oval <- ggplot(size) +
      geom_ellipse(aes(x0 = 0, y0 = 0, a = x / 2, b = y / 2, angle = 0), size = 1.2) + # outer boundary
      geom_tile(aes(x = 0, y = 0, width = 50, height = 50), fill = "#ffffff00", col = "black", size = 1.2) + # centre square
      geom_tile(aes((x = x / 2) - 4.5, y = 0, width = 9, height = 6.7), fill = "#ffffff00", col = "black", size = 1.2) + # right goalsquare
      geom_tile(aes((x = -x / 2) + 4.5, y = 0, width = 9, height = 6.7), fill = "#ffffff00", col = "black", size = 1.2) + # left goalsquare
      geom_circle(aes(x0 = 0, y0 = 0, r = 3), size = 1.2) + # centre circle
      geom_arc(aes(x0 = x / 2, y0 = 0, r = 50, start = -angle, end = -(3.142 - angle)), size = 1.2) + # right 50m arc
      geom_arc(aes(x0 = -x / 2, y0 = 0, r = 50, start = angle, end = 3.142 - angle), size = 1.2) + # left 50m arc
      coord_fixed(ratio = 1) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            plot.margin = margin(0, 0, 0, 0, "pt"),
            text = element_text(size = 20)) 
    
    return(oval)
  })
  
  # set up reactive values for home team
  home_values <- reactiveValues()
  
  # create blank tibble for home team values
  home_values$DT <- tibble(ID = numeric(0),
                           Team = as.character(),
                           Quarter = as.numeric(),
                           x = numeric(),
                           y = numeric(),
                           Retained = numeric(0),
                           Stoppage = numeric(0),
                           TO = numeric(0),
                           Goal = numeric(0),
                           Behind = numeric(0))
  
  # set up reactive values for away team
  away_values <- reactiveValues()
  
  # create blank tibble for away team values
  away_values$DT <- tibble(ID = numeric(0),
                           Team = as.character(),
                           Quarter = as.numeric(),
                           x = numeric(),
                           y = numeric(),
                           Retained = numeric(0),
                           Stoppage = numeric(0),
                           TO = numeric(0),
                           Goal = numeric(0),
                           Behind = numeric(0))
  
  # plot of oval for collecting data points for home team
  output$home_collection <- renderPlot({
    req(input$home)
    oval() +
      geom_point(data = filter(home_values$DT, ID == max(ID)), aes(x, y)) # only display points from last ID 
  })
  
  # plot of oval for collecting data points from away team
  output$away_collection <- renderPlot({
    req(input$away)
    oval() +
      geom_point(data = filter(away_values$DT, ID == max(ID)), aes(x, y)) # only display points from last ID 
  })
  
  # collect the x, y cordinates and summary info for clicks on the home data collection plot
  observeEvent(input$plot_click_h, {
    new_row <- tibble(ID = as.numeric(input$id_h) + 1,
                      Team = as.character(input$home),
                      Quarter = as.numeric(input$quarter),
                      x = round(input$plot_click_h$x, 0),
                      y = round(input$plot_click_h$y, 0))
    
    home_values$DT <- bind_rows(home_values$DT, new_row) 
  })
  
  # mark the row that the retained button or hotkey is clicked
  observeEvent(input$retain_h,{
    home_values$DT <- home_values$DT %>%
      mutate(Retained = if_else(row_number() == max(row_number()), 1, Retained))
  })
  
  # mark the row that the stoppage button or hotkey is clicked
  observeEvent(input$stoppage_h,{
    home_values$DT <- home_values$DT %>%
      mutate(Stoppage = if_else(row_number() == max(row_number()), 1, Stoppage))
  })
  
  # mark the row that the turnover button or hotkey is clicked
  observeEvent(input$to_h,{
    home_values$DT <- home_values$DT %>%
      mutate(TO = if_else(row_number() == max(row_number()), 1, TO))
  })
  
  # mark the row that the goal button or hotkey is clicked
  observeEvent(input$goal_h,{
    home_values$DT <- home_values$DT %>%
      mutate(Goal = if_else(row_number() == max(row_number()), 1, Goal))
  })
  
  # mark the row that the behind button or hotkey is clicked
  observeEvent(input$behind_h,{
    home_values$DT <- home_values$DT %>%
      mutate(Behind = if_else(row_number() == max(row_number()), 1, Behind))
  })
  
  # collect the x, y cordinates and summary info for clicks on the away data collection plot
  observeEvent(input$plot_click_a, {
    new_row <- tibble(ID = as.numeric(input$id_a) + 1,
                      Team = as.character(input$away),
                      Quarter = as.numeric(input$quarter),
                      x = round(input$plot_click_a$x, 0),
                      y = round(input$plot_click_a$y, 0))
    
    away_values$DT <- bind_rows(away_values$DT, new_row) 
  })
  
  # mark the row that the retained button or hotkey is clicked
  observeEvent(input$retain_a,{
    away_values$DT <- away_values$DT %>%
      mutate(Retained = if_else(row_number() == max(row_number()), 1, Retained))
  })
  
  # mark the row that the stoppage button or hotkey is clicked
  observeEvent(input$stoppage_a,{
    away_values$DT <- away_values$DT %>%
      mutate(Stoppage = if_else(row_number() == max(row_number()), 1, Stoppage))
  })
  
  # mark the row that the turnover button or hotkey is clicked
  observeEvent(input$to_a,{
    away_values$DT <- away_values$DT %>%
      mutate(TO = if_else(row_number() == max(row_number()), 1, TO))
  })
  
  # mark the row that the goal button or hotkey is clicked
  observeEvent(input$goal_a,{
    away_values$DT <- away_values$DT %>%
      mutate(Goal = if_else(row_number() == max(row_number()), 1, Goal))
  })
  
  # mark the row that the behind button or hotkey is clicked
  observeEvent(input$behind_a,{
    away_values$DT <- away_values$DT %>%
      mutate(Behind = if_else(row_number() == max(row_number()), 1, Behind))
  })
  
  ##########################################################
  # Visual output tab
  ##########################################################
  
  # reactive object for creating the heatmaps for the home team
  heatmap_home <- reactive({
    plot_data <- home_values$DT %>%
      filter(if(input$time_filter == "Q1") Quarter == 1        # filters for the quarter/half/full match
             else if (input$time_filter == "Q2") Quarter == 2
             else if (input$time_filter == "Q3") Quarter == 3
             else if (input$time_filter == "Q4") Quarter == 4
             else if (input$time_filter == "1st Half") Quarter %in% c(1, 2)
             else if (input$time_filter == "2nd Half") Quarter %in% c(3, 4)
             else Quarter %in% c(1, 2, 3, 4)) %>%
      mutate(x = case_when(input$home == input$right & Quarter %in% c(2, 4) ~ -x, # normalises data so the team is always kicking right 
                           input$home != input$right & Quarter %in% c(1, 3) ~ -x,
                           TRUE ~ x),
             y = case_when(input$home == input$right & Quarter %in% c(2, 4) ~ -y,
                           input$home != input$right & Quarter %in% c(1, 3) ~ -y,
                           TRUE ~ y)) %>%
      group_by(ID) %>%
      filter(row_number() == max(row_number())) %>% # filter to only collect last plot click if more than one collected for each ID
      mutate(Result = case_when(Goal == 1 ~ "Goal", # set up Result variable to enable colored points
                                Behind == 1 ~ "Behind",
                                TRUE ~ "No Score"))
    
    # section required to calculate the intersection of the 50m arcs and boundary
    size <- tibble(x = input$length, y = input$width)
    z <- (((size$x + size$y) / 4.01)^2 - 2500 + (size$x / 2)^2) / (2 * (size$x / 2))
    h <- ((size$x + size$y) / 4)
    a <- sqrt(h^2 - z^2)
    angle <- acos(a / 50)
    
    ggplot(plot_data, aes(x, y)) +
      coord_fixed(ratio = 1) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            plot.margin = margin(0, 0, 0, 0, "pt"),
            legend.title = element_blank(),
            legend.position = "top",
            plot.title = element_text(hjust = 0.5),
            text = element_text(size = 20)) +
      geom_density_bands(aes(fill = stat(density)), col = "grey70", size = 0.2, alpha = 0.5) + # creates the density bands of heatmap
      scale_fill_gradient(high = "red", low = "#ffffff00", guide = "none") + # fills the density bands of the heatmap
      geom_ellipse(aes(x0 = 0, y0 = 0, a = size$x / 2, b = size$y / 2, angle = 0), size = 1.2) + # oval boundary line
      geom_tile(aes(x = 0, y = 0, width = 50, height = 50), fill = "#ffffff00", col = "black", size = 1.2) + # centre square
      geom_tile(aes((x = size$x / 2) - 4.5, y = 0, width = 9, height = 6.7), fill = "#ffffff00", col = "black", size = 1.2) + # right goal square
      geom_tile(aes((x = -size$x / 2) + 4.5, y = 0, width = 9, height = 6.7), fill = "#ffffff00", col = "black", size = 1.2) + # left goal square
      geom_circle(aes(x0 = 0, y0 = 0, r = 3), size = 1.2, inherit.aes = F) + # centre circle
      geom_arc(aes(x0 = size$x / 2, y0 = 0, r = 50, start = -angle, end = -(3.142 - angle)), size = 1.2, inherit.aes = F) + # right 50m arc
      geom_arc(aes(x0 = -size$x / 2, y0 = 0, r = 50, start = angle, end = 3.142 - angle), size = 1.2, inherit.aes = F) + # left 50m arc
      geom_point(aes(col = Result), size = 3) + # location of F50 targets
      scale_color_manual(values = c("Goal" = "blue", "Behind" = "#74a9cf", "No Score" = "grey40")) + 
      xlim(-size$x / 2, size$x / 2) +
      ylim(-size$y / 2, size$y / 2) +
      labs(title = paste(input$home, "I50 Heatmap"))
  })
  
  # reactive object for home team plots requiring segments  
  segments_home <- reactive({
    segments <- home_values$DT %>%
      filter(if(input$time_filter == "Q1") Quarter == 1       # filters for the quarter/half/full match
             else if (input$time_filter == "Q2") Quarter == 2
             else if (input$time_filter == "Q3") Quarter == 3
             else if (input$time_filter == "Q4") Quarter == 4
             else if (input$time_filter == "1st Half") Quarter %in% c(1, 2)
             else if (input$time_filter == "2nd Half") Quarter %in% c(3, 4)
             else Quarter %in% c(1, 2, 3, 4)) %>%
      mutate(x = case_when(input$home == input$right & Quarter %in% c(2, 4) ~ -x, # normalises data so the team is always kicking right
                           input$home != input$right & Quarter %in% c(1, 3) ~ -x,
                           TRUE ~ x),
             y = case_when(input$home == input$right & Quarter %in% c(2, 4) ~ -y,
                           input$home != input$right & Quarter %in% c(1, 3) ~ -y,
                           TRUE ~ y)) %>%
      group_by(ID) %>%
      mutate(xend = lead(x), # sets up the xend variable
             yend = lead(y), # sets up the yend variable
             Retained = lead(Retained), # moves the Retained etc markers to the row above
             Stoppage = lead(Stoppage),
             TO = lead(TO),
             Goal = lead(Goal),
             Behind = lead(Behind)) %>%
      filter(row_number() != max(row_number())) %>% # removes last row which has no xend, yend data
      left_join(., teams, by = "Team")
  })
  
  # reactive object for the home team analysis plot output
  home_vis <- reactive({
    # if statements display the selected analysis type
    if(input$analysis_type == "Inside 50 Heatmaps") {
      heatmap_home() 
    } else if(input$analysis_type == "Full Chain") {
      oval() +
        geom_segment(data = segments_home(), aes(x = x, xend = xend, y = y, yend = yend), size = 1.1, col = segments_home()$Color) +
        labs(title = paste(input$home, input$analysis_type)) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if(input$analysis_type %in% c("Inside 50 Kicks", "D50 Exit Kicks")) {
      # sets up a Result variable to allow colored segments by Result type
      plot_data <- segments_home() %>%
        mutate(Result = case_when(Retained == 1 ~ "Retained",
                                  Stoppage == 1 ~ "Stoppage",
                                  TO == 1 ~ "TO",
                                  TRUE ~ "NA"))
      
      oval() +
        geom_segment(data = plot_data, 
                     aes(x = x, xend = xend, y = y, yend = yend, col = Result), size = 1.1) +
        scale_color_manual(values = c("Retained" = "blue", "Stoppage" = "orange", "TO" = "red", "NA" = "grey50")) +
        theme(legend.title = element_blank(),
              legend.position = "top",
              plot.title = element_text(hjust = 0.5)) +
        labs(title = paste(input$home, input$analysis_type))
    } else {
      # sets up a Result variable to allow colored segments by Result type
      plot_data <- segments_home() %>%
        mutate(xend = xend - x, # normalises xend data to allow all disposals to start at 0
               yend = yend - y, # normalises yend data to allow all disposals to start at 0
               Result = case_when(Retained == 1 ~ "Retained",
                                  Stoppage == 1 ~ "Stoppage",
                                  TO == 1 ~ "TO",
                                  TRUE ~ "NA"))
      
      ggplot(plot_data) +
        geom_circle(aes(x0 = 0, y0 = 0, r = 40), size = 1.1, linetype = "dashed", inherit.aes = F) + # outer ring
        geom_circle(aes(x0 = 0, y0 = 0, r = 15), size = 1.1, linetype = "dashed", inherit.aes = F) + # inner ring
        geom_segment(aes(x = 0, y = 0, xend = xend, yend = yend, col = Result), size = 1.1, arrow = arrow(length = unit(0.2, "cm"))) +
        scale_color_manual(values = c("Retained" = "blue", "Stoppage" = "orange", "TO" = "red", "NA" = "grey50")) +
        theme_minimal() +
        theme(legend.title = element_blank(),
              legend.position = "top",
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              panel.grid = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              text = element_text(size = 20)) +
        coord_fixed(ratio = 1) +
        xlim(-65, 65) +
        ylim(-65, 65) +
        labs(title = paste(input$home, input$analysis_type),
             # expression to provide context around rings and attacking direction
             subtitle = expression(atop("(Inner ring 15m radius, Outer ring 40m radius)", "Attacking direction"~symbol("\256"))))
    }
  })
  
  # output for the home team analysis plot
  output$home_visual <- renderPlot({
    home_vis()
  }, height = 750)
  
  # reactive object for creating the heatmaps for the away team
  heatmap_away <- reactive({
    plot_data <- away_values$DT %>%
      filter(if(input$time_filter == "Q1") Quarter == 1       # filters for the quarter/half/full match
             else if (input$time_filter == "Q2") Quarter == 2
             else if (input$time_filter == "Q3") Quarter == 3
             else if (input$time_filter == "Q4") Quarter == 4
             else if (input$time_filter == "1st Half") Quarter %in% c(1, 2)
             else if (input$time_filter == "2nd Half") Quarter %in% c(3, 4)
             else Quarter %in% c(1, 2, 3, 4)) %>%
      mutate(x = case_when(input$away == input$right & Quarter %in% c(2, 4) ~ -x, # normalise data so the team is always kicking right
                           input$away != input$right & Quarter %in% c(1, 3) ~ -x,
                           TRUE ~ x),
             y = case_when(input$away == input$right & Quarter %in% c(2, 4) ~ -y,
                           input$away != input$right & Quarter %in% c(1, 3) ~ -y,
                           TRUE ~ y)) %>%
      group_by(ID) %>%
      filter(row_number() == max(row_number())) %>% # filter to only collect last plot click if more than one collected for each ID
      mutate(Result = case_when(Goal == 1 ~ "Goal", # set up Result variable to enable colored points
                                Behind == 1 ~ "Behind",
                                TRUE ~ "No Score"))
    
    # section required to calculate the intersection of the 50m arcs and boundary
    size <- tibble(x = input$length, y = input$width)
    z <- (((size$x + size$y) / 4.01)^2 - 2500 + (size$x / 2)^2) / (2 * (size$x / 2))
    h <- ((size$x + size$y) / 4)
    a <- sqrt(h^2 - z^2)
    angle <- acos(a / 50)
    
    ggplot(plot_data, aes(x, y)) +
      coord_fixed(ratio = 1) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.ticks.length = unit(0, "pt"),
            plot.margin = margin(0, 0, 0, 0, "pt"),
            legend.title = element_blank(),
            legend.position = "top",
            plot.title = element_text(hjust = 0.5),
            text = element_text(size = 20)) +
      geom_density_bands(aes(fill = stat(density)), col = "grey70", size = 0.2, alpha = 0.5) + # creates the density bands of heatmap
      scale_fill_gradient(high = "red", low = "#ffffff00", guide = "none") + # fills the density bands of the heatmap
      geom_ellipse(aes(x0 = 0, y0 = 0, a = size$x / 2, b = size$y / 2, angle = 0), size = 1.2) + # oval boundary line
      geom_tile(aes(x = 0, y = 0, width = 50, height = 50), fill = "#ffffff00", col = "black", size = 1.2) + # centre square
      geom_tile(aes((x = size$x / 2) - 4.5, y = 0, width = 9, height = 6.7), fill = "#ffffff00", col = "black", size = 1.2) + # right goalsquare
      geom_tile(aes((x = -size$x / 2) + 4.5, y = 0, width = 9, height = 6.7), fill = "#ffffff00", col = "black", size = 1.2) + # left goalsquare
      geom_circle(aes(x0 = 0, y0 = 0, r = 3), size = 1.2, inherit.aes = F) + # centre circle
      geom_arc(aes(x0 = size$x / 2, y0 = 0, r = 50, start = -angle, end = -(3.142 - angle)), size = 1.2, inherit.aes = F) + # right 50m arc
      geom_arc(aes(x0 = -size$x / 2, y0 = 0, r = 50, start = angle, end = 3.142 - angle), size = 1.2, inherit.aes = F) + # left 50m arc
      geom_point(aes(col = Result), size = 3) +
      scale_color_manual(values = c("Goal" = "blue", "Behind" = "#74a9cf", "No Score" = "grey40")) +
      xlim(-size$x / 2, size$x / 2) +
      ylim(-size$y / 2, size$y / 2) +
      labs(title = paste(input$away, "I50 Heatmap"))
  })
  
  # reactive object for home team plots requiring segments 
  segments_away <- reactive({
    segments <- away_values$DT %>%
      filter(if(input$time_filter == "Q1") Quarter == 1       # filters for the quarter/half/full match
             else if (input$time_filter == "Q2") Quarter == 2
             else if (input$time_filter == "Q3") Quarter == 3
             else if (input$time_filter == "Q4") Quarter == 4
             else if (input$time_filter == "1st Half") Quarter %in% c(1, 2)
             else if (input$time_filter == "2nd Half") Quarter %in% c(3, 4)
             else Quarter %in% c(1, 2, 3, 4)) %>%
      mutate(x = case_when(input$away == input$right & Quarter %in% c(2, 4) ~ -x, # normalise data so the team is always kicking right
                           input$away != input$right & Quarter %in% c(1, 3) ~ -x,
                           TRUE ~ x),
             y = case_when(input$away == input$right & Quarter %in% c(2, 4) ~ -y,
                           input$away != input$right & Quarter %in% c(1, 3) ~ -y,
                           TRUE ~ y)) %>%
      group_by(ID) %>%
      mutate(xend = lead(x), # sets up the xend variable
             yend = lead(y), # sets up the yend variable
             Retained = lead(Retained), # moves the Retained etc markers to the row above
             Stoppage = lead(Stoppage),
             TO = lead(TO),
             Goal = lead(Goal),
             Behind = lead(Behind)) %>%
      filter(row_number() != max(row_number())) %>% # removes last row which has no xend, yend data
      left_join(., teams, by = "Team")
  })
  
  # reactive object for the away team analysis plot output
  away_vis <- reactive({
    # if statements display the selected analysis type
    if(input$analysis_type == "Inside 50 Heatmaps") {
      heatmap_away()
    } else if(input$analysis_type == "Full Chain") {
      oval() +
        geom_segment(data = segments_away(), aes(x = x, xend = xend, y = y, yend = yend), size = 1.1, col = segments_away()$Color) +
        labs(title = paste(input$away, input$analysis_type)) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if(input$analysis_type %in% c("Inside 50 Kicks", "D50 Exit Kicks")) {
      # sets up a Result variable to allow colored segments by Result type
      plot_data <- segments_away() %>%
        mutate(Result = case_when(Retained == 1 ~ "Retained",
                                  Stoppage == 1 ~ "Stoppage",
                                  TO == 1 ~ "TO",
                                  TRUE ~ "NA"))
      
      oval() +
        geom_segment(data = plot_data, 
                     aes(x = x, xend = xend, y = y, yend = yend, col = Result), size = 1.1) +
        scale_color_manual(values = c("Retained" = "blue", "Stoppage" = "orange", "TO" = "red", "NA" = "grey50")) +
        theme(legend.title = element_blank(),
              legend.position = "top",
              plot.title = element_text(hjust = 0.5)) +
        labs(title = paste(input$away, input$analysis_type))
    } else {
      # sets up a Result variable to allow colored segments by Result type
      plot_data <- segments_away() %>%
        mutate(xend = xend - x, # normalises xend data to allow all disposals to start at 0
               yend = yend - y, # normalises yend data to allow all disposals to start at 0
               Result = case_when(Retained == 1 ~ "Retained",
                                  Stoppage == 1 ~ "Stoppage",
                                  TO == 1 ~ "TO",
                                  TRUE ~ "NA"))
      
      ggplot(plot_data) +
        geom_circle(aes(x0 = 0, y0 = 0, r = 40), size = 1.1, linetype = "dashed", inherit.aes = F) + # outer ring
        geom_circle(aes(x0 = 0, y0 = 0, r = 15), size = 1.1, linetype = "dashed", inherit.aes = F) + # inner ring
        geom_segment(aes(x = 0, y = 0, xend = xend, yend = yend, col = Result), size = 1.1, arrow = arrow(length = unit(0.2, "cm"))) +
        scale_color_manual(values = c("Retained" = "blue", "Stoppage" = "orange", "TO" = "red", "NA" = "grey50")) +
        theme_minimal() +
        theme(legend.title = element_blank(),
              legend.position = "top",
              plot.title = element_text(hjust = 0.5),
              panel.grid = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              text = element_text(size = 20)) +
        coord_fixed(ratio = 1) +
        xlim(-65, 65) +
        ylim(-65, 65) +
        labs(title = paste(input$home, input$analysis_type),
             # expression to provide context around rings and attacking direction
             subtitle = expression(atop("(Outer ring 40m radius, Inner ring 15m radius)", "Attacking direction"~symbol("\256"))))
    }
  })

  # output for the away team analysis plot
  output$away_visual <- renderPlot({
    away_vis()
  }, height = 750)
  
  # function to allow for download of home team analysis plot as png
  output$png_home <- downloadHandler(
    filename = paste0(input$home, " visual analysis ", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, home_vis(), scale = 3)
    }
  )
  
  # function to allow for download of away team analysis plot as png
  output$png_away <- downloadHandler(
    filename = paste0(input$away, " visual analysis ", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, away_vis(), scale = 3)
    }
  )
  
  ##########################################################
  # Data output tab
  ##########################################################
  
  # table output for data collected from the home team
  output$table_h <- renderTable({
    home_values$DT
  })
  
  # table output for data collected from the away team
  output$table_a <- renderTable({
    away_values$DT
  })
  
  # function to allow for deletion of data from the home table in the ID defined by input$delete_h
  observeEvent(input$delete_h_button, {
    home_values$DT <- home_values$DT %>%
      filter(ID != input$delete_h)
  })
  
  # function to allow for deletion of data from the away table in the ID defined by input$delete_a
  observeEvent(input$delete_a_button, {
    away_values$DT <- away_values$DT %>%
      filter(ID != input$delete_a)
  })
  
  # saves the home team data to csv
  observeEvent(input$save_h, {
    filename <- paste0(input$home, " analysis ", Sys.Date(), ".csv")
    write_csv(home_values$DT, filename)
  })
  
  # saves the away team data to csv
  observeEvent(input$save_a, {
    filename <- paste0(input$away, " analysis ", Sys.Date(), ".csv")
    write_csv(away_values$DT, filename)
  })
  
}

shinyApp(ui, server)
  
