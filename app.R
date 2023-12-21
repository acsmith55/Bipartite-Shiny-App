# Aaron Smith
# 9.6.2023
#
# Task: Create bipartite graphs.
#
################################################################################
# Packages ----
library(shiny)
library(igraph)
library(tidyverse)
library(shinyjs)
library(visNetwork)
library(shinyWidgets)
library(bslib)

# Data ----
faculty_class <- read_csv("Raw Data/faculty_class.csv") %>%
  select(Category, Breakdown)

faculty_department <- read_csv("Raw Data/faculty_department.csv")
majors_department <- read_csv("Raw Data/majors_department.csv")

# Faculty/Class data is ready, but Faculty/Department & Majors/Department need to be pivoted longer ---- 
faculty_department <- faculty_department %>%
  pivot_longer(cols = c(2:16), 
               names_to = "Bad_Col", 
               values_to = "Breakdown") %>%
  select(-c(Bad_Col)) %>%
  filter(!is.na(Breakdown))

majors_department <- majors_department %>%
  pivot_longer(cols = c(2:9), 
               names_to = "Bad_Col", 
               values_to = "Breakdown") %>%
  select(-c(Bad_Col)) %>%
  filter(!is.na(Breakdown)) 

# Add the word "Major" to major_department majors so the bipartite graph will form ----
majors_department <- majors_department %>%
  mutate(Breakdown = if_else(!str_detect(Breakdown, "(Minor)"), 
                             paste(Breakdown, "(Major)"), 
                             Breakdown))

# Create a data list with each dataframe ----
data_list <- list("faculty_class" = faculty_class, 
                  "faculty_department" = faculty_department, 
                  "majors_department" = majors_department)

# Create a data dropdown list that links to the data list (for the UI) ----
data_dropdown <- list(
  "Subjects that Faculty Members Teach" = "faculty_class",
  "Departments that Faculty Members are from" = "faculty_department",
  "Majors included by Department" = "majors_department"
  
)


# Create the Details Object that explains what is going on to the user ----
details <- paste("This Shiny Dashboard includes 2022-23 data showing the 
            relationships between:
            1) Faculty members and the subjects they teach 
            2) Faculty members and the departments they are from
            3) Student Majors and what departments they are correlated with

            These data are displayed via a bipartite graphing system, which shows
            how different values correlate with other values. You can click on 
            particular values to see what they are related to, and you can drag
            your mouse to better see these relationships.")

# Category list for UI Exclusions ----
category_list <- list("faculty_class$Category" = sort(faculty_class$Category), 
                      "faculty_department$Category" = sort(faculty_department$Category), 
                      "majors_department$Category" = sort(majors_department$Category))


myChoices <- unique(faculty_class$Category)



################################################################################
# User Interface ----
ui <- page_sidebar(
  
  theme = bs_theme(preset = "vapor", bg = '#f0bd18', fg = "purple"),
  
  
  title = "Faculty, Subject, and Department Networks",
  
  
  sidebar = sidebar(
    accordion(
      accordion_panel(
        title = "Category",
    radioButtons(
      inputId = "Category_Selection",
      label = NULL, 
      choices = data_dropdown, 
      selected = data_dropdown$`Subjects that Faculty Members Teach`
    )
      ),
    
    
 
      open = FALSE,
      accordion_panel(
    title = "Subjects to Include (FUNCTIONALITY COMING SOON!)",
    checkboxInput(
      'all', 'Select All/None', value = TRUE
    ),
    
    checkboxGroupInput(
      inputId = "Inclusion_Selection", 
      label = NULL, 
      choices = myChoices, 
    )
      ), 
    
    accordion_panel(
      title = "Details", 
      details
    )
    ),

    width = 450
  ),
    

  mainPanel(
    visNetworkOutput("bipartitePlot", width = "150%", height = "100vh")  # Adjust width and height as needed
  ),
  
  # Add custom CSS to change text color
  tags$style(HTML(".accordion-title {
      color: #f0bd18
    }"))
)

# Server ----
server <- function(input, output, session) {

  
  # Make the Checkbox Group Input start as all being checked
  observe({
    updateCheckboxGroupInput(
      session, "Inclusion_Selection", choices = myChoices, 
      selected = if(input$all) myChoices
    )
  })
  
  # Create the reactive data frame to work off of
  viz_data <- reactive({
    data_list[[input$Category_Selection]]
  })
  
 
  # Reactively remove points if a subject is selected 
  
  
  
  selected_nodes <- reactiveVal(character(0))  # Initialize with an empty vector
  
  observeEvent(input$bipartitePlot_selected, {
    selected_nodes(input$bipartitePlot_selected$nodes)
  })
  
  output$bipartitePlot <- renderVisNetwork({
    # Create a bipartite graph from the dataset
    bipartite <- graph.data.frame(viz_data(), directed = FALSE)
    
    # Set vertex attributes for professors and departments
    V(bipartite)$type <- V(bipartite)$name %in% viz_data()$Category
    
    # Define sizes for categories and breakdowns
    sizes <- ifelse(V(bipartite)$type, 40, 20)  # Adjust sizes as needed
    
    # Define colors for subjects and departments
    default_node_color <- "purple"  # Default node color
    selected_node_color <- "red"      # Node color when selected
    

    # Create a visNetwork graph from the bipartite graph
    visNetwork(main = list(text = names(which(data_dropdown == input$Category_Selection)), 
                           style = "font-family:Didot;color:#f0bd18;font-size:24px;text-align:center;"),
      nodes = data.frame(id = V(bipartite)$name,
                         label = V(bipartite)$name,
                         color = ifelse(V(bipartite)$type, default_node_color, "#f0bd18"),  # Set default node color
                         size = sizes),
      edges = get.data.frame(bipartite, what = "edges"),
      width = "100%", height = "800px"
    ) %>%
      visNodes(
        color = list(
          background = "white",  # Background color of the node
          border = default_node_color,  # Border color of the node
          highlight = selected_node_color,  # Color when selected
          hover = selected_node_color  # Color on hover
        ),
        font = list(
          color = "white"  # Change text color to white
        )
      ) %>%
      visEdges(
        selectionWidth = 5,  # Increase edge width when selected
        color = list(
          highlight = default_node_color,  # Edge color when not selected
          hover = default_node_color,     # Edge color on hover
          inherit = FALSE,                # Do not inherit color from nodes
          opacity = 1                     # Edge opacity
        )) %>%
      visEdges(
        smooth = TRUE
      )
  })
  
  output$text <- renderText({
    paste("Dataset selected:", input$Category_Selection)
  })
  
}

        smooth = FALSE  # Disable edge smoothing for precise color control
shinyApp(ui = ui, server = server)