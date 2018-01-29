ui <- fluidPage(title = 'My First App',
                theme = shinythemes::shinytheme('simplex'),
                
                sidebarLayout(
                  sidebarPanel(width = 3,
                               sliderInput("num_colors",
                                           label = "Number of Colors:",
                                           min = 1, 
                                           max = 9, 
                                           value = 7),
                               selectInput("select",
                                           label = "Select Demographic:",
                                           choices = colnames(map_data)[2:9],
                                           selected = 1)),
                    
                  mainPanel(width = 9,
                            tabsetPanel(
                              tabPanel(title = 'Output Map',
                                       plotOutput(outputId = "map")),
                              tabPanel(title = 'Data Table',
                                       dataTableOutput(outputId = 'table'))))))
                            


