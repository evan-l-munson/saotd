server <- function(input, output, session) {
  
  
  output$TD_Corr_Plot <- renderPlot({
    
    SAoTD::Word.Corr.Plot(WordCorr = TD_Corr, 
                          Correlation = input$Correlation, 
                          layout = input$layout, 
                          edge_color = input$edge_color, 
                          node_color = input$node_color, 
                          node_size = input$node_size, 
                          set_seed = input$set_seed)
    
  })
  
  output$TD_Bigram_Plot <- renderPlot({
    
    SAoTD::Bigram.Network(BiGramDataFrame = TD_Bigram, 
                          number = input$num_bigrams, 
                          layout = input$layout, 
                          edge_color = input$edge_color, 
                          node_color = input$node_color, 
                          node_size = input$node_color, 
                          set_seed = input$set_seed)
    
  })
}
    


    
    
