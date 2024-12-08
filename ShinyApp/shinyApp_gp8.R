library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr) 

podcast_data <- read.csv("final_metrics.csv", encoding = "latin1")


ui <- fluidPage(
  titlePanel("Podcast Clustering Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("podcast", "Choose a Podcast/Episode:", 
                  choices = podcast_data$name, 
                  selected = podcast_data$name[1]),
      helpText("Visualize the metrics and nearest podcasts/episodes.")
    ),
    
    mainPanel(
      plotOutput("metricsPlot"),
      tableOutput("nearestTable")
    )
  )
)


server <- function(input, output) {
  

  selected_podcast <- reactive({
    podcast_data %>% filter(name == input$podcast)
  })
  
  nearest_podcasts <- reactive({
    selected <- selected_podcast()
    
    podcast_data %>%
      mutate(distance = sqrt(
        (Historical.Themes - selected$Historical.Themes)^2 +
          (Biographical.Content - selected$Biographical.Content)^2 +
          (Geographical.Contexts - selected$Geographical.Contexts)^2 +
          (Cultural.Narratives - selected$Cultural.Narratives)^2 +
          (Military.Strategies - selected$Military.Strategies)^2
      )) %>%
      arrange(distance) %>%
      head(6) %>%
      filter(name != selected$name)
  })
  
  output$metricsPlot <- renderPlot({
    selected <- selected_podcast()
    metrics <- selected %>% 
      select(Historical.Themes, Biographical.Content, Geographical.Contexts, 
             Cultural.Narratives, Military.Strategies) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Value")
    
    ggplot(metrics, aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Metrics for:", selected$name),
           x = "Metric", y = "Value") +
      scale_fill_brewer(palette = "Set2")
  })
  
  output$nearestTable <- renderTable({
    nearest_podcasts() %>%
      select(name, distance)
  }, rownames = TRUE)
}

shinyApp(ui = ui, server = server)
