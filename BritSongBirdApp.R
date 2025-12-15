# Load the packages 
library(tidyverse)
library(shiny)
library(ggplot2)
library(maps)


##################### Layout ##################### 

ui <- fluidPage(
  # Create a vertical layout using Navlists
  navlistPanel(
    id = "tabset",
    # Project Name: British Songbird Project
    "British Songbird Project",
    
    # Heading 1: Data Maps
    "Songbird Data Maps",
    # Tab 1: The British Isles
    tabPanel("Project Overview", 
             h3("BritishSongBird Project Overview"),
             "This app allows you to explore a dataset about the songbird species commonly found in the British Isles,
             with data collected in the British Isles and the surrounding regions. 
             Here, you can learn about the most common songbird species in the British Isles, what kind
             of bird call records are most common, and more.",
             plotOutput("map1")),
    
    
    # Tab 2: Songbird record data maps
    tabPanel("Data Locations", 
             h3("Data Locations"),
             "Here you can see the locations of origin of our songbird data.",
             selectInput("maps", label="Which region do you want to see?", choices=c("The British Isles Only", 
                                                                                     "All Regions")),
             plotOutput("map2")),
    
    
    # Heading 2: Data Insight
    "Songbird Data Insight",
    # Tab 3: Common Songbird Species
    tabPanel("Common Songbird Species",
             h3("Common Songbird Species"),
             "Here you can see the most common songbird species in the dataset. 
             These species are commonly seem in the British Isles, but they may 
             also be found in other regions in the world.",
             selectInput("spec", label="Which songbird genus you want to view?", choices=c("N/A")),
             imageOutput("spec")),
    

    # Tab 4: Other Insights
    tabPanel("Other Insights", 
             h3("Other Insights"),
             "Here you can view the other insights we have gained from the dataset.",
             selectInput("other", label="What other insights do you want to view?", choices=c("Complete songbird species list",
                                                                                              "Record count by region",
                                                                                              "Record types")),
             uiOutput("other")),
  )
)


##################### Server ##################### 
server <- function(input, output, session){
  # Load the birdsong data from file
  dataset <- reactive({
    read.csv("birdsong_cleaned.csv")
  })
  
  
  # Heading 1: Data Maps
  # Create the map dataframe
  map <- reactive({ 
    countries <- dataset()$country %>%
      unique()
    
    map <- map_data("world", region=countries) %>%
      rename(country = region)  %>%
      left_join(dataset() %>% select(country, fill), by="country") 
    
    map
  })
  
  # Create the country label dataframe
  map.lab <- reactive({
    map() %>%
      group_by(country) %>%
      summarise(long=mean(long), lat=mean(lat))
  })
  
  # Create the recording locations dataframe
  points <- reactive({dataset() %>%
    group_by(country, latitude, longitute) %>%
    summarise(count = n())
  })
  
  
  # Tab 1: Project Overview
  # Display a map that includes all the regions that have appeared in the dataset
  # Highlight and centered at the British Isles
  output$map1 <- renderPlot({
    ggplot() +
      geom_polygon(data=map(), aes(x=long, y=lat, fill=fill, group=group)) +
      scale_fill_identity() +
      scale_x_continuous(limits = c(-40,30)) +
      scale_y_continuous(limits = c(42,71)) +
      geom_text(data=map.lab(), aes(x=long,y=lat,label=country), color="black")
  })
  
  
  # Tab 2: Songbird record data map
  # Display the map with points that indicate the locations of the data
  output$map2 <- renderPlot({
    
    # Only shows the points that are within the British Isles
    if (input$maps == "The British Isles Only"){
      ggplot() +
        geom_polygon(data=map(), aes(x=long, y=lat, fill=fill, group=group)) +
        scale_fill_identity() +
        geom_point(data=points() %>%filter(str_detect(country,"UK")), aes(x=longitute, y=latitude, size=count/10), color="orange") +
        scale_x_continuous(limits = c(-40,30))  +
        scale_y_continuous(limits = c(39,76)) +
        scale_size_continuous("# of records", labels=c("1", "2", "3", "4", "5")) +
        geom_text(data=map.lab()%>%filter(str_detect(country,"UK")), aes(x=long,y=lat,label=country), color="black")
      
    # Show all the points 
    } else if (input$maps == "All Regions"){
      ggplot() +
        geom_polygon(data=map(), aes(x=long, y=lat, fill=fill, group=group)) +
        scale_fill_identity() +
        geom_point(data=points(), aes(x=longitute, y=latitude, size=count/10), color="orange") +
        scale_x_continuous(limits = c(-40,30))  +
        scale_y_continuous(limits = c(39,76)) +
        scale_size_continuous("# of records", labels=c("1", "2", "3", "4", "5")) +
        geom_text(data=map.lab(), aes(x=long,y=lat,label=country), color="black")
    }
  })
  
  
  
  # Heading 2: Songbird Data Insights
  # Tab 3: Common Songbird Species
  # Load the data of the top 5 songbird genus
  genus.top5 <- reactive({
    dataset() %>%
      group_by(genus) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      slice(1:5)
  })
  # Update the input of Tab 3, so user can select which bird genus she wants to view
  observe({
    updateSelectInput(session, "spec", choices = c("Overview", genus.top5()$genus))
  })
  
  
  # Provide the songbird image that belonged to the genus selected by the user
  output$spec <- renderImage({
    list(
      src = paste("www/", input$spec, ".png", sep=""),
      contentType = "image/png",
      wdith = 700,
      height = 500
    )
    
  }, deleteFile = FALSE)

  
  # Tab 4: Other Insights
  # Render the table that list all song bird species that appear in the dataset
  output$songbirdlist <- renderTable({
    dataset() %>%
      select(genus, species, english_cname) %>%
      rename(
        Genus = genus,
        Species = species, 
        `Common English Name` = english_cname
      ) %>%
      unique
  })
  
  # Render the bar plot that shows: Region vs # of Records
  output$countrycount <- renderPlot({
    dataset() %>%
      ggplot(aes(x=country)) +
      geom_bar(fill="lightblue") +
      labs(
        x = "Region",
        y = "# of Records"
      )
  })
  
  # Render the bar plot that shows: Recording Types vs # of Records
  output$type <- renderPlot(({
    df <- dataset()
    df$type <- factor(df$type, levels=c("Else", "Both", "Call", "Song"))
    df %>%
      ggplot(aes(x=type)) +
      geom_bar(fill="lightblue") +
      labs(
        x = "Recording Type",
        y = "# of Records"
      )
  }))
  
  
  # Display different outpt based on user selection
  output$other <- renderUI({
    if (input$other == "Complete songbird species list"){
      tableOutput("songbirdlist")
    } else if (input$other == "Record count by region"){
      plotOutput("countrycount")
    } else if (input$other == "Record types"){
      plotOutput("type")
    }
  })
  
}




##################### App Starter #####################
shinyApp(ui, server)
