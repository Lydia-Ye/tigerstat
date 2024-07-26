library(shiny)
library(ggplot2)
library(dplyr)
library(readr)


# Load game data 
tigerstat_data <- readr::read_csv("https://stat2games.sites.grinnell.edu/data/tigerstat/getdata.php") 
tigersampling_data <- readr::read_csv("https://stat2games.sites.grinnell.edu/data/tigersampling/getdata.php") 

# Add region to tiger stat data, setting to 1 for all entries
tigerstat_data <- tigerstat_data %>%
  mutate(Region = 1)

# Combine the two dataset
data.all <- rbind(tigerstat_data, tigersampling_data)

data.all$Age <- as.numeric(data.all$Age)
data.all$NoseBlack <- as.numeric(data.all$NoseBlack)
data.all$PawCircumference <- as.numeric(data.all$PawCircumference)
data.all$Weight <- as.numeric(data.all$Weight)
data.all$Size <- as.numeric(data.all$Size)
data.all$Sex <- as.factor(data.all$Sex)

# Filter out outliers
data.all <- data.all %>%
  filter(Age < 250 & Age > 0 &
         Size < 250 & Size > 0 &
         Weight < 750 & Weight > 0 &
         PawCircumference < 250 & PawCircumference < 250)


# Get group IDs and player IDs
data.all$PlayerID <- tolower(data.all$PlayerID)
data.all$GroupID <- tolower(data.all$GroupID)
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))




# Define UI for the app
ui <- fluidPage(
  titlePanel("Tigerstat Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("groupID", 
                  "Group ID:", 
                  choices = c("all", all_groups), 
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput("playerID", 
                  "Player ID:", 
                  choices = c("all", all_players), 
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput(inputId = "xvar",
                  label = "X Variable:",
                  choices = c("Age", "Sex", "NoseBlack",	"PawCircumference", "Weight", "Size", "Region"),
                  selected = "Size",
                  multiple = FALSE),
      
      
      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  choices = c("NoseBlack", "PawCircumference", "Weight", "Size", "Age"),
                  selected = "Weight",
                  multiple = FALSE),
      
      selectInput(inputId = "color",
                  label = "Color by",
                  choices = c("None", "Age", "Sex", "Region" , "NoseBlack",	"PawCircumference", "Weight", "Size"),
                  selected = "Sex",
                  multiple = FALSE),
      
      selectInput(inputId = "facet",
                  label = "Facet by:",
                  choices = c("None", "Sex", "Region"),
                  selected = "None",
                  multiple = FALSE),
      
      selectInput(inputId = "model",
                  label = "Statistical Model:",
                  choices = c("None", "Linear", "Quadratic", "Arcsine" ),
                  multiple = FALSE,
                  selectize = TRUE,
                  selected = "None"),
      
      conditionalPanel(
        condition = "input.model != 'None' && input.color != 'None'",
        checkboxInput('interaction', "Remove Interaction", FALSE)
      ),
      
      downloadButton('downloadData', label = "Tigerstat Data")
    ),
    
    mainPanel(
      plotOutput(outputId = "Plot")
    )
  )
) # ui


# Server 
server <- function(input, output, session) {
  
  # Filter data --------------------------------------------------------------
  filteredData <- reactive({
    req(data.all)  # Ensure the dataset is loaded
    
    # Filter by Group ID
    if (length(input$groupID) > 0) {
      if ("all" %in% input$groupID) {
        plotData <- data.all
      } else {
        plotData <- data.all[data.all$GroupID %in% input$groupID, ]
      }
    } else {
      plotData <- data.all  # No filtering if no group selected
    }
    
    plotData
  })

  # Generate plot --------------------------------------------------------------
  output$Plot <- renderPlot({
    plotData <- filteredData()  
    
    if (input$model == 'None') {
      input$interaction == FALSE
    } else if (input$color == "None") {
      input$interaction == FALSE
    }
    
    #Setting Up
    XVariable <- plotData %>% pull(input$xvar)
    YVariable <- plotData %>% pull(input$yvar)
    
    if (input$model == "Arcsine") {
      # Ensure YVariable is non-negative and in the range [0, 1]
      YVariable <- pmin(pmax(YVariable, 0), 1)
      YVariable <- asin(sqrt(YVariable))
    }
    
    
    # build model
    if ((input$model != "None") && (input$color != "None")) {
      ColorVariable <- plotData %>% pull(input$color)
      
      # Remove Interaction checkbox is selected
      if (input$interaction == TRUE) {
        # Facet option is none
        if (input$facet == "None") {
          if (input$model == "Linear") {
            myModel <- lm(YVariable ~ XVariable + ColorVariable)
          } else if (input$model == "Quadratic") {
            myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable)
          } else if (input$model == "Arcsine") {
            myModel <- lm(YVariable ~ poly(XVariable, 2) + ColorVariable)
          }
          # Adding predicted values column for Linear/Quadratic/Arcsine
          plotData <- cbind(plotData, predict(myModel, interval = "confidence"))
          
          # Facet option is NOT none
        } else {
          # Pulling Facet Variable
          FacetVariable <- plotData %>% pull(input$facet)
          
          # More than one level for facet variable is needed to run the model
          if (input$model == "Linear") {
            myModel <- lm(YVariable ~ XVariable + ColorVariable + FacetVariable)
          } else if (input$model == "Quadratic") {
            myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable + FacetVariable)
          } else if (input$model == "Arcsine") {
            myModel <- lm(YVariable ~ poly(XVariable, 2) + ColorVariable + FacetVariable)
          }
          
          # Adding predicted values column for Linear/Quadratic/Arcsine
          plotData <- cbind(plotData, predict(myModel, interval = "confidence"))
          
        }
      }
    } # build model
    
    
    if (input$model == "Arcsine") {
      plotData <- cbind(plotData, predict(lm(YVariable ~ poly(XVariable, 2)), interval = "confidence"))
    }
    
    
    # Base plot
    myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point() +
      xlab(input$xvar) + ylab(input$yvar) + 
      labs(title = paste("Plot of ", input$yvar, "by", input$xvar))
    
    
    
    # Color by
    if (input$color != "None") {
      myplot <- myplot + aes_string(color = input$color)
    }
    
    # facet by
    if (input$facet != "None") {
      myplot <- myplot + facet_wrap(as.formula(paste("~", input$facet)))
    }
    
    
    # Add model
    # If remove interaction checkbox is not selected
    if (input$model != "None" && input$interaction == FALSE) {
      # Model Option - Linear
      if (input$model == "Linear") {
        myplot <- myplot + stat_smooth(method = "lm", formula = y ~ x, se = FALSE)
        # Model Option - Quadratic
      } else if (input$model == "Quadratic") {
        myplot <- myplot + stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE)
        # Model Option - Arcsine
      } else if (input$model == "Arcsine") {
        myplot <- myplot + geom_line(aes(y = fit), size = 1)
      }
      
      # If remove interaction checkbox is selected
    } else if (input$model != "None" && input$interaction == TRUE) {
      myplot <- myplot + geom_line(aes(y = fit), size = 1)
      
    } else {
      myplot <- myplot # no model
    } # Add model
    
    myplot <- myplot + theme(text = element_text(size = 18))
    
   # Returning visual
    return(myplot)
  })
  


  # Download filtered data -----------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(filteredData(), con)
    }
  )
  
}

shinyApp(ui=ui,server=server)
