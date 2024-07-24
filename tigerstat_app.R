library(shiny)
library(ggplot2)
library(dplyr)
library(readr)


# Load game data 
data.all <- readr::read_csv("https://stat2games.sites.grinnell.edu/data/tigerstat/getdata.php") 


# Get group IDs and player IDs
data.all$PlayerID <- tolower(data.all$PlayerID)
data.all$GroupID <- tolower(data.all$GroupID)
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))

data.all$Age <- as.numeric(data.all$Age)
data.all$NoseBlack <- as.numeric(data.all$NoseBlack)
data.all$PawCircumference <- as.numeric(data.all$PawCircumference)
data.all$Weight <- as.numeric(data.all$Weight)
data.all$Size <- as.numeric(data.all$Size)
data.all$Sex <- as.factor(data.all$Sex)


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
                  choices = c("Age", "Sex", "NoseBlack",	"PawCircumference", "Weight", "Size"),
                  selected = "Size",
                  multiple = FALSE),
      
      
      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  choices = c("NoseBlack", "PawCircumference", "Weight", "Size", "Age"),
                  selected = "Weight",
                  multiple = FALSE),
      
      selectInput(inputId = "color",
                  label = "Color by",
                  choices = c("None", "Age", "Sex", "NoseBlack",	"PawCircumference", "Weight", "Size"),
                  selected = "Sex",
                  multiple = FALSE),
      
      selectInput(inputId = "facet",
                  label = "Facet by:",
                  choices = c("None", "Sex"),
                  selected = "None",
                  multiple = FALSE),
      
      selectInput(inputId = "model",
                  label = "Statistical Model:",
                  choices = c("None", "Linear", "Quadratic", "Cubic" ),
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
    
    
    # build model
    if ((input$model != "None") && (input$color != "None")) {
      #Setting Up
      XVariable <- plotData %>% pull(input$xvar)
      YVariable <- plotData %>% pull(input$yvar)
      ColorVariable <- plotData %>% pull(input$color)
      
      # Remove Interaction checkbox is selected
      if (input$interaction == TRUE) {
        # Facet option is none
        if (input$facet == "None") {
          if (input$model == "Linear") {
            myModel <- lm(YVariable ~ XVariable + ColorVariable)
          } else if (input$model == "Quadratic") {
            myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable)
          } else if (input$model == "Cubic") {
            myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable)
          }
          # Adding predicted values column for Linear/Quadratic/Cubic
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
          } else if (input$model == "Cubic") {
            myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable + FacetVariable)
          }
          
          # Adding predicted values column for Linear/Quadratic/Cubic
          plotData <- cbind(plotData, predict(myModel, interval = "confidence"))
          
        }
        
        # Remove Interaction checkbox it NOT selected
      } else {
        # Facet option is none
        if (input$facet == "None") {
          if (input$model == "Linear") {
            myModel <- lm(YVariable ~ (XVariable + ColorVariable + XVariable * ColorVariable))
          } else if (input$model == "Quadratic") {
            myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable + XVariable * ColorVariable +
                            I(XVariable^2) * ColorVariable)
          } else if (input$model == "Cubic") {
            myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable +
                            XVariable * ColorVariable + I(XVariable^2) * ColorVariable +
                            I(XVariable^3) * ColorVariable)
          }
          # Adding predicted values column for Linear/Quadratic/Cubic
          plotData <- cbind(plotData, predict(myModel, interval = "confidence"))
          
          # Facet option is NOT none
        } else {
          # Pulling Facet Variable
          FacetVariable <- plotData %>% pull(input$facet)
          
          # More than one level for facet variable is needed to run the model
          if (input$model == "Linear") {
            myModel <- lm(YVariable ~ XVariable + ColorVariable + FacetVariable + XVariable * ColorVariable +
                            XVariable * FacetVariable + ColorVariable * FacetVariable)
          } else if (input$model == "Quadratic") {
            myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + ColorVariable + FacetVariable +
                            XVariable * ColorVariable + XVariable * FacetVariable +
                            I(XVariable^2) * ColorVariable + I(XVariable^2) * FacetVariable +
                            ColorVariable * FacetVariable)
          } else if (input$model == "Cubic") {
            myModel <- lm(YVariable ~ XVariable + I(XVariable^2) + I(XVariable^3) + ColorVariable + FacetVariable +
                            XVariable * ColorVariable + XVariable * FacetVariable +
                            I(XVariable^2) * ColorVariable + I(XVariable^2) * FacetVariable +
                            I(XVariable^3) * ColorVariable + I(XVariable^3) * FacetVariable +
                            ColorVariable * FacetVariable)
          }
          # Adding predicted values column for Linear/Quadratic/Cubic
          plotData <- cbind(plotData, predict(myModel, interval = "confidence"))
        }
      }
    } # build model
    
    
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
        # Model Option - Cubic
      } else if (input$model == "Cubic") {
        myplot <- myplot + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE)
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
