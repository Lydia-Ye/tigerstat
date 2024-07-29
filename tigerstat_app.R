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
  filter(Age < 25 & Age > 0 &
         Size < 250 & Size > 0 &
         Weight < 750 & Weight > 0 &
         NoseBlack < 1 & NoseBlack > 0 &
         PawCircumference < 250 & PawCircumference < 250)

data.all <- data.all %>%
  mutate(ArcsineNoseBlack = asin(sqrt(NoseBlack)))


# Get group IDs and player IDs
data.all$PlayerID <- tolower(data.all$PlayerID)
data.all$GroupID <- tolower(data.all$GroupID)
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))




# Define UI for the app
ui <- fluidPage(
  titlePanel("Tiger Stat and Tiger Sampling Visualization"),
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
                  choices = c("Age", "Sex", "NoseBlack", "ArcsineNoseBlack",	"PawCircumference", "Weight", "Size", "Region"),
                  selected = "Size",
                  multiple = FALSE),
      
      
      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  choices = c("NoseBlack", "ArcsineNoseBlack", "PawCircumference", "Weight", "Size", "Age"),
                  selected = "Weight",
                  multiple = FALSE),
      
      selectInput(inputId = "color",
                  label = "Color by",
                  choices = c("None", "Age", "Sex", "Region" , "NoseBlack", "ArcsineNoseBlack",	"PawCircumference", "Weight", "Size"),
                  selected = "Sex",
                  multiple = FALSE),
      
      selectInput(inputId = "facet",
                  label = "Facet by:",
                  choices = c("None", "Sex", "Region"),
                  selected = "None",
                  multiple = FALSE),
      
      selectInput(inputId = "model",
                  label = "Statistical Model:",
                  choices = c("None", "Linear", "Quadratic"),
                  multiple = FALSE,
                  selectize = TRUE,
                  selected = "None"),
      
      conditionalPanel(
        condition = "input.model != 'None' && input.color != 'None'",
        checkboxInput('interaction', "Remove Interaction", FALSE)
      ),
      
      checkboxInput('showCode', "Show code", FALSE),
      
      downloadButton('downloadData', label = "Tigerstat Data")
    ),
    
    mainPanel(
      plotOutput(outputId = "Plot"),
      conditionalPanel(
        condition = "input.showCode == true",
        h4("Code for Data Cleaning:"),
        verbatimTextOutput("filterCode"),
        h4("Code for Data Visualizations:"),
        verbatimTextOutput("plotCode")
        
      )
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
          } 
          # Adding predicted values column for Linear/Quadratic
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
          }
          
          # Adding predicted values column for Linear/Quadratic
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
  
  
  # Generate code for data cleaning --------------------------------------------------------------
  output$filterCode <- renderText({
    if (input$showCode == TRUE) {
      variables <- unique(c(input$xvar, input$yvar,if(input$color != "None") input$color, if(input$facet != "None") input$facet))
      
      code <- "# Load data\n"
      code <- paste0(code, "tigerstat_data <- readr::read_csv('https://stat2games.sites.grinnell.edu/data/tigerstat/getdata.php')\n")
      code <- paste0(code, "tigersampling_data <- readr::read_csv('https://stat2games.sites.grinnell.edu/data/tigersampling/getdata.php')\n\n")
      code <- paste0(code, "# Add region to tiger stat data, setting to 1 for all entries\n")
      code <- paste0(code, "tigerstat_data <- tigerstat_data %>%\n  mutate(Region = 1)\n\n")
      code <- paste0(code, "# Combine the two dataset\n")
      code <- paste0(code, "data.all <- rbind(tigerstat_data, tigersampling_data)\n\n")
      
      # Select only columns used in plotting
      used_vars <- c("GroupID", "PlayerID", variables)
      code <- paste0(code, "# Get selected columns for plotting\n")
      code <- paste0(code, "plotData <- data.all %>% select(", paste(used_vars, collapse = ", "), ")\n\n")
      
      code <- paste0(code, "# Convert selected variables to correct data types\n")
      if ("Age" %in% variables) {
        code <- paste0(code, "data.all$Age <- as.numeric(data.all$Age)\n")
      }
      if ("NoseBlack" %in% variables) {
        code <- paste0(code, "data.all$NoseBlack <- as.numeric(data.all$NoseBlack)\n")
      }
      if ("PawCircumference" %in% variables) {
        code <- paste0(code, "data.all$PawCircumference <- as.numeric(data.all$PawCircumference)\n")
      }
      if ("Weight" %in% variables) {
        code <- paste0(code, "data.all$Weight <- as.numeric(data.all$Weight)\n")
      }
      if ("Size" %in% variables) {
        code <- paste0(code, "data.all$Size <- as.numeric(data.all$Size)\n")
      }
      if ("Sex" %in% variables) {
        code <- paste0(code, "data.all$Sex <- as.factor(data.all$Sex)\n")
      }
      
      code <- paste0(code, "\n# Filter out outliers\n")
      filter_lines <- c()
      if ("Age" %in% variables) {
        filter_lines <- c(filter_lines, "    Age < 250 & Age > 0")
      }
      if ("Size" %in% variables) {
        filter_lines <- c(filter_lines, "    Size < 250 & Size > 0")
      }
      if ("Weight" %in% variables) {
        filter_lines <- c(filter_lines, "    Weight < 750 & Weight > 0")
      }
      if ("NoseBlack" %in% variables) {
        filter_lines <- c(filter_lines, "    NoseBlack < 1 & NoseBlack > 0")
      }
      if ("PawCircumference" %in% variables) {
        filter_lines <- c(filter_lines, "    PawCircumference < 250 & PawCircumference < 250")
      }
      code <- paste0(code, "data.all <- data.all %>%\n  filter(\n", paste(filter_lines, collapse = ",\n"), "\n  )\n")
      
      if ("ArcsineNoseBlack" %in% variables) {
        code <- paste0(code, "\n# Add derived column\n")
        code <- paste0(code, "data.all <- data.all %>%\n  mutate(ArcsineNoseBlack = asin(sqrt(NoseBlack)))\n")
      }
      
      code
    } else {
      NULL
    }
  })
  
  # Generate code for plot --------------------------------------------------------------
  plotCode <- reactive({
    req(input$xvar, input$yvar)
    
    code <- sprintf("ggplot(data = plotData, aes(x = %s, y = %s", input$xvar, input$yvar)
    
    if (input$color != "None") {
      code <- paste0(code, sprintf(", color = %s", input$color))
    }
    code <- paste0(code, ")) +\n")
    
    code <- paste0(code, "  geom_point() +\n")
    
    if (input$facet != "None") {
      code <- paste0(code, sprintf("  facet_wrap(~ %s) +\n", input$facet))
    }
    
    if (input$model != "None") {
      if (input$interaction == FALSE) {
        if (input$model == "Linear") {
          code <- paste0(code, "  stat_smooth(method = 'lm', formula = y ~ x, se = FALSE) +\n")
        } else if (input$model == "Quadratic") {
          code <- paste0(code, "  stat_smooth(method = 'lm', formula = y ~ x + I(x^2), se = FALSE) +\n")
        }
      } else {
        model_code <- ""
        if (input$model == "Linear") {
          model_code <- sprintf("myModel <- lm(%s ~ %s + %s, data = plotData)\n", input$yvar, input$xvar, input$color)
        } else if (input$model == "Quadratic") {
          model_code <- sprintf("myModel <- lm(%s ~ %s + I(%s^2) + %s, data = plotData)\n", input$yvar, input$xvar, input$xvar, input$color)
        }
        
        model_code <- paste0(model_code, "plotData <- cbind(plotData, predict(myModel, interval = 'confidence'))\n\n")
        code <- paste0(model_code, code)
        code <- paste0(code, "  geom_line(aes(y = fit), size = 1) +\n")
      }
    }
    
    code <- paste0(code, sprintf("  xlab('%s') +\n  ylab('%s') +\n", input$xvar, input$yvar))
    code <- paste0(code, sprintf("  labs(title = 'Plot of %s by %s') +\n", input$yvar, input$xvar))
    code <- paste0(code, sprintf("  theme(text = element_text(size = 18)))"))
    
    code
  })
  
  output$plotCode <- renderText({
    req(plotCode())
    plotCode()  # Display the dynamically generated R code for plotting
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
