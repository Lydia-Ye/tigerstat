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
                  selected = "Water",
                  multiple = FALSE),
      
      
      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  choices = c("NoseBlack", "PawCircumference", "Weight", "Size", "Age"),
                  selected = "Yield",
                  multiple = FALSE),
      
      selectInput(inputId = "color",
                  label = "Color by",
                  choices = c("None", "Age", "Sex", "NoseBlack",	"PawCircumference", "Weight", "Size"),
                  selected = "None",
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


# Define Server for the app
server <- function(input, output, session) {

}  

shinyApp(ui=ui,server=server)
