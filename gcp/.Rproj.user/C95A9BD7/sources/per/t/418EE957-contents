#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("bartender.R")

ui <- fluidPage(style = "width:fit-content;",
    # Application title
    titlePanel("Make me a drink, Bartender!"),
    # Sidebar with a slider input for number of bins 
        div(class = "col-sm-12 well",
          p("You walk into a bar and see a figure, slumped, leaning against the wall, behind bar, cleaning a glass in the shadows."),
          p("'What'll you have?' - a gruff voice eminates from the figure."),
          textInput(inputId = "drink",
                    label = "I'll have a:",
                    value = "Whiskey Coke"),
            actionButton(inputId = "makeDrink",
                        label = "Make Drink",
                        width = "100%"),
            p("Only problem, is, you quickly realize he is drunk as he makes you drink."),
            p("Too late, your order is in!")
        ),

        # Show a plot of the generated distribution
            div(class = "col-sm-12 well",
                p("Heres y'r drink:"),
                div(textOutput("contents"))
            )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
    text <- reactiveVal("Click Make Drink!")
    
    observeEvent(input$makeDrink,{
      
      input$makeDrink
      
      cocktail_name <- isolate(as.character(input$drink))
      
      if(nchar(cocktail_name)>100){
        text("Cocktail name must be less than 100 characters!")
      }else{
      
        review <- whats_in_my_cocktail( cocktail_name )
        text(review)
      }
    })
    
    output$contents <- renderText({
        text()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
