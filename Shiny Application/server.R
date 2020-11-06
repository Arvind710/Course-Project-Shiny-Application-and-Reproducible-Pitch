#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(datasets)
library(ggpubr)

TD <- mtcars
TD$am <- factor(TD$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
    
    MT <- reactive({
        paste("mpg ~", input$variable)
    })
    
    MTPoint <- reactive({
        paste("mpg ~", "as.integer(", input$variable, ")")
    })
    
    Marlafit <- reactive({
        lm(as.formula(MTPoint()), data=TD)
    })
    
    output$caption <- renderText({
        MT()
    })
    
    output$mpgBoxPlot <- renderPlot({
        boxplot(as.formula(MT()), 
                data = TD,
                outline = input$outliers)
    })
    
    output$fit <- renderPrint({
        summary(fit())
    })
    
    output$mpgPlot <- renderPlot({
        with(TD, {
            plot(as.formula(MTPoint()))
            abline(fit(), col=2)
        })
    })
    
})
