library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    titlePanel("App that displays ggplot messages and warnings"),
    mainPanel(
        plotOutput("my_plot_that_generates_warnings"),
        tags$br(),
        verbatimTextOutput(outputId='ggplot_warnings')
    )
)

capture_messages_warnings <- function(func) {
    
    messages <- list()
    withCallingHandlers(
        warning = function(cnd) {
            messages <<- append(messages, cnd$message)
            rlang::cnd_muffle(cnd)
        },
        message = function(cnd) {
            messages <<- append(messages, cnd$message)
            rlang::cnd_muffle(cnd)
        },
        func()
    )
    return (paste0(messages, collapse = '\n'))
}

server <- function(input, output) {

    ggplot_object <- reactive({
        print('my log message')
        # ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
        #     geom_point()+
        #     geom_smooth()
        ggplot(nycflights13::flights, aes(x=dep_time, y=arr_time)) +
            geom_point() +
            geom_smooth()
    })

    messages <- reactiveValues(value=NULL)

    output$my_plot_that_generates_warnings <- renderPlot({
        # somehow? this still prints the plot. There is some black magic happening that I don't understand.
        messages$value <- capture_messages_warnings(function() print(ggplot_object()))
    })

    output$ggplot_warnings <- renderPrint({
        cat(messages$value)
    })
}

shinyApp(ui = ui, server = server)
