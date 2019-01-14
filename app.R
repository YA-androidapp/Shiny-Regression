# Copyright (c) 2019 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.
library(shiny)

ui <- fluidPage(
    titlePanel("Calculating linear regression with uploading CSV Files"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                selected = ","),
            radioButtons("quote", "Quote",
                choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                selected = '"'),
            tags$hr(),
            radioButtons("disp", "Display",
                choices = c(Head = "head", All = "all"),
                selected = "head"),
            tags$hr(),
            textInput('formula', "Input a model","")
        ),

        mainPanel(
            tableOutput("contents"),
            plotOutput("pairs"),
            textOutput("texts")
        )
    )
)

server <- function(input, output, session) {
    Data = reactive({
        if (input$submit > 0) {
            formula <- data.frame(formula = input$formula)
            return(list(formula = formula))
        }
    })

    output$contents <- renderTable({
        req(input$file1)
        tryCatch({
            df <- read.csv(input$file1$datapath,
                header = input$header,
                sep = input$sep,
                quote = input$quote
            )
        },
        error = function(e) {
            stop(safeError(e))
        })

        if(input$disp == "head") {
            return(head(df))
        } else {
            return(df)
        }
    })

    output$pairs <- renderPlot({
        req(input$file1)
        tryCatch({
            df <- read.csv(input$file1$datapath,
                header = input$header,
                sep = input$sep,
                quote = input$quote
            )
            pairs(df)
        },
        error = function(e) {
            stop(safeError(e))
        })
    })

    output$texts <- renderPrint({
        req(input$file1)
        tryCatch({
            df <- read.csv(input$file1$datapath,
                header = input$header,
                sep = input$sep,
                quote = input$quote
            )

            if(input$formula==''){
                lastn = colnames(df)[length(colnames(df))]
                updateTextInput(session, "formula", value = paste(lastn, ' ~ . -1', collapse=''))
            }

            f = formula(input$formula, collapse='')
            result <- lm(f, data = df)
            result
        },
        error = function(e) {
            stop(safeError(e))
        })
    })
}

shinyApp(ui, server)
