library(shiny)
library(shinyWidgets)
library(rtools)
library(ggplot2)
source('definitions.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    dataset <- reactive({
        uploaded_file_local <- input$uploadFile$datapath
        
        print(uploaded_file_local)
        if(is.null(uploaded_file_local)) {
    
            read.csv("example_datasets/credit.csv", header=TRUE)
            
        } else {

            read.csv(uploaded_file_local, header=TRUE)
        }
    })
    numeric_summary_data <- reactive({
        rt_explore_numeric_summary(dataset=dataset())
    })
    
    output$selected_target_variable_UI <- renderUI({
        selectInput(inputId='selected_target_variable',
                    label = 'Target Variable',
                    choices = c(select_target_variable, colnames(dataset())),
                    selected = select_target_variable,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
    output$dataset_types_table <- renderDataTable({

        types <- sapply(dataset(), class)
        data.frame(variable=names(types), type=types)
    })
    output$dataset_head_table <- renderDataTable({

        head(dataset(), 500)
    })
    output$selected_numeric_summary_options_UI <- renderUI({
        option_values <- colnames(numeric_summary_data())
        option_values <- option_values[option_values != 'feature']
        checkboxGroupInput(inputId='selected_numeric_summary_options',
                           label='Options',
                           choices=option_values,
                           selected=c('perc_nulls', 'perc_zeros', 'mean', 'coef_of_var', 'skewness', 'min', 
                                      'percentile_50', 'max'),
                           inline=FALSE,
                           width=NULL)
    })
    output$numeric_summary_table <- renderDataTable({

        results <- numeric_summary_data()
        results[, c('feature', input$selected_numeric_summary_options)]
    })
    output$categoric_summary_table <- renderDataTable({

        rt_explore_categoric_summary(dataset=dataset())
    })
    output$correlation_plot <- renderPlot({
        rt_explore_plot_correlations(dataset=dataset(),
                                     corr_threshold=input$selected_correlation_corr_threshold,
                                     p_value_threshold=input$selected_correlation_p_value_threshold,
                                     base_size=input$selected_correlation_base_size,
                                     type='pearson')
    })


    output$selected_target_variable_UI <- renderUI({
        selectInput(inputId='selected_target_variable',
                    label = 'Target Variable',
                    choices = c(select_target_variable, colnames(dataset())),
                    selected = select_target_variable,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })




    output$selected_variable_plot_variable_UI <- renderUI({
        selectInput(inputId='selected_variable_plot_variable',
                    label = 'Variable',
                    choices = c(select_variable, colnames(dataset())),
                    selected = select_variable,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })

    output$selected_variable_plot_comparison_UI <- renderUI({
        selectInput(inputId='selected_variable_plot_comparison',
                    label = 'Comparison Variable',
                    choices = c(select_comparison_variable_optional, colnames(dataset())),
                    selected = select_comparison_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })

    output$variable_plot <- renderPlot({

        req(input$selected_variable_plot_variable)
        req(input$selected_variable_plot_comparison)

        variable_local <- input$selected_variable_plot_variable
        comparison_variable_local <- input$selected_variable_plot_comparison

        if(variable_local != select_variable) {

            if(comparison_variable_local == select_comparison_variable_optional) {

                comparison_variable_local <- NULL
            }

            rt_explore_plot_unique_values(dataset=dataset(),
                                          variable=variable_local,
                                          comparison_variable=comparison_variable_local,
                                          order_by_count=input$selected_variable_plot_order_by_count,
                                          show_group_totals=input$selected_variable_plot_show_variable_totals,
                                          show_comparison_totals=input$selected_variable_plot_show_comparison_totals,
                                          base_size=input$selected_variable_plot_base_size)
        } else {

            NULL
        }
    })
})
