devtools::install_github('shane-kercheval/rtools')

library(shiny)
library(shinyWidgets)
library(rtools)
library(ggplot2)
library(stringr)
source('definitions.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    dataset <- reactive({
        uploaded_file_local <- input$uploadFile$datapath
        
        print(uploaded_file_local)
        if(is.null(uploaded_file_local)) {
            if(file.exists("example_datasets/credit.csv")) {

                read.csv("example_datasets/credit.csv", header=TRUE)
                    
            } else {
                iris
            }

        } else {

            if(str_sub(uploaded_file_local, -4) == '.csv') {
                
                read.csv(uploaded_file_local, header=TRUE)

            } else if(str_sub(uploaded_file_local, -4) == '.RDS') {
            
                readRDS(file=uploaded_file_local)

            } else {
                showModal(modalDialog(
                        title = 'Unknown File Type',
                        'Only `.csv` and `.RDS` files are supported at this time.'
                        ))
                NULL
            }
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
    }, height = function() {
        session$clientData$output_correlation_plot_width * 0.80  # set height to % of width
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
    hide_show_numeric_numeric <- function() {

        log_message('hide_show_numeric_numeric')
        
        # scatterplot

        shinyjs::show('selected_variable_plots_alpha')
        shinyjs::show('div_variable_plots_group_x_zoom_controls')
        shinyjs::show('div_variable_plots_group_y_zoom_controls')
        shinyjs::show('selected_variable_plot_base_size')

        shinyjs::hide('selected_variable_plots_histogram_bins')
        shinyjs::hide('div_variable_plots_group_barchar_controls')
        shinyjs::hide('selected_variable_plot_numeric_graph_type')
    }

    hide_show_numeric_categoric <- function() {
        
        log_message('hide_show_numeric_categoric')
        
        # boxplot

        shinyjs::show('div_variable_plots_group_y_zoom_controls')
        shinyjs::show('selected_variable_plot_base_size')
        shinyjs::show('selected_variable_plot_numeric_graph_type')

        shinyjs::hide('div_variable_plots_group_x_zoom_controls')
        shinyjs::hide('selected_variable_plots_alpha')
        shinyjs::hide('selected_variable_plots_histogram_bins')
        shinyjs::hide('div_variable_plots_group_barchar_controls')
    }

    hide_show_categoric_numeric <- function() {
        
        log_message('hide_show_categoric_numeric')
        
        # scatterplot

        shinyjs::show('div_variable_plots_group_y_zoom_controls')
        shinyjs::show('selected_variable_plot_base_size')

        shinyjs::hide('div_variable_plots_group_x_zoom_controls')
        shinyjs::hide('selected_variable_plots_alpha')
        shinyjs::hide('selected_variable_plots_histogram_bins')
        shinyjs::hide('div_variable_plots_group_barchar_controls')
        shinyjs::hide('selected_variable_plot_numeric_graph_type')
    }

    hide_show_categoric_categoric <- function() {

        log_message('hide_show_categoric_categoric')
        
        # scatterplot

        shinyjs::show('div_variable_plots_group_barchar_controls')
        shinyjs::show('selected_variable_plot_base_size')

        shinyjs::hide('div_variable_plots_group_x_zoom_controls')
        shinyjs::hide('div_variable_plots_group_y_zoom_controls')
        shinyjs::hide('selected_variable_plots_alpha')
        shinyjs::hide('selected_variable_plots_histogram_bins')
        shinyjs::hide('selected_variable_plot_numeric_graph_type')
    }

    output$variable_plot <- renderPlot({

        req(input$selected_variable_plot_variable)
        req(input$selected_variable_plot_comparison)

        variable_local <- input$selected_variable_plot_variable
        comparison_variable_local <- input$selected_variable_plot_comparison

        if(variable_local != select_variable) {

            if(comparison_variable_local == select_comparison_variable_optional) {

                comparison_variable_local <- NULL
            }

            ##################################################################################################
            # Numeric Primary Variable
            ##################################################################################################
            if(is.numeric(dataset()[, variable_local])) {

                ##############################################################################################
                # Numeric Secondary Variable
                ##############################################################################################
                if(!is.null(comparison_variable_local) && is.numeric(dataset()[, comparison_variable_local])) {

                    hide_show_numeric_numeric()

                    #shinyjs::hide(id='selected_variable_plot_numeric_graph_type')
                    rt_explore_plot_scatter(dataset=dataset(),
                                            variable=variable_local,
                                            comparison_variable=comparison_variable_local,
                                            alpha=input$selected_variable_plots_alpha,
                                            x_zoom_min=input$selected_variable_plots_x_zoom_min,
                                            x_zoom_max=input$selected_variable_plots_x_zoom_max,
                                            y_zoom_min=input$selected_variable_plots_y_zoom_min,
                                            y_zoom_max=input$selected_variable_plots_y_zoom_max,
                                            base_size=input$selected_variable_plot_base_size)

                ##############################################################################################
                # NULL Or Categoric Secondary Variable
                ##############################################################################################
                } else {

                    hide_show_numeric_categoric()

                    if(input$selected_variable_plot_numeric_graph_type == 'Boxplot') {

                        rt_explore_plot_boxplot(dataset=dataset(),
                                                variable=variable_local,
                                                comparison_variable=comparison_variable_local,
                                                y_zoom_min=input$selected_variable_plots_y_zoom_min,
                                                y_zoom_max=input$selected_variable_plots_y_zoom_max,
                                                base_size=input$selected_variable_plot_base_size)
                    } else {

                        rt_explore_plot_histogram(dataset=dataset(),
                                                      variable=variable_local,
                                                      num_bins=input$selected_variable_plots_histogram_bins,
                                                      x_zoom_min=input$selected_variable_plots_x_zoom_min,
                                                      x_zoom_max=input$selected_variable_plots_x_zoom_max,
                                                      base_size=input$selected_variable_plot_base_size)
                    }
                }

            ##################################################################################################
            # Categoric Primary Variable
            ##################################################################################################
            } else {

                ##############################################################################################
                # Numeric Secondary Variable
                ##############################################################################################
                if(!is.null(comparison_variable_local) && is.numeric(dataset()[, comparison_variable_local])) {

                    hide_show_categoric_numeric()

                    #shinyjs::hide(id='selected_variable_plot_numeric_graph_type')
                    rt_explore_plot_boxplot(dataset=dataset(),
                                                variable=comparison_variable_local,
                                                comparison_variable=variable_local,
                                                y_zoom_min=input$selected_variable_plots_y_zoom_min,
                                                y_zoom_max=input$selected_variable_plots_y_zoom_max,
                                                base_size=input$selected_variable_plot_base_size)

                ##############################################################################################
                # NULL Or Categoric Secondary Variable
                ##############################################################################################
                } else {
                
                    hide_show_categoric_categoric()
                    rt_explore_plot_unique_values(dataset=dataset(),
                                                  variable=variable_local,
                                                  comparison_variable=comparison_variable_local,
                                                  order_by_count=input$selected_variable_plot_order_by_count,
                                                  show_group_totals=input$selected_variable_plot_show_variable_totals,
                                                  show_comparison_totals=input$selected_variable_plot_show_comparison_totals,
                                                  base_size=input$selected_variable_plot_base_size)
                    }
            }
        } else {

            NULL
        }
    }, height = function() {
        session$clientData$output_variable_plot_width * 0.66  # set height to % of width
    })
})
