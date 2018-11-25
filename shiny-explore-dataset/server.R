#devtools::install_github('shane-kercheval/rtools')

library(shiny)
library(shinyWidgets)
library(shinyBS)
library(rtools)
library(ggplot2)
library(stringr)
library(tidyverse)
library(scales)

source('definitions.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##########################################################################################################
    ##########################################################################################################
    # REACTIVE DATASETS
    ##########################################################################################################
    ##########################################################################################################

    ##########################################################################################################
    # main dataset
    # initialize with small default dataset or upload from file, by user
    ##########################################################################################################
    dataset <- reactive({

        # reactive data
        upload_file_path <- input$uploadFile$datapath

        log_message_block_start('Loading Dataset')
        log_message_variable('upload_file_path', upload_file_path)

        if(is.null(upload_file_path)) {
            
            # only need to show progress when uploading the initial dataset (file upload has it's own progress)
            withProgress(value=1/2, message='Uploading Data',{

                if(file.exists("example_datasets/credit.csv")) {

                    read.csv("example_datasets/credit.csv", header=TRUE)

                } else {

                    iris
                }
            })

        } else {

            if(str_sub(upload_file_path, -4) == '.csv') {
                
                read.csv(upload_file_path, header=TRUE)

            } else if(str_sub(upload_file_path, -4) == '.RDS') {
            
                readRDS(file=upload_file_path)

            } else {

                showModal(
                    modalDialog(title = 'Unknown File Type',
                                'Only `.csv` and `.RDS` files are supported at this time.'))
                NULL
            }
        }
    })

    ##########################################################################################################
    # calculate the numeric summary; it is an expensive operation for large datasets
    ##########################################################################################################    
    numeric_summary_data <- reactive({

        # typically I would do the progress in the while rendering the UI, but this is used while updating
        # the summary options and i'm not sure which will be called first
        withProgress(value=1/2, message='Calculating Numeric Summary',{

            log_message_block_start('Calculating Numeric Summary')
            rt_explore_numeric_summary(dataset=dataset())
        })
    })

    ##########################################################################################################
    # calculate the categoric summary; it is an expensive operation for large datasets
    ##########################################################################################################    
    categoric_summary_data <- reactive({

        withProgress(value=1/2, message='Calculating Categoric Summary',{

            log_message_block_start('Calculating Categoric Summary')
            rt_explore_categoric_summary(dataset=dataset())
        })
    })

    ##########################################################################################################
    ##########################################################################################################
    # REACTIVE UI
    ##########################################################################################################
    ##########################################################################################################

    output$selected_numeric_summary_options_UI <- renderUI({

        # reactive data
        option_values <- colnames(numeric_summary_data())

        option_values <- option_values[option_values != 'feature']
        checkboxGroupInput(inputId='selected_numeric_summary_options',
                           label='Summary Options',
                           choices=option_values,
                           selected=c('perc_nulls', 'perc_zeros', 'mean', 'coef_of_var', 'skewness', 'min', 
                                      'percentile_50', 'max'),
                           inline=FALSE,
                           width=NULL)
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
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })

    output$selected_variable_plot_point_color_UI <- renderUI({

        selectInput(inputId='selected_variable_plot_point_color',
                    label = 'Color Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })

    output$selected_variable_plot_point_size_UI <- renderUI({

        selectInput(inputId='selected_variable_plot_point_size',
                    label = 'Size Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })

    observe({

        req(input$selected_variable_plot_variable)
        req(input$selected_variable_plot_comparison)

        primary_variable_local <- input$selected_variable_plot_variable
        comparison_variable_local <- input$selected_variable_plot_comparison

        if(primary_variable_local == select_variable || comparison_variable_local == select_variable_optional) {

            shinyjs::hide('selected_variable_plot_point_size_UI')
            shinyjs::hide('selected_variable_plot_point_color_UI')

        }

        if(primary_variable_local != select_variable || comparison_variable_local != select_variable_optional) {

            updateCollapse(session, 'collapse_variable_plot_controls', open='Plot Options')
        }
    })

    ##########################################################################################################
    ##########################################################################################################
    # RENDER OUTPUT
    ##########################################################################################################
    ##########################################################################################################

    output$dataset_head_table <- renderDataTable({

        head(dataset(), 500)
    })

    output$dataset_types_table <- renderDataTable({

        withProgress(value=1/2, message='Loading Types',{

            # reactive data            
            dataset_local <- dataset()

            types <- map_chr(colnames(dataset_local), ~ class(dataset_local[, .]))
            data.frame(variable=colnames(dataset_local), type=types)
        })
    })

    output$numeric_summary_table <- renderDataTable({

        # reactive data
        numeric_summary_local <- numeric_summary_data()
        selected_numeric_options_local <- input$selected_numeric_summary_options

        numeric_summary_local[, c('feature', selected_numeric_options_local)]
    })

    output$categoric_summary_table <- renderDataTable({
        
        categoric_summary_data()

    })

    output$correlation_plot <- renderPlot({

        withProgress(value=1/2, message='Calculating Correlations',{

            dataset_local <- dataset()

            if(input$selected_correlation_pretty_text) {

                dataset_local <- rt_pretty_dataset(dataset_local)
            }

            log_message_block_start('Calculating Correlations & Creating Plot')
            log_message_variable('selected_correlation_corr_threshold', input$selected_correlation_corr_threshold)
            log_message_variable('selected_correlation_p_value_threshold', input$selected_correlation_p_value_threshold)
            log_message_variable('selected_correlation_base_size', input$selected_correlation_base_size)
            log_message_variable('selected_correlation_pretty_text', input$selected_correlation_pretty_text)

            # see note about why I use print, in `variable plot` section below.
            print(rt_explore_plot_correlations(dataset=dataset_local,
                                              corr_threshold=input$selected_correlation_corr_threshold,
                                              p_value_threshold=input$selected_correlation_p_value_threshold,
                                              base_size=input$selected_correlation_base_size,
                                              type='pearson'))
        })
    }, height = function() {

        session$clientData$output_correlation_plot_width * 0.66  # set height to % of width

    })

    ##########################################################################################################
    # Variable Plot
    # NOTE: i use `print(xxx_plot)` because ggplot does some sort of lazy evaluation, which means that the 
    # withProgress finishes but the plot is still rendering and if the plot takes a long time to render, the
    # shiny app is still working/blocking but no progress is shown. `print` seems to force evaluation while
    # not affecting return of the plot from the function or it being displayed in shiny
    ##########################################################################################################
    output$variable_plot <- renderPlot({

        req(input$selected_variable_plot_variable)
        req(input$selected_variable_plot_comparison)

        log_message_variable('selected_variable_plots_pretty_text', input$selected_variable_plots_pretty_text)

        # reactive data
        dataset_local <- dataset()
        primary_variable_local <- input$selected_variable_plot_variable
        comparison_variable_local <- input$selected_variable_plot_comparison
        selected_variable_plot_point_size_local <- input$selected_variable_plot_point_size
        selected_variable_plot_point_color_local <- input$selected_variable_plot_point_color

        selected_variable_plots_alpha_local <- input$selected_variable_plots_alpha
        selected_variable_plots_annotate_points_local <- input$selected_variable_plots_annotate_points
        selected_variable_plots_base_size_local <- input$selected_variable_plots_base_size
        selected_variable_plots_histogram_bins_local <- input$selected_variable_plots_histogram_bins
        selected_variable_plots_jitter_local <- input$selected_variable_plots_jitter
        selected_variable_plots_order_by_count_local <- input$selected_variable_plots_order_by_count
        selected_variable_plots_numeric_graph_type_local <- input$selected_variable_plots_numeric_graph_type
        selected_variable_plots_pretty_text_local <- input$selected_variable_plots_pretty_text
        selected_variable_plots_scale_x_log_base_10_local <- input$selected_variable_plots_scale_x_log_base_10
        selected_variable_plots_scale_y_log_base_10_local <- input$selected_variable_plots_scale_y_log_base_10
        selected_variable_plots_show_variable_totals_local <- input$selected_variable_plots_show_variable_totals
        selected_variable_plots_show_comparison_totals_local <- input$selected_variable_plots_show_comparison_totals
        selected_variable_plots_trend_line_local <- input$selected_variable_plots_trend_line
        selected_variable_plots_trend_line_se_local <- input$selected_variable_plots_trend_line_se
        selected_variable_plots_x_zoom_min_local <- input$selected_variable_plots_x_zoom_min
        selected_variable_plots_x_zoom_max_local <- input$selected_variable_plots_x_zoom_max
        selected_variable_plots_y_zoom_min_local <- input$selected_variable_plots_y_zoom_min
        selected_variable_plots_y_zoom_max_local <- input$selected_variable_plots_y_zoom_max                                         

        if(primary_variable_local != select_variable) {

            withProgress(value=1/2, message='Plotting Graph',{

                log_message_block_start('Plotting Variable Graph')
    
                # if there isn't a selection for these variables, then set them to NULL, because they will be
                # passed to rtools functions (and if they aren't null, rtools expects column names)
                if(comparison_variable_local == select_variable_optional) {

                    comparison_variable_local <- NULL
                }
                # these can actually be NULL (unlike comparison_variable_local which is req)
                # these can't be req because they aren't even shown initially
                if(is.null(selected_variable_plot_point_size_local) || 
                    selected_variable_plot_point_size_local == select_variable_optional) {

                    selected_variable_plot_point_size_local <- NULL
                }
                if(is.null(selected_variable_plot_point_color_local) ||
                    selected_variable_plot_point_color_local == select_variable_optional) {

                    selected_variable_plot_point_color_local <- NULL
                }

                log_message_variable('primary_variable', primary_variable_local)
                log_message_variable('comparison_variable', comparison_variable_local)
                log_message_variable('selected_variable_plot_point_size', selected_variable_plot_point_size_local)
                log_message_variable('selected_variable_plot_point_color', selected_variable_plot_point_color_local)
                log_message_variable('selected_variable_plots_base_size', selected_variable_plots_base_size_local)
                log_message_variable('selected_variable_plots_pretty_text', selected_variable_plots_pretty_text_local)
                log_message_variable('selected_variable_plots_annotate_points', selected_variable_plots_annotate_points_local)
                
                
                if(selected_variable_plots_pretty_text_local) {
                    # if we change to pretty text, it will update the columns and all values to be "pretty",
                    # but that means we have to take the variables they selected and change them to be
                    # "pretty" as well so subsetting by them finds the correct column

                    dataset_local <- rt_pretty_dataset(dataset=dataset_local)

                    # R uses the "`My Variable`" syntax for variables with spaces which dplyr's xxx_() relies on
                    primary_variable_local <- rt_pretty_text(primary_variable_local)
                    if(!is.null(comparison_variable_local)) {

                        comparison_variable_local <- rt_pretty_text(comparison_variable_local)
                    }
                    if(!is.null(selected_variable_plot_point_size_local)) {

                        selected_variable_plot_point_size_local <- rt_pretty_text(selected_variable_plot_point_size_local)
                    }
                    if(!is.null(selected_variable_plot_point_color_local)) {

                        selected_variable_plot_point_color_local <- rt_pretty_text(selected_variable_plot_point_color_local)
                    }

                    log_message_variable('updated primary_variable', primary_variable_local)
                    log_message_variable('updated comparison_variable', comparison_variable_local)
                    log_message_variable('updated selected_variable_plot_point_size', selected_variable_plot_point_size_local)
                    log_message_variable('updated selected_variable_plot_point_color', selected_variable_plot_point_color_local)
                    log_message_generic('column names', paste0(colnames(dataset_local), collapse = '; '))
                }


                ##############################################################################################
                # Numeric Primary Variable
                ##############################################################################################
                if(is.numeric(dataset_local[, primary_variable_local])) {

                    ##########################################################################################
                    # Numeric Secondary Variable
                    ##########################################################################################
                    if(!is.null(comparison_variable_local) && is.numeric(dataset_local[, comparison_variable_local])) {

                        hide_show_numeric_numeric(session)

                        log_message('**numeric numeric**')

                        log_message_variable('selected_variable_plots_alpha', selected_variable_plots_alpha_local)
                        log_message_variable('selected_variable_plots_jitter', selected_variable_plots_jitter_local)
                        log_message_variable('selected_variable_plots_trend_line', selected_variable_plots_trend_line_local)
                        log_message_variable('selected_variable_plots_trend_line_se', selected_variable_plots_trend_line_se_local)

                        log_message_variable('selected_variable_plots_x_zoom_min', selected_variable_plots_x_zoom_min_local)
                        log_message_variable('selected_variable_plots_x_zoom_max', selected_variable_plots_x_zoom_max_local)
                        log_message_variable('selected_variable_plots_y_zoom_min', selected_variable_plots_y_zoom_min_local)
                        log_message_variable('selected_variable_plots_y_zoom_max', selected_variable_plots_y_zoom_max_local)
                        log_message_variable('selected_variable_plots_annotate_points', selected_variable_plots_annotate_points_local)
                        log_message_variable('selected_variable_plots_scale_x_log_base_10', selected_variable_plots_scale_x_log_base_10_local)
                        log_message_variable('selected_variable_plots_scale_y_log_base_10', selected_variable_plots_scale_y_log_base_10_local)

                        scatter_plot <- rt_explore_plot_scatter(dataset=dataset_local,
                                                variable=primary_variable_local,
                                                comparison_variable=comparison_variable_local,
                                                color_variable=selected_variable_plot_point_color_local,
                                                size_variable=selected_variable_plot_point_size_local,
                                                alpha=selected_variable_plots_alpha_local,
                                                jitter=selected_variable_plots_jitter_local,
                                                x_zoom_min=selected_variable_plots_x_zoom_min_local,
                                                x_zoom_max=selected_variable_plots_x_zoom_max_local,
                                                y_zoom_min=selected_variable_plots_y_zoom_min_local,
                                                y_zoom_max=selected_variable_plots_y_zoom_max_local,
                                                base_size=selected_variable_plots_base_size_local)

                        scatter_plot <- scale_axes_log10(plot=scatter_plot,
                                                         scale_x=selected_variable_plots_scale_x_log_base_10_local,
                                                         scale_y=selected_variable_plots_scale_y_log_base_10_local)

                        add_confidence_interval <- !is.null(selected_variable_plots_trend_line_se_local) && 
                            selected_variable_plots_trend_line_se_local == 'Yes'
                        scatter_plot <- add_trend_line(plot=scatter_plot,
                                                       trend_line_type=selected_variable_plots_trend_line_local,
                                                       confidence_interval=add_confidence_interval,
                                                       color_variable=selected_variable_plot_point_color_local)
                        
                        scatter_plot <- prettyfy_plot(plot=scatter_plot,
                                      dataset=dataset_local,
                                      comparison_variable=comparison_variable_local,
                                      annotate_points=selected_variable_plots_annotate_points_local)
                        
                        print(scatter_plot)
                    ##########################################################################################
                    # NULL Or Categoric Secondary Variable
                    ##########################################################################################
                    } else {

                        show_boxplot <- selected_variable_plots_numeric_graph_type_local == 'Boxplot'

                        hide_show_numeric_categoric(session=session, showing_boxplot=show_boxplot)

                        if(show_boxplot) {

                            log_message('**numeric null/categoric - boxplot**')

                            log_message_variable('selected_variable_plots_y_zoom_min', selected_variable_plots_y_zoom_min_local)
                            log_message_variable('selected_variable_plots_y_zoom_max', selected_variable_plots_y_zoom_max_local)
                            log_message_variable('selected_variable_plots_scale_y_log_base_10', selected_variable_plots_scale_y_log_base_10_local)


                            box_plot <- rt_explore_plot_boxplot(dataset=dataset_local,
                                                                variable=primary_variable_local,
                                                                comparison_variable=comparison_variable_local,
                                                                y_zoom_min=selected_variable_plots_y_zoom_min_local,
                                                                y_zoom_max=selected_variable_plots_y_zoom_max_local,
                                                                base_size=selected_variable_plots_base_size_local)
                            box_plot <- scale_axes_log10(plot=box_plot,
                                                         scale_x=FALSE,
                                                         scale_y=selected_variable_plots_scale_y_log_base_10_local)

                            print(box_plot)


                        } else {

                            log_message('**numeric null/categoric - histogram**')

                            log_message_variable('selected_variable_plots_histogram_bins', selected_variable_plots_histogram_bins_local)
                            log_message_variable('selected_variable_plots_x_zoom_min', selected_variable_plots_x_zoom_min_local)
                            log_message_variable('selected_variable_plots_x_zoom_max', selected_variable_plots_x_zoom_max_local)
                            log_message_variable('selected_variable_plots_scale_x_log_base_10', selected_variable_plots_scale_x_log_base_10_local)
                            
                            histogram_plot <- rt_explore_plot_histogram(dataset=dataset_local,
                                                                        variable=primary_variable_local,
                                                                        comparison_variable=comparison_variable_local,
                                                                        num_bins=selected_variable_plots_histogram_bins_local,
                                                                        x_zoom_min=selected_variable_plots_x_zoom_min_local,
                                                                        x_zoom_max=selected_variable_plots_x_zoom_max_local,
                                                                        base_size=selected_variable_plots_base_size_local)

                            histogram_plot <- scale_axes_log10(plot=histogram_plot,
                                                               scale_x=selected_variable_plots_scale_x_log_base_10_local,
                                                               scale_y=FALSE)

                            print(histogram_plot)
                        }
                    }

                ##############################################################################################
                # Categoric Primary Variable
                ##############################################################################################
                } else {

                    ##########################################################################################
                    # Numeric Secondary Variable
                    ##########################################################################################
                    if(!is.null(comparison_variable_local) && is.numeric(dataset_local[, comparison_variable_local])) {

                        hide_show_categoric_numeric(session)

                        log_message('**categoric numeric**')

                        log_message_variable('selected_variable_plots_y_zoom_min', selected_variable_plots_y_zoom_min_local)
                        log_message_variable('selected_variable_plots_y_zoom_max', selected_variable_plots_y_zoom_max_local)
                        log_message_variable('selected_variable_plots_scale_y_log_base_10', selected_variable_plots_scale_y_log_base_10_local)

                        box_plot <- rt_explore_plot_boxplot(dataset=dataset_local,
                                                            variable=comparison_variable_local,
                                                            comparison_variable=primary_variable_local,
                                                            y_zoom_min=selected_variable_plots_y_zoom_min_local,
                                                            y_zoom_max=selected_variable_plots_y_zoom_max_local,
                                                            base_size=selected_variable_plots_base_size_local)

                        box_plot <- scale_axes_log10(plot=box_plot,
                                                     scale_x=FALSE,
                                                     scale_y=selected_variable_plots_scale_y_log_base_10_local)

                        print(box_plot)

                    ##########################################################################################
                    # NULL Or Categoric Secondary Variable
                    ##########################################################################################
                    } else {
                    
                        hide_show_categoric_categoric(session)

                        log_message('**categoric null/categoric**')

                        log_message_variable('selected_variable_plots_order_by_count', selected_variable_plots_order_by_count_local)
                        log_message_variable('selected_variable_plots_show_variable_totals', selected_variable_plots_show_variable_totals_local)
                        log_message_variable('selected_variable_plots_show_comparison_totals', selected_variable_plots_show_comparison_totals_local)

                        unique_values_plot <- rt_explore_plot_unique_values(dataset=dataset_local,
                                                                            variable=primary_variable_local,
                                                                            comparison_variable=comparison_variable_local,
                                                                            order_by_count=selected_variable_plots_order_by_count_local,
                                                                            show_group_totals=selected_variable_plots_show_variable_totals_local,
                                                                            show_comparison_totals=selected_variable_plots_show_comparison_totals_local,
                                                                            base_size=selected_variable_plots_base_size_local)

                        print(unique_values_plot)
                    }
                }
            })
        } else {

            NULL
        }
    }, height = function() {

        session$clientData$output_variable_plot_width * 0.66  # set height to % of width
    })
})
