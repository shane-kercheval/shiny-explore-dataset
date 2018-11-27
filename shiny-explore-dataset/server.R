#devtools::install_github('shane-kercheval/rtools')

library(shiny)
library(shinyWidgets)
library(shinyBS)
library(rtools)
library(ggplot2)
library(stringr)
library(tidyverse)
library(scales)
library(lattice)
library(lubridate)

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
    dataset_or_null <- function(file) {
        
        withProgress(value=1/2, message='Uploading Data',{

            if(file.exists(file)) {

                return (read.csv(file, header=TRUE))

            } else {

                return (NULL)
            }
        })
    }

    dataset <- reactive({

        req(input$selected_preloaded_dataset)

        # reactive data
        upload_file_path <- input$uploadFile$datapath
        local_selected_preloaded_dataset <- input$selected_preloaded_dataset

        log_message_block_start('Loading Dataset')
        log_message_variable('upload_file_path', upload_file_path)
        log_message_variable('selected_preloaded_dataset', local_selected_preloaded_dataset)

        if(is.null(upload_file_path)) {
            
            if(local_selected_preloaded_dataset == 'Credit') {

                dataset_or_null('example_datasets/credit.csv')

            } else if(local_selected_preloaded_dataset == 'Housing') {

                dataset_or_null('example_datasets/housing.csv')

            } else if(local_selected_preloaded_dataset == 'Insurance') {

                dataset_or_null('example_datasets/insurance.csv')

            } else if(local_selected_preloaded_dataset == 'Iris') {

                return (iris)

            } else if(local_selected_preloaded_dataset == 'Flights') {

                return (
                    data.frame(nycflights13::flights %>%
                        mutate(date = make_date(year, month, day)) %>%
                        select(-year, -month, -day) %>%
                        select(date, everything())))

            } else if(local_selected_preloaded_dataset == 'Gapminder') {

                return (gapminder::gapminder)

            } else {

                return (NULL)
            }
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
    # Run Regression when user clicks Run button
    ##########################################################################################################    
    regression_results <- eventReactive(input$regression_run_button, {

        if(input$regression_selected_dependent_variable == select_variable) {
            return (NULL)
        }

        local_interaction_term1 <- input$regression_selected_interaction_term1
        local_interaction_term2 <- input$regression_selected_interaction_term2

        withProgress(value=1/2, message='Running Regression',{

            interaction_variables <- NULL

            if(!is.null(local_interaction_term1) && local_interaction_term1 != select_variable &&
               !is.null(local_interaction_term2) && local_interaction_term2 != select_variable) {

                interaction_variables <- list(c(local_interaction_term1,
                                                local_interaction_term2))
            }

            # updates to reactive variables will not trigger an update here, only regression_run_button
            results <- easy_regression(dataset=dataset(),
                                       dependent_variable=input$regression_selected_dependent_variable,
                                       independent_variables=input$regression_selected_independent_variables,
                                       # list of vectors, each element in the list is a pair of interaction terms
                                       # only supporting two interaction variables at the moment
                                       interaction_variables=interaction_variables)

            shinyjs::show('regression_formula_header')
            shinyjs::show('regression_summary_header_UI')
            shinyjs::show('regression_vif_header')
            
            return (results)
        })
    })

    ##########################################################################################################
    ##########################################################################################################
    # REACTIVE UI
    ##########################################################################################################
    ##########################################################################################################

    ##########################################################################################################
    # Variable Plot Reactive UI
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

        local_primary_variable <- input$selected_variable_plot_variable
        local_comparison_variable <- input$selected_variable_plot_comparison

        if(local_primary_variable == select_variable || local_comparison_variable == select_variable_optional) {

            shinyjs::hide('selected_variable_plot_point_size_UI')
            shinyjs::hide('selected_variable_plot_point_color_UI')

        }

        if(local_primary_variable != select_variable || local_comparison_variable != select_variable_optional) {

            updateCollapse(session, 'collapse_variable_plot_controls', open='Plot Options')
        }
    })


    ##########################################################################################################
    # Regression Reactive UI
    ##########################################################################################################
    output$regression_selected_dependent_variable_UI <- renderUI({

        selectInput(inputId='regression_selected_dependent_variable',
                    label='Dependent Variable',
                    choices=c(select_variable, colnames(dataset())),
                    selected=select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })

    output$regression_selected_independent_variables_UI <- renderUI({

        req(input$regression_selected_dependent_variable)

        column_names <- colnames(dataset())
        possible_variables <- column_names[! column_names %in% input$regression_selected_dependent_variable]        

        checkboxGroupInput(inputId='regression_selected_independent_variables',
                           label='Independent Variables',
                           choices=possible_variables,
                           selected=possible_variables,
                           inline=FALSE,
                           width=NULL)
    })

    output$regression_summary_header_UI <- renderUI({

        req(regression_results())

        local_regression_results <- regression_results()

        if(is.null(local_regression_results$reference)) {  # reference is filled for logistic regression

            reference <- ''            

        } else {

            reference <- paste0('(reference: `', local_regression_results$reference, '`)')
        }

        tags$h4(paste(regression_results()$type, 'Summary', reference))
    })

    observeEvent(input$regression_toggle_all_ind_variables, {

        # if none selected, select all, otherwise (if any selected); unselect all
        if(length(input$regression_selected_independent_variables) == 0) {

            column_names <- colnames(dataset())
            possible_variables <- column_names[! column_names %in% input$regression_selected_dependent_variable]        

            updateCheckboxGroupInput(session=session,
                                     inputId='regression_selected_independent_variables',
                                     selected=possible_variables)

        } else {

            updateCheckboxGroupInput(session=session,
                                     inputId='regression_selected_independent_variables',
                                     selected=character(0))
        }

    })

    output$regression_selected_interaction_term1_UI <- renderUI({

        req(input$regression_selected_dependent_variable)

        # cannot select dependent_variable
        column_names <- colnames(dataset())
        possible_variables <- column_names[! column_names %in% input$regression_selected_dependent_variable]

        selectInput(inputId='regression_selected_interaction_term1',
                    label='Interaction Variable 1',
                    choices=c(select_variable, possible_variables),
                    selected=select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })

    output$regression_selected_interaction_term2_UI <- renderUI({

        req(input$regression_selected_dependent_variable)
        req(input$regression_selected_interaction_term1)

        # cannot select dependent_variable or the first term
        column_names <- colnames(dataset())
        possible_variables <- column_names[! column_names %in% c(input$regression_selected_dependent_variable,
                                                                 input$regression_selected_interaction_term1)]

        selectInput(inputId='regression_selected_interaction_term2',
                    label='Interaction Variable 2',
                    choices=c(select_variable, possible_variables),
                    selected=select_variable,
                    multiple=FALSE,
                    selectize=TRUE,
                    width=500,
                    size=NULL)
    })


    ##########################################################################################################
    ##########################################################################################################
    # RENDER OUTPUT
    ##########################################################################################################
    ##########################################################################################################

    ##########################################################################################################
    # Variable Plot Output
    ##########################################################################################################
    output$dataset_head_table <- renderDataTable({

        head(dataset(), 500)
    })

    output$dataset_types_table <- renderDataTable({

        withProgress(value=1/2, message='Loading Types',{

            local_dataset <- dataset()

            types <- map_chr(colnames(local_dataset), ~ class(local_dataset[, .])[1])
            return (data.frame(variable=colnames(local_dataset), type=types))
        })
    })

    output$numeric_summary_table <- renderDataTable({

        local_numeric_summary <- numeric_summary_data()
        local_selected_numeric_options <- input$selected_numeric_summary_options
        local_numeric_summary[, c('feature', local_selected_numeric_options)]
    })

    output$categoric_summary_table <- renderDataTable({
        
        categoric_summary_data()

    })

    output$categoric_summary_text <- renderPrint({
        
        # get R's summary of the categoric data
        summary(dataset()[, as.character(categoric_summary_data()$feature)])

    })

    output$correlation_plot <- renderPlot({

        withProgress(value=1/2, message='Calculating Correlations', {

            local_dataset <- dataset()

            if(input$selected_correlation_pretty_text) {

                local_dataset <- rt_pretty_dataset(local_dataset)
            }

            log_message_block_start('Calculating Correlations & Creating Plot')
            log_message_variable('selected_correlation_corr_threshold', input$selected_correlation_corr_threshold)
            log_message_variable('selected_correlation_p_value_threshold', input$selected_correlation_p_value_threshold)
            log_message_variable('selected_correlation_base_size', input$selected_correlation_base_size)
            log_message_variable('selected_correlation_pretty_text', input$selected_correlation_pretty_text)

            # see note about why I use print, in `variable plot` section below.
            print(rt_explore_plot_correlations(dataset=local_dataset,
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
        local_dataset <- dataset()
        local_primary_variable <- input$selected_variable_plot_variable
        local_comparison_variable <- input$selected_variable_plot_comparison
        local_point_size <- input$selected_variable_plot_point_size
        local_point_color <- input$selected_variable_plot_point_color

        local_transparency <- input$selected_variable_plots_transparency / 100
        local_annotate_points <- input$selected_variable_plots_annotate_points
        local_base_size <- input$selected_variable_plots_base_size
        local_histogram_bins <- input$selected_variable_plots_histogram_bins
        local_jitter <- input$selected_variable_plots_jitter
        local_order_by_count <- input$selected_variable_plots_order_by_count
        local_numeric_graph_type <- input$selected_variable_plots_numeric_graph_type
        local_pretty_text <- input$selected_variable_plots_pretty_text
        local_scale_x_log_base_10 <- input$selected_variable_plots_scale_x_log_base_10
        local_scale_y_log_base_10 <- input$selected_variable_plots_scale_y_log_base_10
        local_show_variable_totals <- input$selected_variable_plots_show_variable_totals
        local_show_comparison_totals <- input$selected_variable_plots_show_comparison_totals
        local_trend_line <- input$selected_variable_plots_trend_line
        local_trend_line_se <- input$selected_variable_plots_trend_line_se
        local_x_zoom_min <- input$selected_variable_plots_x_zoom_min
        local_x_zoom_max <- input$selected_variable_plots_x_zoom_max
        local_y_zoom_min <- input$selected_variable_plots_y_zoom_min
        local_y_zoom_max <- input$selected_variable_plots_y_zoom_max                                         

        if(local_primary_variable != select_variable) {

            withProgress(value=1/2, message='Plotting Graph',{

                log_message_block_start('Plotting Variable Graph')
    
                # if there isn't a selection for these variables, then set them to NULL, because they will be
                # passed to rtools functions (and if they aren't null, rtools expects column names)
                if(local_comparison_variable == select_variable_optional) {

                    local_comparison_variable <- NULL
                }
                # these can actually be NULL (unlike local_comparison_variable which is req)
                # these can't be req because they aren't even shown initially
                if(is.null(local_point_size) || local_point_size == select_variable_optional) {

                    local_point_size <- NULL
                }
                if(is.null(local_point_color) || local_point_color == select_variable_optional) {

                    local_point_color <- NULL
                }

                log_message_variable('primary_variable', local_primary_variable)
                log_message_variable('comparison_variable', local_comparison_variable)
                log_message_variable('selected_variable_plot_point_size', local_point_size)
                log_message_variable('selected_variable_plot_point_color', local_point_color)
                log_message_variable('selected_variable_plots_base_size', local_base_size)
                log_message_variable('selected_variable_plots_pretty_text', local_pretty_text)
                log_message_variable('selected_variable_plots_annotate_points', local_annotate_points)
                
                
                if(local_pretty_text) {
                    # if we change to pretty text, it will update the columns and all values to be "pretty",
                    # but that means we have to take the variables they selected and change them to be
                    # "pretty" as well so subsetting by them finds the correct column

                    local_dataset <- rt_pretty_dataset(dataset=local_dataset)

                    # R uses the "`My Variable`" syntax for variables with spaces which dplyr's xxx_() relies on
                    local_primary_variable <- rt_pretty_text(local_primary_variable)
                    if(!is.null(local_comparison_variable)) {

                        local_comparison_variable <- rt_pretty_text(local_comparison_variable)
                    }
                    if(!is.null(local_point_size)) {

                        local_point_size <- rt_pretty_text(local_point_size)
                    }
                    if(!is.null(local_point_color)) {

                        local_point_color <- rt_pretty_text(local_point_color)
                    }

                    log_message_variable('updated primary_variable', local_primary_variable)
                    log_message_variable('updated comparison_variable', local_comparison_variable)
                    log_message_variable('updated selected_variable_plot_point_size', local_point_size)
                    log_message_variable('updated selected_variable_plot_point_color', local_point_color)
                    log_message_generic('column names', paste0(colnames(local_dataset), collapse = '; '))
                }


                ##############################################################################################
                # Numeric Primary Variable
                ##############################################################################################
                if(is.numeric(local_dataset[, local_primary_variable])) {

                    ##########################################################################################
                    # Numeric Secondary Variable
                    ##########################################################################################
                    if(!is.null(local_comparison_variable) && is.numeric(local_dataset[, local_comparison_variable])) {

                        hide_show_numeric_numeric(session)

                        log_message('**numeric numeric**')

                        log_message_variable('selected_variable_plots_transparency', local_transparency)
                        log_message_variable('selected_variable_plots_jitter', local_jitter)
                        log_message_variable('selected_variable_plots_trend_line', local_trend_line)
                        log_message_variable('selected_variable_plots_trend_line_se', local_trend_line_se)

                        log_message_variable('selected_variable_plots_x_zoom_min', local_x_zoom_min)
                        log_message_variable('selected_variable_plots_x_zoom_max', local_x_zoom_max)
                        log_message_variable('selected_variable_plots_y_zoom_min', local_y_zoom_min)
                        log_message_variable('selected_variable_plots_y_zoom_max', local_y_zoom_max)
                        log_message_variable('selected_variable_plots_annotate_points', local_annotate_points)
                        log_message_variable('selected_variable_plots_scale_x_log_base_10', local_scale_x_log_base_10)
                        log_message_variable('selected_variable_plots_scale_y_log_base_10', local_scale_y_log_base_10)

                        scatter_plot <- rt_explore_plot_scatter(dataset=local_dataset,
                                                                variable=local_primary_variable,
                                                                comparison_variable=local_comparison_variable,
                                                                color_variable=local_point_color,
                                                                size_variable=local_point_size,
                                                                # alpha is a measure of opacity which is the opposite of transparency, but transparency is more user-friendly
                                                                alpha= 1 - local_transparency,
                                                                jitter=local_jitter,
                                                                x_zoom_min=local_x_zoom_min,
                                                                x_zoom_max=local_x_zoom_max,
                                                                y_zoom_min=local_y_zoom_min,
                                                                y_zoom_max=local_y_zoom_max,
                                                                base_size=local_base_size)

                        scatter_plot <- scale_axes_log10(plot=scatter_plot,
                                                         scale_x=local_scale_x_log_base_10,
                                                         scale_y=local_scale_y_log_base_10)

                        add_confidence_interval <- !is.null(local_trend_line_se) && local_trend_line_se == 'Yes'
                        scatter_plot <- add_trend_line(plot=scatter_plot,
                                                       trend_line_type=local_trend_line,
                                                       confidence_interval=add_confidence_interval,
                                                       color_variable=local_point_color)
                        
                        scatter_plot <- prettyfy_plot(plot=scatter_plot,
                                                      dataset=local_dataset,
                                                      comparison_variable=local_comparison_variable,
                                                      annotate_points=local_annotate_points)
                        
                        print(scatter_plot)
                    ##########################################################################################
                    # NULL Or Categoric Secondary Variable
                    ##########################################################################################
                    } else {

                        show_boxplot <- local_numeric_graph_type == 'Boxplot'

                        hide_show_numeric_categoric(session=session, showing_boxplot=show_boxplot)

                        if(show_boxplot) {

                            log_message('**numeric null/categoric - boxplot**')

                            log_message_variable('selected_variable_plots_y_zoom_min', local_y_zoom_min)
                            log_message_variable('selected_variable_plots_y_zoom_max', local_y_zoom_max)
                            log_message_variable('selected_variable_plots_scale_y_log_base_10', local_scale_y_log_base_10)

                            box_plot <- rt_explore_plot_boxplot(dataset=local_dataset,
                                                                variable=local_primary_variable,
                                                                comparison_variable=local_comparison_variable,
                                                                y_zoom_min=local_y_zoom_min,
                                                                y_zoom_max=local_y_zoom_max,
                                                                base_size=local_base_size)
                            box_plot <- scale_axes_log10(plot=box_plot,
                                                         scale_x=FALSE,
                                                         scale_y=local_scale_y_log_base_10)

                            print(box_plot)


                        } else {

                            log_message('**numeric null/categoric - histogram**')

                            log_message_variable('selected_variable_plots_histogram_bins', local_histogram_bins)
                            log_message_variable('selected_variable_plots_x_zoom_min', local_x_zoom_min)
                            log_message_variable('selected_variable_plots_x_zoom_max', local_x_zoom_max)
                            log_message_variable('selected_variable_plots_scale_x_log_base_10', local_scale_x_log_base_10)
                            
                            histogram_plot <- rt_explore_plot_histogram(dataset=local_dataset,
                                                                        variable=local_primary_variable,
                                                                        comparison_variable=local_comparison_variable,
                                                                        num_bins=local_histogram_bins,
                                                                        x_zoom_min=local_x_zoom_min,
                                                                        x_zoom_max=local_x_zoom_max,
                                                                        base_size=local_base_size)

                            histogram_plot <- scale_axes_log10(plot=histogram_plot,
                                                               scale_x=local_scale_x_log_base_10,
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
                    if(!is.null(local_comparison_variable) && is.numeric(local_dataset[, local_comparison_variable])) {

                        hide_show_categoric_numeric(session)

                        log_message('**categoric numeric**')

                        log_message_variable('selected_variable_plots_y_zoom_min', local_y_zoom_min)
                        log_message_variable('selected_variable_plots_y_zoom_max', local_y_zoom_max)
                        log_message_variable('selected_variable_plots_scale_y_log_base_10', local_scale_y_log_base_10)

                        box_plot <- rt_explore_plot_boxplot(dataset=local_dataset,
                                                            variable=local_comparison_variable,
                                                            comparison_variable=local_primary_variable,
                                                            y_zoom_min=local_y_zoom_min,
                                                            y_zoom_max=local_y_zoom_max,
                                                            base_size=local_base_size)

                        box_plot <- scale_axes_log10(plot=box_plot,
                                                     scale_x=FALSE,
                                                     scale_y=local_scale_y_log_base_10)

                        print(box_plot)

                    ##########################################################################################
                    # NULL Or Categoric Secondary Variable
                    ##########################################################################################
                    } else {
                    
                        hide_show_categoric_categoric(session)

                        log_message('**categoric null/categoric**')

                        log_message_variable('selected_variable_plots_order_by_count', local_order_by_count)
                        log_message_variable('selected_variable_plots_show_variable_totals', local_show_variable_totals)
                        log_message_variable('selected_variable_plots_show_comparison_totals', local_show_comparison_totals)

                        unique_values_plot <- rt_explore_plot_unique_values(dataset=local_dataset,
                                                                            variable=local_primary_variable,
                                                                            comparison_variable=local_comparison_variable,
                                                                            order_by_count=local_order_by_count,
                                                                            show_group_totals=local_show_variable_totals,
                                                                            show_comparison_totals=local_show_comparison_totals,
                                                                            base_size=local_base_size)

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

    ##########################################################################################################
    # Regression Output
    ##########################################################################################################
    output$regression_summary_output <- renderPrint({

        req(regression_results())

        summary(regression_results()$results)
    })

    output$regression_number_of_rows_missing_removed <- renderText({

        req(regression_results())

        paste('Number of missing/removed rows from dataset:', length(regression_results()$rows_excluded))
    })

    output$regression_formula <- renderText({

        req(regression_results())

        regression_results()$formula
    })

    output$regression_summary_vif <- renderPrint({

        req(regression_results())

        car::vif(regression_results()$results)
    })

    render_diagnostic_plot <- function(graph_function, graph_width_function) {

        return (
            renderPlot({

                req(regression_results())

                withProgress(value=1/2, message='Creating Regression Diagnostic Graph',{

                    graph_function()
                })
        
            }, height = graph_width_function)
        )
    }

    output$regression_diagnostic_actual_vs_predicted <- render_diagnostic_plot(
        graph_function=function() {
            xyplot(dataset()[, isolate({input$regression_selected_dependent_variable})] ~ predict(regression_results()$results),
                   type=c('p', 'g'),
                   xlab='Predicted', ylab='Actual')
        },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_actual_vs_predicted_width})
    output$regression_diagnostic_residuals_vs_fitted <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=1) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_residuals_vs_fitted_width})
    output$regression_diagnostic_actual_vs_observed <- render_diagnostic_plot(
        graph_function=function() {
            xyplot(predict(regression_results()$results) ~ 1:nrow(dataset()),
                   type=c('p', 'g'),
                   xlab='Observation Number', ylab='Predicted')
        },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_actual_vs_observed_width})
    output$regression_diagnostic_normal_qq <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=2) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_normal_qq_width})
    output$regression_diagnostic_scale_location <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=3) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_scale_location_width})
    output$regression_diagnostic_cooks_distance <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=4) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_cooks_distance_width})
    output$regression_diagnostic_residuals_vs_leverage <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=5) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_residuals_vs_leverage_width})
    output$regression_diagnostic_cooks_distance_vs_leverage <- render_diagnostic_plot(
        graph_function=function() { plot(regression_results()$results, which=6) },
        graph_width_function=function() {0.66 * session$clientData$output_regression_diagnostic_cooks_distance_vs_leverage_width})
})
