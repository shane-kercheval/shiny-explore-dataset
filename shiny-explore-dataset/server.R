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

source('helper_scripts/definitions.R')
source('helper_scripts/logging_functions.R')
source('helper_scripts/generic_helpers.R')
source('helper_scripts/plot_helpers.R')
source('helper_scripts/dynamic_show_hide_controls.R')
source('helper_scripts/reactive_data_helpers.R')
source('helper_scripts/dataset_loading_helpers.R')
source('helper_scripts/numeric_summary_helpers.R')
source('helper_scripts/categoric_summary_helpers.R')
source('helper_scripts/correlation_helpers.R')
source('helper_scripts/variable_plots_helpers.R')
source('helper_scripts/regression_helpers.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##########################################################################################################
    # main dataset
    ##########################################################################################################
    dataset <- reactive__dataset(input, output, session)
    output$dataset_head_table <- renderDataTable__dataset_head_table(dataset)
    output$dataset_types_table <- renderDataTable__dataset_types_table(dataset)

    ##########################################################################################################
    # numeric summary data
    ##########################################################################################################
    numeric_summary_data <- reactive__numeric_summary(input, output, session, dataset)
    output$numeric_summary_table <- renderDataTable__numeric_summary_table(input, numeric_summary_data)
    output$numeric_summary_options_UI <- renderUI__numeric_summary_options_UI(numeric_summary_data)

    ##########################################################################################################
    # categoric summary data
    ##########################################################################################################
    categoric_summary_data <- reactive__categoric_summary(input, output, session, dataset)
    output$categoric_summary_table <- renderDataTable__categoric_summary_table(categoric_summary_data)
    output$categoric_summary_text <- renderPrint__categoric_summary_text(dataset, categoric_summary_data)

    ##########################################################################################################
    # Correlation Plot
    ##########################################################################################################
    output$correlation_plot <- renderPlot__correlation_plot(input, output, session, dataset)

    ##########################################################################################################
    # Viarable Plot
    ##########################################################################################################
    reactive__variable_plots__ggplot <- reactive({

        req(input$variable_plots_variable)
        req(input$variable_plots_comparison)

        # reactive data
        local_dataset <- variable_plots_filtered_dataset()
        local_primary_variable <- input$variable_plots_variable
        local_comparison_variable <- input$variable_plots_comparison
        local_sum_by_variable <- input$variable_plots_sum_by_variable
        local_point_size <- input$variable_plots_point_size
        local_point_color <- input$variable_plots_point_color

        local_transparency <- input$variable_plots_transparency / 100
        local_annotate_points <- input$variable_plots_annotate_points
        local_base_size <- input$variable_plots_base_size
        local_histogram_bins <- input$variable_plots_histogram_bins
        local_jitter <- input$variable_plots_jitter
        local_order_by_count <- input$variable_plots_order_by_count
        local_numeric_graph_type <- input$variable_plots_numeric_graph_type
        local_pretty_text <- input$variable_plots_pretty_text
        local_scale_x_log_base_10 <- input$variable_plots_scale_x_log_base_10
        local_scale_y_log_base_10 <- input$variable_plots_scale_y_log_base_10
        local_show_variable_totals <- input$variable_plots_show_variable_totals
        local_show_comparison_totals <- input$variable_plots_show_comparison_totals
        local_trend_line <- input$variable_plots_trend_line
        local_trend_line_se <- input$variable_plots_trend_line_se
        local_x_zoom_min <- input$variable_plots_x_zoom_min
        local_x_zoom_max <- input$variable_plots_x_zoom_max
        local_y_zoom_min <- input$variable_plots_y_zoom_min
        local_y_zoom_max <- input$variable_plots_y_zoom_max

        local_variable_plots_filter_factor_lump_number <- input$variable_plots_filter_factor_lump_number

        ggplot_object <- NULL

        if(local_primary_variable != select_variable) {


            log_message_block_start('Creating ggplot object')
            
            # if there isn't a selection for these variables, then set them to NULL, because they will be
            # passed to rtools functions (and if they aren't null, rtools expects column names)
            local_comparison_variable <- null_if_select_variable_optional(local_comparison_variable)
            # these can actually be NULL (unlike local_comparison_variable which is req)
            # these can't be req because they aren't even shown initially
            local_sum_by_variable <- null_if_select_variable_optional(local_sum_by_variable)
            local_point_size <- null_if_select_variable_optional(local_point_size)
            local_point_color <- null_if_select_variable_optional(local_point_color)
            local_comparison_variable <- null_if_select_variable_optional(local_comparison_variable)

            if(is.na(local_variable_plots_filter_factor_lump_number) ||
                    local_variable_plots_filter_factor_lump_number == 0) {

                local_variable_plots_filter_factor_lump_number <- NA
            }

            log_message_variable('primary_variable', local_primary_variable)
            log_message_variable('comparison_variable', local_comparison_variable)
            log_message_variable('variable_plots_sum_by_variable', local_sum_by_variable)
            log_message_variable('variable_plots_point_size', local_point_size)
            log_message_variable('variable_plots_point_color', local_point_color)
            log_message_variable('variable_plots_base_size', local_base_size)
            log_message_variable('variable_plots_pretty_text', local_pretty_text)
            log_message_variable('variable_plots_annotate_points', local_annotate_points)
            log_message_variable('variable_plots_filter_factor_lump_number', local_variable_plots_filter_factor_lump_number)
            
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
                log_message_variable('updated variable_plots_point_size', local_point_size)
                log_message_variable('updated variable_plots_point_color', local_point_color)
                log_message_generic('column names', paste0(colnames(local_dataset), collapse = '; '))
            }

            ##############################################################################################
            # Numeric Primary Variable
            ##############################################################################################
            if(is.numeric(local_dataset[, local_primary_variable])) {

                ##########################################################################################
                # Numeric Secondary Variable
                ##########################################################################################
                if(!is.null(local_comparison_variable) &&
                        is.numeric(local_dataset[, local_comparison_variable])) {

                    hide_show_numeric_numeric(session)

                    log_message('**numeric numeric**')

                    log_message_variable('variable_plots_transparency', local_transparency)
                    log_message_variable('variable_plots_jitter', local_jitter)
                    log_message_variable('variable_plots_trend_line', local_trend_line)
                    log_message_variable('variable_plots_trend_line_se', local_trend_line_se)

                    log_message_variable('variable_plots_x_zoom_min', local_x_zoom_min)
                    log_message_variable('variable_plots_x_zoom_max', local_x_zoom_max)
                    log_message_variable('variable_plots_y_zoom_min', local_y_zoom_min)
                    log_message_variable('variable_plots_y_zoom_max', local_y_zoom_max)
                    log_message_variable('variable_plots_annotate_points', local_annotate_points)
                    log_message_variable('variable_plots_scale_x_log_base_10', local_scale_x_log_base_10)
                    log_message_variable('variable_plots_scale_y_log_base_10', local_scale_y_log_base_10)

                    add_confidence_interval <- !is.null(local_trend_line_se) && local_trend_line_se == 'Yes'

                    ggplot_object <- local_dataset %>% 
                            custom_filter(factor_lump_number=local_variable_plots_filter_factor_lump_number) %>%
                            rt_explore_plot_scatter(variable=local_primary_variable,
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
                                                    base_size=local_base_size) %>%
                            scale_axes_log10(scale_x=local_scale_x_log_base_10,
                                             scale_y=local_scale_y_log_base_10) %>%
                            add_trend_line(trend_line_type=local_trend_line,
                                           confidence_interval=add_confidence_interval,
                                           color_variable=local_point_color) %>% 
                            prettyfy_plot(comparison_variable=local_comparison_variable,
                                          annotate_points=local_annotate_points)
                ##########################################################################################
                # NULL Or Categoric Secondary Variable
                ##########################################################################################
                } else {

                    show_boxplot <- local_numeric_graph_type == 'Boxplot'

                    hide_show_numeric_categoric(session=session, showing_boxplot=show_boxplot)

                    if(show_boxplot) {

                        log_message('**numeric null/categoric - boxplot**')

                        log_message_variable('variable_plots_y_zoom_min', local_y_zoom_min)
                        log_message_variable('variable_plots_y_zoom_max', local_y_zoom_max)
                        log_message_variable('variable_plots_scale_y_log_base_10', local_scale_y_log_base_10)

                        ggplot_object <- local_dataset %>%
                                custom_filter(factor_lump_number=local_variable_plots_filter_factor_lump_number) %>%
                                rt_explore_plot_boxplot(variable=local_primary_variable,
                                                        comparison_variable=local_comparison_variable,
                                                        y_zoom_min=local_y_zoom_min,
                                                        y_zoom_max=local_y_zoom_max,
                                                        base_size=local_base_size) %>%
                                scale_axes_log10(scale_x=FALSE,
                                                 scale_y=local_scale_y_log_base_10)

                    } else {

                        log_message('**numeric null/categoric - histogram**')

                        log_message_variable('variable_plots_histogram_bins', local_histogram_bins)
                        log_message_variable('variable_plots_x_zoom_min', local_x_zoom_min)
                        log_message_variable('variable_plots_x_zoom_max', local_x_zoom_max)
                        log_message_variable('variable_plots_scale_x_log_base_10', local_scale_x_log_base_10)
                        
                        ggplot_object <- local_dataset %>%
                                custom_filter(factor_lump_number=local_variable_plots_filter_factor_lump_number) %>%
                                rt_explore_plot_histogram(variable=local_primary_variable,
                                                          comparison_variable=local_comparison_variable,
                                                          num_bins=local_histogram_bins,
                                                          x_zoom_min=local_x_zoom_min,
                                                          x_zoom_max=local_x_zoom_max,
                                                          base_size=local_base_size) %>%
                                scale_axes_log10(scale_x=local_scale_x_log_base_10,
                                                 scale_y=FALSE)
                    }
                }

            ##############################################################################################
            # Categoric Primary Variable
            ##############################################################################################
            } else {

                ##########################################################################################
                # Numeric Secondary Variable
                ##########################################################################################
                if(!is.null(local_comparison_variable) &&
                        is.numeric(local_dataset[, local_comparison_variable])) {

                    hide_show_categoric_numeric(session)

                    log_message('**categoric numeric**')

                    log_message_variable('variable_plots_y_zoom_min', local_y_zoom_min)
                    log_message_variable('variable_plots_y_zoom_max', local_y_zoom_max)
                    log_message_variable('variable_plots_scale_y_log_base_10', local_scale_y_log_base_10)

                    ggplot_object <- local_dataset %>%
                            custom_filter(factor_lump_number=local_variable_plots_filter_factor_lump_number) %>%
                            rt_explore_plot_boxplot(variable=local_comparison_variable,
                                                    comparison_variable=local_primary_variable,
                                                    y_zoom_min=local_y_zoom_min,
                                                    y_zoom_max=local_y_zoom_max,
                                                    base_size=local_base_size) %>%
                            scale_axes_log10(scale_x=FALSE,
                                             scale_y=local_scale_y_log_base_10)
                ##########################################################################################
                # NULL Or Categoric Secondary Variable
                ##########################################################################################
                } else {
                
                    hide_show_categoric_categoric(session)

                    log_message('**categoric null/categoric**')

                    log_message_variable('variable_plots_order_by_count', local_order_by_count)
                    log_message_variable('variable_plots_show_variable_totals', local_show_variable_totals)
                    log_message_variable('variable_plots_show_comparison_totals', local_show_comparison_totals)

                    ggplot_object <- local_dataset %>%
                            custom_filter(factor_lump_number=local_variable_plots_filter_factor_lump_number) %>%
                            rt_explore_plot_value_totals(variable=local_primary_variable,
                                                         comparison_variable=local_comparison_variable,
                                                         sum_by_variable=local_sum_by_variable,
                                                         order_by_count=local_order_by_count,
                                                         show_group_totals=local_show_variable_totals,
                                                         show_comparison_totals=local_show_comparison_totals,
                                                         base_size=local_base_size)
                }
            }
        }

        return (ggplot_object)
    })


    messages <- reactiveValues(value=NULL)
    output$variable_plots_ggplot_messages <- renderPrint({
        cat(messages$value)
    })



    # Viarable Plot - Filters
    filter_controls_list <- reactive__filter_controls_list(input, dataset)
    output$variable_plots_filter_bscollapse_UI <- renderUI__variable_plots_filter_bscollapse_UI(filter_controls_list)
    variable_plots_filtered_dataset <- reactive__variable_plots_filtered_dataset(input, dataset)  # duplicate dataset (which is bad for large datasets) so that the filters don't have to be reapplied every time.
    observeEvent__variable_plots_filter_clear(input, session)
    observeEvent__variable_plots_filter_apply(input, session)
    observe__variable_plots_bscollapse__dynamic_inputs(input, session, dataset)
    observeEvent__variable_plots_filter_use(input, session)

    output$variable_plots <- renderPlot__variable_plot(input, output, session, reactive__variable_plots__ggplot, messages)
    output$variable_plots_variable_UI <- renderUI__variable_plots_variable_UI(dataset)
    output$variable_plots_comparison_UI <- renderUI__variable_plots_comparison_UI(dataset)
    output$variable_plots_sum_by_variable_UI <- renderUI__variable_plots_sum_by_variable_UI(dataset)
    output$variable_plots_point_color_UI <- renderUI__variable_plots_point_color_UI(dataset)
    output$variable_plots_point_size_UI <- renderUI__variable_plots_point_size_UI(dataset)
    observe__variable_plots__hide_show_uncollapse_on_primary_vars(input, output, session)


    ##########################################################################################################
    # Regression Output
    ##########################################################################################################
    # Run Regression when user clicks Run button
    regression_results <- eventReactive__regression_results(input, output, session, dataset)
    output$regression_summary_output <- renderPrint__regression_summary_output(regression_results)
    output$regression_number_of_rows_missing_removed <- renderText__regression_number_of_rows_missing_removed(regression_results)
    output$regression_formula <- renderText__regression_formula(regression_results)
    output$regression_summary_vif <- renderPrint__regression_summary_vif(regression_results)
    output$regression_diagnostic_actual_vs_predicted <- render_diagnostic_plot__regression_diagnostic_actual_vs_predicted(input, session, dataset, regression_results)
    output$regression_diagnostic_residuals_vs_fitted <- render_diagnostic_plot__regression_diagnostic_residuals_vs_fitted(input, session, dataset, regression_results)
    output$regression_diagnostic_actual_vs_observed <- render_diagnostic_plot__regression_diagnostic_actual_vs_observed(input, session, dataset, regression_results)
    output$regression_diagnostic_normal_qq <- render_diagnostic_plot__regression_diagnostic_normal_qq(input, session, dataset, regression_results)
    output$regression_diagnostic_scale_location <- render_diagnostic_plot__regression_diagnostic_scale_location(input, session, dataset, regression_results)
    output$regression_diagnostic_cooks_distance <- render_diagnostic_plot__regression_diagnostic_cooks_distance(input, session, dataset, regression_results)
    output$regression_diagnostic_residuals_vs_leverage <- render_diagnostic_plot__regression_diagnostic_residuals_vs_leverage(input, session, dataset, regression_results)
    output$regression_diagnostic_cooks_distance_vs_leverage <- render_diagnostic_plot__regression_diagnostic_cooks_distance_vs_leverage(input, session, dataset, regression_results)
    # Regression Reactive UI
    output$regression_dependent_variable_UI <- renderUI__regression_dependent_variable_UI(dataset)
    output$regression_independent_variables_UI <- renderUI__regression_independent_variables_UI(input, dataset)
    output$regression_summary_header_UI <- renderUI__regression_summary_header_UI(regression_results)
    output$regression_interaction_term1_UI <- renderUI__regression_interaction_term1_UI(input, dataset)
    output$regression_interaction_term2_UI <- renderUI__regression_interaction_term2_UI(input, dataset)
    observeEvent__regression_toggle_all_ind_variables(input, dataset, session)
})
