############################################################################################################## 
# Variable Plot Output
##############################################################################################################
renderDataTable__dataset_head_table <- function(dataset) {

    renderDataTable({

        head(dataset(), 500)
    })
}

renderDataTable__dataset_types_table <- function(dataset) {

    renderDataTable({

        withProgress(value=1/2, message='Loading Types',{

            local_dataset <- dataset()

            types <- map_chr(colnames(local_dataset), ~ class(local_dataset[, .])[1])
            return (data.frame(variable=colnames(local_dataset), type=types))
        })
    })
}

renderDataTable__numeric_summary_table <- function(input, numeric_summary_data) {

    renderDataTable({

        local_numeric_summary <- numeric_summary_data()
        local_selected_numeric_options <- input$selected_numeric_summary_options
        local_numeric_summary[, c('feature', local_selected_numeric_options)]
    })
}

renderDataTable__categoric_summary_table <- function(categoric_summary_data) {

    renderDataTable({
        
        categoric_summary_data()

    })
}

renderPrint__categoric_summary_text <- function(dataset, categoric_summary_data) {

    renderPrint({
        
        # get R's summary of the categoric data
        summary(dataset()[, as.character(categoric_summary_data()$feature)])

    })
}

renderPlot__correlation_plot <- function(input, output, session, dataset) {

    renderPlot({

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
}

##############################################################################################################
# Variable Plot
# NOTE: i use `print(xxx_plot)` because ggplot does some sort of lazy evaluation, which means that the 
# withProgress finishes but the plot is still rendering and if the plot takes a long time to render, the
# shiny app is still working/blocking but no progress is shown. `print` seems to force evaluation while
# not affecting return of the plot from the function or it being displayed in shiny
##############################################################################################################
renderPlot__variable_plot <- function(input, output, session, dataset) {

    renderPlot({

        req(input$selected_variable_plot_variable)
        req(input$selected_variable_plot_comparison)

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

        local_filter_factor_lump_number <- input$selected_filter_factor_lump_number

        if(local_primary_variable != select_variable) {

            withProgress(value=1/2, message='Plotting Graph',{

                log_message_block_start('Plotting Variable Graph')
    
                # if there isn't a selection for these variables, then set them to NULL, because they will be
                # passed to rtools functions (and if they aren't null, rtools expects column names)
                local_comparison_variable <- null_if_select_variable_optional(local_comparison_variable)
                # these can actually be NULL (unlike local_comparison_variable which is req)
                # these can't be req because they aren't even shown initially
                local_point_size <- null_if_select_variable_optional(local_point_size)
                local_point_color <- null_if_select_variable_optional(local_point_color)
                local_comparison_variable <- null_if_select_variable_optional(local_comparison_variable)

                if(is.na(local_filter_factor_lump_number) || local_filter_factor_lump_number == 0) {

                    local_filter_factor_lump_number <- NA
                }

                log_message_variable('primary_variable', local_primary_variable)
                log_message_variable('comparison_variable', local_comparison_variable)
                log_message_variable('selected_variable_plot_point_size', local_point_size)
                log_message_variable('selected_variable_plot_point_color', local_point_color)
                log_message_variable('selected_variable_plots_base_size', local_base_size)
                log_message_variable('selected_variable_plots_pretty_text', local_pretty_text)
                log_message_variable('selected_variable_plots_annotate_points', local_annotate_points)
                log_message_variable('selected_filter_factor_lump_number', local_filter_factor_lump_number)
                
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

                        add_confidence_interval <- !is.null(local_trend_line_se) && local_trend_line_se == 'Yes'


                        local_dataset %>% 
                            custom_filter(factor_lump_number=local_filter_factor_lump_number) %>%
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
                                          annotate_points=local_annotate_points) %>%
                            print()
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

                            local_dataset %>%
                                custom_filter(factor_lump_number=local_filter_factor_lump_number) %>%
                                rt_explore_plot_boxplot(variable=local_primary_variable,
                                                        comparison_variable=local_comparison_variable,
                                                        y_zoom_min=local_y_zoom_min,
                                                        y_zoom_max=local_y_zoom_max,
                                                        base_size=local_base_size) %>%
                                scale_axes_log10(scale_x=FALSE,
                                                 scale_y=local_scale_y_log_base_10) %>%
                            print()


                        } else {

                            log_message('**numeric null/categoric - histogram**')

                            log_message_variable('selected_variable_plots_histogram_bins', local_histogram_bins)
                            log_message_variable('selected_variable_plots_x_zoom_min', local_x_zoom_min)
                            log_message_variable('selected_variable_plots_x_zoom_max', local_x_zoom_max)
                            log_message_variable('selected_variable_plots_scale_x_log_base_10', local_scale_x_log_base_10)
                            

                            local_dataset %>%
                                custom_filter(factor_lump_number=local_filter_factor_lump_number) %>%
                                rt_explore_plot_histogram(variable=local_primary_variable,
                                                          comparison_variable=local_comparison_variable,
                                                          num_bins=local_histogram_bins,
                                                          x_zoom_min=local_x_zoom_min,
                                                          x_zoom_max=local_x_zoom_max,
                                                          base_size=local_base_size) %>%
                                scale_axes_log10(scale_x=local_scale_x_log_base_10,
                                                 scale_y=FALSE) %>%
                                print()
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

                        local_dataset %>%
                                custom_filter(factor_lump_number=local_filter_factor_lump_number) %>%
                                rt_explore_plot_boxplot(variable=local_comparison_variable,
                                                        comparison_variable=local_primary_variable,
                                                        y_zoom_min=local_y_zoom_min,
                                                        y_zoom_max=local_y_zoom_max,
                                                        base_size=local_base_size) %>%
                                scale_axes_log10(scale_x=FALSE,
                                                 scale_y=local_scale_y_log_base_10) %>%
                                print()

                    ##########################################################################################
                    # NULL Or Categoric Secondary Variable
                    ##########################################################################################
                    } else {
                    
                        hide_show_categoric_categoric(session)

                        log_message('**categoric null/categoric**')

                        log_message_variable('selected_variable_plots_order_by_count', local_order_by_count)
                        log_message_variable('selected_variable_plots_show_variable_totals', local_show_variable_totals)
                        log_message_variable('selected_variable_plots_show_comparison_totals', local_show_comparison_totals)


                        local_dataset %>%
                                custom_filter(factor_lump_number=local_filter_factor_lump_number) %>%
                                rt_explore_plot_unique_values(variable=local_primary_variable,
                                                              comparison_variable=local_comparison_variable,
                                                              order_by_count=local_order_by_count,
                                                              show_group_totals=local_show_variable_totals,
                                                              show_comparison_totals=local_show_comparison_totals,
                                                              base_size=local_base_size) %>%
                                print()
                    }
                }
            })
        } else {

            NULL
        }
    }, height = function() {

        session$clientData$output_variable_plot_width * 0.66  # set height to % of width
    })
}
