

renderUI__variable_plots_variable_UI <- function(dataset) {

    renderUI({
        selectInput(inputId='variable_plots_variable',
                    label = 'Variable',
                    choices = c(select_variable, colnames(dataset())),
                    selected = select_variable,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_comparison_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_comparison',
                    label = 'Comparison Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_sum_by_variable_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_sum_by_variable',
                    label = 'Sum By Variable',
                    choices = c(select_variable_optional, colnames(dataset() %>% select_if(is.numeric))),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_point_color_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_point_color',
                    label = 'Color Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_point_size_UI <- function(dataset) {

    renderUI({

        selectInput(inputId='variable_plots_point_size',
                    label = 'Size Variable',
                    choices = c(select_variable_optional, colnames(dataset())),
                    selected = select_variable_optional,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = 500,
                    size = NULL)
    })
}

renderUI__variable_plots_filter_bscollapse_UI <- function(filter_controls_list) {
 
    renderUI({
 
        tagList(list=filter_controls_list())
    })
}

# Events that control the color of the Filter collapse panel, so that it turns red when the filters haven't
# been applied (i.e. changes that haven't been applied)
observeEvent__variable_plots_filter_clear <- function(input, session) {

    observeEvent(input$variable_plots_filter_clear, ({

        if(isolate(input$variable_plots_filter_use)) {  # only update if we are using the filter

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'danger'))
        }
    }))
}

observeEvent__variable_plots_filter_apply <- function(input, session) {

    observeEvent(input$variable_plots_filter_apply, ({

        if(isolate(input$variable_plots_filter_use)) {  # only update if we are using the filter

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'success'))
        }
    }))
}

observeEvent__variable_plots_filter_use <- function(input, session) {

    observeEvent(input$variable_plots_filter_use, {

        if(input$variable_plots_filter_use) {

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'success'))

        } else {

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'default'))
        }
    })
}

observe__variable_plots_bscollapse__dynamic_inputs <- function(input, session, dataset) {

    observe({

        req(dataset())

        # this is a hack to register all of the dynamic controls to the reactive event listener
        # also use it to check values (i.e. only update colors if the filters are active i.e. any are not null)
        selections <- list()
        for(column_name in colnames(dataset())) {
            value <- input[[paste0('dynamic_filter_variable_plots_', column_name)]]
            selections <- append(selections, value)
        }

        # only update if we are using the filter
        # also, if any of the selections are not null, that means they have been initialized and we can begin
        # to mark as being changed otherwise, the filter section hasn't even been opened
        if(isolate(input$variable_plots_filter_use) && any(map_lgl(selections, ~ !is.null(.)))) {

            updateCollapse(session, "variable_plots_bscollapse", style = list('Filters' = 'danger'))
        }
    })
}







##############################################################################################################
# Variable Plot
# NOTE: i use `print(xxx_plot)` because ggplot does some sort of lazy evaluation, which means that the 
# withProgress finishes but the plot is still rendering and if the plot takes a long time to render, the
# shiny app is still working/blocking but no progress is shown. `print` seems to force evaluation while
# not affecting return of the plot from the function or it being displayed in shiny
##############################################################################################################
get_dynamic_filter_selections <- function(input, columns) {

    # get all of the selections from the dynamic filters without triggering refresh for the first time
    selections_list <- map(columns, ~ isolate(input[[paste0('dynamic_filter_variable_plots_', .)]]))
    names(selections_list) <- columns

    return (selections_list)

}

renderPlot__variable_plot <- function(input, output, session, reactive__variable_plots__ggplot, messages) {

    renderPlot({
        withProgress(value=1/2, message='Plotting Graph',{

           messages$value <- capture_messages_warnings(function() print(reactive__variable_plots__ggplot()))

           log_message_variable('messages$value', messages$value)

        })

    }, height = function() {

        session$clientData$output_variable_plots_width * 0.66  # set height to % of width
    })
}

variable_plots_ggplot_messages__renderPrint <- function(reactive_values) {

    renderPrint({
        cat(reactive_values$value)
    })
}

reactive__variable_plots__ggplot_creator <- function(input, session, variable_plots_filtered_dataset) {
    reactive({

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
}
