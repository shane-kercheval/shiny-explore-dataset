library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
source('definitions.R')

shinyUI(fluidPage(theme = "custom.css",
  
    useShinyjs(),

    titlePanel('Explore Dataset'),
    navlistPanel(
        tabPanel(
            'Load Dataset',
            fluidRow(
                column(5, fileInput(inputId='uploadFile', 'Upload data (.csv/.RDS)'))
            ),
            tags$br(),
            tags$h3('First 500 Records of Dataset:'),
            tags$div(class='results-table', dataTableOutput(outputId='dataset_head_table'))
        ),
        tabPanel(
            'Variable Types',
            tags$div(class='results-table', dataTableOutput(outputId='dataset_types_table'))
        ),
        tabPanel(
            'Numeric Summary',
            column(2,
                   class='column-input-control-style',
                   tags$div(class='input-control-style', uiOutput('selected_numeric_summary_options_UI'))
            ),
            column(10, tags$div(class='results-table', dataTableOutput(outputId='numeric_summary_table')))
        ),
        tabPanel(
            'Categoric Summary',
            tags$div(class='results-table', dataTableOutput(outputId='categoric_summary_table'))
        ),
        tabPanel(
            'Correlations',
            column(
                2,
                class='column-input-control-style',
                tags$div(
                    class='input-control-style',
                    sliderTextInput(inputId='selected_correlation_corr_threshold',
                                    label='Min Correlation Threshold', ## percent increase
                                    choices = seq(0, 1, 0.05),
                                    selected = 0,
                                    grid = TRUE),
                    sliderTextInput(inputId='selected_correlation_p_value_threshold',
                                    label='Max P-Value Treshold',
                                    choices = seq(0, 1, 0.05),
                                    selected = 1,
                                    grid = TRUE),
                    sliderTextInput(inputId='selected_correlation_base_size',
                                    label='Text Size',
                                    choices = seq(6, 20, 1),
                                    selected = 15,
                                    grid = TRUE),
                    checkboxInput(inputId='selected_correlation_pretty_text',
                                       label='Pretty Text', value = FALSE, width = NULL)
                    )
            ),
            column(10,
                   plotOutput(outputId='correlation_plot')
            )
        ),
        tabPanel(
            'Variable Plots',
            column(3,
                class='column-input-control-style',

                bsCollapse(id='collapse_variable_plot_controls', open = 'Variables', multiple = TRUE,
                    bsCollapsePanel(
                        'Variables',
                        uiOutput('selected_variable_plot_variable_UI'),
                        uiOutput('selected_variable_plot_comparison_UI'),
                        uiOutput('selected_variable_plot_point_color_UI'),
                        uiOutput('selected_variable_plot_point_size_UI'),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Plot Options',
                        shinyjs::hidden(
                            selectInput(inputId='selected_variable_plots_numeric_graph_type',
                                    label='Type',
                                    choices=c('Boxplot', 'Histogram'),
                                    selected='Boxplot')
                        ),
                        shinyjs::hidden(tags$div(id='div_variable_plots_group_barchar_controls',
                             checkboxInput(inputId='selected_variable_plots_order_by_count',
                                           label='Order By Totals', value = TRUE, width = NULL),
                             checkboxInput(inputId='selected_variable_plots_show_variable_totals',
                                           label='Show Variable Totals', value = TRUE, width = NULL),
                             checkboxInput(inputId='selected_variable_plots_show_comparison_totals',
                                           label='Show Comparison Totals', value = TRUE, width = NULL)
                        )),
                        shinyjs::hidden(
                             numericInput(inputId='selected_variable_plots_histogram_bins',
                                          label='Number of Bins',
                                          value=30)                 
                        ),
                        shinyjs::hidden(tags$div(id='div_variable_plots_group_scatter_controls',
                            sliderTextInput(inputId='selected_variable_plots_alpha',
                                            label='Transparency',
                                            choices = seq(0.1, 1, 0.1),
                                            selected = 0.3,
                                            grid = TRUE),
                            checkboxInput(inputId='selected_variable_plots_jitter',
                                          label='Jitter', value = FALSE, width = NULL),
                            radioButtons(inputId='selected_variable_plots_trend_line',
                                                    label='Trend Line:',
                                                    choices=c('None', 'Straight', 'Smooth'),
                                                    selected = 'None',
                                                    inline = TRUE,
                                                    width = NULL),
                            radioButtons(inputId='selected_variable_plots_trend_line_se',
                                                    label='Trend Confidence Interval:',
                                                    choices=c('No', 'Yes'),
                                                    selected = 'Yes',
                                                    inline = TRUE,
                                                    width = NULL)
                        )),
                        shinyjs::hidden(tags$div(id='div_variable_plots_group_x_zoom_controls',
                             checkboxInput(inputId='selected_variable_plots_scale_x_log_base_10',
                                           label='Scale X-Axis Log 10', value = FALSE, width = NULL),
                             numericInput(inputId='selected_variable_plots_x_zoom_min',
                                          label='X-Axis Zoom Min',
                                          value=NULL),
                             numericInput(inputId='selected_variable_plots_x_zoom_max',
                                          label='X-Axis Zoom Max',
                                          value=NULL)                         
                        )),
                        shinyjs::hidden(tags$div(id='div_variable_plots_group_y_zoom_controls',
                             checkboxInput(inputId='selected_variable_plots_scale_y_log_base_10',
                                           label='Scale Y-Axis Log 10', value = FALSE, width = NULL),
                             numericInput(inputId='selected_variable_plots_y_zoom_min',
                                          label='Y-Axis Zoom Min',
                                          value=NULL),
                             numericInput(inputId='selected_variable_plots_y_zoom_max',
                                          label='Y-Axis Zoom Max',
                                          value=NULL)                         
                        )),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Pretty Options',
                        sliderTextInput(inputId='selected_variable_plots_base_size',
                                        label='Text Size',
                                        choices = seq(6, 20, 1),
                                        selected = 15,
                                        grid = TRUE),
                        checkboxInput(inputId='selected_variable_plots_pretty_text',
                                      label='Pretty Text', value = FALSE, width = NULL),
                        shinyjs::hidden(
                           checkboxInput(inputId='selected_variable_plots_annotate_points',
                                         label='Annotate Points', value = FALSE, width = NULL)
                        ),
                        style='default'
                    )
                )
            ),
            column(9,
                plotOutput(outputId='variable_plot')
            )
        ),
        widths=c(2,10)
    )
))
