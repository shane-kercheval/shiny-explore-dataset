library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

source('helper_scripts/definitions.R')

##############################################################################################################
#### naming conventions
#
# <section>_<control>
##############################################################################################################
shinyUI(fluidPage(theme="custom.css",

    useShinyjs(),

    titlePanel('Explore Dataset'),
    navlistPanel(
        tabPanel(
            'Load Dataset',
            fluidRow(
                column(4, selectInput(inputId='preloaded_dataset',
                                      label='Preloaded Datasets',
                                      choices=c('Diamonds',
                                                'Credit',
                                                'Housing',
                                                'Insurance',
                                                'Iris',
                                                'Flights',
                                                'Gapminder'),
                                      selected='Diamonds')
                ),
                column(5, fileInput(inputId='uploadFile', 'Upload data (.csv/.RDS)'))
            ),
            tags$br(),
            tabsetPanel(type='tabs',
                tabPanel(
                    "First 500 Records of Dataset",
                    tags$div(class='results-table', dataTableOutput(outputId='dataset_head_table'))
                ),
                tabPanel(
                    'Variable Types',
                    tags$div(class='results-table', dataTableOutput(outputId='dataset_types_table'))
                )
            )
        ),
        tabPanel(
            'Numeric Summary',
            column(2,
                   class='column-input-control-style',
                   tags$div(class='input-control-style', uiOutput('numeric_summary_options_UI'))
            ),
            column(10, tags$div(class='results-table', dataTableOutput(outputId='numeric_summary_table')))
        ),
        tabPanel(
            'Categoric Summary',
            tags$div(class='results-table', dataTableOutput(outputId='categoric_summary_table')),
            tags$br(),
            h4('Summary of Values'),
            tags$div(style='width: 800px', verbatimTextOutput(outputId='categoric_summary_text'))
        ),
        tabPanel(
            'Correlations',
            column(
                2,
                class='column-input-control-style',
                tags$div(
                    class='input-control-style',
                    sliderTextInput(inputId='correlation_corr_threshold',
                                    label='Min Correlation Threshold', ## percent increase
                                    choices=seq(0, 1, 0.05),
                                    selected=0,
                                    grid=TRUE),
                    sliderTextInput(inputId='correlation_p_value_threshold',
                                    label='Max P-Value Treshold',
                                    choices=seq(0, 1, 0.05),
                                    selected=1,
                                    grid=TRUE),
                    sliderTextInput(inputId='correlation_base_size',
                                    label='Text Size',
                                    choices=seq(6, 20, 1),
                                    selected=15,
                                    grid=TRUE),
                    checkboxInput(inputId='correlation_pretty_text',
                                  label='Pretty Text', value=FALSE, width=NULL)
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

                bsCollapse(id='variable_plots_bscollapse', open='Variables', multiple=TRUE,
                    bsCollapsePanel(
                        'Variables',
                        uiOutput('variable_plots_variable_UI'),
                        uiOutput('variable_plots_comparison_UI'),
                        # NOTE: the variables below can't be hidden initially (but rather are dynamically hidden)
                        # because the values need to load the first time this tab is clicked
                        # if not, when they become active (and therefore the values change to the default,
                        # they will trigger an unncessary plot refresh
                        uiOutput('variable_plots_sum_by_variable_UI'),
                        uiOutput('variable_plots_point_color_UI'),
                        uiOutput('variable_plots_point_size_UI'),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Filters',
                        fluidRow(
                            column(4,
                                checkboxInput(inputId='variable_plots_filter_use',
                                              label='Use Filters', value=FALSE, width=NULL)
                            ),
                            column(8,
                                tags$div(style='margin-bottom: 10px;', actionButton(inputId='variable_plots_filter_apply', label='Apply Filters')),
                                tags$div(style='margin-bottom: 20px;', actionButton(inputId='variable_plots_filter_clear', label='Clear Filters'))
                            )
                        ),
                        uiOutput('variable_plots_filter_bscollapse_UI')
                    ),
                    bsCollapsePanel(
                        'Plot Options',
                        shinyjs::hidden(
                            selectInput(inputId='variable_plots_numeric_graph_type',
                                    label='Type',
                                    choices=c('Boxplot', 'Histogram'),
                                    selected='Boxplot')
                        ),
                        shinyjs::hidden(tags$div(id='div_variable_plots_group_barchar_controls',
                             checkboxInput(inputId='variable_plots_order_by_count',
                                           label='Order By Totals', value=TRUE, width=NULL),
                             checkboxInput(inputId='variable_plots_show_variable_totals',
                                           label='Show Variable Totals', value=TRUE, width=NULL),
                             checkboxInput(inputId='variable_plots_show_comparison_totals',
                                           label='Show Comparison Totals', value=TRUE, width=NULL)
                        )),
                        shinyjs::hidden(
                             numericInput(inputId='variable_plots_histogram_bins',
                                          label='Number of Bins',
                                          value=30)                 
                        ),
                        shinyjs::hidden(tags$div(id='div_variable_plots_group_scatter_controls',
                            sliderTextInput(inputId='variable_plots_transparency',
                                            label='Transparency',
                                            choices=c(seq(0, 90, 10), 99),
                                            selected=60,
                                            post  = " %",
                                            grid=TRUE),
                            checkboxInput(inputId='variable_plots_jitter',
                                          label='Jitter', value=FALSE, width=NULL),
                            radioButtons(inputId='variable_plots_trend_line',
                                         label='Trend Line:',
                                         choices=c('None', 'Straight', 'Smooth'),
                                         selected='None',
                                         inline=TRUE,
                                         width=NULL),
                            radioButtons(inputId='variable_plots_trend_line_se',
                                         label='Trend Confidence Interval:',
                                         choices=c('No', 'Yes'),
                                         selected='Yes',
                                         inline=TRUE,
                                         width=NULL)
                        )),
                        shinyjs::hidden(tags$div(id='div_variable_plots_group_x_zoom_controls',
                             checkboxInput(inputId='variable_plots_scale_x_log_base_10',
                                           label='Scale X-Axis Log 10', value=FALSE, width=NULL),
                             numericInput(inputId='variable_plots_x_zoom_min',
                                          label='X-Axis Zoom Min',
                                          value=NULL),
                             numericInput(inputId='variable_plots_x_zoom_max',
                                          label='X-Axis Zoom Max',
                                          value=NULL)                         
                        )),
                        shinyjs::hidden(tags$div(id='div_variable_plots_group_y_zoom_controls',
                             checkboxInput(inputId='variable_plots_scale_y_log_base_10',
                                           label='Scale Y-Axis Log 10', value=FALSE, width=NULL),
                             numericInput(inputId='variable_plots_y_zoom_min',
                                          label='Y-Axis Zoom Min',
                                          value=NULL),
                             numericInput(inputId='variable_plots_y_zoom_max',
                                          label='Y-Axis Zoom Max',
                                          value=NULL)                         
                        )),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Pretty Options',
                        sliderTextInput(inputId='variable_plots_base_size',
                                        label='Text Size',
                                        choices=seq(6, 20, 1),
                                        selected=15,
                                        grid=TRUE),
                        checkboxInput(inputId='variable_plots_pretty_text',
                                      label='Pretty Text', value=FALSE, width=NULL),
                        shinyjs::hidden(
                           checkboxInput(inputId='variable_plots_annotate_points',
                                         label='Annotate Points', value=FALSE, width=NULL)
                        ),
                        numericInput(inputId='variable_plots_filter_factor_lump_number',
                                     label='Top N Categories',
                                     value=NULL),
                        style='default'
                    )
                )
            ),
            column(9,
                tags$div(plotOutput(outputId='variable_plots')),
                tags$div(verbatimTextOutput(outputId='variable_plots_ggplot_messages'))
            )
        ),
        tabPanel(
            'Regression',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='regression_collapse_controls', open='Variables', multiple=TRUE,
                    bsCollapsePanel(
                        'Variables',
                        uiOutput('regression_dependent_variable_UI'),
                        uiOutput('regression_independent_variables_UI'),
                        actionButton(inputId='regression_toggle_all_ind_variables',
                                     label='Toggle All Variables'),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Interaction Effects',
                        uiOutput('regression_interaction_term1_UI'),
                        uiOutput('regression_interaction_term2_UI'),
                        style='default'
                    )
                )
            ),
            column(9,
                tabsetPanel(type="tabs",
                    tabPanel("Output",
                        tags$br(),
                        actionButton(inputId='regression_run_button', label='Run Regression'),
                        tags$br(),tags$br(),
                        hidden(tags$h4(id='regression_formula_header', 'Formula')),
                        verbatimTextOutput(outputId='regression_formula'),
                        tags$br(),
                        hidden(uiOutput('regression_summary_header_UI')),
                        verbatimTextOutput(outputId='regression_number_of_rows_missing_removed'),
                        tags$br(),
                        verbatimTextOutput(outputId='regression_summary_output')
                    ),
                    tabPanel("VIFs",
                        hidden(tags$h4(id='regression_vif_header', 'Variance Inflation Factors')),
                        verbatimTextOutput(outputId='regression_summary_vif')
                    ),
                    tabPanel("Diagnostic Plots",
                        tags$br(),
                        tabsetPanel(type="tabs",
                            tabPanel("Actual vs Predicted",
                                plotOutput(outputId='regression_diagnostic_actual_vs_predicted')
                            ),
                            tabPanel("Residuals vs Fittted",
                                plotOutput(outputId='regression_diagnostic_residuals_vs_fitted')
                            ),
                            tabPanel("Actual vs Observed",
                                plotOutput(outputId='regression_diagnostic_actual_vs_observed')
                            ),
                            tabPanel("Normal Q-Q",
                                plotOutput(outputId='regression_diagnostic_normal_qq')
                            ),
                            tabPanel("Scale-Location",
                                plotOutput(outputId='regression_diagnostic_scale_location')
                            ),
                            tabPanel("Cooks Distance",
                                plotOutput(outputId='regression_diagnostic_cooks_distance')
                            ),
                            tabPanel("Residuals vs. Leverage",
                                plotOutput(outputId='regression_diagnostic_residuals_vs_leverage')
                            ),
                            tabPanel("Cooks Distance vs Leverage",
                                plotOutput(outputId='regression_diagnostic_cooks_distance_vs_leverage')
                            )
                        )
                    )
                )   
            )
        ),
        widths=c(2,10)
    )
))
