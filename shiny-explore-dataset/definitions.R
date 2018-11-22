select_target_variable <- "<Select target variable, if applicable>"
select_variable <- "<Select variable, if applicable>"
select_comparison_variable_optional <- "<Select comparison variable (optional)>"

theme_base_size <- 16

div_custom <- function(v_height_perc=90, output) {
    return (
        tags$div(style=paste0("height:", v_height_perc, "vh !important;"), 
                 output)
    )
}
