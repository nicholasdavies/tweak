#' Output base or ggplot2 plot
#' @export
dash_plot = function(..., width = "100%", aspect = 1.618)
{
    # TODO allow dynamic dots
    if (...length() != 1) {
        stop("Must pass exactly one plot expression to output_plot().")
    }
    q = rlang::enquos(...)[[1]]
    id = ...names()[1]

    tweak.do({
        # Input ID for plot
        if (is.null(id) || id == "") {
            id = paste0("tweak_plot_", length(plan$output) + 1)
        }

        # Add outputs
        plan$output = c(plan$output,
            list(list(type = "plot", q = q, aspect = aspect)))
        names(plan$output)[length(plan$output)] = id

        # Add plot to dashboard
        tweak.do.return(plan, list(
            shiny::plotOutput(id, width = width, height = "auto")
        ))
    }, q = q, id = id, width = width, aspect = aspect)
}

#' Output arbitrary HTML
#' @export
dash_html = function(...)
{
    # TODO allow dynamic dots
    if (...length() != 1) {
        stop("Must pass exactly one html expression to output_html().")
    }
    q = rlang::enquos(...)[[1]]
    id = ...names()[1]

    tweak.do({
        # Input ID for html
        if (is.null(id) || id == "") {
            id = paste0("tweak_html_", length(plan$output) + 1)
        }

        # Add outputs
        plan$output = c(plan$output,
            list(list(type = "html", q = q)))
        names(plan$output)[length(plan$output)] = id

        # Add plot to dashboard
        tweak.do.return(plan, list(shiny::htmlOutput(id)))
    }, q = q, id = id)
}

#' Display values of all inputs and calcs
#'
#' @export
dash_monitor = function()
{
    dash_html({
        html = list(shiny::h3("Inputs"))
        if (length(names(input)) == 0) {
            html[[length(html) + 1]] = shiny::span(style = "font-style:italic", "None")
        } else {
            for (nm in names(input)) {
                html[[length(html) + 1]] = shiny::div(
                    paste0(nm, ": ", input[[nm]]),
                    shiny::span(style = "font-style:italic",
                        paste0("[", paste0(class(input[[nm]]), collapse = ", "), "]")
                    )
                )
            }
        }
        container = shiny::wellPanel()
        container$children = html
        return (container)
    })
}
