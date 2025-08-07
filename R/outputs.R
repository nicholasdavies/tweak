#' Output plot
#' @export
output_plot = function(expr, .envir = parent.frame())
{
    tweak.do({
        # Unique name for plot
        plot_name = paste0("plot", length(d@output) + 1)

        # Add outputs
        d@output = c(d@output,
            list(list(type = "plot", code = code, env = env)))
        names(d@output)[length(d@output)] = plot_name

        # Add plot to dashboard
        exec_dashboard(d, list(shiny::plotOutput(plot_name)))
    }, code = substitute(expr), env = .envir)
}
