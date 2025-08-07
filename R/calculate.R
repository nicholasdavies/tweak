#' Calculate value
#' @export
calculate = function(..., .envir = parent.frame())
{
    tweak.do({
        # Extract values
        values = list()
        for (i in rlang::seq2(2, length(args))) {
            values[[i - 1]] = args[[i]]
        }
        names(values) = names(args)[-1]

        # Ensure all values have names
        if (!rlang::is_named2(values)) {
            stop("All expressions in calculate() must be named.")
        }

        # Add calcs
        for (v in seq_along(values)) {
            d@calc = c(d@calc,
                list(list(type = "calc", code = values[[v]], env = env)))
            names(d@calc)[length(d@calc)] = names(values)[v]
        }

        # Finalize
        exec_dashboard(d)
    }, args = substitute(list(...)), env = .envir)
}
