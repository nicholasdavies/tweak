#' Create inputs
#' @export
inputs = function(...)
{
    tweak.do({
        # Read control spec
        ctrl = process_input(def)

        # Add inputs
        d@input = c(d@input, ctrl$item)

        # Add controls to dashboard
        exec_dashboard(d, ctrl$html)
    }, def = list(...))
}
