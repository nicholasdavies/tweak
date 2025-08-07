# Sentinel for no panel / no group
blank = "~BLANK"

# Get "keys" for all elements in [x] that have [level] levels of named nesting
keys = function(x, level)
{
    if (level == 1) {
        return (as.list(names(x)))
    }
    n = as.list(names(x))
    k = list()
    for (i in seq_along(n)) {
        m = keys(x[[i]], level - 1)
        k = c(k, lapply(m, function(mj) c(n[[i]], unlist(mj))))
    }
    return (k)
}

#' Create empty dashboard
#' @export
dashboard = function()
{
    # Indexed by page then group
    blank_container =
        list(list(list(html = list())))
    names(blank_container) = blank
    names(blank_container[[1]]) = blank

    structure(
        list(
            head = blank_container,
            main = blank_container,
            conf = blank_container,
            foot = blank_container,
            curr = 2, # main
            input = list(),
            output = list(),
            design = list(
                ui = ui_sidebar,
                args = list()
            )
        ),
        class = "tweak.dashboard"
    )
}

# Operator overload
#' @export
`+.tweak.dashboard` = function(d, x)
{
    add_to_dashboard(d, x)
}

#' Internal dispatch
add_to_dashboard = function(d, x)
{
    UseMethod("add_to_dashboard", x)
}

#' Methods
add_to_dashboard.tweak.do = function(d, x)
{
    # Run doer
    d = eval(x$body, envir = c(list(d = d), x$args))

    return (d)
}

#' Methods
add_to_dashboard.shiny.tag = function(d, x)
{
    # Add HTML to dashboard
    d[[d$curr]]$html = c(d[[d$curr]]$html, list(x))

    return (d)
}

#' Default fallback for unsupported types
add_to_dashboard.default = function(d, x)
{
    stop("Cannot add object of class '",
        paste(class(x), collapse = "/"),
        "' to a tweak.dashboard.",
        call. = FALSE
    )
}

#' Dashboard doer
do_dashboard = function(body, ...)
{
    structure(list(body = substitute(body), args = list(...)), class = "tweak.do")
}

#' Section setters
#' @export
section_head = function()
{
    do_dashboard({
        d$curr = c("head", blank, blank)
        return (d)
    })
}

#' @export
section_main = function()
{
    do_dashboard({
        d$curr = c("main", blank, blank)
        return (d)
    })
}

#' @export
section_conf = function()
{
    do_dashboard({
        d$curr = c("conf", blank, blank)
        return (d)
    })
}

#' @export
section_foot = function()
{
    do_dashboard({
        d$curr = c("foot", blank, blank)
        return (d)
    })
}


#' Group setter
#' @export
group = function(group = NULL)
{
    do_dashboard({
        if (is.null(group)) {
            d$curr[3] = blank
        } else if (is.character(group) && length(group) == 1) {
            d$curr[3] = group
        } else {
            stop("group must be either a character string or NULL.")
        }
        return (d)
    }, group = group)
}

#' Create inputs
#' @export
inputs = function(...)
{
    do_dashboard({
        # Read control spec
        ctrl = process_input(args)

        # Add controls to dashboard
        d[[d$curr]]$html = c(d[[d$curr]]$html, ctrl$html)

        # Add inputs
        d$input = c(d$input, ctrl$item)

        return (d)
    }, args = list(...))
}

#' Output plot
#' @export
output_plot = function(expr, .envir = parent.frame())
{
    do_dashboard({
        # Unique name for plot
        plot_name = paste0("plot", length(d$output) + 1)

        # Add plot to dashboard
        d[[d$curr]]$html = c(d[[d$curr]]$html,
            list(shiny::plotOutput(plot_name)))

        # Add outputs
        d$output = c(d$output,
            list(list(type = "plot", code = code, env = env)))
        names(d$output)[length(d$output)] = plot_name

        return (d)
    }, code = substitute(expr), env = .envir)
}

#' Set design to sidebar
#' @export
design_sidebar = function()
{
    do_dashboard({
        d$design = list(ui = ui_sidebar, args = list())
        return (d)
    })
}

#' Sidebar ui
ui_sidebar = function(d)
{
    # Define page layout
    content = list()

    # Get keys
    k_head = keys(d$head, 2)
    k_main = keys(d$main, 2)
    k_conf = keys(d$conf, 2)
    k_foot = keys(d$foot, 2)

    # HTML for sections
    h_head = Reduce(function(a, k) c(a, d$head[[k]]$html), k_head, init = list())
    h_main = Reduce(function(a, k) c(a, d$main[[k]]$html), k_main, init = list())
    h_conf = Reduce(function(a, k) c(a, d$conf[[k]]$html), k_conf, init = list())
    h_foot = Reduce(function(a, k) c(a, d$foot[[k]]$html), k_foot, init = list())

    # Head, if used
    if (length(h_head)) {
        content[[1]] = shiny::fluidRow(h_head)
    }

    # Foot, if used
    if (length(h_foot)) {
        h_main = c(shiny::fluidRow(h_main), shiny::fluidRow(h_foot))
    }

    # Config / main
    if (length(h_conf)) {
        content[[length(content) + 1]] = shiny::fluidRow(
            shiny::column(3, h_conf),
            shiny::column(9, h_main)
        )
    } else {
        content[[length(content) + 1]] = shiny::fluidRow(h_main)
    }

    return (shiny::fluidPage(content))
}

#' Set design to console
#' @export
design_console = function()
{
    do_dashboard({
        d$design = list(ui = ui_console, args = list())
        return (d)
    })
}

#' Console ui
ui_console = function(d)
{
    # Define page layout
    content = list()

    # Get keys
    k_head = keys(d$head, 2)
    k_main = keys(d$main, 2)
    k_conf = keys(d$conf, 2)
    k_foot = keys(d$foot, 2)

    # HTML for sections
    h_head = Reduce(function(a, k) c(a, d$head[[k]]$html), k_head, init = list())
    h_main = Reduce(function(a, k) c(a, d$main[[k]]$html), k_main, init = list())
    h_conf = Reduce(function(a, k) c(a, d$conf[[k]]$html), k_conf, init = list())
    h_foot = Reduce(function(a, k) c(a, d$foot[[k]]$html), k_foot, init = list())

    # Head, if used
    if (length(h_head)) {
        content[[1]] = shiny::column(12, shiny::fluidRow(h_head))
    }

    if (length(h_main)) {
        content[[length(content) + 1]] = shiny::column(12, shiny::fluidRow(h_main))
    }

    if (length(h_conf)) {
        content[[length(content) + 1]] = shiny::column(12, shiny::fluidRow(h_conf))
    }

    if (length(h_foot)) {
        content[[length(content) + 1]] = shiny::column(12, shiny::fluidRow(h_foot))
    }

    return (shiny::fluidPage(content))
}

#' Get environment (list) of input variables for a given output
output_env = function(d, req_vars, input)
{
    # Index used input variables into input
    req_env = lapply(req_vars, function(name) input[[name]])
    names(req_env) = req_vars

    # Convert dropdown list values from character to originally specified type
    for (v in req_vars) {
        if (!inherits(d$input[[v]], "shiny.tag") && d$input[[v]]$type == "select") {
            req_env[[v]] = d$input[[v]]$values[[as.integer(req_env[[v]])]];
        }
    }

    return (c(req_env, list(input = input)))
}

#' Display dashboard
#' @export
print.tweak.dashboard = function(d)
{
    # -- UI --

    ui = do.call(d$design$ui, c(list(d = d), d$design$args))

    # -- SERVER --

    # Server function: execute all custom code
    server = function(input, output) {
        # Processing for each output
        req_var = list()
        for (op in names(d$output)) {
            # Get input vars used in output code
            req_var[[op]] = intersect(all.vars(d$output[[op]]$code), names(d$input))

            # Render output
            if (d$output[[op]]$type == "plot") {
                output[[op]] = shiny::renderPlot({
                    # Evaluate plot code
                    eval(d$output[[op]]$code,
                        envir = output_env(d, req_var[[op]], input),
                        enclos = d$output[[op]]$env)
                })
            }
        }
    }

    print(shiny::shinyApp(ui, server))
}

