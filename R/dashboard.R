# Bit class and derived classes

setClass("tweak.bit")
setClass("tweak.do",     contains = "tweak.bit", slots = c(x = "list"))
setClass("tweak.list",   contains = "tweak.bit", slots = c(x = "list"))
setClass("tweak.layout", contains = "tweak.bit", slots = c(x = "list"))
setClass("tweak.panel",  contains = "tweak.bit", slots = c(x = "list"))
setOldClass("shiny.tag")

tweak.do = function(body, ...)
{
    new("tweak.do",
        x = list(body = substitute(body), args = list(...))
    )
}

tweak.group = function(...)
{
    new("tweak.group", x = list(...))
}

tweak.layout = function(do_layout, do_panel)
{
    new("tweak.layout", x = list(do_layout = do_layout, do_panel = do_panel))
}

tweak.panel = function(title, icon)
{
    new("tweak.panel", x = list(title = title, icon = icon))
}

add_error = function(cl, b)
{
    stop("Cannot add object of class '",
        paste(class(b), collapse = "/"),
        "' to a ", cl, ".",
        call. = FALSE
    )
}

# tag + tag, tag + bit (use manual dispatch for S3)
.S3method("+", "shiny.tag",
    function(e1, e2) {
        cat(0);
        if (inherits(e2, "tweak.bit") || inherits(e2, "shiny.tag")) {
            return (tweak.group(e1, e2))
        }
        add_error("shiny.tag", e2)
    }
)

# group + group (add unwrapped), group + other bit, group + tag (add to existing group)
setMethod("+", c("tweak.group", "tweak.group"), function(e1, e2) do.call(tweak.group, c(e1@x, list(e2@x))) )
setMethod("+", c("tweak.group", "tweak.bit"),   function(e1, e2) do.call(tweak.group, c(e1@x, list(e2))) )
setMethod("+", c("tweak.group", "shiny.tag"),   function(e1, e2) do.call(tweak.group, c(e1@x, list(e2))) )
setMethod("+", c("tweak.group", "ANY"),         function(e1, e2) add_error("tweak.group", e2) )

# bit + bit, bit + tag (create new group)
setMethod("+", c("tweak.bit", "tweak.group"), function(e1, e2) tweak.group(e1, e2@x) )
setMethod("+", c("tweak.bit", "tweak.bit"),   function(e1, e2) tweak.group(e1, e2) )
setMethod("+", c("tweak.bit", "shiny.tag"),   function(e1, e2) tweak.group(e1, e2) )
setMethod("+", c("tweak.bit", "ANY"),         function(e1, e2) add_error("tweak.bit", e2) )


# Dashboard class
setClass("tweak.dashboard",
    slots = c(
        html = "list",
        curr = "numeric",
        calc = "list",
        input = "list",
        output = "list",
        design = "list"
    )
)

#' Create empty dashboard
#' @export
dashboard = function()
{
    new("tweak.dashboard",
        html = list(
            head = list(),
            main = list(),
            conf = list(),
            foot = list()
        ),
        curr = .main.idx,
        calc = list(),
        input = list(),
        output = list(),
        design = list(
            ui = ui_sidebar,
            args = list()
        )
    )
}

setMethod("+", c("tweak.dashboard", "shiny.tag"),
    function(e1, e2) {
        # Add HTML to dashboard
        e1@html[[e1@curr]] = c(e1@html[[e1@curr]], list(e2))
        return (e1)
    }
)

setMethod("+", "tweak.dashboard",
    function(e1, e2) {
        # Recursive function to process the element to be added
        # Run and remove any doers, unclass tweak.bit groups
        consolidate = function(l, env) {
            if (inherits(l, "shiny.tag")) {
                # Bare html: return html to add
                return (list(l))
            } else if (inherits(l, "tweak.do")) {
                # Do command: execute on dashboard and return html to add
                exec = eval(l@x$body, envir = c(list(d = env$d), l@x$args))
                env$d = exec$d
                return (exec$html)
            } else if (inherits(l, "tweak.group")) {
                # Group: treat as list
                l = l@x
            } else if (inherits(l, "tweak.bit")) {
                # Any other tweak bit: return to add unprocessed; will be
                # processed when UI is generated in print.tweak.dashboard()
                return (list(l))
            }

            # If we now have a list, l was either a list to begin with or a
            # tweak.group containing a list of elements. Recursively process
            # each element, then return the processed list to add.
            if (is.list(l)) {
                nl = list()
                for (i in seq_along(l)) {
                    nl = c(nl, consolidate(l[[i]], env))
                }
                return (list(nl))
            }

            # Any other type is not allowed.
            add_error("tweak.dashboard", e2)
        }

        # Pass the dashboard in within an environment to allow modification
        env = rlang::new_environment(list(d = e1))
        e2 = consolidate(e2, env)

        # Reflect changes to dashboard, add html
        e1 = env$d
        e1@html[[e1@curr]] = c(e1@html[[e1@curr]], e2)

        return (e1)
    }
)

# Show dashboard
# setMethod("show", "tweak.dashboard", function(object) print.tweak.dashboard(object))

exec_dashboard = function(d, html = NULL)
{
    list(d = d, html = html)
}

#' Get environment (list) of input/calc variables for a given output/calc
eval_env = function(d, input, req_input, calc, req_calc)
{
    # Index used input variables into input
    req_env = c(
        lapply(req_input, function(name) input[[name]]),
        lapply(req_calc, function(name) calc[[name]]())
    )
    names(req_env) = c(req_input, req_calc)

    # Convert dropdown list values from character to originally specified type
    for (v in req_input) {
        if (!inherits(d@input[[v]], "shiny.tag") && d@input[[v]]$type == "select") {
            req_env[[v]] = d@input[[v]]$values[[as.integer(req_env[[v]])]];
        }
    }

    return (c(req_env, list(input = input)))
}

make_ui = function(d)
{
    do.call(d@design$ui, c(list(d = d), d@design$args))
}

#' Print tweak dashboard
print.tweak.dashboard = function(d)
{
    # -- UI --

    ui = make_ui(d)

    # -- SERVER --

    var_names = c(names(d@calc), names(d@input))

    # Server function: execute all custom code
    server = function(input, output) {
        req_calc = list()
        req_input = list()
        calc = list()

        # Processing for each calc
        # TODO combine calc and output
        # TODO make sure req() statements are present
        for (ca in names(d@calc)) {
            # Get vars used in calc code
            req_calc[[ca]] = intersect(setdiff(all.vars(d@calc[[ca]]$code), ca), names(d@calc))
            req_input[[ca]] = intersect(all.vars(d@calc[[ca]]$code), names(d@input))

            # Do calculations
            if (d@calc[[ca]]$type == "calc") {
                # Evaluate calculation
                calc[[ca]] = shiny::reactive({
                    eval(d@calc[[ca]]$code,
                        envir = eval_env(d, input, req_input[[ca]], calc, req_calc[[ca]]),
                        enclos = d@calc[[ca]]$env)
                })
            }
        }

        # Processing for each output
        for (op in names(d@output)) {
            # Get vars used in output code
            req_calc[[op]] = intersect(all.vars(d@output[[op]]$code), names(d@calc))
            req_input[[op]] = intersect(all.vars(d@output[[op]]$code), names(d@input))

            # Render output
            if (d@output[[op]]$type == "plot") {
                output[[op]] = shiny::renderPlot({
                    # Evaluate plot code
                    eval(d@output[[op]]$code,
                        envir = eval_env(d, input, req_input[[op]], calc, req_calc[[op]]),
                        enclos = d@output[[op]]$env)
                })
            }
        }
    }

    print(shiny::shinyApp(ui, server))
}
