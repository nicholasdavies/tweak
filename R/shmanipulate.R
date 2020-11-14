# Interpret x as control spec
parse_control = function(x)
{
    if ("shiny.tag" %in% class(x)) {
        x
    } else if (is.list(x) | (length(x) > 1 & is.character(x))) {
        values = unname(x);
        choices = as.character(seq_along(x));
        names(choices) = if (!is.null(names(x))) names(x) else as.character(x);
        list(type = "select", choices = choices, values = values, init = NULL)
    } else if (length(x) == 1 & is.logical(x)) {
        list(type = "checkbox", init = x)
    } else if (length(x) == 1 & is.numeric(x)) {
        list(type = "numeric", init = x)
    } else if (length(x) == 1 & is.character(x)) {
        list(type = "text", init = x)
    } else if (length(x) == 1 & class(x) == "Date") {
        list(type = "date", init = x)
    } else if (is.numeric(x)) {
        parse_slider(x)
    } else {
        stop("shmanipulate: cannot interpret control specification ", x);
    }
}

# Parse numeric vector of 2, 3, or 4 elements into a list with named elements min, max, init, and by.
parse_slider = function(x)
{
    if (length(x) == 2) {
        slider = list(type = "slider", min = x[1], max = x[2], init = x[1], by = 0);
    } else if (length(x) == 3 && x[1] >= x[2]) {
        slider = list(type = "slider", min = x[2], max = x[3], init = x[1], by = 0);
    } else if (length(x) == 3) {
        slider = list(type = "slider", min = x[1], max = x[2], init = x[1], by = x[3]);
    } else if (length(x) == 4) {
        slider = list(type = "slider", min = x[2], max = x[3], init = x[1], by = x[4]);
    } else {
        stop("shmanipulate: malformed slider (expecting 2, 3, or 4 numeric values).");
    }

    if (slider$min >= slider$max) { stop("shmanipulate: slider max must be greater than min."); }
    if (slider$init < slider$min || slider$init > slider$max) { stop("shmanipulate: slider start must be between min and max.") }
    if (slider$by < 0) { stop("shmanipulate: slider by must be non-negative."); }

    # Set by to something sensible
    if (slider$by == 0) {
        magnitude = floor(max(log10(abs(c(slider$min, slider$max)))));
        slider$by = 10 ^ (magnitude - 2);
    }

    return (slider)
}

# Search a shiny.tag class object for an inputId
get_input_id = function(x)
{
    if (!("shiny.tag" %in% class(x))) {
        return (NULL)
    }

    if (x$name %in% c('input', 'select', 'button', 'div') && !is.null(x$attribs$id)) {
        return (x$attribs$id)
    }

    for (xx in x$children) {
        result = get_input_id(xx);
        if (!is.null(result)) {
            return (result)
        }
    }

    return (NULL)
}

# Slice list in quantiles [a/d, b/d)
slice = function(l, a, b, d)
{
    n = length(l);
    i = (n * a) / d + 1;
    j = (n * b) / d + 1;
    ind = seq_along(l);
    return (l[ind >= i & ind < j])
}

# Put controls in a grid
gridify = function(controls, ncol, position)
{
    if (position %in% c("top", "bottom")) {
        if (ncol == 1) {
            shiny::column(4, controls, offset = 4)
        } else if (ncol == 2) {
            list(
                shiny::column(4, slice(controls, 0, 1, 2), offset = 2),
                shiny::column(4, slice(controls, 1, 2, 2))
            )
        } else if (ncol == 3) {
            list(
                shiny::column(4, slice(controls, 0, 1, 3)),
                shiny::column(4, slice(controls, 1, 2, 3)),
                shiny::column(4, slice(controls, 2, 3, 3))
            )
        } else {
            stop("shmanipulate: ncol must be 1, 2, or 3.");
        }
    } else if (position %in% c("left", "right")) {
        shiny::column(4, controls)
    }
}

# Lay out plot and controls
layout = function(controls, ncol, position)
{
    if (position == "top") {
        list(
            shiny::fluidRow(gridify(controls, ncol, position)),
            shiny::fluidRow(shiny::plotOutput("plot"))
        )
    } else if (position == "bottom") {
        list(
            shiny::fluidRow(shiny::plotOutput("plot")),
            shiny::fluidRow(gridify(controls, ncol, position))
        )
    } else if (position == "left") {
        shiny::fluidRow(
            gridify(controls, ncol, position),
            shiny::column(8, shiny::plotOutput("plot"))
        )
    } else if (position == "right") {
        shiny::fluidRow(
            shiny::column(8, shiny::plotOutput("plot")),
            gridify(controls, ncol, position)
        )
    } else {
        stop('shmanipulate: position (of controls) must be "top", "bottom", "left", or "right".')
    }
}

# Turn control spec into realized shiny tag structure
realize_control = function(name, x)
{
    if ("shiny.tag" %in% class(x)) {
        x
    } else if (x$type == "select") {
        shiny::selectInput(inputId = name, label = name, choices = x$choices, selected = x$init);
    } else if (x$type == "checkbox") {
        shiny::checkboxInput(inputId = name, label = name, value = x$init);
    } else if (x$type == "numeric") {
        shiny::numericInput(inputId = name, label = name, value = x$init);
    } else if (x$type == "text") {
        shiny::textInput(inputId = name, label = name, value = x$init);
    } else if (x$type == "date") {
        shiny::dateInput(inputId = name, label = name, value = x$init);
    } else if (x$type == "slider") {
        shiny::sliderInput(inputId = name, label = name, min = x$min, max = x$max, value = x$init, step = ifelse(x$by == 0, NULL, x$by))
    } else {
        stop("shmanipulate: unknown control type.");
    }
}

# Pad out options list with default values
pad_options = function(options, ...)
{
    defaults = list(...);
    for (nm in names(defaults)) {
        if (is.null(options[[nm]])) {
            options[[nm]] = defaults[[nm]];
        }
    }
    return (options)
}

#' Manipulate a plot
#'
#' Easily manipulate a plot using controls like sliders, drop-down lists and
#' date pickers.
#'
#' \code{shmanip} and \code{shmanipulate} do the same thing, but \code{shmanip}
#' runs as a gadget (i.e. in the RStudio Viewer pane) by default and with
#' controls on the left of the plot, while \code{shmanipulate} runs in a new
#' window by default and with controls on the bottom.
#'
#' @param expr an expression that evaluates to a plot using base plotting
#' functions, \code{ggplot}, etc.
#' @param ... variables within the \code{expr} expression to be manipulated.
#'   These can be specified in one of two ways:
#'   \describe{
#'     \item{\strong{The easy way}}{The easy way is to specify the variables
#'     to be manipulated as named arguments to \code{shmanipulate}. How you
#'     specify the value of each argument determines the default value of the
#'     variable and how it is manipulated. Examples for each:
#'       \itemize{
#'         \item{\code{x = c(min, max)} for a numeric slider between
#'         \code{min} and \code{max}; you can optionally provide a
#'         starting value before \code{min} and/or a step value after
#'         \code{max} (see examples).}
#'         \item{\code{y = list(...)} for a fixed set of options
#'         in a dropdown menu. If the \code{list} has names, these will
#'         be shown. The first element is selected by default. For
#'         convenience, a character vector with more than one element
#'         will also be interpreted as a dropdown menu.}
#'         \item{\code{z = TRUE} or \code{z = FALSE} for a
#'         logical value controlled by a checkbox.}
#'         \item{\code{foo = "Some text"} for a character
#'         string controlled by text input.}
#'         \item{\code{bar = 123.456} for a numeric value
#'         controlled by text input.}
#'         \item{\code{baz = as.Date("2020-01-01")} for a
#'         \code{Date} object with a calendar input.}
#'       }
#'     See below for an example.
#'     }
#'     \item{\strong{The more flexible way}}{The more flexible way
#'     is to specify the variables to be manipulated as input controls
#'     using the \code{shiny} package. In this case the names of the
#'     arguments are ignored, and the variable names are taken from
#'     the \code{inputId} argument to the Shiny input control. An
#'     example is below. }
#'   }
#' @param options a \code{list} containing further settings:
#' \describe{
#'   \item{\code{position}}{where the controls are positioned relative to the
#'   plot; either \code{"bottom"} (default for \code{shmanipulate}), \code{"top"},
#'   \code{"left"} (default for \code{shmanip}), or \code{"right"}.}
#'   \item{\code{ncol}}{if \code{position} is \code{"top"} or \code{"bottom"},
#'   the number of columns to distribute controls across; can be \code{1} (default),
#'   \code{2}, or \code{3}.}
#'   \item{\code{gadget}}{\code{FALSE} (default for \code{shmanipulate}) to run in a
#'   new window, or \code{TRUE} (default for \code{shmanip}) to run as a gadget, i.e.
#'   in the RStudio viewer pane.}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Specifying controls: the easy way
#' shmanipulate( { x = 0:10; plot(A * x^2 + B * x + C, col = if(blue) 4 else 1, main = plot_title, ylim = c(-5, 10)) },
#'     A = c(0, 0.1), # a slider from 0 to 0.1
#'     B = 1,       # a numeric text input with starting value 1
#'     C = list(one = 1, two = 2, three = 3), # a dropdown list with named values
#'     plot_title = "Example title", # freeform text input
#'     blue = FALSE                  # checkbox
#' )
#'
#' # Specifying controls: the flexible way
#' library(shiny)
#' library(ggplot2)
#'
#' shmanipulate({
#'         dat = data.frame(date = start_date + 0:(n_days - 1),
#'             value = start_value * exp(0:(n_days - 1) * growth_rate) + rnorm(n_days, 0, noise));
#'         ggplot(dat) +
#'             geom_line(aes(x = date, y = value))
#'     },
#'     dateInput(inputId = "start_date", label = "Start date", value = "2020-01-01"),
#'     numericInput(inputId = "start_value", label = "Starting value", value = 1, min = 0, max = 10, step = 1),
#'     sliderInput(inputId = "growth_rate", label = "Growth rate", min = 0, max = 1, value = 0, step = 0.01),
#'     numericInput(inputId = "n_days", label = "Number of days", value = 30, min = 1, max = 60, step = 1),
#'     sliderInput(inputId = "noise", label = "Noise", min = 0, max = 1, value = 0, step = 0.01)
#' )
#'
#' # Different kinds of numeric sliders
#' shmanipulate({ x = 0:100; plot(A * x^2 + B * x + C, ylim = c(-2000, 2000)) },
#'     A = c(0.5, 0, 1),         # slider from 0 to 1, with starting value 0.5
#'     B = c(0, 10, 0.25),       # slider from 0 to 10, with step 0.25
#'     C = c(0, -1000, 1000, 50) # slider from -1000 to 1000, with starting value 0 and step 50
#' )
#'
#' # shmanipulate plus curve
#' shmanipulate(curve(dbeta(x, alpha, beta), 0, 1), alpha = c(1, 100), beta = c(1, 100))
#'
#' # Quickly explore a numeric data.frame
#' data(quakes)
#' shmanipulate(if (x == y) hist(quakes[[x]], xlab = x) else plot(quakes[[x]], quakes[[y]], xlab = x, ylab = y),
#'     x = names(quakes), y = names(quakes))
#' }
#' @rdname shmanipulate
shmanipulate = function(expr, ..., options = list(), .envir = parent.frame())
{
    # Process options
    options = pad_options(options,
        ncol = 1,
        position = "bottom",
        gadget = FALSE
    );

    # Read named and unnamed (list) arguments
    ellipses = list(...);
    args = list();
    for (i in seq_along(ellipses)) {
        if (!is.null(names(ellipses)) && names(ellipses)[i] != "") {
            args = c(args, list(ellipses[[i]]));
            names(args)[i] = names(ellipses)[i];
        } else {
            args = c(args, list(ellipses[[i]]));
            names(args)[i] = ""
        }
    }

    # Turn ... arguments into input controls
    args = lapply(args, parse_control);
    controls = mapply(realize_control, names(args), args, SIMPLIFY = FALSE);
    arg_names = unname(sapply(controls, get_input_id));

    if (any(is.null(arg_names))) {
        stop("shmanipulate: could not find names for all parameters.");
    }

    # Define page layout
    ui = shiny::fluidPage(layout(controls, options$ncol, options$position));

    # Simple server to render plot based on updates to inputs
    expr_txt = deparse(substitute(expr));
    server = function(input, output, session)
    {
        output$plot = shiny::renderPlot({
            # get server values
            args2 = lapply(arg_names, function(name) input[[name]]);
            names(args2) = arg_names;

            # convert dropdown list values from character to originally specified type
            for (i in seq_along(args)) {
                if (!"shiny.tag" %in% class(args[[i]]) && args[[i]]$type == "select") {
                    args2[[i]] = args[[i]]$values[[as.integer(args2[[i]])]];
                }
            }

            # evaluate plotting command
            eval(parse(text = expr_txt), args2, .envir)
        });
    }

    # Run app
    if (options$gadget) {
        shiny::runGadget(ui, server)
    } else {
        shiny::shinyApp(ui, server)
    }
}

#' @rdname shmanipulate
#' @export
shmanip = function(expr, ..., options = list(), .envir = parent.frame())
{
    # Process options
    options = pad_options(options,
        ncol = 1,
        position = "left",
        gadget = TRUE
    );

    shmanipulate(expr, ..., options, .envir)
}
