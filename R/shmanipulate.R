# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}


library(shiny)

# Interpret x as control spec
parse_control = function(x)
{
    if ("shiny.tag" %in% class(x)) {
        x
    } else if (is.list(x)) {
        list(type = "select", choices = x, init = NULL)
    } else if (length(x) == 1 & is.logical(x)) {
        list(type = "checkbox", init = x)
    } else if (length(x) == 1 & is.numeric(x)) {
        list(type = "numeric", init = x)
    } else if (length(x) == 1 & is.character(x)) {
        list(type = "text", init = x)
    } else if (length(x) == 1 & is.Date(x)) {
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
            column(4, controls, offset = 4)
        } else if (ncol == 2) {
            list(
                column(4, slice(controls, 0, 1, 2), offset = 2),
                column(4, slice(controls, 1, 2, 2))
            )
        } else if (ncol == 3) {
            list(
                column(4, slice(controls, 0, 1, 3)),
                column(4, slice(controls, 1, 2, 3)),
                column(4, slice(controls, 2, 3, 3))
            )
        } else {
            stop("shmanipulate: ncol must be 1, 2, or 3.");
        }
    } else if (position %in% c("left", "right")) {
        column(4, controls)
    }
}

# Lay out plot and controls
layout = function(controls, ncol, position)
{
    if (position == "top") {
        list(
            fluidRow(gridify(controls, ncol, position)),
            fluidRow(plotOutput("plot"))
        )
    } else if (position == "bottom") {
        list(
            fluidRow(plotOutput("plot")),
            fluidRow(gridify(controls, ncol, position))
        )
    } else if (position == "left") {
        fluidRow(
            gridify(controls, ncol, position),
            column(8, plotOutput("plot"))
        )
    } else if (position == "right") {
        fluidRow(
            column(8, plotOutput("plot")),
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
        selectInput(inputId = name, label = name, choices = x$choices, selected = x$init);
    } else if (x$type == "checkbox") {
        checkboxInput(inputId = name, label = name, value = x$init);
    } else if (x$type == "numeric") {
        numericInput(inputId = name, label = name, value = x$init);
    } else if (x$type == "text") {
        textInput(inputId = name, label = name, value = x$init);
    } else if (x$type == "date") {
        dateInput(inputId = name, label = name, value = x$init);
    } else if (x$type == "slider") {
        sliderInput(inputId = name, label = name, min = x$min, max = x$max, value = x$init, step = ifelse(x$by == 0, NULL, x$by))
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

# Manipulate plot
# TODO: put options into list
# TODO: just have numeric output
shmanipulate = function(expr, ..., options)
{
    # Process options
    options = pad_options(options,
        ncol = 1,
        position = "bottom"
    );

    # Turn ... arguments into input controls
    args = lapply(list(...), parse_control);
    controls = mapply(realize_control, names(args), args, SIMPLIFY = FALSE);
    arg_names = unname(sapply(controls, get_input_id));
    arg_names = arg_names[!sapply(arg_names, is.null)];

    print(arg_names)

    # Define page layout
    ui = fluidPage(layout(controls, options$ncol, options$position));

    # Simple server to render plot based on updates to inputs
    expr_txt = deparse(substitute(expr));
    server = function(input, output, session)
    {
        output$plot = renderPlot({
            args2 = lapply(arg_names, function(name) input[[name]]);
            names(args2) = arg_names;
            eval(parse(text = expr_txt), args2)
        });
    }

    # Run app
    shinyApp(ui, server)
}

# to test
# X check if works with all shiny input types
# X check if works with no ...
# X check if works with ggplot

shmanipulate( { plot(0:100, ifelse(flip, -1, 1) * m*(0:100) + b, col = colour, pch = plot_letter); title(main = plot_title) },
    m = c(0, -2, 2),
    dateInput("b", "Start date", "2020-01-01"),
    actionButton("flip", "Flip"),
    colour = as.list(1:8),
    plot_title = "Title",
    selectInput("plot_letter", "Which letter", letters),
    submitButton("Apply"),
    options = list(position = "right"))

shmanipulate( { d = data.frame(x = 0:100, y = m + sin(b * (0:100))); ggplot(d) + geom_line(aes(x, y)) },
    m = c(0, -2, 2), b = 0, flip = FALSE, colour = as.list(1:8), plot_title = "Title",
    selectInput("plot_letter", "A label", letters),
    options = list(position = "right"))

shmanipulate( plot(0:100, sin((0:100)), col = 4, pch = "s"),
    options = list(position = "right"))


# shmanipulate(function(x, y) { plot(x, y) }, x = c(1, 2), y = c(1, 2, 1))

# # single editable inputs
# x = 1 # short edit
# x = "foo" # short edit
# x = TRUE # checkbox
#
# # drop down list
# x = list(1, 2, 3, 4, 5) # unnamed
# x = list(a = 1, b = 2, c = 3, d = 4, e = 5) # named
#
# # range input
# x = c(1, 5) # min, max
# x = c(1, 5, 1) # min, max, by
# x = c(2.5, 1, 5) # start, min, max
# x = c(2.5, 1, 5, 1) # start, min, max, by
#
# # button
# "Go"

