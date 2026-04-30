#' Manipulate a plot
#'
#' Easily manipulate a plot using controls like sliders, drop-down lists and
#' date pickers.
#'
#' @param .expr an expression that evaluates to a plot using base plotting
#' functions, \code{ggplot}, etc.
#' @param ... variables within the \code{.expr} expression to be manipulated.
#'   These can be specified in one of two ways:
#'   \describe{
#'     \item{\strong{The easy way}}{The easy way is to specify the variables
#'     to be manipulated as named arguments to \code{tweak}. How you
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
#'         \item{\code{a = NA} for an integer that will count up by one each
#'         time a button is pressed. Note that this must be logical \code{NA},
#'         e.g. \code{NA_character_}, \code{NA_real_}, etc. will not make a
#'         button.}
#'         \item{\code{foo = "Some text"} for a character
#'         string controlled by text input.}
#'         \item{\code{bar = 123.456} for a numeric value
#'         controlled by text input.}
#'         \item{\code{baz = as.Date("2020-01-01")} for a
#'         \code{Date} object with a calendar input.}
#'       }
#'     To give a custom label to the control, put the label on the left side of
#'     a `~` formula, e.g.
#'     \code{b = "Slope" ~ c(-10, 10)}
#'     See below for examples.
#'     }
#'     \item{\strong{The more flexible way}}{The more flexible way
#'     is to specify the variables to be manipulated as input controls
#'     using the \code{shiny} package. In this case the names of the
#'     arguments are ignored (to avoid confusion, keep arguments unnamed
#'     that are Shiny input controls), and the variable names are taken from
#'     the \code{inputId} argument to the Shiny input control. An
#'     example is below. }
#'   }
#' @param options a \code{list} containing further settings:
#' \describe{
#'   \item{\code{position}}{where the controls are positioned relative to the
#'   plot; either \code{"bottom"} (default), \code{"top"}, \code{"left"}, or
#'   \code{"right"}.}
#'   \item{\code{ncol}}{if \code{position} is \code{"top"} or \code{"bottom"},
#'   the number of columns to distribute controls across; can be \code{NA}
#'   (automatic, default), \code{1}, \code{2}, \code{3}, or \code{4}.}
#'   \item{\code{gadget}}{\code{FALSE} (default) to run in a new window, or
#'   \code{TRUE} to run as a gadget, i.e. in the RStudio viewer pane.}
#'   \item{\code{plot_height}}{Height of the plot in pixels, with \code{400} as
#'   the default.}
#' }
#' @param .envir environment in which to evaluate \code{.expr}.
#'
#' @examples
#' if (interactive()) {
#' # Specifying controls: the easy way
#' tweak({
#'         x = 0:10;
#'         plot(A * x^2 + B * x + C,
#'             col = if (blue) "blue" else "black",
#'             main = plot_title,
#'             ylim = c(-5, 10)
#'         )
#'     },
#'     A = c(0, 0.1), # slider from 0 to 0.1
#'     B = 1,         # numeric text input with starting value 1
#'     C = list(one = 1, two = 2, three = 3), # dropdown list with named values
#'     plot_title = "Example title", # freeform text input
#'     blue = FALSE                  # checkbox
#' )
#'
#' # Specifying controls: the flexible way
#' library(shiny)
#' library(ggplot2)
#'
#' tweak({
#'         dat = data.frame(
#'             date = start_date + 0:(n_days - 1),
#'             value = start_value * exp(0:(n_days - 1) * growth_rate) +
#'                 rnorm(n_days, 0, noise)
#'         )
#'         ggplot(dat) +
#'             geom_line(aes(x = date, y = value))
#'     },
#'     dateInput(inputId = "start_date",
#'         label = "Start date", value = "2020-01-01"),
#'     numericInput(inputId = "start_value",
#'         label = "Starting value", value = 1, min = 0, max = 10, step = 1),
#'     sliderInput(inputId = "growth_rate",
#'         label = "Growth rate", min = 0, max = 1, value = 0, step = 0.01),
#'     numericInput(inputId = "n_days",
#'         label = "Number of days", value = 30, min = 1, max = 60, step = 1),
#'     sliderInput(inputId = "noise",
#'         label = "Noise", min = 0, max = 1, value = 0, step = 0.01)
#' )
#'
#' # Different kinds of numeric sliders
#' tweak({ x = 0:100; plot(A * x^2 + B * x + C, ylim = c(-2000, 2000)) },
#'     A = c(0.5, 0, 1),         # slider from 0 to 1, with starting value 0.5
#'     B = c(0, 10, 0.25),       # slider from 0 to 10, with step 0.25
#'     C = c(0, -1000, 1000, 50) # slider from -1000 to 1000, starting value 0 and step 50
#' )
#'
#' # tweak plus curve
#' tweak(curve(dbeta(x, alpha, beta), 0, 1), alpha = c(1, 100), beta = c(1, 100))
#'
#' # Quickly explore a numeric data.frame
#' data(quakes)
#' tweak(
#'     if (x == y) {
#'         hist(quakes[[x]], xlab = x)
#'     } else {
#'         plot(quakes[[x]], quakes[[y]], xlab = x, ylab = y)
#'     },
#'     x = "Variable 1" ~ names(quakes),
#'     y = "Variable 2" ~ names(quakes))
#' }
#' @export
tweak = function(.expr, ..., options = list(), .envir = parent.frame())
{
    # Process options
    options = pad_options(options,
        ncol = NA,
        position = "bottom",
        gadget = FALSE,
        plot_height = "400px"
    );

    # Process inputs
    ctrl = make_inputs(list(...), .envir, allow_shiny = TRUE)

    # Define page layout
    ui = shiny::fluidPage(tweak_layout(ctrl$html, options$ncol, options$position, options$plot_height));

    # Simple server to render plot based on updates to inputs
    expr_q = substitute(.expr);
    server = function(input, output, session)
    {
        output$plot = shiny::renderPlot({
            # get server values
            args2 = lapply(names(ctrl$item), function(name) input[[name]]);
            names(args2) = names(ctrl$item);

            # convert dropdown list values from character to originally specified type
            for (i in seq_along(ctrl$item)) {
                if (!inherits(ctrl$item[[i]], "shiny.tag") && ctrl$item[[i]]$type == "select") {
                    args2[[i]] = ctrl$item[[i]]$values[[as.integer(args2[[i]])]];
                }
            }

            # evaluate plotting command
            eval(expr_q, args2, .envir)
        });
    }

    # Run app
    if (options$gadget) {
        shiny::runGadget(ui, server)
    } else {
        shiny::shinyApp(ui, server)
    }
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
        } else if (ncol == 4) {
            list(
                shiny::column(3, slice(controls, 0, 1, 4)),
                shiny::column(3, slice(controls, 1, 2, 4)),
                shiny::column(3, slice(controls, 2, 3, 4)),
                shiny::column(3, slice(controls, 3, 4, 4))
            )
        } else {
            stop("tweak: ncol must be 1, 2, 3, or 4.");
        }
    } else if (position %in% c("left", "right")) {
        shiny::column(4, controls)
    }
}

# Lay out plot and controls
tweak_layout = function(controls, ncol, position, plot_height)
{
    if (is.na(ncol)) {
        ncol = max(1, min(4, ceiling(length(controls) / 4)))
    }

    if (position == "top") {
        list(
            shiny::fluidRow(gridify(controls, ncol, position)),
            shiny::fluidRow(shiny::plotOutput("plot", height = plot_height))
        )
    } else if (position == "bottom") {
        list(
            shiny::fluidRow(shiny::plotOutput("plot", height = plot_height)),
            shiny::fluidRow(gridify(controls, ncol, position))
        )
    } else if (position == "left") {
        shiny::fluidRow(
            gridify(controls, ncol, position),
            shiny::column(8, shiny::plotOutput("plot", height = plot_height))
        )
    } else if (position == "right") {
        shiny::fluidRow(
            shiny::column(8, shiny::plotOutput("plot", height = plot_height)),
            gridify(controls, ncol, position)
        )
    } else {
        stop('tweak: position (of controls) must be "top", "bottom", "left", or "right".')
    }
}
