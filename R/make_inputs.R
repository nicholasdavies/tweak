# Read a list of arguments as shorthand input notation
# Returns list with:
#  $item -- list of lists describing elements
#  $html -- list of parsed html content
make_inputs = function(args, env, allow_shiny)
{
    names = rlang::names2(args)
    if (!allow_shiny && any(names == "")) {
        stop("All inputs must be named.")
    }

    # Turn arguments into input controls
    item = mapply(parse_input, names, args,
        MoreArgs = list(env = env, allow_shiny = allow_shiny), SIMPLIFY = FALSE)
    html = mapply(realize_input, names(item), item, SIMPLIFY = FALSE);

    # Ensure all controls have an input id
    if (allow_shiny) {
        input_ids = unname(vapply(html, get_input_id, character(1)));
        names[names == ""] = input_ids[names == ""]
        if (any(names == "")) {
            stop("tweak: could not find names for all parameters.");
        }
        names(item) = names;
        names(html) = names;
    }

    return (list(item = item, html = html))
}

# Interpret x as a shorthand input specification
# id: the input id to assign (e.g. LHS of id = label ~ ctrl)
# x: the label and control to assign (e.g. RHS of id = label ~ ctrl)
# env: environment in which to interpret label and ctrl
# allow_shiny: whether to allow e.g. textInput as ctrl
parse_input = function(id, x, env, allow_shiny)
{
    # Get label (or assign default) and extract control spec value
    if (is.call(x) && length(x) == 3 && identical(x[[1]], as.name("~"))) {
        # Formula input: id = label ~ ctrl
        label = as_label(eval(x[[2]], env))
        ctrl = eval(x[[3]], env)
    } else if (is.call(x) && length(x) == 2 && identical(x[[1]], as.name("~"))) {
        # One sided formula, id = ~ctrl
        label = label(id)
        ctrl = eval(x[[2]], env)
    } else {
        # Anything else, interpret as value passed in; the label will be the
        # variable name
        label = label(id)
        ctrl = x
    }

    # Interpret control spec as tweak.input
    if (inherits(ctrl, "shiny.tag")) {
        if (allow_shiny) {
            ctrl
        } else {
            stop("Shiny inputs/HTML tags not allowed here.")
        }
    } else if (inherits(ctrl, "tweak.input")) {
        ctrl
    } else if (inherits(ctrl, "tweak.label")) {
        stop("Label specified, but no control.")
    } else if (is.list(ctrl) || (length(ctrl) > 1 && is.character(ctrl)) || is.factor(ctrl)) {
        values = unname(ctrl);
        choices = as.character(seq_along(ctrl));
        names(choices) = if (!is.null(names(ctrl))) names(ctrl) else as.character(ctrl);
        tweak.input(type = "select", label = label, choices = choices, values = values, init = NULL)
    } else if (length(ctrl) == 1 && is.logical(ctrl) && is.na(ctrl)) {
        tweak.input(type = "button", label = label)
    } else if (length(ctrl) == 1 && is.logical(ctrl)) {
        tweak.input(type = "checkbox", label = label, init = ctrl)
    } else if (length(ctrl) == 1 && is.numeric(ctrl)) {
        tweak.input(type = "numeric", label = label, init = ctrl)
    } else if (length(ctrl) == 1 && is.character(ctrl)) {
        tweak.input(type = "text", label = label, init = ctrl)
    } else if (length(ctrl) == 1 && inherits(ctrl, "Date")) {
        tweak.input(type = "date", label = label, init = ctrl)
    } else if (is.numeric(ctrl)) {
        if (length(ctrl) == 2) {
            slider = tweak.input(type = "slider", label = label, min = ctrl[1], max = ctrl[2], init = ctrl[1], by = 0);
        } else if (length(ctrl) == 3 && ctrl[1] >= ctrl[2]) {
            slider = tweak.input(type = "slider", label = label, min = ctrl[2], max = ctrl[3], init = ctrl[1], by = 0);
        } else if (length(ctrl) == 3) {
            slider = tweak.input(type = "slider", label = label, min = ctrl[1], max = ctrl[2], init = ctrl[1], by = ctrl[3]);
        } else if (length(ctrl) == 4) {
            slider = tweak.input(type = "slider", label = label, min = ctrl[2], max = ctrl[3], init = ctrl[1], by = ctrl[4]);
        } else {
            stop("tweak: malformed slider (expecting 2, 3, or 4 numeric values).");
        }

        if (slider$min >= slider$max) { stop("tweak: slider max must be greater than min."); }
        if (slider$init < slider$min || slider$init > slider$max) { stop("tweak: slider start must be between min and max.") }
        if (slider$by < 0) { stop("tweak: slider by must be non-negative."); }

        # Set by to something sensible
        if (slider$by == 0) {
            magnitude = floor(max(log10(abs(c(slider$min, slider$max)))));
            slider$by = 10 ^ (magnitude - 2);
        }

        slider
    } else {
        stop("tweak: cannot interpret control specification ", ctrl);
    }
}

# Turn control spec into realized shiny tag structure
realize_input = function(id, input)
{
    if (inherits(input, "shiny.tag")) {
        input
    } else if (input$type == "select") {
        shiny::selectInput(inputId = id, label = as_tags(input$label), choices = input$choices, selected = input$init)
    } else if (input$type == "button") {
        shiny::actionButton(inputId = id, label = input$label$title, icon = input$label$icon)
    } else if (input$type == "checkbox") {
        shiny::checkboxInput(inputId = id, label = as_tags(input$label), value = input$init)
    } else if (input$type == "numeric") {
        shiny::numericInput(inputId = id, label = as_tags(input$label), value = input$init)
    } else if (input$type == "text") {
        shiny::textInput(inputId = id, label = as_tags(input$label), value = input$init)
    } else if (input$type == "date") {
        shiny::dateInput(inputId = id, label = as_tags(input$label), value = input$init)
    } else if (input$type == "slider") {
        shiny::sliderInput(inputId = id, label = as_tags(input$label),
            min = input$min, max = input$max, value = input$init, step = ifelse(input$by == 0, NULL, input$by))
    } else {
        stop("tweak: unknown control type.");
    }
}
