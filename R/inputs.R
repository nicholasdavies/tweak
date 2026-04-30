#' Create inputs
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Input specs
#' @export
dash_input = function(..., .envir = parent.frame())
{
    # Make input widgets from ...
    ctrl = make_inputs(rlang::list2(...), .envir, allow_shiny = FALSE)

    tweak.do({
        # Add inputs
        plan$input = c(plan$input, ctrl$item)

        # Add controls to dashboard
        tweak.do.return(plan, ctrl$html)
    }, ctrl = ctrl)
}

#' Button to update an output
#' @export
dash_update = function(target_id, label = "Update")
{
    tweak.do({
        # Generate id for this update button
        button_id = paste0("tweak_update_", target_id)

        # Add to list of updaters
        plan$update = c(plan$update, structure(target_id, names = button_id))

        # Add button to dashboard
        tweak.do.return(plan, list(bslib::input_task_button(
            id = button_id, label = label$title, icon = label$icon)))

    }, target_id = target_id, label = as_label(label))
}

#' Navbar with no contents
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Navbar labels
#' @export
dash_bar = function(..., label = NULL)
{
    args = rlang::list2(...)

    tweak.do({
        # Get variable name and initially selected item
        anames = rlang::names2(args)
        if (sum(anames != "") != 1) {
            stop("Must provide a variable name to dash_bar.")
        }
        id = anames[anames != ""]

        # Create item and html
        item = structure(list(tweak.input(type = "bar", label = label)), names = id)
        tabs = lapply(unname(args), function(a) {
            label = as_label(a);
            bslib::nav_panel(label$title, icon = label$icon)
        })
        html = list(do.call(bslib::navset_underline, c(tabs, list(id = id))))

        # Add label
        if (!empty(label)) {
            idx = find_tag(html, exactly("ul"), class = contains("nav"))
            html[[idx]]$children = c(
                list(maybe("span", class = "navbar-text", as_tags(label))),
                html[[idx]]$children
            )
        }

        # Add input
        plan$input = c(plan$input, item)

        # Add control to dashboard
        tweak.do.return(plan, html)
    }, args = args, label = as_label(label))
}
