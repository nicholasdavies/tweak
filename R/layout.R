#' @export
panel = function(label = NULL)
{
    tweak.panel(as_label(label), "normal")
}

#' @export
panel_blank = function()
{
    tweak.panel(as_label(NULL), "blank")
}

#' @export
layout_end = function()
{
    tweak.end()
}

#' Lay out in columns
#' @export
layout_cols = function()
{
    tweak.begin(do_layout_cols, do_panel_cols)
}

do_layout_cols = function(panels, layout_id)
{
    list(
        if (length(panels$blank)) panels$blank,
        if (length(panels$normal)) do.call(bslib::layout_columns, panels$normal)
    )
}

do_panel_cols = function(panel, panel_i, panel_id)
{
    do_panel_auto(panel)
}

#' Lay out in tabs
#' @export
layout_stack = function(input_id = NULL)
{
    tweak.begin(do_layout_stack(input_id), do_panel_stack)
}

do_layout_stack = function(input_id) function(panels, layout_id)
{
    list(
        if (length(panels$blank)) panels$blank,
        if (length(panels$normal)) do.call(bslib::navset_card_underline, c(panels$normal, list(id = input_id)))
    )
}

do_panel_stack = function(panel, panel_i, panel_id)
{
    if (panel$type == "blank") {
        do_panel_auto(panel, panel_i, panel_id)
    } else {
        bslib::nav_panel(
            title = if (empty(panel$label)) panel_i else panel$label$title,
            panel$body,
            icon = panel$label$icon
        )
    }
}

#' Lay out in tabs
#' @export
layout_stack_auto = function()
{
    tweak.begin(do_layout_stack_auto, do_panel_stack_auto)
}

do_layout_stack_auto = function(panels, layout_id)
{
    list(
        if (length(panels$blank)) panels$blank,
        if (length(panels$normal)) do.call(bslib::navset_card_underline, panels$normal)
    )
}

do_panel_stack_auto = function(panel, panel_i, panel_id)
{
    if (panel$type == "blank") {
        do_panel_auto(panel, panel_i, panel_id)
    } else {
        n_items = length(panel$body)
        n_cols = 1
        if (n_items > 1) n_cols = 2
        if (n_items > 6) n_cols = 3
        if (n_items > 9) n_cols = 4

        n_rows = ceiling(n_items / n_cols)

        # TODO make arg
        by_col = TRUE
        if (by_col) {
            elements = list()
            if (n_items > 0) {
                for (r in 1:n_rows) {
                    elements = c(elements, panel$body[seq(r, n_items, n_rows)])
                }
            }
        } else {
            elements = panel$body
        }

        body = do.call(bslib::layout_column_wrap, c(unname(elements), list(width = 1/n_cols)))

        bslib::nav_panel(
            title = if (empty(panel$label)) panel_i else panel$label$title,
            body,
            icon = panel$label$icon
        )
    }
}

#' Lay out in accordion
#' @export
layout_collapse = function()
{
    tweak.begin(do_layout_collapse, do_panel_collapse)
}

do_layout_collapse = function(panels, layout_id)
{
    list(
        if (length(panels$blank)) panels$blank,
        if (length(panels$normal)) do.call(bslib::accordion, panels$normal)
    )
}

do_panel_collapse = function(panel, panel_i, panel_id)
{
    if (panel$type == "blank") {
        do_panel_auto(panel, panel_i, panel_id)
    } else {
        title = if (empty(panel$label)) panel_i else panel$label$title;
        bslib::accordion_panel(
            title = title,
            panel$body,
            icon = panel$label$icon,
            value = as.character(title)
        )
    }
}

#' @export
layout_auto = function()
{
    tweak.begin(do_layout_auto, do_panel_auto)
}

do_layout_auto = function(panels, layout_id)
{
    list(
        if (length(panels$blank)) panels$blank,
        if (length(panels$normal)) panels$normal
    )
}

do_panel_auto = function(panel, panel_i, panel_id)
{
    list(maybe("h1", as_tags(panel$label)), panel$body)
}

#' @export
layout_head = function()
{
    tweak.begin(do_layout_head, do_panel_head)
}

do_layout_head = function(panels, layout_id)
{
    # See 'supported content' in https://getbootstrap.com/docs/5.3/components/navbar/

    shiny::tag("nav", list(
        class = "navbar navbar-expand-md",
        `data-bs-theme` = "light",
        shiny::div(
            class = "container-fluid",
            # Navbar title
            shiny::span(class = "navbar-brand", "Head"),
            # Toggle menu for small screens
            shiny::tags$button(
                class = "navbar-toggler",
                type = "button",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = paste0("#", layout_id),
                `aria-controls` = layout_id,
                `aria-expanded` = "false",
                `aria-label` = "Toggle menu",
                shiny::span(class = "navbar-toggler-icon")
            ),
            # Dropdowns
            shiny::div(
                id = layout_id,
                class = "collapse navbar-collapse",
                shiny::tags$ul(
                    class = "navbar-nav me-auto mb-2 mb-md-0",
                    !!!panels$normal
                ),
                shiny::tags$form(class = "d-flex flex-column flex-md-row gap-2 mb-md-0",
                    !!!panels$blank
                )
            )
        )
    ))
}

do_panel_head = function(panel, panel_i, panel_id)
{
    if (panel$type == "blank") {
        do_panel_auto(panel, panel_i, panel_id)
    } else {
        shiny::tags$li(class = "nav-item dropdown",
            shiny::a(
                class = "nav-link dropdown-toggle",
                href = "#",
                role = "button",
                `data-bs-toggle` = "dropdown",
                `data-bs-auto-close` = "outside", # added
                `aria-expanded` = "false",
                panel$label$title # TODO add icon. And for all similar.
            ),
            shiny::div(
                class = "dropdown-menu p-3 rounded-3 border shadow",
                style = "z-index: 1050", # Put above common bootstrap controls
                panel$body
            )
        )
    }
}

