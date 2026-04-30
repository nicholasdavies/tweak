dashboard_frontmatter = function()
{
    # shiny::tags$head()
}

default_design = function()
{
    list(
        ui = ui_sidebar,
        args = list(),
        head = list(layout = layout_head,     args = list()),
        main = list(layout = layout_stack,    args = list()),
        conf = list(layout = layout_collapse, args = list()),
        foot = list(layout = layout_auto,     args = list())
    )
}

#' Set design to sidebar
#' @export
design_sidebar = function(theme = NULL, ...)
{
    tweak.opt({
        args$preset = theme
        plan$design = list(
            ui = ui_sidebar,
            args = args,
            head = list(layout = layout_head,     args = list()),
            main = list(layout = layout_stack,    args = list()),
            conf = list(layout = layout_collapse, args = list()),
            foot = list(layout = layout_auto,     args = list())
        )
        return (plan)
    }, theme = theme, args = list(...))
}

#' Sidebar ui
ui_sidebar = function(plan, bs_theme_args)
{
    # Based on bslib::page_sidebar.
    bslib::page_fluid(
        dashboard_frontmatter(),
        theme = do.call(bslib::bs_theme, bs_theme_args),
        class = "bslib-page-sidebar",
        maybe("h1", as_tags(plan$info$head$label)),
        plan$html$head,
        bslib::layout_sidebar(
            sidebar = bslib::sidebar(
                title = maybe("header", class = "sidebar-title", as_tags(plan$info$conf$label)),
                plan$html$conf
            ),
            maybe("h2", as_tags(plan$info$main$label)),
            plan$html$main,
            fillable = FALSE
        ),
        maybe("h2", as_tags(plan$info$foot$label)),
        plan$html$foot
    )
}

#' Set design to console
#' @export
design_console = function(theme = NULL, ...)
{
    tweak.opt({
        args$preset = theme
        plan$design = list(
            ui = ui_console,
            args = args,
            head = list(layout = layout_head,       args = list()),
            main = list(layout = layout_stack,      args = list()),
            conf = list(layout = layout_stack_auto, args = list()),
            foot = list(layout = layout_auto,       args = list())
        )
        return (plan)
    }, theme = theme, args = list(...))
}

#' Console ui
ui_console = function(plan, bs_theme_args)
{
    bslib::page_fixed(
        dashboard_frontmatter(),
        theme = do.call(bslib::bs_theme, bs_theme_args),
        maybe("h1", as_tags(plan$info$head$label)),
        bslib::card(
            if (length(plan$html$head))
                bslib::card_header(plan$html$head),
            maybe("h2", as_tags(plan$info$main$label)),
            plan$html$main,
            if (length(plan$html$conf) || !empty(plan$info$head$label))
                bslib::card_footer(maybe("h2", as_tags(plan$info$conf$label)), plan$html$conf),
            style = "margin-top: 15px"
        ),
        maybe("h2", as_tags(plan$info$foot$label)),
        if (length(plan$html$foot)) plan$html$foot
    )
}
