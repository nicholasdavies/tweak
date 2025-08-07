#' Set design to sidebar
#' @export
design_sidebar = function()
{
    tweak.do({
        d@design = list(ui = ui_sidebar, args = list())
        exec_dashboard(d)
    })
}

#' Sidebar ui
ui_sidebar = function(d)
{
    # Based on bslib::page_sidebar.
    bslib::page_fluid(
        class = "bslib-page-sidebar",
        make_layout(d@html$head),
        bslib::layout_sidebar(
            sidebar = make_layout(d@html$conf),
            make_layout(d@html$main),
            fillable = FALSE
        ),
        make_layout(d@html$foot)
    )
}

#' Set design to console
#' @export
design_console = function()
{
    tweak.do({
        d@design = list(ui = ui_console, args = list())
        exec_dashboard(d)
    })
}

#' Console ui
ui_console = function(d)
{
    bslib::page_fixed(
        bslib::card(
            if (length(d@html$head)) bslib::card_header(make_layout(d@html$head)),
            make_layout(d@html$main),
            if (length(d@html$conf)) bslib::card_footer(make_layout(d@html$conf)),
            style = "margin-top: 15px"
        ),
        if (length(d@html$foot)) bslib::card_footer(make_layout(d@html$foot))
    )
}
