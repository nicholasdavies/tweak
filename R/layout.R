#' @export
panel = function(title = NULL, icon = NULL)
{
    tweak.panel(title, icon)
}

#' Lay out in columns
#' @export
layout_cols = function()
{
    tweak.layout(do_layout_cols, do_panel_cols)
}

do_layout_cols = function(panels)
{
    do.call(bslib::layout_columns, panels)
}

do_panel_cols = function(panel)
{
    do_panel_auto(panel)
}

#' Lay out in tabs
#' @export
layout_stack = function()
{
    tweak.layout(do_layout_stack, do_panel_stack)
}

do_layout_stack = function(panels)
{
    do.call(bslib::navset_card_underline, panels)
}

do_panel_stack = function(panel)
{
    bslib::nav_panel(title = panel$panel$title,
        panel$content,
        icon = if (!is.null(panel$panel$icon)) shiny::icon(panel$panel$icon))
}


#' @export
layout_auto = function()
{
    tweak.layout(do_layout_auto, do_panel_auto)
}

do_layout_auto = function(panels)
{
    panels
}

do_panel_auto = function(panel)
{
    if (!is.null(panel$panel$title) || !is.null(panel$panel$icon)) {
        c(
            list(shiny::h1(
                if (!is.null(panel$panel$icon)) shiny::icon(panel$panel$icon),
                if (!is.null(panel$panel$title)) panel$panel$title
            )),
            panel$content
        )
    } else {
        panel$content
    }
}

make_layout = function(bits)
{
    make_layout0(bits)$content
}

make_layout0 = function(bits)
{
    if (!length(bits)) {
        return (NULL)
    }

    next_layout = function(bits, i) {
        for (j in i:length(bits)) {
            if (inherits(bits[[j]], "tweak.layout")) {
                return (j)
            }
        }
        return (j + 1)
    }

    i = 0
    content = list()
    do_layout = do_layout_auto
    do_panel = do_panel_auto
    panel = NULL

    while (TRUE) {
        j = next_layout(bits, i + 1)
        indices = rlang::seq2(i + 1, j - 1)
        i = j
        if (length(indices)) {
            tags = list()
            for (k in indices) {
                if (inherits(bits[[k]], "shiny.tag")) {
                    tags = c(tags, bits[k])
                } else if (inherits(bits[[k]], "tweak.panel")) {
                    panel = bits[[k]]@x
                } else { # list of tags
                    # TODO make tagList? or check type?
                    tags = c(tags, list(do_panel(make_layout0(bits[[k]]))))
                }
            }
            content = c(content, list(do_layout(tags)))
        }

        if (i <= length(bits)) {
            do_layout = bits[[i]]@x$do_layout
            do_panel = bits[[i]]@x$do_panel
        } else {
            break
        }
    }

    return (list(panel = panel, content = content))
}
