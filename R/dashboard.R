# Bit class and derived classes

setOldClass("tweak.label")
setOldClass("shiny.tag")

setClass("tweak.bit")
setClass("tweak.list",  contains = "tweak.bit", slots = c(x = "list"))
setClass("tweak.sect",  contains = "tweak.bit", slots = c(section = "character", label = "tweak.label"))
setClass("tweak.do",    contains = "tweak.bit", slots = c(body = "language", args = "list"))
setClass("tweak.begin", contains = "tweak.bit", slots = c(do_layout = "function", do_panel = "function"))
setClass("tweak.panel", contains = "tweak.bit", slots = c(label = "tweak.label", type = "character"))
setClass("tweak.end",   contains = "tweak.bit")
setClass("tweak.opt",   contains = "tweak.bit", slots = c(body = "language", args = "list"))

tweak.list = function(...)
{
    new("tweak.list", x = list(...))
}

tweak.sect = function(section, label)
{
    new("tweak.sect", section = section, label = label)
}

tweak.do = function(body, ...)
{
    new("tweak.do", body = substitute(body), args = list(...))
}

tweak.do.return = function(plan, html = NULL)
{
    list(plan = plan, html = html)
}

tweak.begin = function(do_layout, do_panel)
{
    new("tweak.begin", do_layout = do_layout, do_panel = do_panel)
}

tweak.panel = function(label, type)
{
    new("tweak.panel", label = label, type = type)
}

tweak.end = function()
{
    new("tweak.end")
}

tweak.opt = function(body, ...)
{
    new("tweak.opt", body = substitute(body), args = list(...))
}

tweak.input = function(...)
{
    structure(list(...), class = "tweak.input")
}

# TODO be able to add this directly to a dashboard.
tweak.label = function(title, icon)
{
    structure(list(title = title, icon = icon), class = "tweak.label")
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
#' @export
`+.shiny.tag` = function(e1, e2) {
    if (inherits(e2, "tweak.list")) {
        do.call(tweak.list, c(list(e1), e2@x))
    } else if (inherits(e2, "tweak.bit") || inherits(e2, "shiny.tag")) {
        return (tweak.list(e1, e2))
    }
    add_error("shiny.tag", e2)
}

# list + list, list + other bit, list + tag (add to existing list)
setMethod("+", c("tweak.list", "tweak.list"), function(e1, e2) do.call(tweak.list, c(e1@x, e2@x)) )
setMethod("+", c("tweak.list", "tweak.bit"),  function(e1, e2) do.call(tweak.list, c(e1@x, list(e2))) )
setMethod("+", c("tweak.list", "shiny.tag"),  function(e1, e2) do.call(tweak.list, c(e1@x, list(e2))) )
setMethod("+", c("tweak.list", "ANY"),        function(e1, e2) add_error("tweak.group", e2) )

# bit + bit, bit + tag (add to list)
setMethod("+", c("tweak.bit", "tweak.list"),  function(e1, e2) do.call(tweak.list, c(list(e1), e2@x)) )
setMethod("+", c("tweak.bit", "tweak.bit"),   function(e1, e2) tweak.list(e1, e2) )
setMethod("+", c("tweak.bit", "shiny.tag"),   function(e1, e2) tweak.list(e1, e2) )
setMethod("+", c("tweak.bit", "ANY"),         function(e1, e2) add_error("tweak.bit", e2) )


# Dashboard class
setClass("tweak.dashboard", slots = c(x = "list"))
setMethod("+", c("tweak.dashboard", "tweak.list"), function(e1, e2) { e1@x = c(e1@x, e2@x);     return (e1) } )
setMethod("+", c("tweak.dashboard", "tweak.bit"),  function(e1, e2) { e1@x = c(e1@x, e2);       return (e1) } )
setMethod("+", c("tweak.dashboard", "shiny.tag"),  function(e1, e2) { e1@x = c(e1@x, list(e2)); return (e1) } )
setMethod("+", c("tweak.dashboard", "ANY"),        function(e1, e2) add_error("tweak.dashboard", e2) )

#' Create empty dashboard
#' @export
dashboard = function()
{
    new("tweak.dashboard", x = list())
}

# Show dashboard
setMethod("show", "tweak.dashboard", function(object) print.tweak.dashboard(object))
