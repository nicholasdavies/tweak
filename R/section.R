# Indices into html of the four sections
.head.idx = 1
.main.idx = 2
.conf.idx = 3
.foot.idx = 4

#' Section setters
#' @export
section_head = function()
{
    tweak.do({
        d@curr = .head.idx
        exec_dashboard(d)
    })
}

#' @export
section_main = function()
{
    tweak.do({
        d@curr = .main.idx
        exec_dashboard(d)
    })
}

#' @export
section_conf = function()
{
    tweak.do({
        d@curr = .conf.idx
        exec_dashboard(d)
    })
}

#' @export
section_foot = function()
{
    tweak.do({
        d@curr = .foot.idx
        exec_dashboard(d)
    })
}
