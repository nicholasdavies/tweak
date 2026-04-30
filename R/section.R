#' Section setters
#' @export
section_head = function(label = NULL)
{
    tweak.sect("head", as_label(label))
}

#' @export
section_main = function(label = NULL)
{
    tweak.sect("main", as_label(label))
}

#' @export
section_conf = function(label = NULL)
{
    tweak.sect("conf", as_label(label))
}

#' @export
section_foot = function(label = NULL)
{
    tweak.sect("foot", as_label(label))
}
