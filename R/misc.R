#' @export
label = function(title, icon = NULL)
{
    if (rlang::is_string(icon)) {
        icon = shiny::icon(icon)
    }

    tweak.label(title, icon)
}

as_label = function(x)
{
    if (is.null(x)) {
        tweak.label(NULL, NULL)
    } else if (inherits(x, "tweak.label")) {
        x
    } else {
        tweak.label(x, NULL)
    }
}

as_tags = function(x)
{
    if (inherits(x, "tweak.label")) {
        if (empty(x)) {
            NULL
        } else {
            list(x$icon, x$title)
        }
    } else if (is.null(x)) {
        NULL
    } else {
        stop("Unknown type ", paste(class(x), collapse = "/"))
    }
}

empty = function(x)
{
    if (inherits(x, "tweak.label")) {
        is.null(x$title) && is.null(x$icon)
    } else {
        is.null(x)
    }
}

if_else_chain = function(varname, options, qs)
{
    if_token = quote(if (a == 1) {})[[1]]
    if (length(options) > 1) {
        return (as.call(list(
            if_token,
            rlang::expr(!!as.name(varname) == !!options[[1]]),
            rlang::quo_get_expr(qs[[1]]),
            if_else_chain(varname, options[-1], qs[-1])
        )))
    } else {
        return (as.call(list(
            if_token,
            rlang::expr(!!as.name(varname) == !!options[[1]]),
            rlang::quo_get_expr(qs[[1]])
        )))
    }
}

maybe = function(tag, ...)
{
    args = list(...)
    unnamed = rlang::names2(args) == ""
    if (all(vapply(args[unnamed], empty, logical(1)))) {
        NULL
    } else {
        shiny::tag(tag, args)
    }
}

exactly = function(str)
{
    function(x) x == str
}

whatever = function()
{
    function(x) TRUE
}

contains = function(pattern)
{
    function(x) grepl(pattern, x)
}

# In a list of shiny.tags, find a tag
find_tag = function(html, tag_name, ...)
{
    result = find_tag0(html, tag_name, list(...))
    if (is.null(result)) {
        stop("Cannot find tag.")
    }
    return (result)
}

find_tag0 = function(html, tag_name, attr)
{
    if (inherits(html, "shiny.tag") || !is.list(html)) {
        stop("find_tag must be used with a list of shiny.tags.")
    }

    for (i in seq_along(html)) {
        tag = html[[i]]
        if (inherits(tag, "shiny.tag")) {
            if (tag_name(tag$name)) {
                satisfied = TRUE
                for (j in seq_along(attr)) {
                    satisfied = satisfied && names(attr)[j] %in% names(tag$attribs) &&
                        attr[[j]](tag$attribs[[names(attr)[j]]])
                    if (!satisfied) break;
                }
                if (satisfied) return (i);
            }

            nest = find_tag0(tag$children, tag_name, attr)
            if (!is.null(nest)) {
                return (c(i, 3, nest)) # 3 is for 'children' element
            }
        }
    }

    return (NULL)
}

# Pad out options list with default values
pad_options = function(options, ...)
{
    defaults = list(...);
    for (nm in names(defaults)) {
        if (is.null(options[[nm]])) {
            options[[nm]] = defaults[[nm]];
        }
    }
    return (options)
}

# Search a shiny.tag class object for an inputId
get_input_id = function(x)
{
    if (!inherits(x, "shiny.tag")) {
        return (NULL)
    }

    if (x$name %in% c('input', 'select', 'button', 'div') && !is.null(x$attribs$id)) {
        return (x$attribs$id)
    }

    for (xx in x$children) {
        result = get_input_id(xx);
        if (!is.null(result)) {
            return (result)
        }
    }

    return (NULL)
}

# Slice list in quantiles [a/d, b/d)
slice = function(l, a, b, d)
{
    n = length(l);
    i = (n * a) / d + 1;
    j = (n * b) / d + 1;
    ind = seq_along(l);
    return (l[ind >= i & ind < j])
}

# Given a list-like (structure), return a list with elements
# list(zero = integer(1), span = integer(N)), where each `zero` is the index of
# an element of structure for which predicate is true and each span is the
# indices of elements up to the next zero. If structure[[1]] does not satisfy
# predicate, then the first zero is 0.
parts = function(structure, predicate)
{
    # Find indices of elements of structure for which predicate is TRUE
    # These indices are the "zero" of each part
    zero_indices = which(vapply(structure, predicate, logical(1)))

    # Get each zero and span
    p = list()

    if (length(zero_indices)) {
        if (zero_indices[1] != 1) {
            p = list(list(zero = 0, span = 1:(zero_indices[1] - 1)))
        }
        i = unname(zero_indices[1])
        zero_indices = zero_indices[-1]
        for (z in c(zero_indices, length(structure) + 1)) {
            part_indices = rlang::seq2(i + 1, z - 1)
            p = c(p, list(list(zero = i, span = part_indices)))
            i = z
        }
    } else {
        p = list(list(zero = 0, span = seq_along(structure)))
    }

    return (p)
}
