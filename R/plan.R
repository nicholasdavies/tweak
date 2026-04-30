make_plan = function(spec)
{
    # Create plan for structure
    plan = structure(list(
        html = list(
            head = list(),
            main = list(),
            conf = list(),
            foot = list()
        ),
        info = list(
            head = list(label = NULL),
            main = list(label = NULL),
            conf = list(label = NULL),
            foot = list(label = NULL)
        ),
        input = list(),
        output = list(),
        server = list(),
        server0 = list(),
        update = character(),
        design = default_design()
    ), class = "tweak.plan")

    # Find execute, and remove options
    options = which(vapply(seq_along(spec@x), function(i) inherits(spec@x[[i]], "tweak.opt"), logical(1)))
    if (length(options)) {
        for (s in spec@x[options]) {
            plan = eval(s@body, envir = c(list(plan = plan), s@args))
        }
        spec@x = spec@x[-options]
    }

    # Find section indices
    sec_parts = parts(spec@x, function(bit) inherits(bit, "tweak.sect"))
    # Default section is "main"
    sec_kinds = vapply(sec_parts,
        function(s) if (s$zero == 0) "main" else spec@x[[s$zero]]@section, character(1))

    # Coalesce sections - as there may be missing sections from spec or multiple
    # sections of the same kind. Postcondition: `sections` is list with entries
    # head, main, conf, foot, each a list of spec entries.
    indices = list()
    for (sname in c("head", "main", "conf", "foot")) {
        indices[[sname]] = Reduce(function(a, b) c(a, b$span), sec_parts[sec_kinds == sname], integer(0))
    }
    sections = lapply(indices, function(i) spec@x[i])

    # Fill in plan$info
    for (spart in sec_parts) {
        if (spart$zero > 0) {
            s = spec@x[[spart$zero]]@section
            plan$info[[s]] = list(label = spec@x[[spart$zero]]@label)
        }
    }

    # Realize all sections
    for (s in seq_along(sections)) {
        sec = sections[[s]]
        secname = names(sections)[s]
        if (length(sec)) {
            # Insert default layouts; unless the section consists of a layout
            # at the top level, wrap it in the default layout.
            if (!(inherits(sec[[1]], "tweak.begin") && layout_past_end(sec, 1) > length(sec))) {
                sec = c(
                    list(do.call(plan$design[[secname]]$layout, plan$design[[secname]]$args)),
                    sec,
                    list(layout_end())
                )
            }

            # Insert blank panels; any layout not followed by panel gets
            # a blank_panel() inserted.
            i = 1;
            while (i <= length(sec)) {
                if (inherits(sec[[i]], "tweak.begin") && i < length(sec) && !inherits(sec[[i + 1]], "tweak.panel")) {
                    sec = c(sec[1:i], list(panel_blank()), sec[rlang::seq2(i + 1, length(sec))])
                    i = i + 2;
                } else {
                    i = i + 1;
                }
            }

            # Realize layout of section
            sec_id = paste0("tweak-dash-", secname)
            layout = realize_layout(sec, plan, NULL, sec_id)
            plan = layout$plan
            plan$html[[secname]] = layout$html
        }
    }

    return (plan)
}

panel_past_end = function(spec, panel_i)
{
    level = 0
    for (i in rlang::seq2(panel_i + 1, length(spec))) {
        if (inherits(spec[[i]], "tweak.begin")) {
            level = level + 1
        } else if (inherits(spec[[i]], "tweak.end")) {
            level = level - 1
            if (level < 0) {
                return (i)
            }
        } else if (inherits(spec[[i]], "tweak.panel") && level == 0) {
            return (i)
        }
    }

    if (level != 0) {
        stop("Layout imbalance.")
    } else {
        return (length(spec) + 1)
    }
}

layout_past_end = function(spec, panel_i)
{
    level = 1 # panel_i already points to tweak.begin
    for (i in rlang::seq2(panel_i + 1, length(spec))) {
        if (inherits(spec[[i]], "tweak.begin")) {
            level = level + 1
        } else if (inherits(spec[[i]], "tweak.end")) {
            level = level - 1
            if (level == 0) {
                return (i + 1)
            }
        }
    }

    stop("Layout imbalance.")
}

realize_layout = function(spec, plan, do_panel, id)
{
    html = list()

    i = 1
    panel_i = 1

    while (i <= length(spec)) {
        s = spec[[i]]
        element_id = paste0(id, "-", i)

        if (inherits(s, "shiny.tag")) {
            # Bare html: add to html
            html = c(html, list(s))
            i = i + 1
        } else if (inherits(s, "tweak.do")) {
            # Do command: execute on dashboard and add html
            exec = eval(s@body, envir = c(list(plan = plan), s@args))
            plan = exec$plan
            html = c(html, exec$html)
            i = i + 1
        } else if (inherits(s, "tweak.panel")) {
            # Start of panel: do panel til start of next panel at this level
            j = panel_past_end(spec, i)
            group = realize_layout(spec[rlang::seq2(i + 1, j - 1)], plan, NULL, element_id)
            plan = group$plan
            # TODO this should be like a parsed panel or something, with its own constructor?
            panel = list(label = s@label, type = s@type, body = group$html)
            html[[s@type]] = c(html[[s@type]], list(do_panel(panel, panel_i, element_id)))
            panel_i = panel_i + 1
            i = j
        } else if (inherits(s, "tweak.begin")) {
            # Start of layout:
            j = layout_past_end(spec, i)
            group = realize_layout(spec[rlang::seq2(i + 1, j - 1)], plan, s@do_panel, element_id)
            plan = group$plan
            html = c(html, list(s@do_layout(group$html, element_id)))
            i = j
        } else if (inherits(s, "tweak.end")) {
            # TODO ideally remove this if possible; or error on imbalance if not already
            i = i + 1
        } else {
            stop("I'm not sure what to do with a ", paste(class(s), collapse = "/"), call. = FALSE)
        }
    }
    return (list(plan = plan, html = html))
}
