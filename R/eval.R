input_binding_template = function(v)
{
    if (missing(v)) {
        if (!inherits(iplan, "shiny.tag") && iplan$type == "select") {
            # Convert dropdown list values to originally specified type
            return (iplan$values[[as.integer(input[[nm]])]]);
        }
        return (input[[nm]])
    } else {
        stop("Attempted assignment to input variable ", nm)
    }
}

make_input_binding = function(input, nm, iplan)
{
    binding_env = rlang::env(input = input, nm = nm, iplan = iplan)
    func = input_binding_template
    environment(func) = binding_env
    return (func)
}

calc_binding_template = function(v)
{
    if (missing(v)) {
        return (calc[[nm]]())
    } else {
        stop("Attempted assignment to calc variable ", nm)
    }
}

make_calc_binding = function(calc, nm)
{
    binding_env = rlang::env(calc = calc, nm = nm)
    func = calc_binding_template
    environment(func) = binding_env
    return (func)
}

make_eval_env = function(plan, session)
{
    env = rlang::new_environment()
    input = session$input
    output = session$output
    calc = session$userData$tweak.calc

    # Add input bindings to env
    sapply(names(plan$input), function(nm)
        makeActiveBinding(nm, make_input_binding(input, nm, plan$input[[nm]]), env))

    # Add calc bindings to env
    for (o in seq_along(plan$output)) {
        if (plan$output[[o]]$type == "calc") {
            nm = names(plan$output)[o]
            makeActiveBinding(nm, make_calc_binding(calc, nm), env)
        }
    }

    # Add input and output to env
    env$input = input
    env$output = output

    return (env)
}

# q is a quosure, env is the data environment
# This functions like eval(), using the quosure q's environment
# as its enclosing environment, but allows env to be an environment and
# allows assignment to active bindings in env to use the function interface.
# It also allows an early return from q using a function wrapper trick.
eval2 = function(q, env)
{
    body = rlang::quo_get_expr(q)
    wrapper = quote((function() { BODY })())
    wrapper[[1]][[2]][[3]] = body
    q = rlang::quo_set_expr(q, wrapper)
    rlang::eval_tidy(q, rlang::new_data_mask(env))
}

bind_updaters = function(x, input, updater_ids)
{
    if (length(updater_ids) == 0) {
        # No updaters: just return x
        x
    } else {
        # Updaters: bind to elements of input
        updaters = lapply(updater_ids, function(id) rlang::expr(input[[!!id]]))
        eval(rlang::call2(quote(shiny::bindEvent), x = x, !!!updaters,
            ignoreNULL = FALSE, ignoreInit = FALSE))
    }
}
