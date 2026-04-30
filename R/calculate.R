#' Calculate value
#' @export
calculate = function(...)
{
    # TODO allow dynamic dots
    qs = rlang::enquos(...)

    if (!rlang::is_named(qs) || anyDuplicated(names(qs))) {
        stop("All quantities to calculate must have unique names.")
    }

    tweak.do({
        # Add calcs
        for (q in seq_along(qs)) {
            plan$output = c(plan$output,
                list(list(type = "calc", q = qs[[q]])))
            names(plan$output)[length(plan$output)] = names(qs)[q]
        }

        # # Construct server code
        # for (q in seq_along(qs)) {
        #     server = rlang::expr({
        #         session$userData$tweak.calc[[!!id]] = shiny::reactive(
        #             !!rlang::quo_get_expr(qs[[q]]),
        #             env = make_eval_env(plan, session)
        #         ) %>% bind_updaters(input, names(plan$update[plan$update == !!id]))
        #     })
        #     plan$server = c(plan$server, server)
        # }

        # Finalize
        tweak.do.return(plan)
    }, qs = qs)
}
