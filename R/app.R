#' Print tweak dashboard
print.tweak.dashboard = function(d)
{
    # TODO add type checking and so on

    # Parse dashboard into app plan structure
    plan = make_plan(d)

    # -- UI --
    ui = do.call(plan$design$ui, c(list(plan = plan), list(plan$design$args)))

    # -- SERVER --

    # TODO make this an option...
    thematic::thematic_shiny()

    # Server function: execute all custom code
    server = function(input, output, session) {
        session$userData$tweak.calc = shiny::reactiveValues()

        plot_height = function(id, aspect) function() {
            session$clientData[[paste0("output_", id, "_width")]] / aspect
        }

        # Processing for each output
        # TODO make sure req() statements are present
        for (o in names(plan$output)) {
            out = plan$output[[o]]
            updater_ids = names(plan$update[plan$update == o])

            if (out$type == "calc") {
                session$userData$tweak.calc[[o]] = shiny::reactive(
                    eval2(out$q, make_eval_env(plan, session)),
                    env = rlang::env(out = out)
                ) |> bind_updaters(input, updater_ids)
            } else if (out$type == "plot") {
                output[[o]] = shiny::renderPlot(
                    eval2(out$q, make_eval_env(plan, session)),
                    env = rlang::env(out = out),
                    height = shiny::reactive(
                        session$clientData[[paste0("output_", o, "_width")]] / out$aspect,
                        env = rlang::env(out = out, o = o)
                    ),
                    execOnResize = TRUE
                ) |> bind_updaters(input, updater_ids)
            } else if (out$type == "html") {
                output[[o]] = shiny::renderUI(
                    eval2(out$q, make_eval_env(plan, session)),
                    env = rlang::env(out = out)
                ) |> bind_updaters(input, updater_ids)
            }
        }

        # Miscellaneous server code
        for (s in plan$server0) {
            s$func(s, input, output, session, calc, plan)
        }
    }

    print(shiny::shinyApp(ui, server))
}
