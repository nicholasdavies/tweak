# X section
# X group
# X Label controls
# X Document labelling controls
# X print.tweak.dashboard
# X inputs
# X html
# X output_plot
# X list dropdown values
# X auto input$x
# X design_sidebar(), design_console()
# X layout_*(), (), +
# rename inputs to input?
# input_*
# rename layout_*, since this is used by bslib
# maybe for layout, group_pages, group_columns, group_none
# for decorations on the group, panel(title, icon) ?
#

# TODO where there is a width/height being specified, use validateCssUnit
# I think this applies to overshiny, not to tweak::tweak (though it may
# apply to other components of tweak, IF these are not already being run
# through shiny/bslib which are probably smart enough to work it out)

# What's the game plan. Calculate is up and running. I need a group_stack()
# type of thing, plus a panel() kind of thing

library(tweak)
library(shiny)
library(ggplot2)

class(tweak.group() + tweak.group())

I think the problem is this.
bit + bit + bit --> (bit + bit) + bit --> group + bit --> group(bit1, bit2, bit3)

BUT

(bit + bit) + bit  -- ALSO --> group(bit1, bit2, bit3)
when what we want is group(group(bit1, bit2), bit3)

layout_stack() +
    (inputs(...))
    (inputs(...))
    (inputs(...) + inputs(...))

dashboard() +
    section_head() +
    h1("My app") +
    section_main() +
    calculate(dots = {
        dots = data.frame(x = runif(100, -0.75, 0.75))
        dots$y = a * dots$x + 0.1 * rnorm(100)
        return (dots)
    }) +
    output_plot({
        ggplot() +
            geom_abline(slope = a, intercept = b) +
            geom_point(data = dots, aes(x, y)) +
            xlim(-1, 1) +
            ylim(-1, 1)
    }) +
    section_conf() +
    layout_stack() +
    panel(title = "Tab 1",
        layout_cols() +
        panel(title = "Hello", icon = "gear",
            inputs("Slope", a = c(1, -1, 1))
        ) +
        panel(title = "Howdy", icon = "gears",
            inputs("Intercept", b = c(0, -1, 1))
        )
    ) +
    panel(title = "Tab 2",
        h1("Hello there")
    ) +
    section_foot() +
    p("Made by some person") +
    design_console()


dashboard() +
    section_head() +
    h1("My app") +
    section_main() +
    calculate(dots = {
        dots = data.frame(x = runif(100, -0.75, 0.75))
        dots$y = a * dots$x + 0.1 * rnorm(100)
        return (dots)
    }) +
    output_plot({
        ggplot() +
            geom_abline(slope = a, intercept = b) +
            geom_point(data = dots, aes(x, y)) +
            xlim(-1, 1) +
            ylim(-1, 1)
    }) +
    section_conf() +
    layout_stack() +
    panel(title = "Tab 1") +
        layout_cols() +
        panel(title = "Hello", icon = "gear") +
            inputs("Slope", a = c(1, -1, 1)) +
        panel(title = "Howdy", icon = "gears") +
            inputs("Intercept", b = c(0, -1, 1)) +
        layout_end() +
    panel(title = "Tab 2") +
        h1("Hello there") +
    layout_end() +
    section_foot() +
    p("Made by some person") +
    design_console()



dashboard(
    section_head(
        h1("My app")
    ),
    section_main(
        calculate(dots = {
            dots = data.frame(x = runif(100, -0.75, 0.75))
            dots$y = a * dots$x + 0.1 * rnorm(100)
            return (dots)
        }),
        output_plot({
            ggplot() +
                geom_abline(slope = a, intercept = b) +
                geom_point(data = dots, aes(x, y)) +
                xlim(-1, 1) +
                ylim(-1, 1)
        }),
    ),
    section_conf(
        layout_stack(
            panel("Tab 1",
                layout_cols(
                    panel("Hello", icon("gear"),
                        inputs("Slope", a = c(1, -1, 1))
                    ),
                    panel("Howdy", icon("gears"),
                        inputs("Intercept", b = c(0, -1, 1))
                    )
                )
            ),
            panel("Tab 2",
                h1("Hello there")
            )
        )
    ),
    section_foot(
        p("Made by some person")
    ),
    design_console()
)


dashboard() +
    section_head() +
    h1("My app") +
    section_main() +
    calculate(dots = {
        dots = data.frame(x = runif(100, -0.75, 0.75))
        dots$y = a * dots$x + 0.1 * rnorm(100)
        return (dots)
    }) +
    output_plot({
        ggplot() +
            geom_abline(slope = a, intercept = b) +
            geom_point(data = dots, aes(x, y)) +
            xlim(-1, 1) +
            ylim(-1, 1)
    }) +
    section_conf() +
    layout_stack() +
    (
        panel("Tab 1") +
        layout_cols() +
        (
            panel(title = "Hello", icon = "gear") +
            inputs("Slope", a = c(1, -1, 1))
        ) +
        (
            panel(title = "Howdy", icon = "gears") +
            inputs("Intercept", b = c(0, -1, 1))
        )
    ) +
    (
        panel("Tab 2") +
        h1("Hello there")
    ) +
    section_foot() +
    p("Made by some person") +
    design_console()

tweak:::make_ui(d)

zero = 0

dashboard() +
    section_head() +
        h1("My dashboard") +
    section_conf() +
        inputs("Slope", a = c(1, -1, 1),
            "Intercept", b = c(0, -2, 2),
            "Point", point = list(c(1, 1), c(2, 2))) +
    section_main() +
        output_plot({
            ggplot(data.frame(x = zero:3, y = zero:3)) +
                annotate("point", x = point[1], y = point[2]) +
                geom_abline(slope = a, intercept = b) +
                xlim(0, 3) + ylim(0, 3)
        }) +
    section_foot() +
        p("Created by Samwise Gamgee") +
    design_sidebar()

# sections:
#   head (panels e.g. about, help, other settings)
#   main (panels e.g. in covidm_shiny2 plots)
#   conf (panels e.g. in covidm_shiny2 console)
#   foot (panels e.g. popups on the bottom...?)

# maybe
# section_head(), section_main(), section_conf(), section_foot()
# layout_page(), layout_group()
# item_input
# item_plot
# dash_input
# dash_plot
# output_plot, output_text etc
# controls(), control_slider, etc
# or
# { ggplot() } (i.e. just bare)
# or
# { a = control_slider() } (nah)
# dashboard() + { h1(paste("My app, you are on page", page)) }
# dashboard() + output_html(h1(paste("My app, you are on page", page)))


#' ---- HEAD ----
#' PANEL --- BODY
#' ---- FOOT ----
#' then: pages
#' then: groups
BODY
quote(
    test <- compiled: function(x, y)
    {
        return (x + y)
    }
)


Kinds of parameters:
    population
    *_rate, *_rate_vac [0, inf)
    prop_* [0, 1]
    *_risk [0, 1]
    intervention
    vaccination
    time_dependence
    population_change
    time_end [0, inf)
    increment [0, inf)
    erlang_subcompartments [1..5)



model_diphtheria(
  population,
  transmission_rate = 4/4.5,
  infectiousness_rate = 1/3,
  recovery_rate = 1/3,
  reporting_rate = 0.03,
  prop_hosp = 0.01,
  hosp_entry_rate = 0.2,
  hosp_exit_rate = 0.2,
  prop_vaccinated = 0 * population[["demography_vector"]],
  intervention = NULL,
  time_dependence = NULL,
  population_change = NULL,
  time_end = 100,
  increment = 1
)

model_default(
  population,
  transmission_rate = 1.3/7,
  infectiousness_rate = 1/2,
  recovery_rate = 1/7,
  intervention = NULL,
  vaccination = NULL,
  time_dependence = NULL,
  time_end = 100,
  increment = 1
)

model_ebola(
  population,
  transmission_rate = 1.5/12,
  erlang_subcompartments = 2,
  infectiousness_rate = erlang_subcompartments/5,
  removal_rate = erlang_subcompartments/12,
  prop_community = 0.9,
  etu_risk = 0.7,
  funeral_risk = 0.5,
  intervention = NULL,
  time_dependence = NULL,
  time_end = 100,
  replicates = 100
)

model_vacamole(
  population,
  transmission_rate = 1.3/7,
  transmission_rate_vax = 0.8 * transmission_rate,
  infectiousness_rate = 1/2,
  hospitalisation_rate = 1/1000,
  hospitalisation_rate_vax = 0.8 * hospitalisation_rate,
  mortality_rate = 1/1000,
  mortality_rate_vax = 0.8 * mortality_rate,
  recovery_rate = 1/7,
  intervention = NULL,
  vaccination = NULL,
  time_dependence = NULL,
  time_end = 100,
  increment = 1
)
