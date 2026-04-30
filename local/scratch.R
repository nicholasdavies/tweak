# X section
# X group
# X Label controls
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

library(tweak)
library(shiny)
library(ggplot2)

ndot = 0
dashboard() +
    calculate(dots = {
        dots = data.frame(x = runif(100 + ndot * 100, -0.75, 0.75))
        dots$y = a * dots$x + b + 0.1 * rnorm(100 + ndot * 100)
        dots
    }) +
    section_head("My app") +
    section_main() +
        dash_html(h2(HTML(paste0("y = ", a, "x ", if (b >= 0) "+ " else "&minus; ", abs(b))))) +
        dash_plot(myplot = {
            ggplot() +
                geom_abline(slope = a, intercept = b) +
                geom_point(data = dots, aes(x, y)) +
                coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))
        }) +
    section_conf() +
        panel(title = "Settings 1", icon = "gears") +
            dash_input(a = "Slope" ~ c(1, -1, 1)) +
            dash_input(b = "Intercept" ~ c(0, -1, 1)) +
            dash_update("dots", "Update dots") +
            dash_input(ndot = "Add dots" ~ NA) +
        panel(title = "Settings 2", icon = "ghost") +
            h1("Boo!") +
    section_foot() +
        p(HTML("&copy; 2025 some person")) +
    design_console("yeti")

# c(0, 1)       slider
# list(...)     select
# TRUE          check
# "text"        text
# 123.456       numeric
# 2020-01-01    date
#               button
#

spec
View(spec)

p = tweak:::make_plan(spec)
View(p)

# Things from covidm_shiny2 to implement:

# Big tabs?

# Help button
div(id = "help_loc", style = "position: absolute; right: 10px; top: 10px",
    bsButton("help_tooltips", label = HTML("Show help"), icon = icon("question-circle", "fa"), style = "info", size = "small", type = "toggle", value = FALSE)
)

# Compare button
div(id = "compare_loc", style = "position: absolute; right: 112px; top: 10px",
    bsButton("compare", label = HTML("Compare"), icon = icon("exchange-alt", "fa"), style = "info", size = "small", type = "toggle", value = TRUE)
)

# Intervention rectangles

# X Action button

# Hierarchical selector; i.e., admin 0, admin 1, etc.

# Nice location selector with Leaflet and demographics

# Contact matrix selector

# table. rHandsontableOutput?

# dash_* functions for the built-in input types, as well as a
# tweak.input(type = "custom", html = {...}, value = {...} (e.g. as done for dropdowns), ...)
# for extending tweak, such as with contact matrix selector

# download, upload, upload to table, action button

# appropriate auto layout for head and foot

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
