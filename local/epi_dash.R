library(data.table)
library(ggplot2)
library(tweak)
library(shiny)
library(socialmixr)
library(lubridate)
library(epidemics)
library(overshiny)
library(magrittr) # needed for current use of %>% in outputs.R, calculate.R

# start with layout I guess
# dash_monitor() or something -- would show inputs etc.
# layouts in head and foot need to be added
#

# OK
# What I've been trying to do with this server code is to allow people to
# refactor their tweak dashboards as shiny dashboards. This is proving
# complex. Maybe instead I can allow them to insert their own custom server
# code instead. Then they can have e.g.
# db = dashboard() + {...} + dash_input(..., .tweak = FALSE) + server(...)

# TODO
# X katex for math (through md)
# X formula notation for naming/iconing inputs: dash_input(R0 = "Basic reproduction number" ~ c(1.1, 4.5))
# X NA for button
# X label function
# dynamic dots throughout
# additional inputs:
#  file upload input (fileInput)
#  date range, slider range
#  checkbox group (alternative to select?)
#  password, text area
#  radio buttons (alternative to select?)
#  input_switch (variant of check)
# X dash_bar (just a bare selection bar)

# TODO
# dash_input currently evaluates any formulae first, then saves the result
# in a tweak.do. I think this is right so that dash_input(a = "Face" ~ c(0, foo))
# is the same as dash_input(a = c(0, foo)) in capturing "foo" at the time that
# dash_input is called. And then anything that takes an expression waits to
# evaluate it, which is what is required (can't call ggplot() + ... once and
# have it be right every time). But make sure this is consistent.


# contact matrix setup
mat_choices = polymod$participants[, as.character(unique(country))]
polymod_survey = survey(polymod$participants, polymod$contacts)

# control panels
dem_panel = panel(label("Demographics", "globe-africa")) +
    calculate(matrix =
        socialmixr::contact_matrix(
            polymod_survey,
            countries = mat_country,
            symmetric = TRUE,
            age.limits = seq(0, 75, 5))
    ) +
    layout_cols() +
        panel() +
            dash_input(mat_country = "Country" ~ mat_choices) +
            dash_plot(aspect = 16/9, ggplot(matrix$demography) +
                geom_col(aes(x = population, y = age.group))
            ) +
        panel() +
            dash_plot(aspect = 1, ggplot(reshape2::melt(matrix$matrix)) +
                geom_raster(aes(x = age.group, y = contact.age.group, fill = value)) +
                scale_fill_viridis_c() +
                theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5))
            ) +
    layout_end()

epi_panel = panel(label("Epidemic", "viruses")) +
    dash_input(
        epi_seed_date = "Epidemic start date" ~ ymd("2025-01-01"),
        epi_sim_time = "Simulate for" ~ list("1 year" = 365, "2 years" = 730,
            "3 years" = 1095, "4 years" = 1460, "5 years" = 1825),
        epi_seed_size = "Starting number of infections" ~ c(10, 1, 100),
        epi_R0 = "Basic reproduction number, <i>R</i><sub>0</sub>" ~ c(2.4, 1.1, 4.5, 0.1),
        epi_immune = "Proportion immune at start" ~ c(0, 1, 0.01),
        epi_rho = "Case ascertainment rate" ~ c(1, 0.01, 1, 0.01)
    )

inf_panel = panel(label("Infection", "lungs-virus")) +
    dash_input(
        inf_latent_p = "Latent period" ~ c(2, 1, 7),
        inf_infect_p = "Infectious period" ~ c(4, 1, 7),
        ihr = "Infection hospitalisation ratio" ~ c(0.1, 0, 1),
        ifr = "Infection fatality ratio" ~ c(0.01, 0, 1)
    )

int_panel = panel(label("Interventions", "star-of-life")) +
    dash_input(bogosity = label("Bogosity", "biohazard") ~ c(0, 100000))

# TODO this doesn't work well for debugging.
# If instead of expressions we pass a single name, can we treat that as
# a function? And can we get accurate debugging? ONLY TIME WILL TELL

dashboard() +
    section_head(HTML("<code>{epidemics}</code> model dashboard")) +
    section_main() +
        calculate(dynamics = {
            total_population_size = sum(matrix$demography$population)
            init = matrix(c(
                    (1 - epi_immune) * (1 - epi_seed_size / total_population_size), # S
                    0,                                                              # E
                    (1 - epi_immune) * epi_seed_size / total_population_size,       # I
                    epi_immune,                                                     # R
                    0                                                               # V
                ),
                ncol = 5,
                nrow = length(matrix$demography$population),
                byrow = TRUE)
            data = model_default(population = population(
                    name = mat_country,
                    contact_matrix = matrix$matrix,
                    demography_vector = matrix$demography$population,
                    initial_conditions = init
                ),
                transmission_rate = epi_R0 / inf_infect_p,
                infectiousness_rate = 1 / inf_latent_p,
                recovery_rate = 1 / inf_infect_p,
                time_end = epi_sim_time
            )
            data = as.data.table(data)
            return (data[, .(value = sum(value)), by = .(time, compartment)][])
        }) +
        calculate(incidence = {
            start = dynamics[compartment == "infectious", value[1]]
            dynamics[compartment == "susceptible", .(time = time, incidence = c(start, -diff(value)))]
        }) +
        dash_bar(label = "Plots",
            which_plot = label("Cases", "head-side-cough"),
            label("Hospital", "ambulance"),
            label("Deaths", "times"),
            label("R Number", "wave-square"),
            label("Dynamics", "project-diagram")
        ) +
        dash_plot({
            if (which_plot == "Cases") {
                p = ggplot(incidence) + geom_line(aes(x = time + epi_seed_date, y = incidence * epi_rho))
            } else if (which_plot == "Hospital") {
                p = ggplot(incidence) + geom_line(aes(x = time + epi_seed_date, y = incidence * ihr))
            } else if (which_plot == "Deaths") {
                p = ggplot(incidence) + geom_line(aes(x = time + epi_seed_date, y = incidence * ifr))
            } else if (which_plot == "R Number") {
                total_population_size = sum(matrix$demography$population)
                p = ggplot(dynamics[compartment == "susceptible"]) +
                    geom_line(aes(x = time + epi_seed_date, y = epi_R0 * value / total_population_size))
            } else if (which_plot == "Dynamics") {
                p = ggplot(dynamics) + geom_line(aes(x = time + epi_seed_date, y = value, colour = compartment))
                    theme(legend.position = c(0.1, 0.7))
            }
            remargin(p, 0.02, 0.02, 0.1, 0.1)
        }) +
    section_conf() +
        dem_panel +
        epi_panel +
        inf_panel +
        int_panel +
    section_foot() +
        p(HTML("Made by the Centre for Mathematical Modelling of Infectious Diseases",
            "&middot; London School of Hygiene and Tropical Medicine")) +
    design_console(bslib_spacer = "0.5rem 1rem")

