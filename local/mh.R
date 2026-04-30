library(data.table)
library(ggplot2)
library(tweak)
library(overshiny)
library(inshiny)
library(deSolve)

ddt = function(t, x, par) with(as.list(c(x, par)), {
    # population totals
    S = Su + Sm
    E = Eu + Em
    I = Iu + Im
    R = Ru + Rm

    U = Su + Eu + Iu + Ru
    M = Sm + Em + Im + Rm

    N = U + M

    mh_new = mh_incidence / (1000 * 365)
    mh_rec = 1 / (mh_duration * 365 / 12)

    beta = R0 / infect_period
    foi = beta * I / N

    act = 1 / latent_period
    rec = 1 / infect_period

    dSu = -foi * Su                       - mh_new * Su + mh_rec * Sm
    dEu =  foi * Su - act * Eu            - mh_new * Eu + mh_rec * Em
    dIu =             act * Eu - rec * Iu - mh_new * Iu + mh_rec * Im
    dRu =                        rec * Iu - mh_new * Ru + mh_rec * Rm

    dSm = -foi * Sm                       + mh_new * Su - mh_rec * Sm
    dEm =  foi * Sm - act * Em            + mh_new * Eu - mh_rec * Em
    dIm =             act * Em - rec * Im + mh_new * Iu - mh_rec * Im
    dRm =                        rec * Im + mh_new * Ru - mh_rec * Rm

    list(c(dSu, dEu, dIu, dRu, dSm, dEm, dIm, dRm))
})

do_plot = function(dynamics) {
    ggplot(melt(dynamics, id.vars = c("date", "time"))) +
        geom_line(aes(x = date, y = value, colour = variable)) +
        labs(x = NULL, y = "Number", colour = "Compartment")
}

dashboard() +
    section_main("MHPSS modelling") +
        calculate(dynamics = {
            par = list(
                R0 = epi_R0,
                latent_period = epi_latent, # days
                infect_period = epi_infect, # days
                mh_incidence = mh_incidence,
                mh_duration = mh_duration
            )

            N = 20e6
            eq_M = mh_duration * mh_incidence / (12000 + mh_duration * mh_incidence)

            x = as.data.table(ode(
                y = c(
                    Su = (N - epi_seed_size) * (1 - eq_M), Eu = 0, Iu = epi_seed_size * (1 - eq_M), Ru = 0,
                    Sm = (N - epi_seed_size) * eq_M,       Em = 0, Im = epi_seed_size * eq_M,       Rm = 0
                ),
                times = seq(0, epi_sim_time, 1),
                func = ddt,
                parms = par
            ))

            x[, date := time + epi_seed_date]
            return (x[])
        }) +
        dash_plot(do_plot(dynamics)) +
    section_conf() +
        panel(label("Mental health", "brain")) +
            dash_input(
                mh_incidence = "Incidence (per 1,000 person-years)" ~ c(10, 100, 1),
                mh_duration = "Duration (months)" ~ c(24, 0, 60, 1)) +
        panel(label("Epidemic", "viruses")) +
            dash_input(
                epi_seed_date = "Epidemic start date" ~ ymd("2026-01-01"),
                epi_sim_time = "Simulate for" ~ list("1 year" = 365, "2 years" = 730,
                    "3 years" = 1095, "4 years" = 1460, "5 years" = 1825),
                epi_seed_size = "Starting number of infections" ~ c(1, 100),
                epi_R0 = "Basic reproduction number, R_0" ~ c(2.4, 1.1, 4.5, 0.1),
                epi_latent = "Latent period, days" ~ c(2, 0.1, 14.0, 0.1),
                epi_infect = "Infectious period, days" ~ c(2, 0.1, 14.0, 0.1)) +
    design_sidebar()



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

