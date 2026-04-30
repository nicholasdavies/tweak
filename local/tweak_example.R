library(deSolve)
library(tweak)
library(ggplot2)

# Model here...
run_model = function(N, I0, beta, delta, gamma, omega, duration)
{
    # set up deSolve model
    state = c(S = N - I0, E = 0, I = I0, R = 0)
    times = 0:duration
    func = function(t, y, params) {
        with(c(y, params), {
            dS = -beta * I/N * S                         + omega * R
            dE =  beta * I/N * S - delta * E
            dI =                   delta * E - gamma * I
            dR =                               gamma * I - omega * R
            return (list(c(dS, dE, dI, dR)))
        })
    }
    params = list(beta = beta, delta = delta, gamma = gamma, omega = omega)

    return (as.data.frame(deSolve::ode(state, times, func, params)))
}

# test of running model
data = run_model(1000, 1, 2, 0.25, 0.33, 0.01, 1000) |>
    tidyr::pivot_longer(!time)

ggplot(data) +
    geom_line(aes(time, value, colour = name))

# How to use tweak
tweak({
    results = run_model(N, I0, beta, delta, gamma, omega, duration) |>
        tidyr::pivot_longer(!time)

    ggplot(results) +
        geom_line(aes(time, value, colour = name))
},
    N = 1000,   # freeform numeric input
    I0 = c(0, 10),  # slider from 0 to 10
    beta = c(2, 0.1, 10),  # slider from 0.1 to 10 with starting value 2
    delta = c(0.25, 0.01, 1, 0.01), # also specifying steps of 0.01
    gamma = c(0.33, 0.01, 1, 0.01), # same
    omega = "Waning rate" ~ c(0, 0.1), # custom label "Waning rate" using formula syntax
    duration = 365
)
