# the proportion of the work is equally assigned to each team member

library(ggplot2)

slot_checker <- function(df_station, max_queue) {
    # Function to check the available slot in the station
    # Parameters:
    #   - df_station(data.frame): the statistics of the stations
    #   - max_queue(int): maximum number of queue in the stations e.g., French: Inf, British: 20
    # Return:
    #   - availability(bool): Whether there is an available slot or not
    # total_lower_capacity <- nrow(df_station[df_station$queue < max_queue, ])
    total_lower_capacity <- length(which(df_station$queue < max_queue))
    if (total_lower_capacity > 0) {
        availability <- TRUE
    } else {
        availability <- FALSE
    }
    return(availability)
}

add_a_new_car <- function(df_station, max_queue) {
    # Function to add a new car to the shorthest queue (if slot available)
    # Parameters:
    #   - df_station(data.frame): the statistics of the stations
    #   - max_queue(int): maximum number of queue in the stations e.g., French: Inf, British: 20
    # Return:
    #   - df_station(data.frame): the statistics of the stations with new car if slot is available.
    short_queue <- min(df_station$queue)
    short_stations <- which(df_station$queue == short_queue & df_station$queue < max_queue)

    if (length(short_stations) > 0) {
        df_station$queue[short_stations[1]] <- df_station$queue[short_stations[1]] + 1
    }
    return(df_station)
}

process_a_car <- function(state, df_station, pmin, pmax) {
    # Function to process a car (update the queue, subtract a car from the queue, assign a new car to be processed)
    # Parameters:
    #   - state(int): The current state (s)
    #   - df_station(data.frame): the statistics of the stations
    #   - pmin(int): the lower bound of processing time (alpha in the Uniform distribution)
    #   - pmin(int): the upper bound of processing time (beta in the Uniform distribution)
    # Return:
    #   - df_station(data.frame): the statistics of the stations with updated state and queue
    df_station["next_car"] <- state <= df_station$next_state

    available_station <- which(df_station$next_car == TRUE & df_station$queue > 0)

    processing_time <- runif(total_available_station, min = pmin, max = pmax)
    processing_time <- ceiling(processing_time)

    if (length(available_station) > 0) {
        df_station$next_state[available_station] <- state - processing_time
        df_station$queue[available_station] <- df_station$queue[available_station] - 1
    }
    return(df_station)
}

qsim <- function(mf = 5, mb = 5, a.rate = .1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20) {
    # Function to simulate the system
    # Parameters:
    #   - mf(int): Number of French stations
    #   - mb(int): Number of British stations
    #   - a.rate(float): is the probability of a car arriving each second.
    #   - trb(int): upper bound of processing time in british stations (beta)
    #   - trf(int): upper bound of processing time in french stations (beta)
    #   - tmb(int): lower bound of processing time in british stations (alpha)
    #   - tmf(int): lower bound of processing time in french stations (alpha)
    #   - maxb(int):  the maximum british queue length (per station)
    # Returns:
    #   - nf(vector): a vector giving the average length of the french queues, for each simulation second
    #   - nb(vector): a vector giving the average length of the british queues, for each simulation second
    #   - eq(vector): a vector giving the average expected waiting time

    total_state <- 2 * 60 * 60
    france_station <- data.frame(
        "station" = 1:mf,
        "queue" = rep(0, mf),
        "next_state" = total_state,
        "next_car" = TRUE
    )
    british_station <- data.frame(
        "station" = 1:mf,
        "queue" = rep(0, mf),
        "next_state" = total_state,
        "next_car" = TRUE
    )

    nf <- c()
    nb <- c()
    eq <- c()

    while (total_state > 0) {
        # To stop the process 30 mins before departure.
        if (total_state >= (30 * 60)) {
            is_car_arrive <- rbinom(1, 1, a.rate)
        } else {
            is_car_arrive <- FALSE
        }

        # Check the british availibility
        is_british_available <- slot_checker(british_station, max_queue = maxb)

        # Get Stats for queue at the current state
        n_state_f <- sum(france_station$queue)
        n_state_b <- sum(british_station$queue)

        # Add a new car to the French stations
        if (is_car_arrive) {
            france_station <- add_a_new_car(france_station, max_queue = Inf)
        }

        # Process a car on the French Station
        if (is_british_available) {
            france_station <- process_a_car(total_state, france_station, pmin = tmf, pmax = tmf + trf)
        }

        # Add a new car to the British stations
        if (is_british_available & sum(france_station$queue) < n_state_f) {
            british_station <- add_a_new_car(british_station, max_queue = maxb)
            n_state_f <- sum(france_station$queue)
        }

        # Process a car on the British Station
        british_station <- process_a_car(total_state, british_station, pmin = tmb, pmax = tmb + trb)

        # Get statistic for average processing time, derived from normal distribution
        mean_proc_time_french <- (trf + tmf + trf) / 2
        mean_proc_time_british <- (trb + tmb + trb) / 2

        # Combine the satistics data
        nf <- append(nf, mean(france_station$queue))
        nb <- append(nb, mean(british_station$queue))
        eq <- append(eq, (n_state_f * mean_proc_time_french) + (n_state_b * mean_proc_time_british))

        # Go one step ahead
        total_state <- total_state - 1
    }
    return(list(nf = nf, nb = nb, eq = eq))
}

res1 <- qsim(mf = 5, mb = 5, a.rate = .1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20)
res2 <- qsim(mf = 5, mb = 5, a.rate = .1, trb = 40, trf = 40, tmb = 40, tmf = 30, maxb = 20)
### discussion of the implications of small extra delays in British checking
# The average queue length change over time at British Station shows an increase.
# A small increase in extra delays of 10 seconds leads to a significant rise in the mean of average queue length change over time from 0.46 to 2.22.
# This means that a 20% increase in extra delays causes a nearly 38% increase in the mean of average queue length change over time.
# Similarly, the mean of expected waiting time for the whole system also increases by nearly 36%, from 1051.74 to 1432.54.

df_stat1 <- data.frame(
    time = 1:7200,
    nf = unlist(res1$nf),
    nb = unlist(res1$nb),
    eq = unlist(res1$eq)
)
df_stat2 <- data.frame(
    time = 1:7200,
    nf = unlist(res2$nf),
    nb = unlist(res2$nb),
    eq = unlist(res2$eq)
)

# Combine these plot into a single of graph (4 panels 2x2)
par(mfrow = c(2, 2))
plot(df_stat1$nf, type = "l", xlab = "Time", ylab = "avg. queue length", main = "The Avg. Queue length Changing \n Over Time (tmb = 30)", col = "red")
lines(df_stat1$nb, col = "blue")
legend(x = "topleft", inset = 0, legend = c("French", "British"), col = c("Red", "Blue"), lwd = 5, cex = .5, horiz = TRUE)
plot(df_stat1$eq, type = "l", xlab = "Time", ylab = "Expected Queuing Time", main = "The Expected Waiting Time (tmb = 30)")
plot(df_stat2$nf, type = "l", xlab = "Time", ylab = "avg. queue length", main = "The Avg. Queue length Changing \n Over Time (tmb = 40)", col = "red")
lines(df_stat2$nb, col = "blue")
legend(x = "topleft", inset = 0, legend = c("French", "British"), col = c("Red", "Blue"), lwd = 5, cex = .5, horiz = TRUE)
plot(df_stat2$eq, type = "l", xlab = "Time", ylab = "Expected Queuing Time", main = "The Expected Waiting Time (tmb = 40)")


# Simulation through 100 iterations with default parameters
simulations <- c()
n_simulation <- 100
for (i in 1:n_simulation) {
    iter_res <- qsim(mf = 5, mb = 5, a.rate = .1, trb = 40, trf = 40, tmb = 40, tmf = 30, maxb = 20)
    gt_0 <- (iter_res$nf[7200] + iter_res$nb[7200]) > 0
    simulations <- c(simulations, gt_0)
}
probability_missing_car <- sum(simulations) / n_simulation
# Proabbility of at least 1 car left
cat(probability_missing_car)


# Findings: 
#1. We saw the probability of at least there is one missing car with the default parameter on 100runs is 0. We could get a higher probability by increasing the `a.rate`. All of 100 simulation have at least 1 car left if we doubled the a.rate (0.2)
#2. We noticed that by using default parameters, which the distribution of processing time between French and British stations are the same, it made the British statios never reach maximum queue (20cars). We can either add more stations on the French or reduce number of British station.
#3. We optimized our initial code that reduced the time duration (@simulation) from 6s to 0.8s by reducing the dataframe size, slicing using `which`, and use df$col instead of df['col']. Further, this finding could be a rule of thumb of utilizing dataframe.