#the proportion of the work is equally assigned to each team member 

library(ggplot2)

slot_checker <- function(df_station) {
    # Function to check the available slot in the station
    # Parameters:
    #   - df_station(data.frame): the statistics of the stations
    # Return:
    #   - availability(bool): Whether there is an available slot or not
    total_lower_capacity <- nrow(df_station[df_station$queue < df_station$max_queue, ])
    if (total_lower_capacity > 0) {
        availability <- TRUE
    } else {
        availability <- FALSE
    }
    return(availability)
}

add_a_new_car <- function(df_station) {
    # Function to add a new car to the shorthest queue (if slot available)
    # Parameters:
    #   - df_station(data.frame): the statistics of the stations
    # Return:
    #   - df_station(data.frame): the statistics of the stations with new car if slot is available.
    short_queue <- min(df_station["queue"])
    short_stations <- df_station[df_station$queue == short_queue, ]
    short_stations <- df_station[df_station$queue == short_queue & df_station$queue < df_station$max_queue, ]

    if (nrow(short_stations) > 0) {
        first_short_stations <- min(short_stations["station"])
        df_station[df_station$station == first_short_stations, ]["queue"] <- df_station[df_station$station == first_short_stations, ]["queue"] + 1
    }
    return(df_station)
}

process_a_car <- function(df_station, pmin, pmax) {
    # Function to process a car (update the queue, subtract a car from the queue, assign a new car to be processed)
    # Parameters:
    #   - df_station(data.frame): the statistics of the stations
    #   - pmin(int): the lower bound of processing time (alpha in the Uniform distribution)
    #   - pmin(int): the upper bound of processing time (beta in the Uniform distribution)
    # Return:
    #   - df_station(data.frame): the statistics of the stations with updated state and queue
    df_station["next_car"] <- df_station$state <= df_station$next_state

    available_station <- df_station[df_station$next_car == TRUE & df_station$queue > 0, ]

    total_available_station <- nrow(available_station)

    processing_time <- runif(total_available_station, min = pmin, max = pmax)
    processing_time <- ceiling(processing_time)

    if (total_available_station > 0) {
        df_station[df_station$next_car == TRUE & df_station$queue > 0, ]["next_state"] <- available_station["state"] - processing_time
        df_station[df_station$next_car == TRUE & df_station$queue > 0, ]["ptime"] <- processing_time
        df_station[df_station$next_car == TRUE & df_station$queue > 0, ]["queue"] <- available_station["queue"] - 1
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
        "state" = total_state, 
        "next_state" = total_state, 
        "next_car" = TRUE, 
        max_queue = Inf, 
        ptime = 0
        )
    british_station <- data.frame(
        "station" = 1:mf, 
        "queue" = rep(0, mf), 
        "state" = total_state, 
        "next_state" = total_state, 
        "next_car" = TRUE, 
        max_queue = maxb, 
        ptime = 0
        )

    nf <- c()
    nb <- c()
    eq <- c()

    for (i in 1:total_state) {
        # TODO: To stop the process 30 mins before departure.
        if (min(france_station$state) >= (30 * 60)) {
            is_car_arrive <- rbinom(1, 1, a.rate)
        } else {
            is_car_arrive <- FALSE
        }
        is_british_available <- slot_checker(british_station)

        n_state_f <- sum(france_station["queue"])
        n_state_b <- sum(british_station["queue"])

        # Function to add a new car to the queue on French
        if (is_car_arrive) {
            france_station <- add_a_new_car(france_station)
        }

        # Function to add a new car to the queue on British
        if (is_british_available) {
            # Process a car on the French Station
            france_station <- process_a_car(france_station, pmin = tmf, pmax = tmf + trf)
        }

        # Add a new car to British Station
        if (is_british_available & sum(france_station["queue"]) < n_state_f) {
            british_station <- add_a_new_car(british_station)
            n_state_f <- sum(france_station["queue"])
        }

        british_station <- process_a_car(british_station, pmin = tmb, pmax = tmb + trb)

        # Statistic
        #mean_proc_time_french <- mean(france_station$ptime)
        mean_proc_time_french <- (trf + tmf + trf)/2
        #mean_proc_time_british <- mean(british_station$ptime)
        mean_proc_time_british <- (trb + tmb + trb)/2

        nf <- c(nf, mean(france_station$queue))
        nb <- c(nb, mean(british_station$queue))
        eq <- c(eq, (n_state_f * mean_proc_time_french) + (n_state_b * mean_proc_time_british))

        france_station["state"] <- france_station["state"] - 1
        british_station["state"] <- british_station["state"] - 1
    }
    return(list(nf = nf, nb = nb, eq = eq))
}

res1 <- qsim(mf = 5, mb = 5, a.rate = .1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20)
res2 <- qsim(mf = 5, mb = 5, a.rate = .1, trb = 40, trf = 40, tmb = 40, tmf = 30, maxb = 20)

df_stat1 <- data.frame(
    nf = unlist(res1$nf),
    nb = unlist(res1$nb),
    eq = unlist(res1$eq),
    idx = 1:7200
)
df_stat2 <- data.frame(
    nf = unlist(res2$nf),
    nb = unlist(res2$nb),
    eq = unlist(res2$eq),
    idx = 1:7200
)


# Todo: combine these plot into a single of graph (4 panels 2x2)

#option1
install.packages("gridExtra")
library(gridExtra)
plot1 <-ggplot(data = df_stat1, aes(x = idx, y = nf)) + geom_bar(stat = "identity", color = "blue")
plot2 <-ggplot(data = df_stat1, aes(x = idx, y = nb)) + geom_bar(stat = "identity", color = "blue")
plot3 <-ggplot(data = df_stat1, aes(x = idx, y = eq)) + geom_bar(stat = "identity", color = "blue")
plot4 <-ggplot(data = df_stat2, aes(x = idx, y = eq)) + geom_bar(stat = "identity", color = "blue")
grid.arrange (plot1, plot2, plot3, plot4)

#option2 
par(mfrow = c(2, 2))
plot(df_stat1$nf, type = "l", xlab = "Time", ylab = "French Queue Length", main = "the french queues")
plot(df_stat1$nb, type = "l", xlab = "Time", ylab = "British Queue Length", main = "the british queues")
plot(df_stat1$eq, type = "l", xlab = "Time", ylab = "Expected Queuing Time", main = "the expected waiting time")
plot(df_stat2$eq, type = "l", xlab = "Time", ylab = "Expected Queuing Time(tmb = 40)", main = "the expected waiting time (tmb = 40)")








