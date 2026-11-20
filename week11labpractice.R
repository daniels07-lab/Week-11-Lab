#r script practice

#r script s



#hello this is daniel







#hello this is daniel

#Problem 5, Function 1

# Solution (for tests):
df <- read.csv("~/Desktop/STAT 1560/Data/sample_bike.csv")

#' Estimate hourly bike-trip rates
#' 
#' @description: estimating the rate of user requests to travel from an origin station to a
#' destination station, per hour
#' @param 

sharon solution for problem 5
# Solution (for tests):
data <- read.csv("~/Desktop/STAT 1560/Data/sample_bike.csv")

#' estimate hourly bike-trip rates
#' 
#' @description: estimating the rate of user requests to travel from an origin station to a
#' destination station, per hour
#' @param data is a dataframe that stores the data regarding start/end time of start/end
#' stations for individual bike and customer id's
#' @param window_start is the beginning of the observation window 
#' @param window_end is the end of the observation window
#' @return the data frame returns a table with columns origin, destination, hour,
#' observed rate, availability, rate
#' @example
#' estimate_arrival_rates(df, "2022-02-16", "2022-02-21")

estimate_arrival_rates <- function(data, window_start, window_end) {
  
  #checking input columns
  input <- c("X", "start_station", "end_station",
             "start_time", "end_time")
  
  if (!all(input %in% names(data))) {
    stop("missing some column(s)")
  }
  
  #converting data types
  df <- data %>%
    mutate(
      start_station = as.character(start_station),
      end_station = as.character(end_station),
      start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
      end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S")
    )
  
  window_start <- as.POSIXct(window_start)
  window_end <- as.POSIXct(window_end)
  
  #checking the observation window using window start/ed
  if (length(window_start) != 1 ||
      length(window_end) != 1 ||
      is.na(window_start) ||
      is.na(window_end) ||
      window_end <= window_start) {
    stop("observation window is not valid")
  }
  
  n_days <- as.numeric(difftime(window_end, window_start, units = "days"))
  
  if (n_days %% 1 != 0 || n_days <= 0) {
    stop("window must contain integer days")
  }
  
  #customer trip
  customer <- df %>%
    filter(start_station != "R", end_station != "R")
  
  #removing a bike for customer departures (-1)
  departures <- data.frame(
    station = customer$start_station,
    time = customer$start_time,
    inventory_change = rep(-1, nrow(customer)),
    event_type = rep("C", nrow(customer)),
    order = rep(2, nrow(customer)),
    row = customer$X
  )
  
  #adding a bike for customer arrivals (+1)
  arrivals <- data.frame(
    station = customer$end_station,
    time = customer$end_time,
    inventory_change = rep(1, nrow(customer)),
    event_type = rep("C", nrow(customer)),
    order = rep(3, nrow(customer)),
    row = customer$X
  )
  
  #rebalancing additions when a bike is added (+1)
  added <- df %>%
    filter(start_station == "R", end_station != "R")
  
  additions <- data.frame(
    station = added$end_station,
    time = added$end_time,
    inventory_change = rep(1, nrow(added)),
    event_type = rep("R", nrow(added)),
    order = rep(1, nrow(added)),
    row = added$X
  )
  
  #rebalancing removals when a bike is removed (-1)
  removed <- df %>%
    filter(end_station == "R", start_station != "R")
  
  removals <- data.frame(
    station = removed$start_station,
    time = removed$start_time,
    inventory_change = rep(-1, nrow(removed)),
    event_type = rep("R", nrow(removed)),
    order = rep(4, nrow(removed)),
    row = removed$X
  )
  
  #combining events
  events <- bind_rows(additions, departures, arrivals, removals) %>%
    arrange(time, order, row)
  
  #tracking inventory at each station
  stations <- unique(events$station)
  
  inventory <- setNames(rep(0, length(stations)), stations)
  last_time <- setNames(rep(window_start, length(stations)), stations)
  
  available_time <- matrix(
    0,
    nrow = length(stations),
    ncol = 24,
    dimnames = list(stations, as.character(0:23))
  )
  
  #calculating the time when a station had bikes
  add_available_time <- function(station, start, end) {
    while (start < end) {
      hour <- as.integer(format(start, "%H"))
      next_hour <- as.POSIXct(format(start, "%Y-%m-%d %H:00:00")) + 3600
      finish <- min(next_hour, end)
      available_time[station, hour + 1] <- available_time[station, hour + 1] +
        as.numeric(difftime(finish, start, units = "hours"))
      start <- finish
    }
  }
  
  #loop for all events in chornological time order
  if (nrow(events) > 0) {
    for (i in 1:nrow(events)) {
      station <- events$station[i]
      time <- events$time[i]
      if (time < window_start) {
        inventory[station] <- inventory[station] +
          events$inventory_change[i]
      } else if (time < window_end) {
        #when at least one bike is available
        if (inventory[station] > 0) {
          add_available_time(station, last_time[station], time)
        }
        inventory[station] <- inventory[station] +
          events$inventory_change[i]
        last_time[station] <- time
      }
    }
  }
  
  #remaining time until the end of the window
  for (station in stations) {
    if (inventory[station] > 0) {
      add_available_time(station, last_time[station], window_end)
    }
  }
  #changing hours into fractions of days
  availability_df <- data.frame(
    origin = rep(stations, each = 24),
    hour = rep(0:23, times = length(stations)),
    availability = as.vector(t(available_time)) / n_days
  )
  
  #observed customer departures during the window
  observed <- customer %>%
    filter(start_time >= window_start,
           start_time < window_end) %>%
    mutate(hour = as.integer(format(start_time, "%H"))) %>%
    count(start_station, end_station, hour, name = "departures") %>%
    rename(
      origin = start_station,
      destination = end_station
    )
  
  #creating each station pair and their hours
  pairs <- customer %>%
    select(start_station, end_station) %>%
    distinct() %>%
    rename(origin = start_station,
           destination = end_station)
  
  results <- merge(pairs, data.frame(hour = 0:23)) %>%
    left_join(observed, by = c("origin", "destination", "hour")) %>%
    left_join(availability_df, by = c("origin", "hour")) %>%
    mutate(
      departures = ifelse(is.na(departures), 0, departures),
      observed_rate = departures / n_days,
      rate = ifelse(availability == 0 | is.na(availability),
                    NA_real_, observed_rate / availability)) %>%
    select(origin, destination, hour, observed_rate, availability, rate)
  
  return(results)
}

rates <- estimate_arrival_rates(
  data,
  "2022-02-16 00:00:00",
  "2022-02-22 00:00:00"
)

head(rates)

#small dataset
small <- data.frame(
  X = c(1, 2),
  start_station = c("R", "A"),
  end_station = c("A", "B"),
  start_time = c("2022-02-16 08:50:00", "2022-02-16 09:30:00"),
  end_time = c("2022-02-16 09:00:00", "2022-02-16 09:45:00")
)

small_rates <- estimate_arrival_rates(
  small,
  "2022-02-16",
  "2022-02-18"
)

#i'm not sure why the availability is 0 and rate is NA
test <- small_rates %>%
  filter(origin == "A", destination == "B", hour == 9)

test

#testing for availability = 0, hr10 of station A
test_zero <- small_rates %>%
  filter(origin == "A", destination == "B", hour == 10)

test_zero

stopifnot(
  test_zero$availability == 0,
  is.na(test_zero$rate)
)