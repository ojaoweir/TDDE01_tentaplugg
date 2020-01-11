# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)


# ------------------ Assignment 1 ---------------------------------
# Implement a kernel method to predict the hourly temperatures for a date and place in Sweden.
# To do so, you are provided with the files stations.csv and temps50k.csv. These
# files contain information about weather stations and temperature measurements in the stations
# at different days and times. The data have been kindly provided by the Swedish Meteorological
# and Hydrological Institute (SMHI).
# You are asked to provide a temperature forecast for a date and place in Sweden. The
# forecast should consist of the predicted temperatures from 4 am to 24 pm in an interval of 2
# hours. Use a kernel that is the sum of three Gaussian kernels:
#   Y The first to account for the distance from a station to the point of interest.
# Y The second to account for the distance between the day a temperature measurement
# was made and the day of interest.
# Y The third to account for the distance between the hour of the day a temperature measurement
# was made and the hour of interest.
# Choose an appropriate smoothing coefficient or width for each of the three kernels above.
# Answer to the following questions:
#   Y Show that your choice for the kernels' width is sensible, i.e. that it gives more weight
# to closer points. Discuss why your of definition of closeness is reasonable.
# Y Instead of combining the three kernels into one by summing them up, multiply them.
# Compare the results obtained in both cases and elaborate on why they may differ.
# Note that the file temps50k.csv may contain temperature measurements that are posterior
# to the day and hour of your forecast. You must filter such measurements out, i.e. they cannot
# be used to compute the forecast. Feel free to use the template below to solve the assignment.

library(geosphere)
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

# Function to calculate the kernel value
kernel_gauss = function(diff, h_val) {
  U = diff / h_val
  U = U^2
  return(exp(-U))
}

kernal_plot = function(diff, h_val) {
  kernal = kernel_gauss(diff, h_val)
  plot(kernal, type="l")
}
# These values are decided by looking at the plots
h_distance <- 75000
h_date <-14
h_time <-4
kernal_plot(distance_diff <- seq(0,250000,1), h_distance)
kernal_plot(seq(0,30,1), h_date)
kernal_plot(seq(0,24,0.5), h_time)

# Function to calculate mod, required for days
mod = function(Lside, Rside) {
  returnValue = Lside - Rside * floor(Lside/Rside)
  return(returnValue)
}

# Functions to calculate the differences
calc_distance = function(input, sts) {
  dist = distHaversine(data.frame(sts$longitude,sts$latitude), input)
  return(dist)
}

# Take into consideration the difference in days  between years
# Two days ahead five years ago should give differnce 2 days
# Also that it allows days backwards from 25/02 to 24/02 is 1 day, not 364 
calc_day = function(input, sts) {
  days = difftime(sts$date, input)
  days = mod(days, 365)
  days = ifelse(days > 365/2, (365 - days), days)
  return(days)
}

calc_time = function(input, sts) {
  time = as.numeric(difftime(input, sts$time, units = "secs"))
  time = time /3600
  time = ifelse(abs(time) > 12, (24 -abs(time)), abs(time))
  return(time)
}

# Main function
predict_temp = function(lat_in, long_in, date_in, times_in,sts) {
  
  # Filter out all dates after the requested date
  sts = sts[as.Date(sts$date) < as.Date(date_in),]
  # Converts the time column from string to time, makes comparison easier later
  sts$time = strptime(sts$time, format = "%H:%M:%S")
  times_in = strptime(times_in, format = "%H:%M:%S")
  date_in = as.Date(date_in)
  
  # Calculating the difference and kernel values for distance and date
  distance = calc_distance(c(long_in, lat_in),sts)
  day = calc_day(date_in, sts)
  kernel_distance = kernel_gauss(distance, h_distance)
  kernel_day = kernel_gauss(day, h_date)
  temps_sum = rep(0,length(times_in))
  temps_mul = rep(0,length(times_in))

    for(i in 1:length(times_in)) {
      # Calculating the difference and kernel values for time
      time = calc_time(times_in[i], sts)
      kernel_time = kernel_gauss(time, h_time)
      
      # Summing/ multiplying all kernel value and calculate the predicted temperature
      k_sum = kernel_distance+kernel_day+kernel_time
      k_mul = kernel_distance*kernel_day*kernel_time
      temps_sum[i] = sts$air_temperature%*% k_sum / sum(k_sum)
      temps_mul[i] = sts$air_temperature%*% k_mul /sum(k_mul)
    }

  #Plot and print result
  plot(temps_sum, type="o")
  print("With summerizing:")
  print(temps_sum)
  plot(temps_mul, type="o")  
  print("With multiplying:")
  print(temps_mul)
}



# Inputs from user
lat <- 58.4274
long <- 14.826
date <- "2013-11-04"
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")

# Give predictions
predict_temp(lat, long, date, times, st)
