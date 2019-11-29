#################### Easy Test ####################

# Load these libraries

library(microbenchmark)
library(PeakSegDP)
library(PeakSegOptimal)
library(ggplot2)

# Taking input to create user friendly data set 

S <- as.integer(readline(prompt="Enter size of Data set: "))
N <- as.integer(readline(prompt="Enter number of Data elements in each set: "))

# Data frame for rpois() values

df <- data.frame(matrix(ncol = S, nrow = N))
creating_dataset <- function(N, S) {
  for (i in 1:S) {
    df[, i] <<- rpois(N,50) # Assuming 50 as vector of (non-negative) means
  }
}

# Two empty variables to hold computation time

cdpa <- c()
pdpa <- c()

# Function to calculate computation time of cDPA() and PeakSegPDPA()

computation_time <- function(N, S) {
  # for() loop used to compute each set computation time
  for (i in 1:S) {
    value_cdpa <- cDPA(df[, i], maxSegments = 2L)
    value_pdpa <- PeakSegPDPA(df[, i], max.segments = 2L)
    mbm <- microbenchmark(value_cdpa, value_pdpa, times = 1000L)
    execution_time <- summary(mbm)$mean      # Extracting computation time
    cdpa <<- append(cdpa, execution_time[1]) # Storing extracted computation time 
    pdpa <<- append(pdpa, execution_time[2]) # Storing extracted computation time 
  }
}

# Function for plotting graph of PeakSegDP::cDPA versus PeakSegOptimal::PeakSegPDPA

computation_graph <- function(S) {
coord <- data.frame(cdpa, pdpa)    # Data frame for co-ordinates of cdpa and pdpa
data_set_num <- c(1:S)
ggplot(coord, aes(data_set_num)) + 
  geom_line(aes(y = cdpa, colour = "cDPA")) + 
  geom_line(aes(y = pdpa, colour = "PeakSegPDPA")) +
  ggtitle("PeakSegDP::cDPA versus PeakSegOptimal::PeakSegPDPA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_log10(breaks = seq(0, S, by = 5)) + 
  scale_y_log10(breaks = seq(0, 1000, by = 2)) + 
  xlab("Data Set Number") + labs(colour = "Function Comparison") + 
  ylab("Execution Time (in Nanoseconds)") + 
  theme_bw(base_size = 15)
}
