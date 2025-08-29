getwd()
setwd("D:\\SLIIT\\Y2S1\\Modules\\IT2120 - Probability and Statistics\\Lab Submissions\\Week 05\\IT24102798")
getwd()


# Question 01

data <- read.table("Exercise - Lab 05.txt", header = TRUE, sep = ",")
Delivery_Times <- data.frame(data)
names(Delivery_Times) <- c("Delivery_Time")
attach(Delivery_Times)


# Question 02

str(Delivery_Times)
histogram_for_delivery_times <- hist(Delivery_Time, 
                                     main = "Histogram for Delivery Times", 
                                     xlab = "Delivery Times", 
                                     ylab = "Frequency", 
                                     breaks = seq(20, 70, length = 10), 
                                     right = FALSE)


# Question 03

# The 4th bar has the most frequency, which is 8 while the 1st and 9th bars have the least frequency, which is 2.
# The graph is slightly skewed to the left side.


# Question 04

breaks <- round(histogram_for_delivery_times$breaks)
freq <- histogram_for_delivery_times$counts
mids <- histogram_for_delivery_times$mids

classes <- c()

for (i in 1:(length(breaks) - 1)) {
  classes[i] <- paste0("[", breaks[i], ",", breaks[i + 1], ")")
}

cbind(Classes = classes, Frequency = freq)

lines(mids, freq)

plot(mids, freq, type = 'l', main = "Frequency Polygon for Delivery Times", xlab = "Delivery Time", ylab = "Frequency", ylim = c(0, max(freq)))

cum.freq <- cumsum(freq)

new <- c()

for(i in i:length(breaks)) {
  if (i == 1) {
    new[i] = 0
  } else {
    new[i] = cum.freq[i-1]
  }
}

plot(breaks, new, type = 'l', main = "Cumulative Frequency Polygon for Delivery Times", xlab = "Delivery Time", ylab = "Cumulative Frequency", ylim = c(0, max(cum.freq)))

cbind(Upper = breaks, CumFreq = new)

