#creating a data frame
vent_data <- as.data.frame(matrix(data = NA, ncol = 2, nrow = 10))
colnames(vent_data) <- c("Vent", "Count")
vent_data$Vent <- c("VentA", "VentA", "VentA", "VentA", "VentA", "VentB", "VentB", "VentB", "VentB", "VentB")
vent_data$Count <- c(13,17,22,4,0,5,0,6,2,20)

#creating data strings (either works, the data frame or strings)
ventA <- c(13,17,22,4,0)
ventb <- c(5,0,6,2,20)
all <- c(ventA,ventb)

#we calculated the difference in mean
#could change mean to median or another function
#using the dataframe:
#Note [1:5] is using the first five data values for vent a etc 
RealMean <- mean(vent_data$Count[1:5]) - mean(vent_data$Count[6:10])
#using the strings:
mean(ventA)-mean(ventb)


#the random resampling
#need a vector to store the random data in:
trials <- integer()

#the loop, running 10000 times:
for (i in 1:10000) {

trial_i <- sample(all, size = 10, replace = F) #this is sampling the string and taking 10 numbers without replacement
mean_i <- mean(trial_i[1:5]) - mean(trial_i[6:10]) #same mean calculation as above

trials[i] <- mean_i #adding that random mean to the vector

}

#plotting
hist(trials)
abline (v=RealMean)

#made it a dataframe to subset from, probably an easier way to do this, but it works
trials_df <- as.data.frame(trials)

#selected the values larger than or equal to the real value
higher_than <- subset(trials_df, trials >= RealMean)
higher_than_length <- length(higher_than$trials) #counts how many values are larger or equal

higher_than_length/10000 # divide by the number of samples to get the pvalue

t.test(vent_data$Count ~ vent_data$Vent) #compare with a t test

