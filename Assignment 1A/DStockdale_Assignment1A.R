# Data and Decision Making Assignment 1

# Declan Stockdale

# 36109 Assignment 1 A: Monte Carlo simulation of a project


# remove all variables
rm(list=ls())

# set seed
set.seed(1)

# set up task dataframe using values in brief
task_id <- c('min', 'ml', 'max')
task1 <-c(2,3,5)
task2_Internal <- c(8,10,16)
task2_Vendor <- c(7,9,13)
task3 <- c(8,9,14)

# Assign column names after dataframe creation
task_df <- data.frame(rbind(task1,task2_Internal,task2_Vendor, task3))
colnames(task_df) <- task_id

# Number of simulations
n = 1000

# Create matrix of size n by 3 
# Extra col for task 4, left empty for now, populated in if loop
time_sim <- as.data.frame(matrix(nrow=n, ncol= 1+(nrow(task_df))))


# Function for inverse triangle distribution
# Repurposed from 05 - monte carlo cost and time estimation of a project - exercise
# Written by Dr Kailash Awati

inv_triangle_cdf <- function(P, vmin, vml, vmax){
  
  Pvml <- (vml-vmin)/(vmax-vmin)
  
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}


# Take random variable, if over 0.75 = 2 else = 3
task_4_prob <- function(P){
  return(ifelse(P <=0.75,2, 3))
}

# loop over each row
for (i in 0:nrow(task_df)+1){
  
  #generate n random numbers (one per trial)
  psim <- runif(n)
  
  # from task df with 3 known values
  
  if (i<nrow(task_df)+1){
    
    #set task time
    vmin <- task_df$min[i]
    vml <- task_df$ml[i]
    vmax <- task_df$max[i]
    
    #simulate n instances of task
    time_sim[,i] <- inv_triangle_cdf(psim,vmin,vml,vmax)
    
    # for last column using only probabilities
  } else {time_sim[,i] <-task_4_prob(psim)}
  
}

# Add up all simulated time lengths. Ifelse selects max from parallel task
time_tot <- time_sim[,1] + 
  ifelse(time_sim[,2]>time_sim[,3], time_sim[,2], time_sim[,3]) +
  time_sim[,4]+
  time_sim[,5]

# save output to csv
task_2_max <- ifelse(time_sim[,2]>time_sim[,3], time_sim[,2], time_sim[,3])
csv_output <- as.data.frame(cbind(time_sim[,1:3],task_2_max, time_sim[,4:5], time_tot))
colnames(csv_output) <- c("task1", 'task2_internal', 'task2_vendor', 'task2_max', 'task3', 'task4', 'total')
write.csv(csv_output,"C:/Users/Declan/Desktop/MDSI/2022/Spring/Data and Decision Making - 36109/11214549_DStockdale_Assignment1A.csv", row.names = FALSE)


#### Create plots 

#Creat hisogram and probability curve
myhist <-hist(time_tot)

multiplier <- myhist$counts / myhist$density
mydensity <- density(time_tot)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist,
     col=rgb(0,0,1,0.5),
     main="Monte Carlo distribution for project timeframe",
     xlab="Simulated number of days for project completion", 
     ylab="Frequency")

lines(mydensity,lwd = 4)


# stats on simulated data
#mean(time_tot)
#max(time_tot)
#min(time_tot)
#median(time_tot)

# Plot cumulative distribution curve
plot(ecdf(time_tot),
     main="Cumulative Distribution of simulated Project time frame",
     xlab="Days to complete", 
     ylab="Probability")



### Generate dataframe of values % percent completed for each day

df_1 <- data.frame(matrix(ncol = 2, nrow = (ceiling(max(time_tot))-floor(min(time_tot)))))
x <- c("Day", "Probability of completion")
colnames(df_1) <- x

for (i in floor(min(time_tot)):ceiling(max(time_tot))){
  df_1[i,1] <- i
  df_1[i,2] <- 100*ecdf(time_tot)(i)
}
na.omit(df_1)

### Genearte datafrme for days corresponding for each % 
df_2 <- data.frame(matrix(ncol = 3, nrow = 10))
colnames(df_2) <- c('Probability %','Number of days')

for (i in (1:10)){
  
  df_2[i,1] <- i*10
  df_2[i,2] <- quantile(ecdf(time_tot),(i/10),type=7)
  df_2[i,3] <- ceiling(quantile(ecdf(time_tot),(i/10),type=7))
}
na.omit(df_2)


################
# simulate late project at earlier stage makes later stages also late

# Idea 1. Create correlated distributions - can't get to work
# Idea 2. If random number > Pvml random number should be re generated as 1- random number

# generate 1000 new samples
n<- 1000

# Generate new matrix
time_sim_correlated <- as.data.frame(matrix(nrow=n, ncol= 1+(nrow(task_df))))


# loop over each row
for (i in 0:nrow(task_df)+1){

  #generate n random numbers (one per trial)
  p_sim_correlated <- runif(n)
  time_sim_correlated[,i] <- p_sim_correlated
}


# check to see if previous column in time_sim_correlated is over pvml

# loop over rows
for (i in 1:nrow(time_sim_correlated)){ 
  
  # for each column in row starting at second column
  for (j in 3:ncol(time_sim_correlated)-1){ 
    
    vmin <- task_df$min[j]
    vml <- task_df$ml[j]
    vmax <- task_df$max[j]
    # calculate Pvml
    Pmvl <- (vml-vmin)/(vmax-vmin)
    
    # if  previous column less than most likely keep
    if (time_sim_correlated[i,j-1] < Pmvl){
      next
    }
    # if more than most likely - rerun runif to values above Pvml
    else {
      
      min_range <- (task_df$max[j]-task_df$ml[j])/
        (task_df$max[j]-task_df$min[j])
      
      time_sim_correlated[i,j] <- runif(1,
                                        min <-min_range,
                                        max = 1)
      
    }
  }}

# copy in case new R reads dataframe at intial stage rather after updating values 
# important as we rely of previous columns to determine if new value needs to be created for current column
time_sim_correlated_copy <- time_sim_correlated
for (i in 0:nrow(task_df)+1){
  
  #generate n random numbers (one per trial)
  #psim <- runif(n)
  
  # from task df with 3 known values
  
  if (i<nrow(task_df)+1){
    
    #set task time
    vmin <- task_df$min[i]
    vml <- task_df$ml[i]
    vmax <- task_df$max[i]
    
    #simulate n instances of task
    time_sim_correlated_copy[,i] <- inv_triangle_cdf(time_sim_correlated[,i],vmin,vml,vmax)
    
    # for last column using only probabilities
  } else {time_sim_correlated_copy[,i] <-task_4_prob(time_sim_correlated[,i])}
  
}

# Add up all simulated time lengths. Ifelse selects max from parallel task
time_tot_correlated <- time_sim_correlated_copy[,1] + 
  ifelse(time_sim_correlated_copy[,2]>time_sim_correlated_copy[,3], time_sim_correlated_copy[,2], time_sim_correlated_copy[,3]) +
  time_sim_correlated_copy[,4]+
  time_sim_correlated_copy[,5]

#### Generate plots from correlated data

#Creat histogram and probability curve
myhist <-hist(time_tot_correlated)


plot(myhist,
     col=rgb(0,0,1,0.5),
     main="MC distribution for project timeframe",
     xlab="Simulated number of days for project completion", 
     ylab="Frequency")


# Plot cumulative distribution curve
plot(ecdf(time_tot_correlated),
     main="Late - Cumulative Distribution of simulated Project time frame",
     xlab="Days to complete", 
     ylab="Probability")


### Generate dataframe of values % percent completed for each day

df_3 <- data.frame(matrix(ncol = 2, nrow = (ceiling(max(time_tot_correlated))-floor(min(time_tot_correlated)))))
x <- c("Day", "Probability of completion")
colnames(df_3) <- x

for (i in floor(min(time_tot_correlated)):ceiling(max(time_tot_correlated))){
  df_3[i,1] <- i
  df_3[i,2] <- 100*ecdf(time_tot_correlated)(i)
}
na.omit(df_3)

### Genearte datafrme for days corresponding for each % 
df_4 <- data.frame(matrix(ncol = 3, nrow = 10))
colnames(df_4) <- c('Probability %','Number of days')

for (i in (1:10)){
  
  df_4[i,1] <- i*10
  df_4[i,2] <- quantile(ecdf(time_tot_correlated),(i/10),type=7)
  df_4[i,3] <- ceiling(quantile(ecdf(time_tot_correlated),(i/10),type=7))
}
na.omit(df_4)


# save output to csv
task_2_max_corr <- ifelse(time_sim_correlated_copy[,2]>time_sim_correlated_copy[,3], time_sim_correlated_copy[,2], time_sim_correlated_copy[,3])

csv_output_corr <- as.data.frame(cbind(time_sim_correlated_copy[,1:3],task_2_max_corr, time_sim_correlated_copy[,4:5], time_tot))
colnames(csv_output_corr) <- c("task1", 'task2_internal', 'task2_vendor', 'task2_max', 'task3', 'task4', 'total')
write.csv(csv_output_corr,"C:/Users/Declan/Desktop/MDSI/2022/Spring/Data and Decision Making - 36109/11214549_DStockdale_Assignment1A_late.csv", row.names = FALSE)
