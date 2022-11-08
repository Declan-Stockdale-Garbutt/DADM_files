## Assignment 2 Declan Stockdale

# 5 year strategy for mining company


#install.packages("plyr")          # Install plyr package
library("plyr") 

#Setup / Intro ====

# This code is used to simulate a risk assessment for a mining operation
# It will use a monte carlo simulation of 10,000 'universes' 
# The goal is to survive, not necessarily be profitable



#Start defining values ====

# starting funds
start_funds = 2200000

# starting price of gold 
gold_init_cost = 1800 # AUD

# gold price changes by fixed value from distribution below each year
AU_price_chage <- c(-250,200, 500)

# New claim_output
mine_output <- c(100,1000, 1800)

# yearly change (15% decrease -> 0.85 of original, 30% increase  = 1.3, 70% increase  = 1.7)
yearly_change <- c(0.85, 1.30,1.70)

# yearly operation costs
operational_costs <-c(500000, 750000, 1000000)

# expenses is 32% of gold sold each year

# legal challenge
legal_challenge <- c(500000, 750000,1000000)

# chance to win legal is 50%. If won, business as usual, if lst, keep revenue for eyar but none else

# New discovery 
chance_of_discovery = 0.15

# cost additional_teams 
additional_teams_cost = 400000

# Creating functions ====

# Function to run triangular distribution given min, most likely and max
inv_triangle_cdf <- function(vmin, vml, vmax){
  
  # Generate random number between 0 and 1
  P = runif(1)
  
  # Calculate  Prob of most likley (Pmvl) converting other values to range between 0 and 1
  Pvml <- (vml-vmin)/(vmax-vmin)
  
  # Calculate prob (P) for random value using relevant formula depending on P > or < than Pvml
  return(ifelse(P < Pvml,
                vmin + sqrt(P*(vml-vmin)*(vmax-vmin)),
                vmax - sqrt((1-P)*(vmax-vml)*(vmax-vmin))))
}


# The below function determines the legal outcomes of each year
# Pseudo code
# Check if case has already been resolved
# if yes, add resolved and results. Legal fees set to 0
# If not, roll for challenge (delay or challenge 50%) roll to determine legal fees
# If delay, add unresolved and delay to output
# If we decide to challenge, change return resolved and outcome
# return legal costs, whether case is resolved and case outcome if applicable

legal_proceedings <- function(resolved,success){
  
  # Is legal resolved if is then skip everything else
  if (resolved == 'resolved') { 
    resolved == 'unresolved'
  }else{
    
    # case is unresolved  - delay or challenge?
    
    # Roll to see costs for year
    legal_costs = inv_triangle_cdf(500000, 750000,1000000) # using fixed values as they never change
    
    # Roll to see if we challenge the legal outcome or delay
    P_challenge = runif(1)
    
    # Challenge or delay P-challenge larger than 0.5 we challenge
    
    # Challenge
    if (P_challenge >=0.90){
      
      P_success = runif(1)

      # We win
      if (P_success >= 0.5){
        resolved = 'resolved'
        success = 'win'
      }  else{# we lost
        
        resolved = 'resolved'
        success = 'loss'
      }
      
    } else{ # Delaying the case for another year
      
      resolved = 'unresolved'
      success = 'delayed'}
    
  }
  
  return(c(as.numeric(legal_costs), resolved,success))
}

# Function to determine mine outcome
# in this case year 4 is the year 5 of operations due to nature of the main simulation strategy used 

# Psuedo code
# First check if mine has been discovered, if yes, set team expenses to 0
# If not, calculate the maximum number of teams we can afford using current funds - gold income (-32%), operationing costs and legal fees
# If original mine was lost in the legal battle, we need the maximum to find the new mine
# Also don't search in last year of operations as it won't be operational 
# Once number of teams has been found, roll for each team
# If any team finds it, break out of the look
# return the mine discovery result, team expenses, and number of teams used

mine_discovery <- function(current_expenses,legal_fees,operational_costs, gold_price, mine_output, mine_discovered,success,year){
  

  # mine has been discovered no need to look for it
  if (mine_discovered == 'found'){
    team_expenses = 0
    num_exploratory_teams = 0
    mine_discovered = 'found'

  } else{ # mine not discovered yet
    
    
    # How many additional teams can be afforded
    
    # if no operational costs, mine output has to be 0 as it wasn't profitable
    if (operational_costs <=1){
      mine_output = 0
    }
    
    # find available funds after accounting for expenses and mine income if any
    available_funds <- current_expenses +(gold_price*mine_output*0.68) - legal_fees- operational_costs 
    
    # If above is less than 400,000 we can't anyone. Set teams to 1 (the budgeted team)
    if (as.numeric(available_funds) < as.numeric(400000)){
      num_exploratory_teams = 1 
      
      
    }else { # we can afford at least one team
      
      # available funds / 400,000 +1 for budgeted team
      num_potential_available_teams <- floor(as.numeric(available_funds)/additional_teams_cost) +1
      
      # Need as many teams as possible if original mine not owned and additional mine not found
      if (success == 'loss' & year != 4){
        
        # need to find new mine ASAP. Go all in with all funds
        num_exploratory_teams = num_potential_available_teams  
        
      }
      
      else{ # randomly choose number of teams
        
        if ( year == 4){ # last year, don't hire anyone as mine won't be operational in last year
          num_exploratory_teams = 1
        }else{ # hire random number between 1 and maximum potential teams
          
          num_exploratory_teams <- sample(1:num_potential_available_teams, 1, replace=TRUE)
        }
        
        
      }
      
    }
    # found how many teams to use, find expenses
    team_expenses = as.numeric((num_exploratory_teams-1)*additional_teams_cost)
    
    # roll to see if ay find the mine, if they do then break loop
    for (team in 1:num_exploratory_teams){
      prob_found = runif(1)
      # 15% change for each team to find, roll for each team
      if (prob_found <= 0.15){
        mine_discovered = 'found'
        break
      }
    }
    
  }
  return(c(mine_discovered,team_expenses,num_exploratory_teams))
}



# Creating table ====
#rows
number_simulations = 10000

# Set column names for each year

col_names <- c('yr_1_funds','yr_1_gold_price_change +/- $', 'yr_1_gold$', 'yr_1_mine_output_change %', 'yr_1_mine_output', 'yr_1_operational_costs','yr_1_legal_costs','yr_1_legal_challenge','yr_1_success','yr_1_mine_discovery','yr_1_team_expenses','yr_1_expenses','yr_1_mines_operating','yr_1_earnings','yr_1_final',
               'yr_2_funds','yr_2_gold_price_change +/- $', 'yr_2_gold$', 'yr_2_mine_output_change %', 'yr_2_mine_output', 'yr_2_operational_costs','yr_2_legal_costs','yr_2_legal_challenge','yr_2_success','yr_2_mine_discovery','yr_2_team_expenses','yr_2_expenses','yr_2_mines_operating','yr_2_earnings','yr_2_final',
               'yr_3_funds','yr_3_gold_price_change +/- $', 'yr_3_gold$', 'yr_3_mine_output_change %', 'yr_3_mine_output', 'yr_3_operational_costs','yr_3_legal_costs','yr_3_legal_challenge','yr_3_success','yr_3_mine_discovery','yr_3_team_expenses','yr_3_expenses','yr_3_mines_operating','yr_3_earnings','yr_3_final',
               'yr_4_funds','yr_4_gold_price_change +/- $', 'yr_4_gold$', 'yr_4_mine_output_change %', 'yr_4_mine_output', 'yr_4_operational_costs','yr_4_legal_costs','yr_4_legal_challenge','yr_4_success','yr_4_mine_discovery','yr_4_team_expenses','yr_4_expenses','yr_4_mines_operating','yr_4_earnings','yr_4_final',
               'yr_5_funds','yr_5_gold_price_change +/- $', 'yr_5_gold$', 'yr_5_mine_output_change %', 'yr_5_mine_output', 'yr_5_operational_costs','yr_5_legal_costs','yr_5_legal_challenge','yr_5_success','yr_5_mine_discovery','yr_5_team_expenses','yr_5_expenses','yr_5_mines_operating','yr_5_earnings','yr_5_final')#,


simulation <- as.data.frame(matrix(nrow=number_simulations, ncol= (length(col_names))))
colnames(simulation) <- col_names

# set seed
set.seed(1)

# Start simulation ====
# [row, column]

for (i in 1:nrow(simulation)){

  # set starting variables
  mine_discovered <- 'unfound'
  resolved <- 'unresolved'
  success <- 'unknown' # changes once we decide to challenge
  mine_operational <- 1 # the mine we start with
  
  # starting funds
  simulation[i,1] = start_funds
  
  # starting gold price, change is 0
  simulation[i,2] = 0 
  simulation[i,3] = as.numeric(gold_init_cost)
  
  
  #mine output change is 0
  simulation[i,4] = 0
  simulation[i,5] = inv_triangle_cdf(mine_output[1], mine_output[2],mine_output[3])
  
  # operating costs
  simulation[i,6] = inv_triangle_cdf(operational_costs[1], operational_costs[2], operational_costs[3])
  
  legal_results = legal_proceedings(resolved,success)
  
  # legal costs
  simulation[i,7] = as.numeric(legal_results[1])
  
  # resolved?
  resolved = legal_results[2]
  simulation[i,8] = resolved
  
  #success?
  success = legal_results[3]
  simulation[i,9] = success
  

  # check if profitable to operate mine (mine output * gold price *0.68) > operating costs
  if (simulation[i,3] * simulation[i,5] * 0.68 < simulation[i,6]){
    # not profitable to mine
    
    # no operating costs
    simulation[i,6] = 0
    
    # no earnings
    simulation[i,13] = 0 # mines operational first year
    simulation[i,14] = 0 # income

    
  } else{ # mine ws profitable
    
    # calculate money generated 
    simulation[i,13] = mine_operational # 1
    simulation[i,14] = (simulation[i,3]*simulation[i,5])*0.68  # money generated
    
  }
  
  mine_results = mine_discovery(simulation[i,1],simulation[i,7], simulation[i,6], simulation[i,3],simulation[i,5], mine_discovered, success,1)
 
  # mine discovered?
  mine_discovered = mine_results[1]
  simulation[i,10] = mine_discovered
  
  # team expenses
  simulation[i,11] = as.numeric(mine_results[2])
  
  
  #expenses (operating costs, legal and mine search team)
  simulation[i,12] = simulation[i,6]+simulation[i,7]+simulation[i,11]

  # Total funds at end of year (initial cash + income - expenses)
  simulation[i,15] =  simulation[i,1]+simulation[i,14]-simulation[i,12]
  
  # new
  for (year in 1:4){ # each year is technically the year after. It adds a multiple of 15 as there are 15 column per year
    
    # set current funds to funds in last column 1+ year*15
    simulation[i,1+15*year] <- simulation[i,15*year]
    
    # changes gold price according to triangular distribution using previous years value
    simulation[i,2+15*year] <- inv_triangle_cdf(AU_price_chage[1], AU_price_chage[2],AU_price_chage[3])
    simulation[i,3+15*year] <- simulation[i,3+15*(year-1)] + simulation[i,2+15*year]
    
    # Calculate mine output using triangular distribution using previous value from last year
    simulation[i,4+15*year] <- inv_triangle_cdf(yearly_change[1], yearly_change[2],yearly_change[3])
    simulation[i,5+15*year] <- simulation[i,5+15*(year-1)] * simulation[i,4+15*year]
    
    # operational costs, random each year, need to generate even if mine isn't profitable
    simulation[i,6+15*year] = inv_triangle_cdf(operational_costs[1], operational_costs[2], operational_costs[3])
    
    # check if mine profitable
    
    if (simulation[i,3+15*year] * simulation[i,5+15*year] * 0.68 < simulation[i,6+15*year]){
      # not profitable to mine
      
      # no operating costs
      simulation[i,6+15*year] = 0

    } else{ # mine was profitable,  calculate operating costs - this may be wrong as I might be re rolling for a value
      simulation[i,6+15*year] = inv_triangle_cdf(operational_costs[1], operational_costs[2], operational_costs[3])
      #simulation[i,6+15*year] <- simulation[i,6+15*year]
    }
    
    # Deal with legal possibilities
    
    if (resolved == 'resolved'){ # There are no legal fees, no change for rest of 5 year plan
      legal_fees = 0
      simulation[i,7+15*year] <- legal_fees
      simulation[i,8+15*year] <- resolved
      simulation[i,9+15*year] <- success
      
    }else{ # case in unresolved
      
      legal_results <-legal_proceedings(resolved, success)
      
      # legal fees 
      legal_fees <- as.numeric(legal_results[1])
      simulation[i,7+15*year] <- legal_fees
      
      # did we resolve or delay?
      resolved = legal_results[2]
      simulation[i,8+15*year] <- resolved
      
      # Win, loss or delay?
      success = legal_results[3]
      simulation[i,9+15*year] <- success
      
    } 
    
    # check all mining possibilities
    
    if (mine_discovered == 'found'){ # mine found
      
      # keep value for subsequent years
      simulation[i,10+15*year] <- mine_discovered
      
      # no team fees
      team_fees = 0
      simulation[i,11+15*year] <- team_fees
      
      
    } else{# decide how many teams to look for mine
      
      mine_discovery_results <-mine_discovery(simulation[i,1+15*year], simulation[i,7+15*year],simulation[i,6+15*year],simulation[i,4+15*year], simulation[i,5+15*year],mine_discovered, success, year)
      
     # found or unfound for additional mine
      mine_discovered = mine_discovery_results[1]
      simulation[i,10+15*year] <- mine_discovered
      
      # get fees for additional teams
      team_fees = as.numeric(mine_discovery_results[2])
      simulation[i,11+15*year] <- team_fees
      
    } # end of mine discovered check
    
    
    # calculate expenses - operating costs, legal, team fees

    # Do we own the original mine?
    if (simulation[i,9+15*year] == 'loss' & simulation[i,9+15*(year-1)] == 'loss'){# Check two loss if a row
      # we don't own original mine
      mine_1_operation_costs = 0
      mine_1_income = 0
      mine_1_operating = 0
      
    }else{# we still own mine
      
      mine_1_operation_costs <- simulation[i,6+15*year]
      
      if (mine_1_operation_costs == 0){# Mine 1 was not profitable and not run this year, set relevant values
        mine_1_income  = 0
        mine_1_operating = 0
        
      }else{ # Mine 1 was profitable and did run
        mine_1_operation_costs <- simulation[i,6+15*year]
        mine_1_income <- simulation[i,3+15*year]*simulation[i,5+15*year]*0.68
        simulation[i,13+15*year] = mine_operational
        mine_1_operating  =1
      }
      
    }
    
    # Check if new mine is operational, found occurs twice, not operational year of finding, only subsequent years
    if (simulation[i,10+15*year] == 'found' & simulation[i,10+15*(year-1)] == 'found'){
      
      # same operating costs as mine 1
      mine_2_operation_costs <- simulation[i,6+15*year]
      
      
      if (mine_2_operation_costs == 0){ # mine not profitable, set relevant values
        
        mine_2_income  = 0
        simulation[i,13+15*year] = 0
        mine_2_operating = 0
        
      }else{ # mine was profitable, set relevant values
        mine_2_operation_costs <- simulation[i,6+15*year]
        mine_2_income <- simulation[i,3+15*year]*simulation[i,5+15*year]*0.68
        
        simulation[i,13+15*year] = 2
        
        mine_2_operating = 1
      }

      
    }else {# not found, set values to 0
      mine_2_operation_costs = 0
      mine_2_income = 0
      mine_2_operating = 0
    }
    
    # add metrics for both mines together and other expenses
    expenses_total <- legal_fees+ mine_1_operation_costs+mine_2_operation_costs+team_fees
    simulation[i,12+15*year] <- expenses_total
    
    # 0, 1, or 2 mines operational
    mine_operating <- mine_1_operating + mine_2_operating
    simulation[i,13+15*year] <- mine_operating    
    
    # mines income from both mines
    mine_revenue <- mine_1_income+mine_2_income
    simulation[i,14+15*year] <- mine_revenue
    
    # calculate end of year funds
    end_of_year_funds <- simulation[i,1+15*year]+mine_revenue - expenses_total
    simulation[i,15+15*year] <- end_of_year_funds
    
  }
}

# Additional analysis ====
# never bankrupt
simulation$outcome_1 <- c('good')
# had at elast 1 mine at end of last year
simulation$outcome_2 <- c('good_luck')


year_1_bankrupt = 0
year_2_bankrupt = 0
year_3_bankrupt = 0
year_4_bankrupt = 0
year_5_bankrupt = 0

for (i in 1:nrow(simulation)){
  for (year in 0:4){
    
    # if any year finished with negative value, its bankrupt even if later years generate revenue
    if (simulation[i,15 + 15*year] <= 0){
      simulation[i,76] = 'bankrupt'
      
    # check mine situation in last year, if no mines, set bad luck (lost legal and didn't find new mine)
    if (year == 4){
      if (simulation[i,10 + 15 * year] == 'unfound' & (simulation[i,9 + 15 * year] == 'loss')){
        simulation[i,77] = 'bad_luck'
        
      }
      
    }
  }
}
}

# Use above bankrupt and bad luck to determine if simulation was successful 
simulation$successful_project <- 'successful project'
for (i in 1:nrow(simulation)){
  if (simulation[i,76] == 'bankrupt' || simulation[i,77] == 'bad_luck' || simulation[i,75] < simulation[i,1]){
    simulation[i,78] <- 'failed project'
  }
  
}


# Add summary to final dataframe

# Default values
simulation$teams_cost <- 0
simulation$year_mine_found <- 'never'
simulation$year_legal_resolved <- 'never'
simulation$legal_loss <- 'never'


# Loop over simulation dataframe to find total team costs and year mine found
for (i in 1:nrow(simulation)){
  
  # set initial values 
  teams_cost = 0
  number_years_no_revenue = 0
  
  for (year in 0:4){
    
    # Running total of teams cost
    teams_cost = teams_cost + simulation[i,11 + 15*year]
    simulation[i,79] = teams_cost


    # change myear mine found, break if it is ever found as future values will also be found and teams cost will be the same
    if (simulation[i,10 + 15*year] == 'found'){
      simulation[i,80] = year+1
      break
    }   
  }
  
}

# Loop over simulation dataframe to find year legal resolved and result
for (i in 1:nrow(simulation)){
  
  for (year in 0:4){

    # change when legal dispute was resolved
    if (simulation[i,8 + 15*year] == 'resolved'){
      simulation[i,81] = year+1

      
      # Change the result so its easier for final analysis then break out of loop
      if (simulation[i,9 + 15*year] == 'loss'){
        simulation[i,82] = 'True'
      }
      
      break
    }
  }
}

# set initial value of years mine was profitable
simulation$number_years_no_revenue <-0

for (i in 1:nrow(simulation)){
  number_years_no_revenue = 0
  
  for (year in 0:4){
    
    # find when operating costs were less than 1, got an error with it set to 0
    # it any column for that row satisfies that condition, add 1 to number of years no revenue
    if(simulation[i,6 + 15*year] < 1){
      
      number_years_no_revenue = number_years_no_revenue+1
      simulation[i,83] = number_years_no_revenue
    }
  }
}
    

# Write to csv ====
write.csv(simulation,"C:/Users/Declan/Desktop/assignment_2_results.csv", row.names = FALSE)



#### Analysis ====

###### Summary stats -0 unused in report
min(simulation$yr_5_final)
max(simulation$yr_5_final)
median(simulation$yr_5_final)
mean(simulation$yr_5_final)
sd(simulation$yr_5_final)



##### Figure 1 in report ====
# Plot of histogram and CDF

options(scipen=999)

par(mar = c(5,5,2,5))
set.seed(15)
h <- hist(
  simulation$yr_5_final,breaks = 50,
  main="Simulated distribution for project financials after 5 years",
  xlab="Simulated finances after 5 years", 
  ylab="Frequency",
  xlim= (c(-3E6,30E6)),
  col='sky blue')

par(new = T)

ec <- ecdf(simulation$yr_5_final)
plot(x = h$mids, y=ec(h$mids)*max(h$counts), col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
lines(x = h$mids, y=ec(h$mids)*max(h$counts), col ='black')
axis(4, at=seq(from = 0, to = max(h$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'black', col.axis = 'black')
mtext(side = 4, line = 3, 'Cumulative Density', col = 'black')
####

######### Table 1 in report ====
#  Quantiles df 
quantile_df <- data.frame(matrix(ncol = 2, nrow = 10))
colnames(quantile_df) <- c('Probability %','Funds after 5 years')


for (i in (1:10)){
  
  quantile_df[i,1] <- i*10
  quantile_df[i,2] <- quantile(ecdf(simulation$yr_5_final),(1-i/10),type=7)
  # quantile_df[i,2] <- ceiling(quantile(ecdf(simulation$yr_5_final),(i/10),type=7))
}
quantile_df
####



############ how many resulted in failed projects using 3 characteristics - unused in report
# no mines, never bankrupt and earned mre than started with
q1_b <- table((simulation[,78]))
percent_neg <- 100 * q1_b[1]/(q1_b[1]+q1_b[2])
percent_neg

percent_pos <- 100-percent_neg
percent_pos

##### Table 2 in report ====
# Find summary stats using characteristics
summary(simulation[simulation$successful_project == 'successful project', 'yr_5_final'])
####

##### Figure 2 in report ====
#Failed mine - mine output
failed_hist_mine_output <-hist(simulation[simulation$successful_project =="failed project",]$yr_1_mine_output)

plot(failed_hist_mine_output,
     col=rgb(1,0,0,0.5),
     main="Year 1 mine output for failed simulations",
     xlab="mine output (ounces)", 
     ylab="Frequency")
#### 

###### Table 3 in report ====
# summary stats of mine output for successful and failed projects
summary(simulation[simulation$successful_project =="successful project",]$yr_1_mine_output)
summary(simulation[simulation$successful_project =="failed project",]$yr_1_mine_output)
#### 

##### Figure 3 in report ==== 
# Successful mine - mine output
success_hist_mine_output<-hist(simulation[simulation$successful_project =="successful project",]$yr_1_mine_output)

par(mar=c(4, 4, 4, 4))

plot(success_hist_mine_output,
     col=rgb(0,0,1,0.5),
     main="Year 1 mine output for successful simulations",
     xlab="mine output (ounces)", 
     ylab="Frequency")
#### 

#### Mine output claim justification ====
# what happened to failed mines that had initial outputs higher than the successful ones? Wy made them fail?
failed_mine_above_avg <-subset(simulation, successful_project == "failed project" & yr_1_mine_output>= 1033)

# % of failed mines that lost legal dispute
100*length(which(failed_mine_above_avg$legal_loss == 'True'))/nrow(failed_mine_above_avg)
table(failed_mine_above_avg$year_legal_resolved)

# % that never found additional mine
100*length(which(failed_mine_above_avg$year_mine_found == 'never'))/nrow(failed_mine_above_avg)
table(failed_mine_above_avg$year_mine_found)
####

# look at successful mines
successful_mines <-subset(simulation, successful_project == "successful project")
#table(successful_mines$year_legal_resolved)/nrow(successful_mines)
#table(successful_mines$legal_loss == 'True')/nrow(successful_mines)
#table(successful_mines$year_mine_found)/nrow(successful_mines)


######## Table 5 in report ====
summary(simulation[simulation$year_legal_resolved ==1,]$yr_5_final)
boxplot(simulation[simulation$year_legal_resolved ==1,]$yr_5_final)


summary(simulation[simulation$year_legal_resolved =='never',]$yr_5_final)
boxplot(simulation[simulation$year_legal_resolved =='never',]$yr_5_final)
####




########## Appendix Table 4 in report ====
# find how many simulations are successful and legal is lost for each year
successful_mine_legal_loss <- subset(successful_mines, legal_loss == "True")
success_loss_legal_found_mine <- data.frame(matrix(ncol = 6, nrow = 5))
colnames(success_loss_legal_found_mine) <- c('Yr lost legal %','yr found mine')

for (i in (1:5)){
  for (j in (1:6)){
    success_loss_legal_found_mine[i,j] = 0
    if (j <6){
    success_loss_legal_found_mine[i,j]<-length(which(successful_mine_legal_loss$year_legal_resolved == i & successful_mine_legal_loss$year_mine_found  == j))
    }else{
      
      for (num in (1:5)){
        success_loss_legal_found_mine[i,j] <-success_loss_legal_found_mine[i,j]+success_loss_legal_found_mine[i,num]
      }
    }
  }
}
# finding percentages of above
success_loss_legal_found_mine_1<-success_loss_legal_found_mine
100*success_loss_legal_found_mine_1/success_loss_legal_found_mine_1$X6
####

######## Table 7 in report ====
# How often is mine found each year for every simulation?
table(simulation$year_mine_found)/nrow(simulation)*100


# successful mine when second mine found
table(successful_mines$year_mine_found)/nrow(successful_mines)*100

# find % for failed simulatiosn for when mine s found
failed_mine <- subset(simulation, successful_project == "failed project")
table(failed_mine$year_mine_found)/nrow(failed_mine)*100
#### 


#### Justify Impact of finding additional claim number ==== 
# average of number of teams deployed ins search of mine
mean(simulation$teams_cost)/4E5
mean(successful_mines$teams_cost)/4E5
mean(failed_mine$teams_cost)/4E5
####

##### Table 4 in report ====
# Find years when mine wasn't profitable
table(simulation$number_years_no_revenue)/nrow(simulation)*100
table(successful_mines$number_years_no_revenue)/nrow(successful_mines)*100
table(failed_mine$number_years_no_revenue)/nrow(failed_mine)*100
####

#### Table 6 in report ====
# find probability of mine success given starting 3 years of no income      
length(which(simulation$yr_1_operational_costs == 0 & simulation$yr_2_operational_costs == 0 & simulation$yr_3_operational_costs == 0 &simulation$successful_project == 'successful project'))/nrow(simulation)*100


# find probability opf mine success given starting 3 years of no income      
length(which(simulation$year_legal_resolved == 5 & simulation$legal_loss == 'True' & simulation$successful_project == 'successful project'))/nrow(simulation)*100
####

#### Appendix 3 in report ====
quantile_df1 <- data.frame(matrix(ncol = 3, nrow = 10))
colnames(quantile_df) <- c('Probability %','Funds after 5 years')


for (i in (1:10)){
  
  quantile_df1[i,1] <- i*10
  quantile_df1[i,2] <- quantile(ecdf(successful_mines$yr_1_mine_output),(1-i/10),type=7)
  quantile_df1[i,3] <- quantile(ecdf(failed_mine$yr_1_mine_output),(1-i/10),type=7)
  # quantile_df[i,2] <- ceiling(quantile(ecdf(simulation$yr_5_final),(i/10),type=7))
}
quantile_df1
####
