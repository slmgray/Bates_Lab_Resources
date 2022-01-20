
#Load Data
community_data <- read.csv("community_data.csv", header=TRUE)

cooccurence <- as.data.frame(matrix(data = NA, ncol = 9, nrow = 4))
colnames(cooccurence) <- c("species", "total_occurence", "site1", "site2", "site3", "site4", "site5", "site6", "freq")
cooccurence$species <- c("SpA", "SpB", "SpC", "SpD")
cooccurence$total_occurence <- community_data$occurence[1:4]

for(i in 1:6){

  xx <- community_data[,2+i]
  
  invert <- sum(xx[5:9])
  SpA <- xx[1]
  SpB <- xx[2]
  SpC <- xx[3]
  SpD <- xx[4]
  
  if( SpA > 0 & invert > 0){
    cooccurence[1,2+i] <- 1
  }else{
    cooccurence[1,2+i] <- 0
    }
  
  if( SpB > 0 & invert > 0){
    cooccurence[2,2+i] <- 1
  }else{
    cooccurence[2,2+i] <- 0
  }
  
  if( SpC > 0 & invert > 0){
    cooccurence[3,2+i] <- 1
  }else{
    cooccurence[3,2+i] <- 0
  }
  
  if( SpD > 0 & invert > 0){
    cooccurence[4,2+i] <- 1
  }else{
    cooccurence[4,2+i] <- 0
  }
  
}

cooccurence$freq <- rowSums(cooccurence[,3:8]) / cooccurence$total_occurence

SpC_freq <- cooccurence$freq[3]

allbut_SpC_freq <- mean(cooccurence$freq[c(1,2,4)])

True_Difference <- SpC_freq - allbut_SpC_freq

#PERMUTATION

trials <- integer()

for (j in 1:10000) {
 
 community_data_trial <- NULL
 
 community_data_trial <- as.data.frame(matrix(data = NA, ncol = 8, nrow = 9))
 colnames(community_data_trial) <- c("Trait.Group", "Species", "site1", "site2", "site3", "site4", "site5", "site6")
 community_data_trial$Trait.Group <- community_data$Trait.Group
 community_data_trial$Species <- community_data$Species
 
 for (k in 1:nrow(community_data_trial)) {

   string <- community_data[k, 3:8]
   
   community_data_trial[k, 3:8] <- sample(string, size=6, replace=FALSE)
   
 }

 cooccurence_trials <- NULL
 
 cooccurence_trials <- as.data.frame(matrix(data = NA, ncol = 9, nrow = 4))
 colnames(cooccurence_trials) <- c("species", "total_occurence", "site1", "site2", "site3", "site4", "site5", "site6", "freq")
 cooccurence_trials$species <- c("SpA", "SpB", "SpC", "SpD")
 cooccurence_trials$total_occurence <- rowSums(community_data_trial[,3:8])[1:4]
  
  for(i in 1:6){
    
    xx <- community_data_trial[,2+i]
    
    invert <- sum(xx[5:9])
    SpA <- xx[1]
    SpB <- xx[2]
    SpC <- xx[3]
    SpD <- xx[4]
    
    if( SpA > 0 & invert > 0){
      cooccurence_trials[1,2+i] <- 1
    }else{
      cooccurence_trials[1,2+i] <- 0
    }
    
    if( SpB > 0 & invert > 0){
      cooccurence_trials[2,2+i] <- 1
    }else{
      cooccurence_trials[2,2+i] <- 0
    }
    
    if( SpC > 0 & invert > 0){
      cooccurence_trials[3,2+i] <- 1
    }else{
      cooccurence_trials[3,2+i] <- 0
    }
    
    if( SpD > 0 & invert > 0){
      cooccurence_trials[4,2+i] <- 1
    }else{
      cooccurence_trials[4,2+i] <- 0
    }
    
  }
  
  cooccurence_trials$freq <- rowSums(cooccurence_trials[,3:8]) / cooccurence_trials$total_occurence
  
  SpC_freq_trial <- cooccurence_trials$freq[3]
  
  allbut_SpC_freq_trial <- mean(cooccurence_trials$freq[c(1,2,4)])
  
  trial_difference <- SpC_freq_trial - allbut_SpC_freq_trial
  
  trials[j] <- trial_difference
  
}

hist(trials)
abline (v=True_Difference)

trials_df <- as.data.frame(trials)

higher_than <- subset(trials_df, trials >= True_Difference)
higher_than_length <- length(higher_than$trials) 

higher_than_length/10000 

