options(dplyr.summarise.inform = FALSE)

# Initiate attentions (feature weights) in the first trial of each stage
initiate_att = function(stage, att_updated, C) {
  
  # Two stimuli, equal attention
  if (stage == 1) {
    f1 = 0.5
    f2 = 0.5
    f3 = NA
    f4 = NA}
  
  else {
    
    # Get the attentions from the previous trial
    f1_updated = att_updated$f1_updated
    f2_updated = att_updated$f2_updated
    f3_updated = att_updated$f3_updated
    f4_updated = att_updated$f4_updated
    
    # Reversal stages
    reversal_stages = c(5,7,9)
    if (stage %in% reversal_stages) {
      # Reversal of attentions within dimension
      f1 = f2_updated
      f2 = f1_updated
      f3 = f3_updated
      f4 = f4_updated}
    
    # Reversal stage (1 dimension)
    else if (stage == 2) {
      # Take the last value of the previous stage (i.e. from the last trial before moving to the next stage)
      f1 = f2_updated
      f2 = f1_updated
      f3 = NA
      f4 = NA}
    
    # Introduction of new stimuli
    else if (stage == 3) {
      # Mostly keep attention from known features, but allow some spreading to the new features/dimension
      f1 = (1 - C)*0.25 + C*f1_updated
      f2 = (1 - C)*0.25 + C*f2_updated
      f3 = (1 - C)*0.25
      f4 = (1 - C)*0.25}
    
    # Compound stimuli, overlapping
    else if (stage == 4) {
      # Move to the center, add uncertainty
      f1 = (1 - C)*0.25 + C*f1_updated
      f2 = (1 - C)*0.25 + C*f2_updated
      f3 = (1 - C)*0.25 + C*f3_updated
      f4 = (1 - C)*0.25 + C*f4_updated}
    
    # Intra-dimensional shift, new stimuli
    else if (stage == 6) {
      # Averaged attention of features within dimensions, increasing uncertainty
      f1 = (1 - C)*0.25 + C*(f1_updated + f2_updated)/2
      f2 = (1 - C)*0.25 + C*(f1_updated + f2_updated)/2
      f3 = (1 - C)*0.25 + C*(f3_updated + f4_updated)/2
      f4 = (1 - C)*0.25 + C*(f3_updated + f4_updated)/2}
    
    # Extra-dimensional shift, new stimuli
    else if (stage == 8) {
      # Averaged attention of features within dimensions, but reversed, increasing uncertainty
      f1 = (1 - C)*0.25 + C*(f3_updated + f4_updated)/2
      f2 = (1 - C)*0.25 + C*(f3_updated + f4_updated)/2
      f3 = (1 - C)*0.25 + C*(f1_updated + f2_updated)/2
      f4 = (1 - C)*0.25 + C*(f1_updated + f2_updated)/2}
  }
  
  return(list(f1 = f1, f2 = f2, f3 = f3, f4 = f4))
}

# Get probability for making the correct choice
get_choice_prop = function(s, t, f1, f2, f3, f4, stimulus, d, lambda) {
  
  # Make f3 and f4 0 if it's stage 1 or 2 (since the stimuli don't exist)
  if (s %in% c(1,2)) {
    f3 = 0
    f4 = 0
  }
  
  # Get decision consistency parameter
  d_param = d*exp(-lambda*(t - 1))
  
  # If stimulus 1, the stimulus value of the correct stimulusis a combination of 1 + 3
  if (stimulus == 1) {
    stim_att = f1^d_param + f3^d_param}
  
  # If stimulus 2, the stimulus value of the correct stimulus is a combination of 1 + 4
  else if (stimulus == 2) {
    stim_att = f1^d_param + f4^d_param}
  
  # Using softmax to get the probability of the correct choice
  choice_p = stim_att / (f1^d_param + f2^d_param + f3^d_param + f4^d_param)
  
  return(choice_p)
  
}

# Function to get the feedback (sigma) for f1-f4 (feature weights)
get_weights = function(s, f1, f2, f3, f4, stimulus, f) {
  
  # If in stage 1,2 only 1 dimension and f is not relevant
  if (s %in% c(1,2)) {
    f = 0
    f3 = 0
    f4 = 0
  }
  
  # If stimulus 1: reallocation of f2, f4 (false stim)
  if (stimulus == 1) { 
    w1 = (1 - f)*f2 + f*f4 
    w2 = -f2
    w3 = (1 - f)*f4 + f*f2
    w4 = -f4}
  
  # If stimulus 2: reallocation of f2, f3 (false stim)
  else if (stimulus == 2) { 
    w1 = (1 - f)*f2 + f*f3
    w2 = -f2
    w3 = -f3
    w4 = (1 - f)*f3 + f*f2
  }
  
  return(list(w1 = w1, w2 = w2, w3 = w3, w4 = w4))
}

# Update the attention weights using the feedback and alpha 
update_attention = function(f1, f2, f3, f4, w1, w2, w3, w4, alpha) {
  
  f1_updated = f1 + alpha*w1  
  f2_updated = f2 + alpha*w2
  f3_updated = f3 + alpha*w3  
  f4_updated = f4 + alpha*w4  
  
  return(list(f1_updated = f1_updated, f2_updated = f2_updated, f3_updated = f3_updated, f4_updated = f4_updated))
}

# Running the entire simulation
run_simulation = function(params, n_agents, sim_seeds) {
  
  simdf = NULL
  
  # Extract parameters
  group = params$group
  n_correct = params$n_correct
  f = params$f
  r = params$r
  p = params$p
  d = params$d
  lambda = params$lambda
  C = params$C
  C = params$C
  
  # Define initials for experiment 
  n_trials = 50
  n_stages = 9
  
  # Run through agents
  for (agent in 1:n_agents) {
    
    # Set seed for the agent
    set.seed(sim_seeds[agent])
    
    # End experiment if reached 50 trials
    end_experiment = F
    
    # Loop through stages
    for (stage in 1:n_stages) {
      
      # If 50 trials in the last stage, end experiment
      if (end_experiment == T) {break}  
      
      # Create empty vectors to save data in
      trial = rep(NA, n_trials)
      f1 = rep(NA, n_trials)
      f2 = rep(NA, n_trials)
      f3 = rep(NA, n_trials)
      f4 = rep(NA, n_trials)
      w1 = rep(NA, n_trials)
      w2 = rep(NA, n_trials)
      w3 = rep(NA, n_trials)
      w4 = rep(NA, n_trials)
      stimulus = rep(NA, n_trials)
      choicep = rep(NA, n_trials)
      choice = rep(NA, n_trials)
      
      # Make correct count 0, as it's the beginning of the stage
      correct_count = 0
      
      # Loop through trials
      for (t in 1:n_trials) {
        
        # Save trial in vector
        trial[t] = t
        
        # If it's the first trial: define attentions and correct count
        if (t == 1) {att = initiate_att(stage, att_updated, C)}
        else {att = att_updated}
        
        # Extract attentions
        f1[t] = att$f1
        f2[t] = att$f2
        f3[t] = att$f3
        f4[t] = att$f4
        
        # Generate a stimuli (1 for stage1 and stage2, otherwise balanced between 1 and 2)
        stimulus[t] = ifelse(stage %in% c(1,2), 1, (rbinom(1, 1,0.5) + 1))
        
        # Generate choice probability and choice
        choicep[t] = get_choice_prop(stage, t, f1[t], f2[t], f3[t], f4[t], stimulus[t], d, lambda)
        choice[t] = as.numeric(rbinom(1, 1, choicep[t]))
        
        # If correct add to list of correct count, otherwise reset to 0
        correct_count = ifelse(choice[t] == 1, correct_count + 1, 0)
        
        # Calculate weights (sigma, feedback)
        weights = get_weights(stage, f1[t], f2[t], f3[t], f4[t], stimulus[t], f)
        w1[t] = weights$w1
        w2[t] = weights$w2
        w3[t] = weights$w3
        w4[t] = weights$w4
        
        # Define alpha based on the choice
        alpha = ifelse(choice[t] == 1, r, p)
        # Updated attention and save in last_att for next trial 
        att_updated = update_attention(f1[t], f2[t], f3[t], f4[t], w1[t], w2[t], w3[t], w4[t], alpha)
        
        # If 6 correct in a row, break stage and move to next one 
        if (correct_count == n_correct) {break}
        
        # If 50 trials, then end the experiment
        if (t == 50) {end_experiment = T}
        else {end_experiment = F}
      }
      
      # Save stage data in tibble
      temp = tibble(
        # Save experiment data
        group = group,
        agent = agent,
        seed_agent = sim_seeds[agent],
        stage = stage,
        trial = trial,
        f1 = f1,
        f2 = f2,
        f3 = f3,
        f4 = f4,
        w1 = w1,
        w2 = w2,
        w3 = w3,
        w4 = w4,
        stimulus = stimulus,
        choicep = choicep,
        choice = choice,
        stage_completed = ifelse(end_experiment == F, 1, 0),
        
        # Also save parameters
        n_correct = n_correct,
        d = d,
        lambda = lambda,
        r = r,
        p = p,
        C = C,
        f = f
      )
      
      # Remove rows without data (stage ended after 6 consecutive correct)
      temp = temp[rowSums(is.na(temp)) != ncol(temp) - 12,]
      
      # Append stage data to overall data
      if (exists("simdf")) {simdf = rbind(simdf, temp)} else {simdf = temp}
    }
  }
  
  return(simdf) 
}