# Load libraries
pacman::p_load(tidyverse, random, brms, comprehenr)
options(dplyr.summarise.inform = FALSE)

######## Feature Reinforcement Learning Model #######################################################

# Parameters 
# alpha: learning rate for the features
# beta: choice determinism/inverse temperature

# Function for initiating weights at the beginning of each stage
initiate_weights = function(stage, f1_updated, f2_updated, f3_updated, f4_updated) {
  
  # Initialize to 0
  if (stage == 1) {
    
    f1 = 0
    f2 = 0
    f3 = 0
    f4 = 0
    
  # Reversal with 1 dimension
  } else if (stage == 2) {
    
    f1 = f2_updated
    f2 = f1_updated
    f3 = 0
    f4 = 0
    
  # New dimension introduced
  } else if (stage == 3) {
    
    f1 = f1_updated
    f2 = f2_updated
    f3 = 0
    f4 = 0
      
  # Overlapping stimuli, but same relevance/correctness
  } else if (stage == 4) {
    
    f1 = f1_updated
    f2 = f2_updated
    f3 = f3_updated
    f4 = f4_updated
    
  # Reversal, same relevant dim
  } else if (stage == 5) {
    
    f1 = f2_updated
    f2 = f1_updated
    f3 = f3_updated
    f4 = f4_updated
    
  # New stimuli
  } else if (stage == 6) {
    
    f1 = 0
    f2 = 0
    f3 = 0
    f4 = 0
  
  # Reversal, same relevant dim
  } else if (stage == 7) {
    
    f1 = f2_updated
    f2 = f1_updated
    f3 = f3_updated
    f4 = f4_updated
    
  # New stimuli, and new relevant dimension
  } else if (stage == 8) {
    
    f1 = 0 # dim2
    f2 = 0 # dim2 
    f3 = 0 # dim1
    f4 = 0 # dim1
    
  # Reversal of relevant feature 
  } else if (stage == 9) {
    
    f1 = f2_updated # dim2
    f2 = f1_updated # dim2
    f3 = f3_updated # dim1
    f4 = f4_updated # dim1
    
  }
  
  return(list(f1 = f1, f2 = f2, f3 = f3, f4 = f4))
}


# Function for feature reinforcement learning model
fRL_model = function(stage, stimulus, f1, f2, f3, f4, alpha, beta) {
  
  # GET STIMULUS VALUES
  
  if (stage < 3) {
    
    v1 = f1
    v2 = f2
    
  } else {
  
    if (stimulus == 1) {
      v1 = sum(f1, f3)
      v2 = sum(f2, f4)
      
    } else if (stimulus == 2) {
      
      v1 = sum(f1, f4)
      v2 = sum(f2, f3)
      
    }
  }
  
  # GET CORRECT PROBABILITY AND CHOICE
  
  # Probability of the correct choice (as softmax)
  correctp = exp(beta*v1) / (exp(beta*v1) + exp(beta*v2))
  
  # Generate choice (if 0 = wrong, if 1 = correct)
  choice = as.numeric(rbinom(1, 1, correctp))
  
  # FEATURE WEIGHT UPDATING
  if (stage < 3) {
    
    f1_updated = f1 + alpha*(1 - v1)
    f2_updated = f2 + alpha*(-1 - v2)
    f3_updated = 0
    f4_updated = 0
  
  } else {
  
    if (stimulus == 1) {
      
      f1_updated = f1 + alpha*(1 - v1)
      f2_updated = f2 + alpha*(-1 - v2)
      f3_updated = f3 + alpha*(1 - v1)
      f4_updated = f4 + alpha*(-1 - v2)
      
    } else if (stimulus == 2) {
      
      f1_updated = f1 + alpha*(1 - v1)
      f2_updated = f2 + alpha*(-1 - v2)
      f3_updated = f3 + alpha*(-1 - v2)
      f4_updated = f4 + alpha*(1 - v1)
      
    }
  }
    
  # SAVE OUTPUT
  
  return(list(correctp = correctp,
              choice = choice,
              f1_updated = f1_updated, 
              f2_updated = f2_updated, 
              f3_updated = f3_updated,
              f4_updated = f4_updated))
  
}

######## Combined Attention-Modulated Feature Reinforcement Learning Model #######################################################

# Parameters 
# alpha: learning rate for the features (f)
# beta: choice determinism/inverse temperature
# theta: dimension weight 

cafRL_model = function(stage, stimulus, f1, f2, f3, f4, alpha, beta, theta) {
  
  
  # DEFINE FORMULA TO CALCULTE PROBABILITY FOR THE CORRECT CHOICE IN EACH STAGE
  
  # In stages with 1 dimension
  if (stage %in% c(1,2)) { 
    
    # Define choice probability for correct choice (f1 is the relevant feature)
    correctp_formula = quote(exp(beta*f1) / (exp(beta*f1) + exp(beta*f2)))
    
  # In stages with 2 dimensions 
  } else if (stage %in% c(3,4,5,6,7)) {
    
    # If stimulus set 1 (f1f3, f2f4)
    if (stimulus == 1) {
      
      correctp_formula = quote((exp(beta*((1/(1+exp(-theta)))*f1 + (1-(1/(1+exp(-theta))))*f3)) /    # v1 / 
                                  (exp(beta*((1/(1+exp(-theta)))*f1 + (1-(1/(1+exp(-theta))))*f3)) +    # (v1 + 
                                     exp(beta*((1/(1+exp(-theta)))*f2 + (1-(1/(1+exp(-theta))))*f4)))))   # v2)
      
    # If stimulus set 2 (f1f4, f2f3)
    } else if (stimulus == 2) {
      
      correctp_formula = quote((exp(beta*((1/(1+exp(-theta)))*f1 + (1-(1/(1+exp(-theta))))*f4)) /    # v1 / 
                                  (exp(beta*((1/(1+exp(-theta)))*f1 + (1-(1/(1+exp(-theta))))*f4)) +    # (v1 + 
                                     exp(beta*((1/(1+exp(-theta)))*f2 + (1-(1/(1+exp(-theta))))*f3)))))   # v2)
      
    }
    
  # In stages with 2 dimensions with ED shift
  } else if (stage %in% c(8,9)) {
    
    # If stimulus set 1 (f1f3, f2f4)
    if (stimulus == 1) {
      
      correctp_formula = quote(exp(beta*((1-(1/(1+exp(-theta))))*f1 + (1/(1+exp(-theta)))*f3)) / # v1 / 
                                  (exp(beta*((1-(1/(1+exp(-theta))))*f1 + (1/(1+exp(-theta)))*f3)) + # (v1 +
                                   exp(beta*((1-(1/(1+exp(-theta))))*f2 + (1/(1+exp(-theta)))*f4)))) # v2)
      
      # If stimulus set 2 (f1f4, f2f3)
    } else if (stimulus == 2) {
      
      correctp_formula = quote(exp(beta*((1-(1/(1+exp(-theta))))*f1 + (1/(1+exp(-theta)))*f4)) / # v1 / 
                                 (exp(beta*((1-(1/(1+exp(-theta))))*f1 + (1/(1+exp(-theta)))*f4)) + # (v1 +
                                    exp(beta*((1-(1/(1+exp(-theta))))*f2 + (1/(1+exp(-theta)))*f3)))) # v2)
    
    }
  
  }
    
  # GET CORRECT PROBABILITY AND CHOICE
  
  # Get probability of being correct
  correctp = eval(correctp_formula)
  # Generate choice (if 0 = wrong, if 1 = correct)
  choice = as.numeric(rbinom(1, 1, correctp))
  
  # UPDATE THE FEATURE AND DIMENSION WEIGHTS
  
  # Calculate loss
  L = substitute((1 - correctp_formula)^2, list(correctp_formula=correctp_formula))
  
  # Update the feature weights and dimension weight (theta) with the same lr (alpha)
  f1_updated = f1 - alpha*eval(D(L, "f1"))
  f2_updated = f2 - alpha*eval(D(L, "f2"))
  f3_updated = f3 - alpha*eval(D(L, "f3"))
  f4_updated = f4 - alpha*eval(D(L, "f4"))
  theta_updated = theta - alpha*eval((D(L, "theta")))
  
  # SAVE OUTPUT
  
  return(list(correctp = correctp,
              choice = choice,
              f1_updated = f1_updated, 
              f2_updated = f2_updated, 
              f3_updated = f3_updated,
              f4_updated = f4_updated,
              theta_updated = theta_updated))
  
}


######## Running Simulation with Model ##############################################################################################################

run_simulation = function(params, n_agents, sim_seeds) {
  
  simdf = NULL
  
  # Extract parameters
  model = params$model
  group = params$group
  n_correct = params$n_correct
  alpha = params$alpha
  beta = params$beta
  theta_init = params$theta_init
  epsilon = params$epsilon
  
  # Define initials for experiment 
  n_trials = 50
  n_stages = 9
  
  # Run through agents
  for (agent in 1:n_agents) {
    
    # Set seed for the agent
    set.seed(sim_seeds[agent])
    
    # Reset end experiment to false for agent
    end_experiment = F
    
    for (stage in 1:n_stages) {
      
      # If 50 trials reached, experiment will be ended
      if (end_experiment == T) {break}
      
      # Reset correct count in each stage
      correct_count = 0
      
      # Define vectors to save data from the stage
      trial_vector = rep(NA, n_trials)
      stimulus_vector = rep(NA, n_trials)
      f1_vector = rep(NA, n_trials) 
      f2_vector = rep(NA, n_trials)
      f3_vector = rep(NA, n_trials)
      f4_vector = rep(NA, n_trials)
      theta_vector = rep(NA, n_trials)
      correctp_vector = rep(NA,n_trials)
      choice_vector = rep(NA, n_trials)
      
      for (t in 1:n_trials) {
        
        # DEFINE FEATURE WEIGHTS
        
        if (t == 1) {f = initiate_weights(stage, f1_updated, f2_updated, f3_updated, f4_updated)}
        else {f = list(f1 = f1_updated, f2 = f2_updated, f3 = f3_updated, f4 = f4_updated)}
        
        # Save attentions
        f1_vector[t] = f$f1
        f2_vector[t] = f$f2
        f3_vector[t] = f$f3
        f4_vector[t] = f$f4
    
        # DEFINE DIMENSION WEIGHT (only applies to cafRL and safRL)
        
        if (stage %in% c(1,2)) {
          
          # No theta because only one dimension
          theta_vector[t] = NA
          
        } else if ((stage == 3) & (model != "fRL")) {
          
          # In stage 3, trial 1, use the initial theta values parameter, otherwise the updated one
          theta_vector[t] = ifelse(t == 1, theta_init, theta_updated)
        
        } else if ((stage > 3) & (model != "fRL")) { 
          
          # In all other stages, use the updated theta
          theta_vector[t] = theta_updated
          
        }
        
        # GENERATE STIMULI 
        
        stimulus_vector[t] = ifelse(stage %in% c(1,2), 1, (rbinom(1,1,0.5)+1)) 
        
        # GENERATE CHOICE AND UPDATE WEIGHTS DEPENDING ON MODEL
        
        if (model == "fRL") { 
          
          out = fRL_model(stage, stimulus_vector[t], f1_vector[t], f2_vector[t], f3_vector[t], f4_vector[t], alpha, beta)
        
        } else if (model == "cafRL") {
        
          out = cafRL_model(stage, stimulus_vector[t], f1_vector[t], f2_vector[t], f3_vector[t], f4_vector[t], alpha, beta, theta_vector[t])
          
        } 
        
        # SAVE FOR NEXT TRIAL
        
        f1_updated = out$f1_updated
        f2_updated = out$f2_updated
        f3_updated = out$f3_updated
        f4_updated = out$f4_updated
        theta_updated = out$theta_updated
        
        # SAVE OTHER RELVANT DATA 
        
        trial_vector[t] = t
        correctp_vector[t] = out$correctp
        choice_vector[t] = out$choice
        
        # MOVE TO NEXT STAGE IF CRITERION FULFILLED
        
        correct_count = ifelse(choice_vector[t] == 1, correct_count+1, 0)
        if (correct_count == n_correct) {break}
        
        # END EXPERIMENT IF 50 TRIALS REACHED
        
        if (t == 50) {end_experiment = T}
        else {(end_experiment = F)}
        
      }
      
      # SAVE STAGE DATA
      
      temp = tibble(
        # Over trial parameters
        model = model, 
        group = group, 
        agent = agent,
        seed_agent = sim_seeds[agent],
        stage = stage, 
        trial = trial_vector,
        f1 = f1_vector, 
        f2 = f2_vector,
        f3 = f3_vector, 
        f4 = f4_vector,
        theta = theta_vector,
        stimulus = stimulus_vector,
        correctp = correctp_vector,
        choice = choice_vector,
        stage_completed = ifelse(end_experiment == F, 1, 0),
        # Fixed parameters
        n_correct = n_correct,
        alpha = alpha,
        beta = beta,
        theta_init = theta_init,
        epsilon = epsilon
      )
      
      temp = temp[!is.na(temp$trial),]
      if (exists("simdf")) {simdf = rbind(simdf, temp)} else {simdf = temp}
      
    }
  }
  return(simdf)
}
