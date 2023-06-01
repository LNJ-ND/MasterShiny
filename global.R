library(shiny)
library(ggplot2)
library(plotly)
#library(modules)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(tidyverse)


# ---------- DATA SIMULATION --------------------------------

# Simulate data for Y-RL
get_data_yearsley = function(n_agents, f1, r1, p1, d1, lambda1, C1, f2, r2, p2, d2, lambda2, C2) {
  
  source("simulation/simulation_functions_yearsley.R")
  
  params1 = list(
    group = "Group 1",
    n_correct = 6,
    f = f1,
    r = r1,
    p = p1,
    d = d1,
    lambda = lambda1,
    C = C1)
  
  params2 = list(
    group = "Group 2",
    n_correct = 6,
    f = f2,
    r = r2,
    p = p2,
    d = d2,
    lambda = lambda2,
    C = C2)
  
  data1 = run_simulation(params1, n_agents=n_agents, seq(1,n_agents,1))
  data2 = run_simulation(params2, n_agents=n_agents, seq(1,n_agents,1))
  data = rbind(data1, data2)
  
  return(data)
  
}

# Simulate data for T-fRL and T-cafRL
get_data_talwar = function(model, n_agents, alpha1, beta1, theta01, alpha2, beta2, theta02) {
  
  source("simulation/simulation_functions_talwar.R")
  
  params1 = list(
    model = model,
    group = "Group 1",
    n_correct = 6,
    alpha = alpha1, 
    beta = beta1,
    theta_init = theta01,
    epsilon = NA)
  
  params2 = list(
    model = model,
    group = "Group 2",
    n_correct = 6,
    alpha = alpha2, 
    beta = beta2,
    theta_init = theta02,
    epsilon = NA)
  
  data1 = run_simulation(params1, n_agents=10, seq(1,n_agents,1))
  data2 = run_simulation(params2, n_agents=10, seq(1,n_agents,1))
  data = rbind(data1, data2)
  
}

# Simulate data for LN-fRL and LN-cafRL
get_data_ln = function(model, n_agents, alpha_pos1, alpha_neg1, beta1, theta01, alpha_pos2, alpha_neg2, beta2, theta02) {
  
  source("simulation/simulation_functions_ln.R")
  
  params1 = list(
    model = model,
    group = "Group 1",
    n_correct = 6,
    alpha_pos = alpha_pos1, 
    alpha_neg = alpha_neg1, 
    beta = beta1,
    theta_init = theta01,
    epsilon = NA)
  
  params2 = list(
    model = model,
    group = "Group 2",
    n_correct = 6,
    alpha_pos = alpha_pos2, 
    alpha_neg = alpha_neg2, 
    beta = beta2,
    theta_init = theta02,
    epsilon = NA)
  
  data1 = run_simulation(params1, n_agents=n_agents, seq(1,n_agents,1))
  data2 = run_simulation(params2, n_agents=n_agents, seq(1,n_agents,1))
  data = rbind(data1, data2)
  
}

# ---------- PLOT FUNCTIONS --------------------------------

# Plot attrition rate
plot_attrition = function(data, n_agents) {
  
  attrition = data %>% filter(stage_completed == 1) %>% 
    group_by(group, stage) %>% 
    summarise(attrition_rate = length(unique(agent))/n_agents)
  
  plot = ggplot(attrition, aes(x = stage, y = attrition_rate, color = group)) + 
    geom_line() + 
    geom_point() + 
    scale_color_manual(values = c("Group 1" = "#CB3437", "Group 2" = "#042E7F")) +
    scale_x_continuous(n.breaks = 9, limits=c(1,9)) +
    ylim(0, 1) + 
    labs(y="% of Agents Completing the Stage", x="Stage") +
    theme_bw() +
    theme(legend.position = "none")
  
  return(ggplotly(plot))
}

# Plot errors to criterion
plot_error_to_criterion = function(data) {
  
  errors = data %>% 
    group_by(group, agent, stage) %>%
    summarise(error_to_criterion_agent = sum(choice == 0)) %>% 
    group_by(group, stage) %>% 
    summarise(error_to_criterion = mean(error_to_criterion_agent))
  
  plot = ggplot(errors, aes(x = stage, y = error_to_criterion, color = group)) + 
    geom_line() + 
    geom_point() + 
    scale_color_manual(values = c("Group 1" = "#CB3437", "Group 2" = "#042E7F")) +
    scale_x_continuous(n.breaks = 9, limits=c(1,9)) +
    labs(y="Number of Errors to Criterion", x="Stage") +
    theme_bw() +
    theme(legend.position = "none")
  
  return(ggplotly(plot))
  
}

# Plot trial to criterion 
plot_trial_to_criterion = function(data) {
  
  trials = data %>% 
    group_by(group, agent, stage) %>%
    summarise(trial_to_criterion_agent = max(trial)) %>% 
    group_by(group, stage) %>% 
    summarise(trial_to_criterion = mean(trial_to_criterion_agent))
  
  plot = ggplot(trials, aes(x = stage, y = trial_to_criterion, color = group)) + 
    geom_line() + 
    geom_point() + 
    scale_color_manual(values = c("Group 1" = "#CB3437", "Group 2" = "#042E7F")) +
    scale_x_continuous(n.breaks = 9, limits=c(1,9)) +
    labs(y="Number of Trials to Criterion", x="Stage") +
    theme_bw() +
    theme(legend.position = "none")
  
  return(ggplotly(plot))
  
}


#-------------------- TEXT ---------------------------

# HOME # 

home = HTML("<p>This app allows you to simulate data of the IED task based on different cognitive models.<br>The cognitive models included are based on models developed by <a href='https://onlinelibrary.wiley.com/doi/full/10.1002/aur.2484?casa_token=h3TPxiRZ9TYAAAAA%3AzlRJsWT9G0ZLPS2_i5-Myw4Z13rzup_fbm7Yhrq51SEFzeetxP37yxOMoe9LrV5u9nkk_oJRttMD868'>Yearsley et al. (2021)</a> and 
                <a href='https://www.biorxiv.org/content/10.1101/2021.10.05.463165v1.abstract'>Talwar et al. (2021)</a>, while two additional models were added based on these existing models.</p>
             <p>Please use the sidebar to simulate data for a model of interest. For each model, you can also find additional information about the model and its parameters, and can download simulated data.<br>Information regarding the columns in the downloaded data can be found on the Additional Information page.</p>")

about = HTML("<p>This app was developed as part of the Master's Thesis by Louise Nyholm Jensen and Nicole Dwenger in the Spring Semester of 2023.<br>
                 All code for this shiny app can be found on <a href='https://github.com/LNJ-ND/MasterShiny'>GitHub</a>.</p>
              <p>For questions please contact us at 201809497@post.au.dk or 201805351@post.au.dk.</p>")

# ADDIITONAL INFORMATION

info_all = HTML("<ul>
                <li><b>model</b>: Defines the model used for simulation, e.g. fRL or lnfRL.</li>
                <li><b>group</b>: Defines the group the agent belongs to.</li>
                <li><b>agent</b>: Defines the id of the agent.</li>
                <li><b>seed_agent</b>: Defines the seed the data of the agent was simulated with.</li>
                <li><b>stage</b>: Defines the stage in the task (out of 9 stages).</li>
                <li><b>trial</b>: Defines the trial in a stage (resets at the beginning of each stage.</li> 
                <li><b>f1</b>: Defines the feature weight of f1. Note that f1 always refers to the correct feature in a given trial.</li>
                <li><b>f2</b>: Defines the feature weight of f2, referring to the incorrect feature of the relevant dimension in each trial.</li>
                <li><b>f3</b>: Defines the feature weight of f3, referring to the first feature of the second dimension.</li>
                <li><b>f4</b>: Defines the feature weight of f4, referring to the second feature of the second dimension.</li>
                <li><b>stimulus</b>: Defines the stimulus set, (SS1 or SS2).</li>
                <li><b>choicep/correctp</b>: Defines the probability of the subject choosing the correct stimulus.</li>
                <li><b>choice</b>: Defines the choice of the agent (1 = correct, 0 = incorrect).</li>
                <li><b>stage_completed</b>: Defines whether the agent completed the stage.</li>
                <li><b>n_correct</b>: Defines criterion, i.e. number of correct choices necessary to complete a stage.</li>
                </ul>")

info_yearsley = HTML("<ul>
                    <li><b>w1</b>: Defines the update signal used for updating of f1 (referred to as feedback function in Yearsley et al (2021)).</li>
                    <li><b>w2</b>: Defines the update signal used for updating of f2 (referred to as feedback function in Yearsley et al (2021)).</li>
                    <li><b>w3</b>: Defines the update signal used for updating of f3 (referred to as feedback function in Yearsley et al (2021)).</li>
                    <li><b>w4</b>: Defines the update signal used for updating of f4 (referred to as feedback function in Yearsley et al (2021)).</li>
                    <li><b>d</b>: Defines the decision consistency parameter, fixed at 3.</li>
                    <li><b>lambda</b>: Defines the disengagement parameter (referred to as focus in Yearsley et al (2021)).</li>
                    <li><b>r</b>: Defines the reward learning rate.</li>
                    <li><b>p</b>: Defines the punishment learning rate.</li>
                    <li><b>C</b>: Defines the continuation parameter (referred to as control switch in Yearsley et al., (2021)).</li>
                    <li><b>f</b>: Defines 𝜙, the dimension shift parameter (referred to as attention switch parameter in Yearsley et al., (2021)).</li>
                    </ul>")

info_talwar = HTML("<ul>
                    <li><b>alpha</b>: Defines the generic learning rate.</li>
                    <li><b>beta</b>: Defines the inverse temperature.</li>
                    <li><b>theta</b>: Defines the dimension weight in each trial.</li>
                    <li><b>theta_init</b>: Defines the initial dimension weight, 𝜃_0</li>
                    <li><b>epsilon</b>: Irrelevant for the included models.</li>
                    </ul>")

info_ln = HTML("<ul>
                <li><b>alpha_pos</b>: Defines the reward learning rate.</li>
                <li><b>alpha_neg</b>: Defines the punishment learning rate.</li>
                <li><b>beta</b>: Defines the inverse temperature.</li>
                <li><b>theta</b>: Defines the dimension weight in each trial.</li>
                <li><b>theta_init</b>: Defines the initial dimension weight, 𝜃_0</li>
                <li><b>epsilon</b>: Irrelevant for the included models.</li>
                </ul>")


# YEARSLEY # 

model_yearsley = HTML("<p>This model was implemented based on <a href='https://onlinelibrary.wiley.com/doi/full/10.1002/aur.2484?casa_token=h3TPxiRZ9TYAAAAA%3AzlRJsWT9G0ZLPS2_i5-Myw4Z13rzup_fbm7Yhrq51SEFzeetxP37yxOMoe9LrV5u9nkk_oJRttMD868'>Yearsley et al. (2021)</a>.
                        It assumes that all four features are associated with a feature weight, which determines the expected value of each stimulus. The expected values determine the choice of the agent. Subsequent to a choice, the agent updates 
                        its feature weights based on a update signal The update signal allows for reallocation of weight across features of the same dimension and features of the opposite dimension.</p>")


parameters_yearsley = HTML("<b><i>Free Parameters:</b></i>
                            <p><b>Reward Learning Rate (𝛼_r)</b>: Determines updating of feature weights subsequent to positive feedback.</p>
                            <p><b>Punishment Learning Rate (𝛼_p)</b>: Determines updating of feature weights subsequent to negative feedback.</p>
                            <p><b>Dimension Shift Paramter (𝜙)</b>: Determines shifting of feature weight across dimensions. If 𝜙< 0.5, there is increased shifting within features of the same dimension. If 𝜙> 0.5, there is increased shifting across features of opposite dimensions.</p>
                            <b><i>Fixed Parameters:</b></i>
                            <p><b>Initial Decision Consistency (d_0)</b>: Determines the initial decision consistency, multiplied by exp(-λ*(trial-1)) to get decision consistency.</p>
                            <p><b>Disengagement Parameter (λ)</b>: Determines focus of the agent, which decreases throughout trails in a stage by exp(-λ*(trial-1)), and affects decision consistency.</p>
                            <p><b>Continuation Parameter (C)</b>: Determines how much an agent relies on previous weights or resets weight at the beginning of a stage with new stimuli. If C = 0 the agent resets all weights, if C = 1 the agent only relies on previous weights.</p>")
  
  
  
# TALWAR fRL # 

model_talwar1 = HTML("<p>This model was implemented based on <a href='https://www.biorxiv.org/content/10.1101/2021.10.05.463165v1.abstract'>Talwar et al. (2021)</a>.
                        It assumed that all four features are associated with a feature weight, which determines the expected value of each stimulus. The expected values determine the choice of the agent. Subsequent to a choice, the agent updates 
                        its feature weights based on a prediction error and a generic learning rate. This model cannot account for the fact that features belong to different dimensions.</p>")


parameters_talwar1 = HTML("<p><b>Generic Learning Rate (𝛼_r)</b>: Determines updating of feature weights subsequent to positive or negative feedback.</p>
                            <p><b>Inverse Temperature (𝜙)</b>: Determines choice consistency. Smaller values lead to more random choices, while larger values lead to more deterministic choices.</p>")


# TALWAR cafRL # 

model_talwar2 = HTML("<p>This model was implemented based on <a href='https://www.biorxiv.org/content/10.1101/2021.10.05.463165v1.abstract'>Talwar et al. (2021)</a>.
                        It assumed that all four features are associated with a feature weight and a dimension. A dimension weight accounts for dimensional biases of the agent
                        and impacts the expected value of a stimulus. Further, the expected values determine the choice of the agent. Subsequent to a choice, the agent updates 
                        its feature weights and dimension weight based on backpropagation and a generic learning rate.</p>")


parameters_talwar2 = HTML("<p><b>Generic Learning Rate (𝛼_r)</b>: Determines updating of feature weights subsequent to positive or negative feedback.</p>
                            <p><b>Inverse Temperature (𝛽)</b>: Determines choice consistency. Smaller values lead to more random choices, while larger values lead to more deterministic choices.</p>
                            <p><b>Initial Dimension Weight (𝜃_0)</b>: Determines dimension bias when the second dimension is introduced. Note that the value ranges from -∞ to ∞, but here it is defined on a scale from -5 to 5, as the logit() of -5 and 5 closely approximate 0 and 1, respectively. If 𝜃_0 > the agent weights the intiially relevant dimension more when the second dimension is introduced in stage 3, while if 𝜃_0 < 0, the agent weighs the newly introduced dimension more. </p>")

# LN fRL # 

model_ln1 = HTML("<p>This model was inspired and adopted from from <a href='https://www.biorxiv.org/content/10.1101/2021.10.05.463165v1.abstract'>Talwar et al. (2021)</a>, but modified by LN.
                      It assumes that all four features are associated with a feature weight, which determines the expected value of each stimulus. The expected values determine the choice of the agent. Subsequent to a choice, the agent updates 
                      its feature weights based on a prediction error, and two separate learning rates. This model cannot account for the fact that features belong to different dimensions.</p>")


parameters_ln1 = HTML("<p><b>Reward Learning Rate (𝛼_r)</b>: Determines updating of feature weights subsequent to positive feedback.</p>
                       <p><b>Punishment Learning Rate (𝛼_p)</b>: Determines updating of feature weights subsequent to negative feedback.</p>
                       <p><b>Inverse Temperature (𝜙)</b>: Determines choice consistency. Smaller values lead to more random choices, while larger values lead to more deterministic choices.</p>")


# LN cafRL # 

model_ln2 = HTML("<p>This model was inspired and adopted from <a href='https://www.biorxiv.org/content/10.1101/2021.10.05.463165v1.abstract'>Talwar et al. (2021)</a>, but modified by LN.
                        It assumes that all four features are associated with a feature weight and a dimension. A dimension weight accounts for dimensional biases of the agent
                        and impacts the expected value of a stimulus. Further, the expected values determine the choice of the agent. Subsequent to a choice, the agent updates 
                        its feature weights and dimension weight based on backpropagation and two separate learning rates.</p>")


parameters_ln2 = HTML("<p><b>Reward Learning Rate (𝛼_r)</b>: Determines updating of feature weights subsequent to positive feedback.</p>
                           <p><b>Punishment Learning Rate (𝛼_p)</b>: Determines updating of feature weights subsequent to negative feedback.</p>
                           <p><b>Inverse Temperature (𝛽)</b>: Determines choice consistency. Smaller values lead to more random choices, while larger values lead to more deterministic choices.</p>
                           <p><b>Initial Dimension Weight (𝜃_0)</b>: Determines dimension bias when the second dimension is introduced. Note that the value ranges from -∞ to ∞, but here it is defined on a scale from -5 to 5, as they approxiate 0 and 1, respectively. If 𝜃_0 > the agent weights the intiially relevant dimension more when the second dimension is introduced in stage 3, while if 𝜃_0 < 0, the agent weighs the newly introduced dimension more.</p>")
