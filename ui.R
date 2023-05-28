
dashboardPage(skin = "black",
  
  dashboardHeader(title="Cognitive Modelling - IED", titleWidth = 250),
  
  dashboardSidebar(collapsed = F, width = 250,#width = 250, 

         sidebarMenu(id="sidebarmenu",
           menuItem("Home", tabName = "home", icon=icon("house")),
           menuItem("Data Simulation", tabName = "sim", icon=icon("sliders"),
                menuSubItem("Yearsley (Y-RL)", tabName = "sim_yearsley"), 
                menuSubItem("Talwar fRL (T-fRL)", tabName = "sim_talwar1"),
                menuSubItem("Talwar cafRL (T-caFRL)", tabName = "sim_talwar2"),
                menuSubItem("LN fRL (LN-fRL)", tabName = "sim_ln1"),
                menuSubItem("LN cafRL (LN-cafRL)", tabName = "sim_ln2")),
           menuItem("Additional Information", tabName ="info", icon=icon("circle-info")))),
  
  
  dashboardBody(
    
    # Set all slider colors
    setSliderColor(c(rep("#CB3437", 3), rep("#042E7F", 3), 
                     rep("#CB3437", 3), rep("#042E7F", 3),
                     rep("#CB3437", 2), rep("#042E7F", 2), 
                     rep("#CB3437", 3), rep("#042E7F", 3),
                     rep("#CB3437", 3), rep("#042E7F", 3),
                     rep("#CB3437", 4), rep("#042E7F", 4)), sliderId = c(1:36)),
    
    # Styling
    tags$style('.nav-tabs-custom .nav-tabs li.active {border-top-color: #242D31;}"'),
    
    tabItems(
      
      # ----------- HOME ------------------------------------------------------------------------
      
      tabItem("home",
              
        h2("Data Simulation based on Cognitive Models of the IED Task!"),
        home,
        hr(style = "border-top: 1px solid #000000;"),
        about),
      
      
      # ----------- DATA SIMULATION --------------------------------------------------------------
      
      # ----------- YEARSLEY ------------
      
      tabItem("sim_yearsley",
          
        h2("Y-RL: Reinforcement Learning Model by Yearsley et al. (2021)"), 
        
        br(), 
        fluidRow(
          
          # Input Box
          tabBox(width=8, height=350, side = "left",
                 
            tabPanel("Free Parameters",
              
            column(width=6,

                h4("Free Parameters Group 1"),
                sliderInput("yearsley_r1", "Reward Learning Rate (𝛼_r)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                sliderInput("yearsley_p1", "Punishment Learning Rate (𝛼_p)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                sliderInput("yearsley_f1", "Attention Switching Parameter (𝜙)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%")),

            column(width=6,

                h4("Free Parameters Group 2"),
                sliderInput("yearsley_r2", "Reward Learning Rate (𝛼_r)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                sliderInput("yearsley_p2", "Punishment Learning Rate (𝛼_p)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                sliderInput("yearsley_f2", "Attention Switching Parameter (𝜙)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"))),
            
            tabPanel("Fixed Parameters",
                     
            column(width=6,
                     
                 h4("Fixed Parameters Group 1"),
                 sliderInput("yearsley_d1", "Initial Decision Consistency (d_0)", min=0, max=20, value=3, step=1, ticks=F, width="100%"),
                 sliderInput("yearsley_lambda1", "Disengagement Parameter (λ)", min=0, max=1, value=0.2, step=0.05, ticks=F, width="100%"),
                 sliderInput("yearsley_C1", "Continuity Parameter (C)", min=0, max=1, value=0.95, step=0.05, ticks=F, width="100%")),
            
            column(width=6,
                   
               h4("Fixed Parameters Group 2"),
               sliderInput("yearsley_d2", "Initial Decision Consistency (d_0)", min=0, max=20, value=3, step=1, ticks=F, width="100%"),
               sliderInput("yearsley_lambda2", "Disengagement Parameter (λ)", min=0, max=1, value=0.2, step=0.05, ticks=F, width="100%"),
               sliderInput("yearsley_C2", "Continuity Parameter (C)", min=0, max=1, value=0.95, step=0.05, ticks=F, width="100%"))),
            
            tabPanel("Agents",
                     
               h4("Number of Simulated Agents"),
               p("Note that an increase number of agents will slow down the visulisations. We recommend simulating with 10 agents."),
               numericInput("yearsley_agents", "Number Agents", min=1, max=500, value=10))),
          
          # Info Box
          tabBox(width=4, height=350, side = "left",
            tabPanel("Model", model_yearsley),
            tabPanel("Parameters", parameters_yearsley, style="overflow-y:scroll; max-height: 290px"),
            tabPanel("Download", p("You can download the simulated data here:"),
              downloadLink('yearsley_download', 'Download data!')))),
        
        fluidRow(
        
          # Plots
          box(title = "Attrition Rate", width = 4, plotlyOutput("yearsley_attrition")),
          box(title = "Errors to Criterion", width = 4, plotlyOutput("yearsley_error")),
          box(title = "Trials to Criterion", width = 4, plotlyOutput("yearsley_trial")))),
      
      
      # ----------- TALWAR fRL ------------
      
      tabItem("sim_talwar1",
              
        h2("T-fRL: Feature Reinforcement Learning Model by Talwar et al. (2021)"), br(), 
      
        fluidRow(
          
          tabBox(width=8, height=300, side = "left",
                 
            tabPanel("Free Parameters",
          
            column(width=6,
                   
               h4("Free Parameters Group 1"),
               sliderInput("talwar1_alpha1", "Generic Learning Rate (𝛼)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
               sliderInput("talwar1_beta1", "Inverse Temperature (𝛽)", min=0, max=20, value=2, step=1, ticks=F, width="100%")),
               
                   
            column(width=6,
                   
               h4("Free Parameters Group 2"),
               sliderInput("talwar1_alpha2", "Generic Learning Rate (𝛼)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
               sliderInput("talwar1_beta2", "Inverse Temperature (𝛽)", min=0, max=20, value=2, step=1, ticks=F, width="100%"))),
            
            tabPanel("Agents",
                     
               h4("Number of Simulated Agents"),
               p("Note that an increase number of agents will slow down the visulisations. We recommend simulating with 10 agents."),
               numericInput("talwar1_agents", "Number Agents", min=1, max=500, value=10))),
      
          
          # Info Box
          tabBox(width=4, height=300, side = "left",
                 
             tabPanel("Model", model_talwar1),
             tabPanel("Parameters", parameters_talwar1),
             tabPanel("Download Data", p("You can download the simulated data here:"),
                      downloadLink('talwar1_download', 'Download data!')))),
    
        fluidRow(
          
          # Plots
          box(title = "Attrition Rate", width = 4, plotlyOutput("talwar1_attrition")),
          box(title = "Errors to Criterion", width = 4, plotlyOutput("talwar1_error")),
          box(title = "Trials to Criterion", width = 4, plotlyOutput("talwar1_trial")))),
      
      
    
      # ----------- TALWAR cafRL ------------
    
      tabItem("sim_talwar2",
              
        h2("T-cafRL: Combined-Attention Feature Reinforcement Learning Model by Talwar et al. (2021)"), br(), 
        
        fluidRow(
          
          # Input Box
          tabBox(width=8, height=350, side = "left",
                 
            tabPanel("Free Parameters",
                     
            column(width=6,
                       
               h4("Free Parameters Group 1"),
               sliderInput("talwar2_alpha1", "Generic Learning Rate (𝛼)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
               sliderInput("talwar2_beta1", "Inverse Temperature (𝛽)", min=0, max=20, value=2, step=1, ticks=F, width="100%"),
               sliderInput("talwar2_theta01", "Initial Dimension Weight (𝜃_0)", min=0, max=1, value=0.5, step=0.05, ticks=F, width="100%")),
        
            
            column(width=6,
                       
               h4("Free Parameters Group 2"),
               sliderInput("talwar2_alpha2", "Generic Learning Rate (𝛼)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
               sliderInput("talwar2_beta2", "Inverse Temperature (𝛽)", min=0, max=20, value=2, step=1, ticks=F, width="100%"),
               sliderInput("talwar2_theta02", "Initial Dimension Weight (𝜃_0)", min=0, max=1, value=0.5, step=0.05, ticks=F, width="100%"))),
            
            tabPanel("Agents",
                     
               h4("Number of Simulated Agents"),
               p("Note that an increase number of agents will slow down the visulisations. We recommend simulating with 10 agents."),
               numericInput("talwar2_agents", "Number Agents", min=1, max=500, value=10))),
      
          
          # Info Box
          tabBox(width=4, height=350,

             tabPanel("Model", model_talwar2),
             tabPanel("Parameters", parameters_talwar2),
             tabPanel("Download Data", p("You can download the simulated data here:"),
                      downloadLink('talwar2_download', 'Download data!')))),
    
        fluidRow(
          
          # Plots
          box(title = "Attrition Rate", width = 4, plotlyOutput("talwar2_attrition")),
          box(title = "Errors to Criterion", width = 4, plotlyOutput("talwar2_error")),
          box(title = "Trials to Criterion", width = 4, plotlyOutput("talwar2_trial")))),
      
      
      # ----------- LN fRL ------------
      
      tabItem("sim_ln1",
              
        h2("LN-fRL: Feature Reinforcement Learning Model by LN"), br(), 
        
        fluidRow(
          
          # Input Box
          tabBox(width=8, height=350, side = "left",
                 
              tabPanel("Free Parameters",
              
              column(width=6,
                     
                     h4("Free Parameters Group 1"),
                     sliderInput("ln1_alpha_pos1", "Reward Learning Rate (𝛼_p)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                     sliderInput("ln1_alpha_neg1", "Punishment Learning Rate (𝛼_r)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                     sliderInput("ln1_beta1", "Inverse Temperature (𝛽)", min=0, max=20, value=2, step=1, ticks=F, width="100%")),
              
              
              column(width=6,
                     
                     h4("Free Parameters Group 2"),
                     sliderInput("ln1_alpha_pos2", "Reward Learning Rate (𝛼_p)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                     sliderInput("ln1_alpha_neg2", "Punishmnet Learning Rate (𝛼_r)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                     sliderInput("ln1_beta2", "Inverse Temperature (𝛽)", min=0, max=20, value=2, step=1, ticks=F, width="100%"))),
          
          
              tabPanel("Agents",
                   
                   h4("Number of Simulated Agents"),
                   p("Note that an increase number of agents will slow down the visulisations. We recommend simulating with 10 agents."),
                   numericInput("ln1_agents", "Number Agents", min=1, max=500, value=10))),
          
          # Info Box
          tabBox(width=4, height=350,
                 
                 tabPanel("Model", model_ln1),
                 tabPanel("Parameters", parameters_ln1),
                 tabPanel("Download Data", p("You can download the simulated data here:"),
                          downloadLink('ln1_download', 'Download data!')))),
        
        fluidRow(
          
          # Plots
          box(title = "Attrition Rate", width = 4, plotlyOutput("ln1_attrition")),
          box(title = "Errors to Criterion", width = 4, plotlyOutput("ln1_error")),
          box(title = "Trials to Criterion", width = 4, plotlyOutput("ln1_trial")))),
      
      
      # ----------- LN cafRL ------------
      
      tabItem("sim_ln2",
              
        h2("LN-cafRL: Combined-Attention Feature Reinforcement Learning Model by LN"), br(), 
        
        fluidRow(
          
          # Input Box
          tabBox(width=8, height=450, side = "left",
                 
              tabPanel("Free Parameters",
              
              column(width=6,
                     
                     h4("Free Parameters Group 1"),
                     sliderInput("ln2_alpha_pos1", "Reward Learning Rate (𝛼_p)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                     sliderInput("ln2_alpha_neg1", "Punishment Learning Rate (𝛼_r)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                     sliderInput("ln2_beta1", "Inverse Temperature (𝛽)", min=0, max=20, value=2, step=1, ticks=F, width="100%"),
                     sliderInput("ln2_theta01", "Initial Dimension Weight (𝜃_0)", min=0, max=1, value=0.5, step=0.05, ticks=F, width="100%")),
              
              
              column(width=6,
                     
                     h4("Free Parameters Group 2"),
                     sliderInput("ln2_alpha_pos2", "Reward Learning Rate (𝛼_p)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                     sliderInput("ln2_alpha_neg2", "Punishmnet Learning Rate (𝛼_r)", min=0, max=1, value=0.50, step=0.05, ticks=F, width="100%"),
                     sliderInput("ln2_beta2", "Inverse Temperature (𝛽)", min=0, max=20, value=2, step=1, ticks=F, width="100%"),
                     sliderInput("ln2_theta02", "Initial Dimension Weight (𝜃_0)", min=0, max=1, value=0.5, step=0.05, ticks=F, width="100%"))),
              
              tabPanel("Agents",
                       
                       h4("Number of Simulated Agents"),
                       p("Note that an increase number of agents will slow down the visulisations. We recommend simulating with 10 agents."),
                       numericInput("ln2_agents", "Number Agents", min=1, max=500, value=10))),
          
          
          # Info Box
          tabBox(width=4, height=450,
                 
                 tabPanel("Model", model_ln2),
                 tabPanel("Parameters", parameters_ln2),
                 tabPanel("Download Data", p("You can download the simulated data here:"),
                          downloadLink('ln2_download', 'Download data!')))),
        
        fluidRow(
          
          # Plots
          box(title = "Attrition Rate", width = 4, plotlyOutput("ln2_attrition")),
          box(title = "Errors to Criterion", width = 4, plotlyOutput("ln2_error")),
          box(title = "Trials to Criterion", width = 4, plotlyOutput("ln2_trial")))),
      
      
      tabItem("info",
              
        h3("Additional Information"),
        p("This page displays additional information regarding columns in the simulated data, which can be downloaded for each model."),
        h4("Columns across all datasets:"),
        info_all,
        h4("Columns specific to Y-RL:"),
        info_yearsley,
        h4("Columns specific to T-fRL and T-cafRL:"),
        info_talwar,
        h4("Columns specific to LN-fRL and LN-cafRL:"),
        info_ln)
              
      
)))
