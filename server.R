
function(input, output, session) {
  
  
  # ----------- GENERALS -------------------------------------------------------
  
  output$downloadData <- downloadHandler(
      filename = function() {paste('data-', Sys.Date(), '.csv', sep='')},
      content = function(con) {write.csv(data, con)}
    )
  
  
  # ----------- DATA SIMULATION ------------------------------------------------
  # ----------- YEARSLEY ------------
  
  # Get the data
  yearsley_data = reactive({get_data_yearsley(input$yearsley_agents,
                                              input$yearsley_f1, input$yearsley_r1, input$yearsley_p1, input$yearsley_d1, input$yearsley_lambda1, input$yearsley_C1,
                                              input$yearsley_f2, input$yearsley_r2, input$yearsley_p2, input$yearsley_d2, input$yearsley_lambda2, input$yearsley_C2)})
  
  # Get the plots
  output$yearsley_attrition = renderPlotly(plot_attrition(yearsley_data(), input$yearsley_agents))
  output$yearsley_error = renderPlotly(plot_error_to_criterion(yearsley_data()))
  output$yearsley_trial = renderPlotly(plot_trial_to_criterion(yearsley_data()))
  
  # Download
  output$table = renderTable(yearsley_data())
  
  output$yearsley_download <- downloadHandler(
    filename = function() {paste("Y-RL", ".csv", sep = "")},
    content = function(file) {write.csv(yearsley_data(), file)})
  
  # ----------- TALWAR fRL ------------
  
  # Get the data
  talwar1_data = reactive({get_data_talwar("fRL", input$talwar1_agents,
                                           input$talwar1_alpha1, input$talwar1_beta1, NA,
                                           input$talwar1_alpha2, input$talwar1_beta2, NA)})
  
  # Get the plots
  output$talwar1_attrition = renderPlotly(plot_attrition(talwar1_data(), input$talwar1_agents))
  output$talwar1_error = renderPlotly(plot_error_to_criterion(talwar1_data()))
  output$talwar1_trial = renderPlotly(plot_trial_to_criterion(talwar1_data()))
  
  # Download
  output$table = renderTable(talwar1_data())
  output$talwar1_download <- downloadHandler(
    filename = function() {paste("T-fRL", ".csv", sep = "")},
    content = function(file) {write.csv(talwar1_data(), file)})
  
  # ----------- TALWAR cafRL ------------
  
  # Get the data
  talwar2_data = reactive({get_data_talwar("cafRL", input$talwar2_agents,
                                           input$talwar2_alpha1, input$talwar2_beta1, input$talwar2_theta01, 
                                           input$talwar2_alpha2, input$talwar2_beta2, input$talwar2_theta02)})
  
  # Get the plots
  output$talwar2_attrition = renderPlotly(plot_attrition(talwar2_data(), input$talwar2_agents))
  output$talwar2_error = renderPlotly(plot_error_to_criterion(talwar2_data()))
  output$talwar2_trial = renderPlotly(plot_trial_to_criterion(talwar2_data()))
  
  # Download
  output$table = renderTable(talwar2_data())
  output$talwar2_download <- downloadHandler(
    filename = function() {paste("T-cafRL", ".csv", sep = "")},
    content = function(file) {write.csv(talwar2_data(), file)})
  
  
  # ----------- LN fRL ------------
  
  # Get the data
  ln1_data = reactive({get_data_ln("fRL", input$ln1_agents,
                                   input$ln1_alpha_pos1, input$ln1_alpha_neg1, input$ln1_beta1, NA, 
                                   input$ln1_alpha_pos2, input$ln1_alpha_neg2, input$ln1_beta2, NA)})
  
  # Get the plots
  output$ln1_attrition = renderPlotly(plot_attrition(ln1_data(), input$ln1_agents))
  output$ln1_error = renderPlotly(plot_error_to_criterion(ln1_data()))
  output$ln1_trial = renderPlotly(plot_trial_to_criterion(ln1_data()))
  
  # Download
  output$table = renderTable(ln1_data())
  output$ln1_download <- downloadHandler(
    filename = function() {paste("LN-fRL", ".csv", sep = "")},
    content = function(file) {write.csv(ln1_data(), file)})
  
  # ----------- LN cafRL ------------
  
  # Get the data
  ln2_data = reactive({get_data_ln("cafRL", input$ln2_agents,
                                   input$ln2_alpha_pos1, input$ln2_alpha_neg1, input$ln2_beta1, input$ln2_theta01, 
                                   input$ln2_alpha_pos2, input$ln2_alpha_neg2, input$ln2_beta2, input$ln2_theta02)})
  
  # Get the plots
  output$ln2_attrition = renderPlotly(plot_attrition(ln2_data(), input$ln2_agents))
  output$ln2_error = renderPlotly(plot_error_to_criterion(ln2_data()))
  output$ln2_trial = renderPlotly(plot_trial_to_criterion(ln2_data()))
  
  # Download
  output$table = renderTable(ln2_data())
  output$ln2_download <- downloadHandler(
    filename = function() {paste("LN-cafRL", ".csv", sep = "")},
    content = function(file) {write.csv(ln2_data(), file)})
  
}
  
