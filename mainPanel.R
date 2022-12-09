loadMain <- function(short, type) {
  # ---------------Model-based Marginal Manhattan Plot---------------
  output$mb_mrg_mhplot <- renderPlot({
    if ("mb_mrg_mhplot_opt" %in% mhOpts()) {
      manhattan_plot(df(), pcol = "P_Value_Marginal", title = "Model-based Marginal P-values")
    }
  })
  
  
  df1 <- reactive({
    nearPoints(df(), input$mb_mrg_mhplot_click, xvar = "POS", yvar = "P_Value_Marginal")
  })
  
  
  output$mb_mrg_pointData <- DT::renderDT({
    if (nrow(df1()) > 0) {
      snp_table(df1())
    }
  })
  
  
  observe({
    row <- input$mb_mrg_pointData_rows_selected
    if (!is.null(row)) {
      showTab(inputId = "mb_mrg_snpInfoTabs", target = "Summary Statistics")
    } else {
      hideTab(inputId = "mb_mrg_snpInfoTabs", target = "Summary Statistics")
    }
    
    output[["mb_mrg_header"]]  <- renderText({
      paste0(df1()[row, "SNPID", drop = TRUE])
    })
    
    output$mb_mrg_url     <- DT::renderDataTable({
      url_table(row, df1())
    })
    
    output$mb_mrg_beta_se <- DT::renderDataTable({
      beta_se_table(row, df1(), FALSE, FALSE)
    })
    
    output$mb_mrg_covars  <- DT::renderDataTable({ 
      covar_table(row, df1(), FALSE)
    })
    
    output$mb_mrg_pvals   <- DT::renderDataTable({ 
      pvalue_table(row, df1(), FALSE)
    })
  })
  
  
}