library(shiny)
library(ggplot2)
library(data.table)
library(DT)
source("manhattanPlot.R")
source("ssTables.R")
source("tabPanel.R")


# Define UI
ui <- fluidPage(
  h1(id="big-heading", "GxE Viewer"),
  tags$style(HTML("#big-heading{font-size: 60px; padding-left: 50px;}")),
  hr(), br(),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      fileInput("file1", "*GEM Output File:", 
                accept = c("text/plain", ".txt", ".out")),
      
      fluidRow(
        column(
          width = 5,
          checkboxGroupInput(inputId = "mhplot", 
                             label   = "",
                             choices = c("Marginal"    = "mrg_mhplot",
                                         "Interaction" = "int_mhplot",
                                         "Joint"       = "jnt_mhplot"))
        ),
        column(
          width = 3,
          checkboxGroupInput(inputId = "mb_mhplot_opt", 
                             label   = "",
                             choices = c("MB" = "mb_mrg_mhplot_opt",
                                         "MB" = "mb_int_mhplot_opt",
                                         "MB" = "mb_jnt_mhplot_opt"))
        ),
        column(
          width = 1,
          checkboxGroupInput(inputId = "rb_mhplot_opt", 
                             label   = "",
                             choices = c("RB" = "rb_mrg_mhplot_opt",
                                         "RB" = "rb_int_mhplot_opt",
                                         "RB" = "rb_jnt_mhplot_opt"))
        )
      )
    ),
    
    mainPanel(
      width = 7,
      tabsetPanel(id   = "manTabs",
                  type = "tabs",
                  tabPanel("Marginal",
                           tabsetPanel(id   = "mrgManTabs",
                                       type = "tabs",
                                       testsPanel("Model-based", "mb", "mrg"),
                                       testsPanel("Robust", "rb", "mrg"))),
                  tabPanel("Interaction",
                           tabsetPanel(id   = "intManTabs",
                                       type = "tabs",
                                       testsPanel("Model-based", "mb", "int"),
                                       testsPanel("Robust", "rb", "int"))),
                  tabPanel("Joint",
                           tabsetPanel(id   = "jntManTabs",
                                       type = "tabs",
                                       testsPanel("Model-based", "mb", "jnt"),
                                       testsPanel("Robust", "rb", "jnt"))))
    )
  )
)




server <- function(input, output, session) {
  # ---------------Read Input File---------------
  df <- reactive({
    req(input$file1)
    df <- as.data.frame(fread(input$file1$datapath, sep = "\t", header = T))
    df$P_Value_Marginal <- -log10(df$P_Value_Marginal)
    df$P_Value_Interaction <- -log10(df$P_Value_Interaction)
    df$P_Value_Joint <- -log10(df$P_Value_Joint)
    df$robust_P_Value_Marginal <- -log10(df$robust_P_Value_Marginal)
    df$robust_P_Value_Interaction <- -log10(df$robust_P_Value_Interaction)
    df$robust_P_Value_Joint <- -log10(df$robust_P_Value_Joint)
    return(df)
  })
  
  
  
  # ---------------Manhattan Plot Options---------------
  mhOpts <- reactive({
    return(c(input$mb_mhplot_opt, input$rb_mhplot_opt))
  })
  
  
  
  # ---------------Level 1 Tabs---------------
  observe({
    if ("mrg_mhplot" %in% input$mhplot) {
      showTab(inputId = "manTabs", target = "Marginal")
    } else {
      hideTab(inputId = "manTabs", target = "Marginal")
    }
    
    if ("int_mhplot" %in% input$mhplot) {
      showTab(inputId = "manTabs", target = "Interaction")
    } else {
      hideTab(inputId = "manTabs", target = "Interaction")
    }
    
    if ("jnt_mhplot" %in% input$mhplot) {
      showTab(inputId = "manTabs", target = "Joint")
    } else {
      hideTab(inputId = "manTabs", target = "Joint")
    }
  })
  
  
  
  # ---------------Level 2 Tabs Model-based---------------
  observe({
    if ("mb_mrg_mhplot_opt" %in% input$mb_mhplot_opt) {
      showTab(inputId = "mrgManTabs", target = "Model-based")
    } else {
      hideTab(inputId = "mrgManTabs", target = "Model-based")
    }
    
    if ("mb_int_mhplot_opt" %in% input$mb_mhplot_opt) {
      showTab(inputId = "intManTabs", target = "Model-based")
    } else {
      hideTab(inputId = "intManTabs", target = "Model-based")
    }
    
    if ("mb_jnt_mhplot_opt" %in% input$mb_mhplot_opt) {
      showTab(inputId = "jntManTabs", target = "Model-based")
    } else {
      hideTab(inputId = "jntManTabs", target = "Model-based")
    }
  })
  
    
  
  # ---------------Level 2 Tabs Robust---------------
  observe({
    if ("rb_mrg_mhplot_opt" %in% input$rb_mhplot_opt) {
      showTab(inputId = "mrgManTabs", target = "Robust")
    } else {
      hideTab(inputId = "mrgManTabs", target = "Robust")
    }
    
    if ("rb_int_mhplot_opt" %in% input$rb_mhplot_opt) {
      showTab(inputId = "intManTabs", target = "Robust")
    } else {
      hideTab(inputId = "intManTabs", target = "Robust")
    }
    
    if ("rb_jnt_mhplot_opt" %in% input$rb_mhplot_opt) {
      showTab(inputId = "jntManTabs", target = "Robust")
    } else {
      hideTab(inputId = "jntManTabs", target = "Robust")
    }
  })
  
  
  
  

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
  
  
  
  
  
  # ---------------Robust Marginal Manhattan Plot---------------
  output$rb_mrg_mhplot <- renderPlot({
    if ("rb_mrg_mhplot_opt" %in% mhOpts()) {
      manhattan_plot(df(), pcol = "robust_P_Value_Marginal", title = "Robust Marginal P-values")
    }
  })
  
  
  df2 <- reactive({
    nearPoints(df(), input$rb_mrg_mhplot_click, xvar = "POS", yvar = "robust_P_Value_Marginal")
  })
  
  
  output$rb_mrg_pointData <- DT::renderDT({
    if (nrow(df2()) > 0) {
      snp_table(df2())
    }
  })
  
  
  observe({
    row <- input$rb_mrg_pointData_rows_selected
    if (!is.null(row)) {
      showTab(inputId = "rb_mrg_snpInfoTabs", target = "Summary Statistics")
    } else {
      hideTab(inputId = "rb_mrg_snpInfoTabs", target = "Summary Statistics")
    }
    
    output$rb_mrg_header  <- renderText({
      paste0(df2()[row, "SNPID", drop = TRUE])
    })
    
    output$rb_mrg_url     <- DT::renderDataTable({
      url_table(row, df2())
    })
    
    output$rb_mrg_beta_se <- DT::renderDataTable({
      beta_se_table(row, df2(), TRUE, TRUE)
    })
    output$rb_mrg_covars  <- DT::renderDataTable({ 
      covar_table(row, df2(), TRUE)
    })
    output$rb_mrg_pvals   <- DT::renderDataTable({ 
      pvalue_table(row, df2(), TRUE)
    })
  })





  # --------------Model-based Interaction Manhattan Plot---------------
  output$mb_int_mhplot <- renderPlot({
    if ("mb_int_mhplot_opt" %in% mhOpts()) {
      manhattan_plot(df(), pcol = "P_Value_Interaction", title = "Model-based Interaction P-values")
    }
  })
  
  
  df3 <- reactive({
    nearPoints(df(), input$mb_int_mhplot_click, xvar = "POS", yvar = "P_Value_Interaction")
  })
  
  
  output$mb_int_pointData <- DT::renderDT({
    if (nrow(df3()) > 0) {
      snp_table(df3())
    }
  })
  
  
  observe({
    row <- input$mb_int_pointData_rows_selected
    if (!is.null(row)) {
      showTab(inputId = "mb_int_snpInfoTabs", target = "Summary Statistics")
    } else {
      hideTab(inputId = "mb_int_snpInfoTabs", target = "Summary Statistics")
    }
    
    output$mb_int_header  <- renderText({
      paste0(df3()[row, "SNPID", drop = TRUE])
    })
    
    output$mb_int_url     <- DT::renderDataTable({
      url_table(row, df3())
    })
    
    output$mb_int_beta_se <- DT::renderDataTable({
      beta_se_table(row, df3(), FALSE, FALSE)
    })
    output$mb_int_covars  <- DT::renderDataTable({ 
      covar_table(row, df3(), FALSE)
    })
    output$mb_int_pvals   <- DT::renderDataTable({ 
      pvalue_table(row, df3(), FALSE)
    })
  })





  # ---------------Robust Interaction Manhattan Plot---------------
  output$rb_int_mhplot <- renderPlot({
    if ("rb_int_mhplot_opt" %in% mhOpts()) {
      manhattan_plot(df(), pcol = "robust_P_Value_Interaction", title = "Robust Interaction P-values")
    }
  })
  
  
  df4 <- reactive({
    nearPoints(df(), input$rb_int_mhplot_click, xvar = "POS", yvar = "robust_P_Value_Interaction")
  })
  
  
  output$rb_int_pointData <- DT::renderDT({
    if (nrow(df4()) > 0) {
      snp_table(df4())
    }
  })
  
  
  observe({
    row <- input$rb_int_pointData_rows_selected
    if (!is.null(row)) {
      showTab(inputId = "rb_int_snpInfoTabs", target = "Summary Statistics")
    } else {
      hideTab(inputId = "rb_int_snpInfoTabs", target = "Summary Statistics")
    }
    
    output$rb_int_header  <- renderText({
      paste0(df4()[row, "SNPID", drop = TRUE])
    })
    
    output$rb_int_url     <- DT::renderDataTable({
      url_table(row, df4())
    })
    
    output$rb_int_beta_se <- DT::renderDataTable({
      beta_se_table(row, df4(), TRUE, TRUE)
    })
    output$rb_int_covars  <- DT::renderDataTable({ 
      covar_table(row, df4(), TRUE)
    })
    output$rb_int_pvals   <- DT::renderDataTable({ 
      pvalue_table(row, df4(), TRUE)
    })
  })





  # ---------------Model-based Joint Manhattan Plot---------------
  output$mb_jnt_mhplot <- renderPlot({
    if ("mb_jnt_mhplot_opt" %in% mhOpts()) {
      manhattan_plot(df(), pcol = "P_Value_Joint", title = "Model-based Joint P-values")
    }
  })
  
  
  df5 <- reactive({
    nearPoints(df(), input$mb_jnt_mhplot_click, xvar = "POS", yvar = "P_Value_Joint")
  })
  
  
  output$mb_jnt_pointData <- DT::renderDT({
    if (nrow(df5()) > 0) {
      snp_table(df5())
    }
  })
  
  
  observe({
    row <- input$mb_jnt_pointData_rows_selected
    if (!is.null(row)) {
      showTab(inputId = "mb_jnt_snpInfoTabs", target = "Summary Statistics")
    } else {
      hideTab(inputId = "mb_jnt_snpInfoTabs", target = "Summary Statistics")
    }
    
    output$mb_jnt_header  <- renderText({
      paste0(df5()[row, "SNPID", drop = TRUE])
    })
    
    output$mb_jnt_url     <- DT::renderDataTable({
      url_table(row, df5())
    })
    
    output$mb_jnt_beta_se <- DT::renderDataTable({
      beta_se_table(row, df5(), FALSE, FALSE)
    })
    output$mb_jnt_covars  <- DT::renderDataTable({ 
      covar_table(row, df5(), FALSE)
    })
    output$mb_jnt_pvals   <- DT::renderDataTable({ 
      pvalue_table(row, df5(), FALSE)
    })
  })





  # ---------------Robust Joint Manhattan Plot---------------
  output$rb_jnt_mhplot <- renderPlot({
    if ("rb_jnt_mhplot_opt" %in% mhOpts()) {
      manhattan_plot(df(), pcol = "robust_P_Value_Joint", title = "Robust Joint P-values")
    }
  })
  
  
  df6 <- reactive({
    nearPoints(df(), input$rb_jnt_mhplot_click, xvar = "POS", yvar = "robust_P_Value_Joint")
  })
  
  
  output$rb_jnt_pointData <- DT::renderDT({
    if (nrow(df6()) > 0) {
      snp_table(df6())
    }
  })
  
  
  observe({
    row <- input$rb_jnt_pointData_rows_selected
    if (!is.null(row)) {
      showTab(inputId = "rb_jnt_snpInfoTabs", target = "Summary Statistics")
    } else {
      hideTab(inputId = "rb_jnt_snpInfoTabs", target = "Summary Statistics")
    }
    
    output$rb_jnt_header  <- renderText({
      paste0(df6()[row, "SNPID", drop = TRUE])
    })
    
    output$rb_jnt_url     <- DT::renderDataTable({
      url_table(row, df6())
    })
    
    output$rb_jnt_beta_se <- DT::renderDataTable({
      beta_se_table(row, df6(), TRUE, TRUE)
    })
    output$rb_jnt_covars  <- DT::renderDataTable({ 
      covar_table(row, df6(), TRUE)
    })
    output$rb_jnt_pvals   <- DT::renderDataTable({ 
      pvalue_table(row, df6(), TRUE)
    })
  })

}

shinyApp(ui, server)