library(shiny)
library(ggplot2)
library(data.table)
library(DT)

# Define UI
ui <- fluidPage(
  h1(id="big-heading", "GxE Viewer"),
  tags$style(HTML("#big-heading{font-size: 60px; padding-left: 50px;}")),
  hr(),
  br(),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      fileInput("file1", "Choose file:", 
                accept = c("text/plain", ".txt")),
      
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
                                       tabPanel("Model-based",
                                                fluidRow(
                                                          column(width = 12,
                                                            plotOutput("mb_mrg_mhplot", click = "mb_mrg_mhplot_click", brush =  "mb_mrg_mhplot_brush", height = "310px"),
                                                            br(),
                                                            br(),
                                                            br(),
                                                            DTOutput("mb_mrg_pointData"),
                                                            br(),
                                                            br(),
                                                            textOutput("mb_mrg_header"),
                                                            fluidRow(
                                                              column(
                                                                width = 2,
                                                                DTOutput("mb_mrg_url")
                                                              )
                                                            ),
                                                            br(),
                                                            tags$head(tags$style("#mb_mrg_header{color: Black;
                                                                                                 font-size: 25px;
                                                                                                 font-style: italic;
                                                                                                 font-weight: bold;
                                                                                                 font-family: FreeMono, monospace;
                                                                                                 text-decoration: underline;
                                                                                                 max-width:100%;
                                                                                                 }"
                                                                                )
                                                            ),
                                                            fluidRow(
                                                              column(
                                                                width = 5,
                                                                DTOutput("mb_mrg_beta_se")
                                                              ),
                                                              column(
                                                                width = 4,
                                                                DTOutput("mb_mrg_cov")
                                                              ),
                                                              column(
                                                                width = 3,
                                                                DTOutput("mb_mrg_p")
                                                              )
                                                            ),
                                                            br(),
                                                            br(),
                                                            br(),
                                                            br(),
                                                            br()
                                                          )
                                                        )
                                       ),
                                       tabPanel("Robust",
                                                fluidRow(
                                                column(width = 12,
                                                         plotOutput("rb_mrg_mhplot", click = "rb_mrg_mhplot_click", brush =  "rb_mrg_mhplot_brush", height = "310px"),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         DTOutput("rb_mrg_pointData"),
                                                         br(),
                                                         br(),
                                                         textOutput("rb_mrg_header"),
                                                         fluidRow(
                                                           column(
                                                             width = 2,
                                                             DTOutput("rb_mrg_url")
                                                           )
                                                         ),
                                                         br(),
                                                         tags$head(tags$style("#rb_mrg_header{color: Black;
                                                                                                 font-size: 25px;
                                                                                                 font-style: italic;
                                                                                                 font-weight: bold;
                                                                                                 font-family: FreeMono, monospace;
                                                                                                 text-decoration: underline;
                                                                                                 max-width:100%;
                                                                                                 }"
                                                                              )
                                                                   ),
                                                         fluidRow(
                                                           column(
                                                             width = 5,
                                                             DTOutput("rb_mrg_beta_se")
                                                           ),
                                                           column(
                                                             width = 4,
                                                             DTOutput("rb_mrg_cov")
                                                           ),
                                                           column(
                                                             width = 3,
                                                             DTOutput("rb_mrg_p")
                                                           )
                                                         ),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br(),
                                                         br()
                                                )
                                              )
                                       )
                           )
                ),
                tabPanel("Interaction",
                         tabsetPanel(id   = "intManTabs",
                                     type = "tabs",
                                     tabPanel("Model-based",
                                              fluidRow(
                                                column(width = 12,
                                                       plotOutput("mb_int_mhplot", click = "mb_int_mhplot_click", brush =  "mb_int_mhplot_brush", height = "310px"),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       DTOutput("mb_int_pointData"),
                                                       br(),
                                                       br(),
                                                       textOutput("mb_int_header"),
                                                       fluidRow(
                                                         column(
                                                           width = 2,
                                                           DTOutput("mb_int_url")
                                                         )
                                                       ),
                                                       br(),
                                                       tags$head(tags$style("#mb_int_header{color: Black;
                                                                                                 font-size: 25px;
                                                                                                 font-style: italic;
                                                                                                 font-weight: bold;
                                                                                                 font-family: FreeMono, monospace;
                                                                                                 text-decoration: underline;
                                                                                                 max-width:100%;
                                                                                                 }"
                                                       )
                                                       ),
                                                       fluidRow(
                                                         column(
                                                           width = 5,
                                                           DTOutput("mb_int_beta_se")
                                                         ),
                                                         column(
                                                           width = 4,
                                                           DTOutput("mb_int_cov")
                                                         ),
                                                         column(
                                                           width = 3,
                                                           DTOutput("mb_int_p")
                                                         )
                                                       ),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br()
                                                )
                                              )
                                     ),
                                     tabPanel("Robust",
                                              fluidRow(
                                                column(width = 12,
                                                       plotOutput("rb_int_mhplot", click = "rb_int_mhplot_click", brush =  "rb_int_mhplot_brush", height = "310px"),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       DTOutput("rb_int_pointData"),
                                                       br(),
                                                       br(),
                                                       textOutput("rb_int_header"),
                                                       fluidRow(
                                                         column(
                                                           width = 2,
                                                           DTOutput("rb_int_url")
                                                         )
                                                       ),
                                                       br(),
                                                       tags$head(tags$style("#rb_int_header{color: Black;
                                                                                                 font-size: 25px;
                                                                                                 font-style: italic;
                                                                                                 font-weight: bold;
                                                                                                 font-family: FreeMono, monospace;
                                                                                                 text-decoration: underline;
                                                                                                 max-width:100%;
                                                                                                 }"
                                                       )
                                                       ),
                                                       fluidRow(
                                                         column(
                                                           width = 5,
                                                           DTOutput("rb_int_beta_se")
                                                         ),
                                                         column(
                                                           width = 4,
                                                           DTOutput("rb_int_cov")
                                                         ),
                                                         column(
                                                           width = 3,
                                                           DTOutput("rb_int_p")
                                                         )
                                                       ),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br()
                                                )
                                              )
                                     )
                         )
                ),
                tabPanel("Joint",
                         tabsetPanel(id   = "jntManTabs",
                                     type = "tabs",
                                     tabPanel("Model-based",
                                              fluidRow(
                                                column(width = 12,
                                                       plotOutput("mb_jnt_mhplot", click = "mb_jnt_mhplot_click", brush =  "mb_jnt_mhplot_brush", height = "310px"),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       DTOutput("mb_jnt_pointData"),
                                                       br(),
                                                       br(),
                                                       textOutput("mb_jnt_header"),
                                                       fluidRow(
                                                         column(
                                                           width = 2,
                                                           DTOutput("mb_jnt_url")
                                                         )
                                                       ),
                                                       br(),
                                                       tags$head(tags$style("#mb_jnt_header{color: Black;
                                                                                                 font-size: 25px;
                                                                                                 font-style: italic;
                                                                                                 font-weight: bold;
                                                                                                 font-family: FreeMono, monospace;
                                                                                                 text-decoration: underline;
                                                                                                 max-width:100%;
                                                                                                 }"
                                                       )
                                                       ),
                                                       fluidRow(
                                                         column(
                                                           width = 5,
                                                           DTOutput("mb_jnt_beta_se")
                                                         ),
                                                         column(
                                                           width = 4,
                                                           DTOutput("mb_jnt_cov")
                                                         ),
                                                         column(
                                                           width = 3,
                                                           DTOutput("mb_jnt_p")
                                                         )
                                                       ),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br()
                                                )
                                              )
                                     ),
                                     tabPanel("Robust",
                                              fluidRow(
                                                column(width = 12,
                                                       plotOutput("rb_jnt_mhplot", click = "rb_jnt_mhplot_click", brush =  "rb_jnt_mhplot_brush", height = "310px"),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       DTOutput("rb_jnt_pointData"),
                                                       br(),
                                                       br(),
                                                       textOutput("rb_jnt_header"),
                                                       fluidRow(
                                                         column(
                                                           width = 2,
                                                           DTOutput("rb_jnt_url")
                                                         )
                                                       ),
                                                       br(),
                                                       tags$head(tags$style("#rb_jnt_header{color: Black;
                                                                                                 font-size: 25px;
                                                                                                 font-style: italic;
                                                                                                 font-weight: bold;
                                                                                                 font-family: FreeMono, monospace;
                                                                                                 text-decoration: underline;
                                                                                                 max-width:100%;
                                                                                                 }"
                                                       )
                                                       ),
                                                       fluidRow(
                                                         column(
                                                           width = 5,
                                                           DTOutput("rb_jnt_beta_se")
                                                         ),
                                                         column(
                                                           width = 4,
                                                           DTOutput("rb_jnt_cov")
                                                         ),
                                                         column(
                                                           width = 3,
                                                           DTOutput("rb_jnt_p")
                                                         )
                                                       ),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br()
                                                )
                                              )
                                     )
                         )
                )
      )
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
      font.size = 10
      axis.size = 0.5
      y.max <- floor(max(df()$P_Value_Marginal)) + 2
      
      ggplot(df(), aes(x = POS, y = P_Value_Marginal, colour = as.factor(CHR))) +
            geom_point() +
            facet_grid(.~CHR, scale = "free_x", switch = "x") +
            scale_y_continuous(expand = c(0, 0), limit = c(0, y.max),
                               breaks = seq(from = 0, to = y.max, by = 2)) +
            scale_x_continuous() +
            theme(legend.position = "none",
                  panel.background = element_blank(),
                  strip.background = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  panel.spacing.x=unit(0.1, "lines"),
                  axis.line.y = element_line(size = axis.size, color = "black"),
                  axis.ticks.y = element_line(size = axis.size, color = "black"),
                  axis.ticks.length = unit(axis.size * 10, "points"),
                  plot.title = element_text(hjust = (0.5), size = 18),
                  axis.title.y = element_text(size = 15),
                  axis.title.x = element_text(size = 15),
                  axis.text = element_text(size = 10),
                  strip.text.x = element_text(size = 10)) +
        labs(x = "Chromosome", y = "-log10(P)", title = "Model-based Marginal P-values")
    }
  })
  
  df1 <- reactive({
    nearPoints(df(), input$mb_mrg_mhplot_click, xvar = "POS", yvar = "P_Value_Marginal")
  })
  
  
  
  output$mb_mrg_pointData <- DT::renderDT({
    if (nrow(df1()) > 0) {
      temp <- df1()[,c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele", "N_Samples", "AF"), drop = F]
      datatable(temp,
                colnames=c("SNP ID", "CHR", "POS", "Non-Effect Allele", "Effect Allele", "N", "AF"),
                rownames = FALSE,
                class = 'cell-border stripe',
                caption = htmltools::tags$caption("Table 1: SNP Information"),
                selection = "single",
                options = list(scrollX = TRUE, pageLength = 5,
                               lengthChange = F,
                               dom = "tp",
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })
  
  

  output$mb_mrg_header <- renderText({ 
    if (length(input$mb_mrg_pointData_rows_selected)) {
      row <- input$mb_mrg_pointData_rows_selected
      paste0(df1()[row, "SNPID", drop = TRUE])
    }
  })
  
  
  output$mb_mrg_url <- DT::renderDataTable({
    if (length(input$mb_mrg_pointData_rows_selected)) {
      row <- input$mb_mrg_pointData_rows_selected
      urls <- paste0("https://www.ncbi.nlm.nih.gov/snp/", df1()[row, "SNPID", drop =  TRUE])
      m <- matrix(paste0('<a href="',  urls, '">dbSNP</a>'), 1, 1)
      colnames(m) = c('<em>Column 2</em>')
      datatable(m, 
                escape = FALSE,
                colnames = "",
                class = "compact",
                rownames = "Databases:",
                options = list(scrollX = F, pageLength = 1,
                               dom = 't', hover = FALSE, ordering = F,
                               columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    }
  })


  output$mb_mrg_beta_se <- DT::renderDataTable({
    if (length(input$mb_mrg_pointData_rows_selected)) {
      row <- input$mb_mrg_pointData_rows_selected

      temp <- df1()[row, grepl("^Beta_", colnames(df1())), drop = FALSE]
      cn1 <- gsub("Beta_", "", colnames(temp))
      colnames(temp) <- gsub("-", " x ", cn1)
      colnames(temp)[which(colnames(temp) == "G")] <- "Main"
      temp <- rbind(temp, setNames(df1()[row, paste0("SE_Beta_", cn1), drop = F], names(temp)))
      rownames(temp) <- c("Effect Estimates", "Standard Errors")
      datatable(temp,
                caption = "Table 2: Effect Estimates and Standard Errors",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 2,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$mb_mrg_cov <- DT::renderDT({
    if (length(input$mb_mrg_pointData_rows_selected)) {
      row <- input$mb_mrg_pointData_rows_selected
      temp <- df1()[row, grepl("^Cov_", colnames(df1())), drop = FALSE]
      cn1 <- gsub("Cov_", "cov(", colnames(temp))
      cn1 <- gsub("Beta_", "", cn1)
      cn1 <- gsub("G_", "Main, ", cn1)
      cn1 <- gsub("G-", "G x ", cn1)
      cn1 <- gsub("_", ", ", cn1)
      cn1 <- paste0(cn1, ")")
      colnames(temp) <- cn1
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "Covariance"

      datatable(temp,
                caption = "Table 3: Covariances",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 5, dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$mb_mrg_p <- DT::renderDT({
    if (length(input$mb_mrg_pointData_rows_selected)) {
      row <- input$mb_mrg_pointData_rows_selected
      temp <- df1()[row, grepl("^P_Value", colnames(df1())), drop = FALSE]
      colnames(temp) <- gsub("P_Value_", "", colnames(temp))
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "P-value"
      temp$`P-value` <- signif(10^-temp$`P-value`, 6)

      datatable(temp,
                caption = "Table 4: P-values",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 3,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })

  
  
  
  
  
  # ---------------Robust Marginal Manhattan Plot---------------
  output$rb_mrg_mhplot <- renderPlot({
    if ("rb_mrg_mhplot_opt" %in% mhOpts()) {
      font.size = 10
      axis.size = 0.5
      y.max <- floor(max(df()$robust_P_Value_Marginal)) + 2

      ggplot(df(), aes(x = POS, y = robust_P_Value_Marginal, colour = as.factor(CHR))) +
        geom_point() +
        facet_grid(.~CHR, scale = "free_x", switch = "x") +
        scale_y_continuous(expand = c(0, 0), limit = c(0, y.max),
                           breaks = seq(from = 0, to = y.max, by = 2)) +
        scale_x_continuous() +
        theme(legend.position = "none",
              panel.background = element_blank(),
              strip.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              panel.spacing.x=unit(0.1, "lines"),
              axis.line.y = element_line(size = axis.size, color = "black"),
              axis.ticks.y = element_line(size = axis.size, color = "black"),
              axis.ticks.length = unit(axis.size * 10, "points"),
              plot.title = element_text(hjust = (0.5), size = 18),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              axis.text = element_text(size = 10),
              strip.text.x = element_text(size = 10)) +
        labs(x = "Chromosome", y = "-log10(P)", title = "Robust Marginal P-values")
    }
  })

  df2 <- reactive({
    nearPoints(df(), input$rb_mrg_mhplot_click, xvar = "POS", yvar = "robust_P_Value_Marginal")
  })



  output$rb_mrg_pointData <- DT::renderDT({
    if (nrow(df2()) > 0) {
      temp <- df2()[,c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele", "N_Samples", "AF"), drop = F]
      datatable(temp,
                colnames=c("SNP ID", "CHR", "POS", "Non-Effect Allele", "Effect Allele", "N", "AF"),
                rownames = FALSE,
                class = 'cell-border stripe',
                caption = htmltools::tags$caption("Table 1: SNP Information"),
                selection = "single",
                options = list(scrollX = TRUE, pageLength = 5,
                               lengthChange = F,
                               dom = "tp",
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })



  output$rb_mrg_header <- renderText({
    if (length(input$rb_mrg_pointData_rows_selected)) {
      row <- input$rb_mrg_pointData_rows_selected
      paste0(df2()[row, "SNPID", drop = TRUE])
    }
  })


  output$rb_mrg_url <- DT::renderDataTable({
    if (length(input$rb_mrg_pointData_rows_selected)) {
      row <- input$rb_mrg_pointData_rows_selected
      urls <- paste0("https://www.ncbi.nlm.nih.gov/snp/", df2()[row, "SNPID", drop =  TRUE])
      m <- matrix(paste0('<a href="',  urls, '">dbSNP</a>'), 1, 1)
      colnames(m) = c('<em>Column 2</em>')
      datatable(m,
                escape = FALSE,
                colnames = "",
                class = "compact",
                rownames = "Databases:",
                options = list(scrollX = F, pageLength = 1,
                               dom = 't', hover = FALSE, ordering = F,
                               columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    }
  })


  output$rb_mrg_beta_se <- DT::renderDataTable({
    if (length(input$rb_mrg_pointData_rows_selected)) {
      row <- input$rb_mrg_pointData_rows_selected

      temp <- df2()[row, grepl("^Beta_", colnames(df2())), drop = FALSE]
      cn1 <- gsub("Beta_", "", colnames(temp))
      colnames(temp) <- gsub("-", " x ", cn1)
      colnames(temp)[which(colnames(temp) == "G")] <- "Main"
      temp <- rbind(temp, setNames(df2()[row, paste0("robust_SE_Beta_", cn1), drop = F], names(temp)))
      rownames(temp) <- c("Effect Estimates", "Standard Errors")
      datatable(temp,
                caption = "Table 2: Effect Estimates and Robust Standard Errors",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 2,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$rb_mrg_cov <- DT::renderDT({
    if (length(input$rb_mrg_pointData_rows_selected)) {
      row <- input$rb_mrg_pointData_rows_selected
      temp <- df2()[row, grepl("^robust_Cov_", colnames(df2())), drop = FALSE]
      cn1 <- gsub("robust_Cov_", "cov(", colnames(temp))
      cn1 <- gsub("Beta_", "", cn1)
      cn1 <- gsub("G_", "Main, ", cn1)
      cn1 <- gsub("G-", "G x ", cn1)
      cn1 <- gsub("_", ", ", cn1)
      cn1 <- paste0(cn1, ")")
      colnames(temp) <- cn1
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "Covariance"

      datatable(temp,
                caption = "Table 3: Robust Covariances",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 5, dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$rb_mrg_p <- DT::renderDT({
    if (length(input$rb_mrg_pointData_rows_selected)) {
      row <- input$rb_mrg_pointData_rows_selected
      temp <- df2()[row, grepl("^robust_P_Value", colnames(df2())), drop = FALSE]
      colnames(temp) <- gsub("robust_P_Value_", "", colnames(temp))
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "P-value"
      temp$`P-value` <- signif(10^-temp$`P-value`, 6)

      datatable(temp,
                caption = "Table 4: Robust P-values",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 3,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })








  #--------------Model-based Interaction Manhattan Plot---------------
  output$mb_int_mhplot <- renderPlot({
    if ("mb_int_mhplot_opt" %in% mhOpts()) {
      font.size = 10
      axis.size = 0.5
      y.max <- floor(max(df()$P_Value_Interaction)) + 2

      ggplot(df(), aes(x = POS, y = P_Value_Interaction, colour = as.factor(CHR))) +
        geom_point() +
        facet_grid(.~CHR, scale = "free_x", switch = "x") +
        scale_y_continuous(expand = c(0, 0), limit = c(0, y.max),
                           breaks = seq(from = 0, to = y.max, by = 2)) +
        scale_x_continuous() +
        theme(legend.position = "none",
              panel.background = element_blank(),
              strip.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              panel.spacing.x=unit(0.1, "lines"),
              axis.line.y = element_line(size = axis.size, color = "black"),
              axis.ticks.y = element_line(size = axis.size, color = "black"),
              axis.ticks.length = unit(axis.size * 10, "points"),
              plot.title = element_text(hjust = (0.5), size = 18),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              axis.text = element_text(size = 10),
              strip.text.x = element_text(size = 10)) +
        labs(x = "Chromosome", y = "-log10(P)", title = "Model-based Interaction P-values")
    }
  })

  df3 <- reactive({
    nearPoints(df(), input$mb_int_mhplot_click, xvar = "POS", yvar = "P_Value_Interaction")
  })



  output$mb_int_pointData <- DT::renderDT({
    if (nrow(df3()) > 0) {
      temp <- df3()[,c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele", "N_Samples", "AF"), drop = F]
      datatable(temp,
                colnames=c("SNP ID", "CHR", "POS", "Non-Effect Allele", "Effect Allele", "N", "AF"),
                rownames = FALSE,
                class = 'cell-border stripe',
                caption = htmltools::tags$caption("Table 1: SNP Information"),
                selection = "single",
                options = list(scrollX = TRUE, pageLength = 5,
                               lengthChange = F,
                               dom = "tp",
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$mb_int_header <- renderText({
    if (length(input$mb_int_pointData_rows_selected)) {
      row <- input$mb_int_pointData_rows_selected
      paste0(df3()[row, "SNPID", drop = TRUE])
    }
  })


  output$mb_int_url <- DT::renderDataTable({
    if (length(input$mb_int_pointData_rows_selected)) {
      row <- input$mb_int_pointData_rows_selected
      urls <- paste0("https://www.ncbi.nlm.nih.gov/snp/", df3()[row, "SNPID", drop =  TRUE])
      m <- matrix(paste0('<a href="',  urls, '">dbSNP</a>'), 1, 1)
      colnames(m) = c('<em>Column 2</em>')
      datatable(m,
                escape = FALSE,
                colnames = "",
                class = "compact",
                rownames = "Databases:",
                options = list(scrollX = F, pageLength = 1,
                               dom = 't', hover = FALSE, ordering = F,
                               columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    }
  })


  output$mb_int_beta_se <- DT::renderDataTable({
    if (length(input$mb_int_pointData_rows_selected)) {
      row <- input$mb_int_pointData_rows_selected

      temp <- df3()[row, grepl("^Beta_", colnames(df3())), drop = FALSE]
      cn1 <- gsub("Beta_", "", colnames(temp))
      colnames(temp) <- gsub("-", " x ", cn1)
      colnames(temp)[which(colnames(temp) == "G")] <- "Main"
      temp <- rbind(temp, setNames(df3()[row, paste0("SE_Beta_", cn1), drop = F], names(temp)))
      rownames(temp) <- c("Effect Estimates", "Standard Errors")
      datatable(temp,
                caption = "Table 2: Effect Estimates and Standard Errors",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 2,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$mb_int_cov <- DT::renderDT({
    if (length(input$mb_int_pointData_rows_selected)) {
      row <- input$mb_int_pointData_rows_selected
      temp <- df3()[row, grepl("^Cov_", colnames(df3())), drop = FALSE]
      cn1 <- gsub("Cov_", "cov(", colnames(temp))
      cn1 <- gsub("Beta_", "", cn1)
      cn1 <- gsub("G_", "Main, ", cn1)
      cn1 <- gsub("G-", "G x ", cn1)
      cn1 <- gsub("_", ", ", cn1)
      cn1 <- paste0(cn1, ")")
      colnames(temp) <- cn1
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "Covariance"

      datatable(temp,
                caption = "Table 3: Covariances",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 5, dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$mb_int_p <- DT::renderDT({
    if (length(input$mb_int_pointData_rows_selected)) {
      row <- input$mb_int_pointData_rows_selected
      temp <- df3()[row, grepl("^P_Value", colnames(df3())), drop = FALSE]
      colnames(temp) <- gsub("P_Value_", "", colnames(temp))
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "P-value"
      temp$`P-value` <- signif(10^-temp$`P-value`, 6)

      datatable(temp,
                caption = "Table 4: P-values",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 3,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })








  # ---------------Robust Interaction Manhattan Plot---------------
  output$rb_int_mhplot <- renderPlot({
    if ("rb_int_mhplot_opt" %in% mhOpts()) {
      font.size = 10
      axis.size = 0.5
      y.max <- floor(max(df()$robust_P_Value_Interaction)) + 2

      ggplot(df(), aes(x = POS, y = robust_P_Value_Interaction, colour = as.factor(CHR))) +
        geom_point() +
        facet_grid(.~CHR, scale = "free_x", switch = "x") +
        scale_y_continuous(expand = c(0, 0), limit = c(0, y.max),
                           breaks = seq(from = 0, to = y.max, by = 2)) +
        scale_x_continuous() +
        theme(legend.position = "none",
              panel.background = element_blank(),
              strip.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              panel.spacing.x=unit(0.1, "lines"),
              axis.line.y = element_line(size = axis.size, color = "black"),
              axis.ticks.y = element_line(size = axis.size, color = "black"),
              axis.ticks.length = unit(axis.size * 10, "points"),
              plot.title = element_text(hjust = (0.5), size = 18),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              axis.text = element_text(size = 10),
              strip.text.x = element_text(size = 10)) +
        labs(x = "Chromosome", y = "-log10(P)", title = "Robust Interaction P-values")
    }
  })

  df4 <- reactive({
    nearPoints(df(), input$rb_int_mhplot_click, xvar = "POS", yvar = "robust_P_Value_Interaction")
  })



  output$rb_int_pointData <- DT::renderDT({
    if (nrow(df4()) > 0) {
      temp <- df4()[,c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele", "N_Samples", "AF"), drop = F]
      datatable(temp,
                colnames=c("SNP ID", "CHR", "POS", "Non-Effect Allele", "Effect Allele", "N", "AF"),
                rownames = FALSE,
                class = 'cell-border stripe',
                caption = htmltools::tags$caption("Table 1: SNP Information"),
                selection = "single",
                options = list(scrollX = TRUE, pageLength = 5,
                               lengthChange = F,
                               dom = "tp",
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })



  output$rb_int_header <- renderText({
    if (length(input$rb_int_pointData_rows_selected)) {
      row <- input$rb_int_pointData_rows_selected
      paste0(df4()[row, "SNPID", drop = TRUE])
    }
  })


  output$rb_int_url <- DT::renderDataTable({
    if (length(input$rb_int_pointData_rows_selected)) {
      row <- input$rb_int_pointData_rows_selected
      urls <- paste0("https://www.ncbi.nlm.nih.gov/snp/", df4()[row, "SNPID", drop =  TRUE])
      m <- matrix(paste0('<a href="',  urls, '">dbSNP</a>'), 1, 1)
      colnames(m) = c('<em>Column 2</em>')
      datatable(m,
                escape = FALSE,
                colnames = "",
                class = "compact",
                rownames = "Databases:",
                options = list(scrollX = F, pageLength = 1,
                               dom = 't', hover = FALSE, ordering = F,
                               columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    }
  })


  output$rb_int_beta_se <- DT::renderDataTable({
    if (length(input$rb_int_pointData_rows_selected)) {
      row <- input$rb_int_pointData_rows_selected

      temp <- df4()[row, grepl("^Beta_", colnames(df4())), drop = FALSE]
      cn1 <- gsub("Beta_", "", colnames(temp))
      colnames(temp) <- gsub("-", " x ", cn1)
      colnames(temp)[which(colnames(temp) == "G")] <- "Main"
      temp <- rbind(temp, setNames(df4()[row, paste0("robust_SE_Beta_", cn1), drop = F], names(temp)))
      rownames(temp) <- c("Effect Estimates", "Standard Errors")
      datatable(temp,
                caption = "Table 2: Effect Estimates and Robust Standard Errors",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 2,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$rb_int_cov <- DT::renderDT({
    if (length(input$rb_int_pointData_rows_selected)) {
      row <- input$rb_int_pointData_rows_selected
      temp <- df4()[row, grepl("^robust_Cov_", colnames(df4())), drop = FALSE]
      cn1 <- gsub("robust_Cov_", "cov(", colnames(temp))
      cn1 <- gsub("Beta_", "", cn1)
      cn1 <- gsub("G_", "Main, ", cn1)
      cn1 <- gsub("G-", "G x ", cn1)
      cn1 <- gsub("_", ", ", cn1)
      cn1 <- paste0(cn1, ")")
      colnames(temp) <- cn1
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "Covariance"

      datatable(temp,
                caption = "Table 3: Robust Covariances",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 5, dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$rb_int_p <- DT::renderDT({
    if (length(input$rb_int_pointData_rows_selected)) {
      row <- input$rb_int_pointData_rows_selected
      temp <- df4()[row, grepl("^robust_P_Value", colnames(df4())), drop = FALSE]
      colnames(temp) <- gsub("robust_P_Value_", "", colnames(temp))
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "P-value"
      temp$`P-value` <- signif(10^-temp$`P-value`, 6)

      datatable(temp,
                caption = "Table 4: Robust P-values",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 3,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })








  # ---------------Model-based Joint Manhattan Plot---------------
  output$mb_jnt_mhplot <- renderPlot({
    if ("mb_jnt_mhplot_opt" %in% mhOpts()) {
      font.size = 10
      axis.size = 0.5
      y.max <- floor(max(df()$P_Value_Joint)) + 2

      ggplot(df(), aes(x = POS, y = P_Value_Joint, colour = as.factor(CHR))) +
        geom_point() +
        facet_grid(.~CHR, scale = "free_x", switch = "x") +
        scale_y_continuous(expand = c(0, 0), limit = c(0, y.max),
                           breaks = seq(from = 0, to = y.max, by = 2)) +
        scale_x_continuous() +
        theme(legend.position = "none",
              panel.background = element_blank(),
              strip.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              panel.spacing.x=unit(0.1, "lines"),
              axis.line.y = element_line(size = axis.size, color = "black"),
              axis.ticks.y = element_line(size = axis.size, color = "black"),
              axis.ticks.length = unit(axis.size * 10, "points"),
              plot.title = element_text(hjust = (0.5), size = 18),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              axis.text = element_text(size = 10),
              strip.text.x = element_text(size = 10)) +
        labs(x = "Chromosome", y = "-log10(P)", title = "Model-based Joint P-values")
    }
  })

  df5 <- reactive({
    nearPoints(df(), input$mb_jnt_mhplot_click, xvar = "POS", yvar = "P_Value_Joint")
  })



  output$mb_jnt_pointData <- DT::renderDT({
    if (nrow(df5()) > 0) {
      temp <- df5()[,c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele", "N_Samples", "AF"), drop = F]
      datatable(temp,
                colnames=c("SNP ID", "CHR", "POS", "Non-Effect Allele", "Effect Allele", "N", "AF"),
                rownames = FALSE,
                class = 'cell-border stripe',
                caption = htmltools::tags$caption("Table 1: SNP Information"),
                selection = "single",
                options = list(scrollX = TRUE, pageLength = 5,
                               lengthChange = F,
                               dom = "tp",
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })



  output$mb_jnt_header <- renderText({
    if (length(input$mb_jnt_pointData_rows_selected)) {
      row <- input$mb_jnt_pointData_rows_selected
      paste0(df5()[row, "SNPID", drop = TRUE])
    }
  })


  output$mb_jnt_url <- DT::renderDataTable({
    if (length(input$mb_jnt_pointData_rows_selected)) {
      row <- input$mb_jnt_pointData_rows_selected
      urls <- paste0("https://www.ncbi.nlm.nih.gov/snp/", df5()[row, "SNPID", drop =  TRUE])
      m <- matrix(paste0('<a href="',  urls, '">dbSNP</a>'), 1, 1)
      colnames(m) = c('<em>Column 2</em>')
      datatable(m,
                escape = FALSE,
                colnames = "",
                class = "compact",
                rownames = "Databases:",
                options = list(scrollX = F, pageLength = 1,
                               dom = 't', hover = FALSE, ordering = F,
                               columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    }
  })


  output$mb_jnt_beta_se <- DT::renderDataTable({
    if (length(input$mb_jnt_pointData_rows_selected)) {
      row <- input$mb_jnt_pointData_rows_selected

      temp <- df5()[row, grepl("^Beta_", colnames(df5())), drop = FALSE]
      cn1 <- gsub("Beta_", "", colnames(temp))
      colnames(temp) <- gsub("-", " x ", cn1)
      colnames(temp)[which(colnames(temp) == "G")] <- "Main"
      temp <- rbind(temp, setNames(df5()[row, paste0("SE_Beta_", cn1), drop = F], names(temp)))
      rownames(temp) <- c("Effect Estimates", "Standard Errors")
      datatable(temp,
                caption = "Table 2: Effect Estimates and Standard Errors",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 2,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$mb_jnt_cov <- DT::renderDT({
    if (length(input$mb_jnt_pointData_rows_selected)) {
      row <- input$mb_jnt_pointData_rows_selected
      temp <- df5()[row, grepl("^Cov_", colnames(df5())), drop = FALSE]
      cn1 <- gsub("Cov_", "cov(", colnames(temp))
      cn1 <- gsub("Beta_", "", cn1)
      cn1 <- gsub("G_", "Main, ", cn1)
      cn1 <- gsub("G-", "G x ", cn1)
      cn1 <- gsub("_", ", ", cn1)
      cn1 <- paste0(cn1, ")")
      colnames(temp) <- cn1
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "Covariance"

      datatable(temp,
                caption = "Table 3: Covariances",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 5, dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$mb_jnt_p <- DT::renderDT({
    if (length(input$mb_jnt_pointData_rows_selected)) {
      row <- input$mb_jnt_pointData_rows_selected
      temp <- df5()[row, grepl("^P_Value", colnames(df5())), drop = FALSE]
      colnames(temp) <- gsub("P_Value_", "", colnames(temp))
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "P-value"
      temp$`P-value` <- signif(10^-temp$`P-value`, 6)

      datatable(temp,
                caption = "Table 4: P-values",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 3,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })








  # ---------------Robust Joint Manhattan Plot---------------
  output$rb_jnt_mhplot <- renderPlot({
    if ("rb_jnt_mhplot_opt" %in% mhOpts()) {
      font.size = 10
      axis.size = 0.5
      y.max <- floor(max(df()$robust_P_Value_Joint)) + 2

      ggplot(df(), aes(x = POS, y = robust_P_Value_Joint, colour = as.factor(CHR))) +
        geom_point() +
        facet_grid(.~CHR, scale = "free_x", switch = "x") +
        scale_y_continuous(expand = c(0, 0), limit = c(0, y.max),
                           breaks = seq(from = 0, to = y.max, by = 2)) +
        scale_x_continuous() +
        theme(legend.position = "none",
              panel.background = element_blank(),
              strip.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              panel.spacing.x=unit(0.1, "lines"),
              axis.line.y = element_line(size = axis.size, color = "black"),
              axis.ticks.y = element_line(size = axis.size, color = "black"),
              axis.ticks.length = unit(axis.size * 10, "points"),
              plot.title = element_text(hjust = (0.5), size = 18),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              axis.text = element_text(size = 10),
              strip.text.x = element_text(size = 10)) +
        labs(x = "Chromosome", y = "-log10(P)", title = "Robust Joint P-values")
    }
  })

  df6 <- reactive({
    nearPoints(df(), input$rb_jnt_mhplot_click, xvar = "POS", yvar = "robust_P_Value_Joint")
  })



  output$rb_jnt_pointData <- DT::renderDT({
    if (nrow(df6()) > 0) {
      temp <- df6()[,c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele", "N_Samples", "AF"), drop = F]
      datatable(temp,
                colnames=c("SNP ID", "CHR", "POS", "Non-Effect Allele", "Effect Allele", "N", "AF"),
                rownames = FALSE,
                class = 'cell-border stripe',
                caption = htmltools::tags$caption("Table 1: SNP Information"),
                selection = "single",
                options = list(scrollX = TRUE, pageLength = 5,
                               lengthChange = F,
                               dom = "tp",
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })



  output$rb_jnt_header <- renderText({
    if (length(input$rb_jnt_pointData_rows_selected)) {
      row <- input$rb_jnt_pointData_rows_selected
      paste0(df6()[row, "SNPID", drop = TRUE])
    }
  })


  output$rb_jnt_url <- DT::renderDataTable({
    if (length(input$rb_jnt_pointData_rows_selected)) {
      row <- input$rb_jnt_pointData_rows_selected
      urls <- paste0("https://www.ncbi.nlm.nih.gov/snp/", df6()[row, "SNPID", drop =  TRUE])
      m <- matrix(paste0('<a href="',  urls, '">dbSNP</a>'), 1, 1)
      colnames(m) = c('<em>Column 2</em>')
      datatable(m,
                escape = FALSE,
                colnames = "",
                class = "compact",
                rownames = "Databases:",
                options = list(scrollX = F, pageLength = 1,
                               dom = 't', hover = FALSE, ordering = F,
                               columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    }
  })


  output$rb_jnt_beta_se <- DT::renderDataTable({
    if (length(input$rb_jnt_pointData_rows_selected)) {
      row <- input$rb_jnt_pointData_rows_selected

      temp <- df6()[row, grepl("^Beta_", colnames(df6())), drop = FALSE]
      cn1 <- gsub("Beta_", "", colnames(temp))
      colnames(temp) <- gsub("-", " x ", cn1)
      colnames(temp)[which(colnames(temp) == "G")] <- "Main"
      temp <- rbind(temp, setNames(df6()[row, paste0("robust_SE_Beta_", cn1), drop = F], names(temp)))
      rownames(temp) <- c("Effect Estimates", "Standard Errors")
      datatable(temp,
                caption = "Table 2: Effect Estimates and Robust Standard Errors",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 2,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$rb_jnt_cov <- DT::renderDT({
    if (length(input$rb_jnt_pointData_rows_selected)) {
      row <- input$rb_jnt_pointData_rows_selected
      temp <- df6()[row, grepl("^robust_Cov_", colnames(df6())), drop = FALSE]
      cn1 <- gsub("robust_Cov_", "cov(", colnames(temp))
      cn1 <- gsub("Beta_", "", cn1)
      cn1 <- gsub("G_", "Main, ", cn1)
      cn1 <- gsub("G-", "G x ", cn1)
      cn1 <- gsub("_", ", ", cn1)
      cn1 <- paste0(cn1, ")")
      colnames(temp) <- cn1
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "Covariance"

      datatable(temp,
                caption = "Table 3: Robust Covariances",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 5, dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })


  output$rb_jnt_p <- DT::renderDT({
    if (length(input$rb_jnt_pointData_rows_selected)) {
      row <- input$rb_jnt_pointData_rows_selected
      temp <- df6()[row, grepl("^robust_P_Value", colnames(df6())), drop = FALSE]
      colnames(temp) <- gsub("robust_P_Value_", "", colnames(temp))
      temp <- as.data.frame(t(temp))
      colnames(temp) <- "P-value"
      temp$`P-value` <- signif(10^-temp$`P-value`, 6)

      datatable(temp,
                caption = "Table 4: Robust P-values",
                class = 'cell-border stripe',
                options = list(scrollX = TRUE, pageLength = 3,
                               dom = 't', ordering = F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    }
  })
  

}

shinyApp(ui, server)