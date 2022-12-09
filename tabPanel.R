ssTablePanel <- function(short, type) {
  tabPanel("Summary Statistics",
           fluidRow(column = 12,
                    fluidRow(
                             column(width = 5,
                                    DTOutput(paste0(short,"_", type, "_beta_se"))
                             ),
                             column(width = 4,
                                    DTOutput(paste0(short,"_", type, "_covars"))
                             ),
                             column(width = 3,
                                    DTOutput(paste0(short,"_", type, "_pvals"))))))
}


testsPanel <- function(full, short, type) {
  tabPanel(full,
           fluidRow(
                    column(width = 12,
                           plotOutput(paste0(short,"_", type, "_mhplot"), 
                                      click  = paste0(short,"_", type, "_mhplot_click"), 
                                      height = "310px"),
                           br(),
                           DTOutput(paste0(short,"_", type, "_pointData")),
                           br(),
                           textOutput(paste0(short,"_", type, "_header")),
                           fluidRow(column(width = 2,
                                    DTOutput(paste0(short,"_", type, "_url")))),
                           br(),
                           tags$style(HTML(paste0("#", short, "_", type, "_url table.dataTable tr.selected td, table.dataTable td.selected {background-color: white !important;}"))),
                           tags$head(tags$style(paste0("#", short, "_", type, "_header{color: Black;
                                                                                       font-size: 25px;
                                                                                       font-style: italic;
                                                                                       font-weight: bold;
                                                                                       font-family: FreeMono, monospace;
                                                                                       text-decoration: underline;
                                                                                       max-width:100%;}"))),
                            tabsetPanel(id   = paste0(short, "_", type, "_snpInfoTabs"),
                                        type = "tabs",
                                        ssTablePanel(short, type)),
                            br(), br(), br(), br(), br())))
}