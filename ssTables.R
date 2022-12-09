snp_table <- function(df) {
  datatable(df[,c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele", "N_Samples", "AF"), drop = F],
            colnames  = c("SNP ID", "CHR", "POS", "Non-Effect Allele", "Effect Allele", "N", "AF"),
            rownames  = FALSE,
            selection = "single",
            class     = 'cell-border stripe',
            caption   = htmltools::tags$caption("Table 1: SNP Information"),
            options   = list(dom          = "tp",
                             scrollX      = TRUE, 
                             pageLength   = 5,
                             lengthChange = F,
                             columnDefs   = list(list(className = 'dt-center', targets = "_all"))))
}


beta_se_table <- function(row, df, robust_beta, robust_se) {
  if (is.null(row)) {
    return(NULL)
  }
  
  beta_prefix <- ""
  se_prefix   <- ""
  caption     <- "Table 2: "
  
  if (robust_beta & any(grepl("robust_Beta_", colnames(df)))) {
    beta_prefix <- "robust_"
    caption <- paste0(caption, "Robust Effect Estimates and ")
  } else {
    caption <- paste0(caption, "Effect Estimates and ")
  }
  
  if (robust_se) {
    se_prefix <- "robust_"
    caption   <- paste0(caption, "Robust Standard Errors")
  } else {
    caption <- paste0(caption, "Standard Errors")
  }
  
  beta_cols <- paste0(beta_prefix, c("Beta_Marginal", "Beta_G", colnames(df)[grepl("^Beta_G-", colnames(df))]))
  temp  <- df[row, beta_cols, drop = FALSE]
  betas <- gsub(paste0(beta_prefix, "Beta_"), "", colnames(temp))
  
  se_cols <- paste0(se_prefix, "SE_Beta_", betas)
  temp  <- rbind(temp, setNames(df[row, se_cols, drop = FALSE], names(temp)))
  
  colnames(temp) <- c("Marginal", "Main", gsub("-", " x ", betas[3:length(betas)]))
  rownames(temp) <- c("Effect Estimates", "Standard Errors")
  
  datatable(temp,
            class   = 'cell-border stripe',
            caption = caption,
            options = list(scrollX = TRUE, 
                           pageLength = 2,
                           dom = 't', 
                           ordering = FALSE,
                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
}


covar_table <- function(row, df, robust) {
  if (is.null(row)) {
    return(NULL)
  }
  
  prefix  <- ""
  caption <- "Table 3: Covariances"
  
  if (robust) {
    prefix  <- "robust_"
    caption <- "Table 3: Robust Covariances"
  }
  
  cov_cols <- paste0(prefix, colnames(df)[grepl("^Cov", colnames(df))])
  temp <- df[row, cov_cols, drop = FALSE]
  
  cn1 <- gsub("^Cov_|^robust_Cov_", "cov(", colnames(temp))
  cn1 <- gsub("Beta_", "", cn1)
  cn1 <- gsub("G_", "Main, ", cn1)
  cn1 <- gsub("G-", "G x ", cn1)
  cn1 <- gsub("_", ", ", cn1)
  colnames(temp) <- paste0(cn1, ")")
  rownames(temp) <- "Covariance"
  temp["Covariance", ] <- signif(temp["Covariance",], 6)
  
  datatable(t(temp),
            class = 'cell-border stripe',
            caption = caption,
            options = list(dom = 't',
                           pageLength = 5, 
                           scrollY    = TRUE, 
                           ordering   = FALSE,
                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
}


pvalue_table <- function(row, df, robust) {
  if (is.null(row)) {
    return(NULL)
  }
  
  prefix  <- ""
  caption <- "Table 4: P-values"
  
  if (robust) {
    prefix  <- "robust_"
    caption <- "Table 4: Robust P-values"
  }
  
  p_cols <- paste0(prefix, colnames(df)[grepl("^P_Value", colnames(df))])
  temp   <- df[row, p_cols, drop = FALSE]
  
  colnames(temp) <- gsub("^P_Value_|^robust_P_Value_", "", colnames(temp))
  rownames(temp) <- "P-value"
  temp["P-value", ] <- signif(10^-temp["P-value",], 6)
  
  datatable(t(temp),
            caption = caption,
            class   = 'cell-border stripe',
            options = list(dom        = 't',
                           scrollX    = TRUE,
                           pageLength = 3,
                           ordering   = FALSE,
                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
}


url_table <- function(row, df) {
  if (is.null(row)) {
    return(NULL)
  }
  
  db <- c("https://www.ncbi.nlm.nih.gov/snp/")
  
  urls <- paste0(db, df[row, "SNPID", drop =  TRUE])
  m <- matrix(paste0('<a href="',  urls, '">dbSNP</a>'), 1, 1)
  datatable(m, 
            escape   = FALSE,
            colnames = "",
            class    = "compact",
            rownames = "Databases:",
            options  = list(dom        = 't',
                            pageLength = 1,
                            scrollX    = FALSE, 
                            ordering   = FALSE,
                            columnDefs = list(list(className = 'dt-left', targets = "_all"))))
}