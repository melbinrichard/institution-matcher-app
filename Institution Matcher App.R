# Auto-install required packages if not already installed
required_packages <- c(
  "shiny", "shinythemes", "readxl", "writexl", "pdftools",
  "tesseract", "stringdist", "docxtractr", "DT", "shinycssloaders", "tools"
)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(required_packages, install_if_missing))

# Load libraries
library(shiny)
library(shinythemes)
library(readxl)
library(writexl)
library(pdftools)
library(tesseract)
library(stringdist)
library(docxtractr)
library(DT)
library(shinycssloaders)

normalize_text <- function(text) {
  text <- tolower(text)
  text <- gsub("[^a-z0-9 ]", " ", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(tags$style(HTML("
    .btn { font-weight: bold; }
    .form-group { margin-bottom: 20px; }
    .shiny-input-container { margin-bottom: 15px; }
    .well { background-color: #f7f7f7; padding: 20px; border-radius: 8px; }
  "))),
  titlePanel("Institution Matcher App"),
  
  sidebarLayout(
    sidebarPanel(
      class = "well",
      helpText("Upload an Excel (.xlsx) file with a single sheet. The first row must be headers, and the first column must contain Institution Names."),
      fileInput("excel_file", "Master Institution List (.xlsx)", accept = ".xlsx"),
      tags$hr(),
      
      helpText("Upload up to 20 CV files (.pdf, .docx, .jpg, .png). File names will be used for reference."),
      fileInput("cv_files", "Candidate CV Files", multiple = TRUE, accept = c(".pdf", ".docx", ".jpg", ".jpeg", ".png")),
      tags$hr(),
      
      actionButton("run_match", "Start Matching", class = "btn btn-primary"),
      tags$span(HTML("&nbsp;")),
      actionButton("reset_all", "Clear All", class = "btn btn-warning"),
      br(), br(),
      tags$hr(),
      
      downloadButton("download_results", "Download Matched Institutions", class = "btn btn-success")
    ),
    
    mainPanel(
      withSpinner(DTOutput("match_table"))
    )
  )
)

server <- function(input, output, session) {
  results_data <- reactiveVal()
  
  observeEvent(input$reset_all, {
    session$reload()
  })
  
  observeEvent(input$run_match, {
    req(input$excel_file)
    req(input$cv_files)
    
    if (length(input$cv_files$datapath) > 20) {
      showModal(modalDialog(
        title = "Upload Limit Exceeded",
        "You can upload a maximum of 20 CV files at once.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    institutions_df <- read_excel(input$excel_file$datapath, col_types = "text")
    institution_names <- institutions_df[[1]]
    institution_names_norm <- sapply(institution_names, normalize_text)
    institution_regexes <- paste0("\\b", institution_names_norm, "\\b")
    
    institution_columns <- names(institutions_df)
    result_cols <- c("CV File Name", "Institution Name", "Match Similarity", institution_columns[-1])
    results <- data.frame(matrix(ncol = length(result_cols), nrow = 0))
    colnames(results) <- result_cols
    
    cv_paths <- input$cv_files$datapath
    cv_names <- input$cv_files$name
    n_cv <- length(cv_paths)
    
    withProgress(message = "Processing CVs", value = 0, {
      for (i in seq_along(cv_paths)) {
        current_cv <- cv_names[i]
        incProgress(1/n_cv, detail = paste("Working on:", current_cv))
        
        cv_path <- cv_paths[i]
        cv_filename <- cv_names[i]
        ext <- tolower(tools::file_ext(cv_path))
        cv_text <- ""
        
        if (ext == "pdf") {
          pages <- tryCatch(pdf_text(cv_path), error = function(e) NULL)
          cv_text <- if (!is.null(pages)) paste(pages, collapse = "\n") else ""
        } else if (ext %in% c("jpg", "jpeg", "png")) {
          cv_text <- tryCatch(ocr(cv_path, engine = tesseract("eng")), error = function(e) "")
          cv_text <- gsub("[^[:print:]]", " ", cv_text)
        } else if (ext == "docx") {
          try_text <- tryCatch(paste(docx_extract_all_text(read_docx(cv_path)), collapse = "\n"), error = function(e) "")
          cv_text <- try_text
        }
        
        edu_matches <- regmatches(cv_text, gregexpr("(?i)(education)(.*?)(experience|projects|skills|certifications|$)", cv_text, perl = TRUE))
        edu_section <- if (length(edu_matches[[1]]) > 0) paste(edu_matches[[1]], collapse = " ") else cv_text
        edu_text_norm <- normalize_text(edu_section)
        
        text_words <- unlist(strsplit(edu_text_norm, " "))
        cv_chunks <- unlist(lapply(5:10, function(w) {
          if (length(text_words) >= w) sapply(1:(length(text_words) - w + 1), function(j) {
            paste(text_words[j:(j + w - 1)], collapse = " ")
          }) else NULL
        }))
        cv_chunks <- unique(cv_chunks[nchar(cv_chunks) > 5])
        cv_chunks_norm <- as.character(sapply(cv_chunks, normalize_text))
        
        matches_found <- list()
        seen_institutions <- character()
        
        for (j in seq_len(nrow(institutions_df))) {
          inst_name <- institution_names[j]
          inst_norm <- institution_names_norm[j]
          inst_regex <- institution_regexes[j]
          if (inst_name %in% seen_institutions) next
          
          possible_dupes <- which(institution_names == inst_name)
          multiple_entries <- length(possible_dupes) > 1
          matched <- FALSE
          score <- NA
          
          if (grepl(inst_regex, edu_text_norm)) {
            matched <- TRUE
            score <- "exact"
          } else {
            dists <- stringdist(inst_norm, cv_chunks_norm, method = "lv")
            max_lens <- pmax(nchar(inst_norm), nchar(cv_chunks_norm))
            similarities <- 1 - (dists / max_lens)
            if (any(similarities >= 0.90)) {
              best_idx <- which.max(similarities)
              matched <- TRUE
              score <- round(similarities[best_idx], 3)
            }
          }
          
          if (matched) {
            matched_details <- NULL
            if (multiple_entries) {
              matched_details <- as.list(rep("", length(institution_columns) - 1))
              names(matched_details) <- institution_columns[-1]
              score <- "duplicate institutions, check manually"
            } else {
              matched_details <- as.list(institutions_df[j, -1])
            }
            
            new_row <- c(`CV File Name` = cv_filename,
                         `Institution Name` = inst_name,
                         `Match Similarity` = score,
                         matched_details)
            matches_found[[length(matches_found) + 1]] <- new_row
            seen_institutions <- c(seen_institutions, inst_name)
          }
        }
        
        if (length(matches_found) == 0) {
          empty_row <- as.list(rep("not found", length(result_cols)))
          names(empty_row) <- result_cols
          empty_row$`CV File Name` <- cv_filename
          matches_found[[1]] <- empty_row
        }
        
        for (row in matches_found) {
          row_df <- as.data.frame(t(row), stringsAsFactors = FALSE)
          colnames(row_df) <- result_cols
          results <- rbind(results, row_df)
        }
      }
    })
    
    results[is.na(results)] <- ""
    results_data(results)
  })
  
  output$match_table <- renderDT({
    req(results_data())
    datatable(results_data(), options = list(scrollX = TRUE))
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("matched_results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # FIX: convert all columns to character to avoid data type warnings
      clean_data <- as.data.frame(lapply(results_data(), as.character), stringsAsFactors = FALSE)
      write_xlsx(clean_data, path = file)
    }
  )
}

shinyApp(ui, server)
