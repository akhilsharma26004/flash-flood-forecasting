# Dynamic Shiny dashboard for the Bengalas watershed forecasting project
# Run from the PaperIEEE folder: shiny::runApp("app_dynamic.R", launch.browser = TRUE)

required <- c("shiny", "ggplot2", "data.table")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install missing packages: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))")

library(shiny)
library(ggplot2)
library(data.table)

model_results <- data.frame(
  Model = c("Persistence", "RF: level + local rainfall", "RF: augmented 506 features", "RF: authors-aligned 505 features"),
  MAE = c(0.008399465, 0.009670543, 0.012090007, 0.012174870),
  RMSE = c(0.03213073, 0.03290018, 0.03804183, 0.03808398),
  N = rep(10459, 4), stringsAsFactors = FALSE
)
model_results$MAE_change_vs_persistence_pct <- (model_results$MAE / model_results$MAE[1] - 1) * 100
masking_results <- data.frame(
  Masking_rate = rep(c(0, 10, 20, 40), 2),
  Model = rep(c("RF augmented 506 features", "RF authors-aligned 505 features"), each = 4),
  MAE_change_pct = c(0, 4.89, 8.24, 7.24, 0, 4.67, 6.84, 5.41)
)

project_dir <- getwd()
data_candidates <- c(
  file.path(project_dir, "dataset", "bacia_rio_bengalas_2019_2023_out01h00m.csv"),
  file.path(project_dir, "..", "dataset", "bacia_rio_bengalas_2019_2023_out01h00m.csv")
)
data_path <- data_candidates[file.exists(data_candidates)][1]
if (length(data_path) == 0 || is.na(data_path)) data_path <- NA_character_

css <- "body{background:#f3f6fb;color:#182235}.hero{background:linear-gradient(120deg,#102a43,#176b87);color:white;padding:25px;border-radius:15px;margin:12px 0 20px}.hero h1{font-weight:800;margin-top:0}.muted{color:#64748b;font-size:12px}.card{background:white;border:1px solid #e1e8f0;border-radius:13px;padding:16px;margin-bottom:14px;box-shadow:0 3px 12px rgba(20,40,70,.04)}.kpi{font-size:25px;font-weight:800;color:#116a86}.kpilabel{font-size:13px;color:#64748b}.warning{background:#fff7e6;border-left:5px solid #e6a23c;padding:13px;border-radius:8px}.positive{background:#eaf7f0;border-left:5px solid #26966a;padding:13px;border-radius:8px}.sidebar{background:#fff}.nav-tabs>li>a{font-weight:650}"

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  div(class="hero", h1("Flash-Flood Forecasting | Interactive Research Dashboard"),
      p("One-hour-ahead water-level forecasting • Bengalas watershed • R / Shiny"),
      p(class="muted", "Research analysis only — not a live flood-warning system")),
  sidebarLayout(
    sidebarPanel(class="sidebar",
      h4("Dashboard controls"),
      radioButtons("view_mode", "Analysis view", choices=c("Recorded experiment results"="results", "Explore source dataset"="data"), selected="results"),
      conditionalPanel("input.view_mode == 'results'",
        selectInput("metric", "Model comparison metric", choices=c("MAE"="MAE", "RMSE"="RMSE", "MAE change vs persistence (%)"="MAE_change_vs_persistence_pct"), selected="MAE"),
        checkboxGroupInput("models", "Models to display", choices=model_results$Model, selected=model_results$Model),
        sliderInput("mask_rate", "Recorded rainfall masking level (%)", min=0, max=40, value=20, step=10),
        checkboxGroupInput("mask_models", "Masking models", choices=unique(masking_results$Model), selected=unique(masking_results$Model))
      ),
      conditionalPanel("input.view_mode == 'data'",
        actionButton("load_data", "Load source data", class="btn-primary"),
        br(), br(), uiOutput("date_ui"),
        selectInput("series", "Series to plot", choices=c("Observed water level"="nivel_conselheiro", "One-hour-ahead target"="out01h00m", "Local rainfall"="chuva_conselheiro")),
        selectInput("rain_feature", "Optional grid rainfall feature", choices=c("None"="none"))
      ),
      hr(), p(class="muted", "Recorded model metrics are precomputed. Dataset exploration loads only selected columns after you click Load source data.")
    ),
    mainPanel(
      conditionalPanel("input.view_mode == 'results'",
        fluidRow(
          column(3, div(class="card", div(class="kpi", "10,459"), div(class="kpilabel", "Aligned test rows"))),
          column(3, div(class="card", div(class="kpi", "4"), div(class="kpilabel", "Recorded model variants"))),
          column(3, div(class="card", div(class="kpi", "300"), div(class="kpilabel", "Random Forest trees"))),
          column(3, div(class="card", div(class="kpi", "2026"), div(class="kpilabel", "Experiment seed")))
        ),
        tabsetPanel(
          tabPanel("Performance comparison", br(), div(class="card", h3("Filtered model comparison"), plotOutput("perf_plot", height="400px")), div(class="card", h3("Metrics table"), tableOutput("metrics_table"), downloadButton("download_metrics", "Download filtered metrics CSV"))),
          tabPanel("Rainfall masking", br(), div(class="card", h3("Recorded validation masking results"), plotOutput("mask_plot", height="360px"), tableOutput("mask_table")), div(class="warning", strong("Interpretation guardrail"), p("Masking is simulated, not a test of real sensor outages. Results are non-monotonic. The displayed recorded values are only available at 0%, 10%, 20%, and 40%; no unmeasured rates are invented."))),
          tabPanel("Research interpretation", br(), div(class="card", h3("Main finding"), div(class="warning", strong("Persistence achieved the lowest recorded test error."), p("The current results do not support claiming that Random Forest outperforms persistence.")), h4("Evaluation setup"), tags$ul(tags$li("Chronological eligible split: train 48,916; validation 10,468; test 10,459."), tags$li("Random Forest: 300 trees; mtry = floor(sqrt(number of predictors)); minimum node size = 5."), tags$li("Forecast reconstruction: current observed level plus predicted target change."), tags$li("The test period was inspected during earlier exploratory development; do not describe it as a completely untouched holdout."), tags$li("Target physical units have not been verified from dataset documentation."), tags$li("This app does not train models, ingest live sensors, or issue warnings."))))
        )
      ),
      conditionalPanel("input.view_mode == 'data'",
        div(class="card", h3("Dataset explorer"), uiOutput("data_status"), plotOutput("series_plot", height="400px"), p(class="muted", "Date filtering and plotted values use the source CSV. If no timestamp column is detected, inspect the column names and update the timestamp detection rule in app_dynamic.R.")),
        div(class="card", h3("Preview of selected columns"), tableOutput("data_preview"), downloadButton("download_data", "Download filtered data CSV"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$perf_plot <- renderPlot({
    d <- model_results[model_results$Model %in% input$models, , drop=FALSE]
    validate(need(nrow(d) > 0, "Select at least one model."))
    d$Value <- d[[input$metric]]
    ylab <- switch(input$metric, MAE="MAE (source units not verified)", RMSE="RMSE (source units not verified)", MAE_change_vs_persistence_pct="MAE change vs persistence (%)")
    ggplot(d, aes(x=reorder(Model, Value), y=Value)) + geom_col() + coord_flip() +
      labs(title=paste("Test-set", input$metric, "— filtered models"), x=NULL, y=ylab) +
      theme_minimal(base_size=13) + theme(plot.title=element_text(face="bold"), panel.grid.minor=element_blank())
  })
  output$metrics_table <- renderTable({
    d <- model_results[model_results$Model %in% input$models, , drop=FALSE]
    d$MAE <- signif(d$MAE, 7); d$RMSE <- signif(d$RMSE, 7); d$MAE_change_vs_persistence_pct <- round(d$MAE_change_vs_persistence_pct, 2)
    names(d) <- c("Model", "Test MAE", "Test RMSE", "N", "MAE change vs persistence (%)"); d
  }, striped=TRUE, bordered=TRUE, spacing="s", width="100%")
  output$download_metrics <- downloadHandler(filename=function() "filtered_test_metrics.csv", content=function(file) {
    d <- model_results[model_results$Model %in% input$models, , drop=FALSE]; data.table::fwrite(d, file)
  })
  output$mask_plot <- renderPlot({
    d <- masking_results[masking_results$Masking_rate == input$mask_rate & masking_results$Model %in% input$mask_models, , drop=FALSE]
    validate(need(nrow(d)>0, "Select at least one masking model."))
    ggplot(d, aes(x=reorder(Model, MAE_change_pct), y=MAE_change_pct)) + geom_col() + coord_flip() +
      labs(title=paste0(input$mask_rate, "% simulated masking — validation MAE change"), x=NULL, y="MAE change vs 0% masking (%)") +
      theme_minimal(base_size=13) + theme(plot.title=element_text(face="bold"), panel.grid.minor=element_blank())
  })
  output$mask_table <- renderTable({
    d <- masking_results[masking_results$Masking_rate == input$mask_rate & masking_results$Model %in% input$mask_models, , drop=FALSE]
    names(d) <- c("Masking rate (%)", "Model", "MAE change (%)"); d$`MAE change (%)` <- round(d$`MAE change (%)`, 2); d
  }, striped=TRUE, bordered=TRUE, spacing="s", width="100%")

  raw_data <- eventReactive(input$load_data, {
    req(!is.na(data_path))
    hdr <- names(data.table::fread(data_path, nrows=0, showProgress=FALSE))
    time_candidates <- hdr[grepl("timestamp|datetime|date.?time|^date$|^time$|data_hora|datahora", hdr, ignore.case=TRUE)]
    if (!length(time_candidates)) time_candidates <- hdr[1]
    target_cols <- intersect(c(time_candidates[1], "nivel_conselheiro", "chuva_conselheiro", "out01h00m", "precip0", "precip1", "precip2", "precip3", "precip4", "precip5", "precip10", "precip50", "precip100", "precip200", "precip300", "precip400", "precip503"), hdr)
    validate(need(length(target_cols)>=2, "Could not find expected timestamp/target columns. Check CSV headers."))
    d <- data.table::fread(data_path, select=unique(target_cols), showProgress=TRUE, data.table=FALSE)
    names(d)[names(d)==time_candidates[1]] <- "dashboard_timestamp"
    d$dashboard_timestamp <- suppressWarnings(as.POSIXct(d$dashboard_timestamp, tz="UTC", tryFormats=c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M")))
    if (all(is.na(d$dashboard_timestamp))) {
      d$dashboard_timestamp <- suppressWarnings(as.POSIXct(as.Date(as.character(d[["dashboard_timestamp"]])), tz="UTC"))
    }
    d
  })
  output$data_status <- renderUI({
    if (is.na(data_path)) div(class="warning", strong("CSV not found."), p("Expected dataset/bacia_rio_bengalas_2019_2023_out01h00m.csv inside the project folder."))
    else div(class="positive", strong("Dataset file found"), p(basename(data_path)), p(normalizePath(data_path, winslash="/", mustWork=FALSE)))
  })
  output$date_ui <- renderUI({
    d <- tryCatch(raw_data(), error=function(e) NULL)
    if (is.null(d) || all(is.na(d$dashboard_timestamp))) return(helpText("Load source data first. Timestamp parsing may require a format adjustment."))
    dateRangeInput("date_range", "Date range", start=min(as.Date(d$dashboard_timestamp), na.rm=TRUE), end=max(as.Date(d$dashboard_timestamp), na.rm=TRUE), min=min(as.Date(d$dashboard_timestamp), na.rm=TRUE), max=max(as.Date(d$dashboard_timestamp), na.rm=TRUE))
  })
  filtered_data <- reactive({
    d <- raw_data(); req(d)
    if (!is.null(input$date_range) && length(input$date_range)==2 && !all(is.na(d$dashboard_timestamp))) {
      d <- d[as.Date(d$dashboard_timestamp) >= input$date_range[1] & as.Date(d$dashboard_timestamp) <= input$date_range[2], , drop=FALSE]
    }
    d
  })
  observeEvent(raw_data(), {
    d <- raw_data(); cols <- setdiff(names(d), "dashboard_timestamp")
    updateSelectInput(session, "series", choices=setNames(cols, cols), selected=if ("nivel_conselheiro" %in% cols) "nivel_conselheiro" else cols[1])
    grid <- grep("^precip", cols, value=TRUE)
    updateSelectInput(session, "rain_feature", choices=c("None"="none", setNames(grid, grid)))
  }, once=TRUE)
  output$series_plot <- renderPlot({
    d <- filtered_data(); req(nrow(d)>0)
    chosen <- input$series
    validate(need(chosen %in% names(d), "The selected series is not available in the loaded columns."))
    if (!is.null(input$rain_feature) && input$rain_feature != "none" && input$rain_feature %in% names(d)) chosen <- input$rain_feature
    pdat <- data.frame(Time=d$dashboard_timestamp, Value=suppressWarnings(as.numeric(d[[chosen]])))
    pdat <- pdat[!is.na(pdat$Time) & is.finite(pdat$Value), , drop=FALSE]
    validate(need(nrow(pdat)>0, "No valid numeric observations are available for this selection."))
    ggplot(pdat, aes(x=Time, y=Value)) + geom_line(linewidth=.35) +
      labs(title=paste("Source series:", chosen), x="Timestamp", y=chosen, subtitle=paste(format(min(pdat$Time), "%Y-%m-%d"), "to", format(max(pdat$Time), "%Y-%m-%d"), "| n =", nrow(pdat))) +
      theme_minimal(base_size=13) + theme(plot.title=element_text(face="bold"), panel.grid.minor=element_blank())
  })
  output$data_preview <- renderTable({
    d <- filtered_data(); head(d, 12)
  }, striped=TRUE, bordered=TRUE, spacing="xs", width="100%")
  output$download_data <- downloadHandler(filename=function() "filtered_source_data.csv", content=function(file) data.table::fwrite(filtered_data(), file))
}

shinyApp(ui, server)
