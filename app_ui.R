# ui.R
# تعریف رابط کاربری کامل

app_ui <- shiny::fluidPage(
  theme = bslib::bs_theme(
    bootswatch = "cyborg",
    primary = "#2c3e50",
    success = "#18bc9c",
    info = "#3498db",
    danger = "#e74c3c"
  ),
  shinyjs::useShinyjs(),

  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      body { font-family: 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif; }
      .sidebarPanel {
        background-color: #1a1a1a;
        border-radius: 12px;
        padding: 20px;
      }
      .btn {
        border-radius: 8px;
        font-weight: 500;
        transition: all 0.2s ease;
        margin-bottom: 6px;
      }
      .btn:hover { transform: translateY(-1px); }
      .btn-primary {
        background-color: #375a7f;
        border-color: #2c3e50;
      }
      .btn-primary:hover { background-color: #2c3e50; }
      .btn-success {
        background-color: #18bc9c;
        border-color: #14987d;
      }
      .tab-content {
        background-color: #1f1f1f;
        border-radius: 12px;
        padding: 20px;
        margin-top: 15px;
      }
      .nav-tabs > li > a {
        font-weight: 600;
        color: #bbc6d4;
      }
      .nav-tabs > li.active > a {
        color: white;
        border-bottom: 2px solid #18bc9c;
      }
      pre {
        background-color: #0d0d0d;
        border-radius: 10px;
        padding: 15px;
      }
      h2, h3, h4 { font-weight: 600; }
      .prediction-box {
        background-color: #2a2a2a;
        border-radius: 12px;
        padding: 15px;
        margin-top: 20px;
        border-left: 4px solid #18bc9c;
      }
      .interpretation-box {
        background-color: #2a2a2a;
        border-radius: 12px;
        padding: 15px;
        margin-top: 20px;
        border-left: 4px solid #2c7fb8;
      }
      .error-box {
        background-color: #3a1a1a;
        border-left: 4px solid #e74c3c;
        padding: 15px;
        border-radius: 8px;
        color: #e74c3c;
      }
    "))
  ),

  shiny::titlePanel(
    shiny::div(
      shiny::icon("chart-line"), " Meta-Analysis Software"
    )
  ),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      class = "sidebarPanel",
      width = 3,
      shiny::fileInput("file", label = shiny::span(shiny::icon("file-excel"), " Upload Excel File"), accept = ".xlsx"),
      shiny::uiOutput("sheet_select"),
      br(),
      shiny::selectInput("analysis_type", label = shiny::span(shiny::icon("chart-line"), " Analysis Type"),
                         choices = c("Pairwise Meta-Analysis",
                                     "Dose-Response Meta-Analysis",
                                     "Meta-Regression"),
                         selected = "Pairwise Meta-Analysis"),
      hr(),
      shiny::uiOutput("column_mapping_ui"),
      shiny::uiOutput("subgroup_select"),
      shiny::conditionalPanel(
        condition = "input.analysis_type == 'Meta-Regression'",
        shiny::uiOutput("covariate_select")
      ),
      shiny::conditionalPanel(
        condition = "input.analysis_type == 'Pairwise Meta-Analysis'",
        shiny::selectInput("effect_size", label = shiny::span(shiny::icon("ruler"), " Effect Size"),
                           choices = c("Mean Difference (MD)" = "MD",
                                       "Standardized Mean Difference (SMD)" = "SMD",
                                       "Ratio of Means (ROM)" = "ROM"),
                           selected = "MD")
      ),
      br(),
      shiny::actionButton("analyze", label = shiny::span(shiny::icon("play"), " Run Analysis"),
                          class = "btn-primary btn-lg", style = "width: 100%;"),
      br(), br(),
      shiny::downloadButton("download_report", label = " Save Report (HTML)",
                            class = "btn-success", style = "width: 100%;"),
      br(),
      shiny::div(style = "text-align: center; font-size: 12px; margin-top: 20px; color: #7f8c8d;",
                 "Meta-Analysis Tool | R/Shiny")
    ),

    shiny::mainPanel(
      width = 9,
      shiny::tabsetPanel(
        type = "pills",
        shiny::tabPanel(shiny::span(shiny::icon("clipboard-list"), " Summary"),
                        shiny::uiOutput("summary"),
                        shiny::downloadButton("download_summary", "Download TXT", class = "btn-sm")),
        shiny::tabPanel(shiny::span(shiny::icon("tree"), " Forest Plot"),
                        shiny::plotOutput("forestPlot", height = "2000px"),
                        shiny::uiOutput("forest_interpretation"),
                        shiny::downloadButton("download_forest", "Download PNG", class = "btn-sm")),
        shiny::tabPanel(shiny::span(shiny::icon("chart-simple"), " Funnel Plot"),
                        shiny::plotOutput("funnelPlot", height = "800px"),
                        shiny::uiOutput("funnel_interpretation"),
                        shiny::downloadButton("download_funnel", "Download PNG", class = "btn-sm")),
        shiny::tabPanel(shiny::span(shiny::icon("chart-line"), " Dose-Response"),
                        shiny::conditionalPanel(
                          condition = "input.analysis_type == 'Dose-Response Meta-Analysis'",
                          shiny::plotOutput("doseResponsePlot", height = "600px"),
                          shiny::uiOutput("dose_interpretation"),
                          shiny::downloadButton("download_dose_response", "Download PNG", class = "btn-sm"),
                          shiny::div(class = "prediction-box",
                                     shiny::h4(shiny::icon("syringe"), " Predict Effect for a New Dose"),
                                     shiny::p("Enter a dose value (within or slightly outside the observed range) and click 'Predict Effect Size'."),
                                     shiny::numericInput("new_dose", "Dose value:", value = NULL, step = 0.1),
                                     shiny::actionButton("predict_dr_btn", "Predict Effect Size", class = "btn-info"),
                                     shiny::br(), shiny::br(),
                                     shiny::verbatimTextOutput("predicted_effect"),
                                     shiny::plotOutput("prediction_plot", height = "500px")
                          )
                        )),
        shiny::tabPanel(shiny::span(shiny::icon("chart-simple"), " Meta-Regression"),
                        shiny::conditionalPanel(
                          condition = "input.analysis_type == 'Meta-Regression'",
                          shiny::plotOutput("bubblePlot", height = "600px"),
                          shiny::uiOutput("bubble_interpretation"),
                          shiny::downloadButton("download_bubble", "Download PNG", class = "btn-sm")
                        )),
        shiny::tabPanel(shiny::span(shiny::icon("gear"), " Sensitivity"),
                        shiny::tabsetPanel(
                          shiny::tabPanel("Leave‑One‑Out",
                                          shiny::uiOutput("leave_one_out_ui"),
                                          shiny::uiOutput("loo_interpretation"),
                                          shiny::downloadButton("download_loo", "Download PNG", class = "btn-sm")),
                          shiny::tabPanel("Trim & Fill",
                                          shiny::h4("Trim and Fill Method (Publication Bias Adjustment)"),
                                          shiny::verbatimTextOutput("trimfillSummary"),
                                          shiny::uiOutput("trimfill_interpretation"),
                                          shiny::plotOutput("trimfillPlot", height = "600px"),
                                          shiny::downloadButton("download_trimfill_plot", "Plot PNG", class = "btn-sm"),
                                          shiny::downloadButton("download_trimfill_summary", "Summary TXT", class = "btn-sm"),
                                          shiny::hr(),
                                          shiny::h4("Egger's Test for Funnel Plot Asymmetry"),
                                          shiny::verbatimTextOutput("egger_test_result"),
                                          shiny::hr(),
                                          shiny::h4("Begg's Rank Correlation Test"),
                                          shiny::verbatimTextOutput("begg_test_result"),
                                          shiny::hr(),
                                          shiny::h4("Small Study Analysis (Exclude studies with SE > median)"),
                                          shiny::verbatimTextOutput("small_study_summary"),
                                          shiny::uiOutput("small_study_interpretation"),
                                          shiny::h4("Forest Plot Comparison"),
                                          shiny::plotOutput("small_study_forest", height = "800px"),
                                          shiny::downloadButton("download_small_study_forest", "Download PNG", class = "btn-sm")
                          )
                        ))
      )
    )
  )
)
