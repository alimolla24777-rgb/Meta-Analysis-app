# server.R

app_server <- function(input, output, session) {

  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

  # ================================================================
  # ================================================================

  interpret_effect_size <- function(TE, sm, pval) {
    if (is.na(TE) || is.na(pval)) return("Effect size could not be interpreted.")
    sig <- if (pval < 0.05) "statistically significant" else "not statistically significant"
    direction <- if (TE > 0) "increase" else "decrease"
    magnitude <- ""
    if (sm %in% c("MD", "SMD")) {
      abs_te <- abs(TE)
      if (abs_te < 0.2) magnitude <- "very small"
      else if (abs_te < 0.5) magnitude <- "small"
      else if (abs_te < 0.8) magnitude <- "moderate"
      else magnitude <- "large"
    }
    if (sig == "statistically significant") {
      txt <- sprintf("The pooled effect size (%s) was %.3f, indicating a %s effect that is %s.",
                     sm, TE, magnitude, sig)
    } else {
      txt <- sprintf("The pooled effect (%.3f) was not statistically significant.", TE)
    }
    return(txt)
  }

  interpret_ci <- function(lower, upper, null_val = 0) {
    if (is.na(lower) || is.na(upper)) return("Confidence interval not available.")
    if (lower <= null_val && upper >= null_val) {
      txt <- sprintf("The confidence interval (%.2f to %.2f) crossed the null value (%.1f), indicating no statistically significant effect.",
                     lower, upper, null_val)
    } else {
      txt <- sprintf("The confidence interval (%.2f to %.2f) did not cross the null value (%.1f), indicating statistical significance.",
                     lower, upper, null_val)
    }
    return(txt)
  }

  interpret_pvalue <- function(pval) {
    if (is.na(pval)) return("P-value not available.")
    if (pval < 0.001) return("Highly statistically significant (p < 0.001).")
    else if (pval < 0.05) return(sprintf("Statistically significant (p = %.3f).", pval))
    else return(sprintf("Not statistically significant (p = %.3f).", pval))
  }

  interpret_heterogeneity <- function(I2, Q_pval) {
    if (!is.na(I2) && I2 <= 1) I2 <- I2 * 100
    if (is.na(I2)) return("Heterogeneity could not be calculated.")
    level <- if (I2 < 25) "Low"
    else if (I2 < 50) "Moderate"
    else if (I2 < 75) "Substantial"
    else "Considerable"
    txt <- sprintf("Heterogeneity was %s (I² = %.1f%%).", level, I2)
    if (!is.na(Q_pval)) {
      if (Q_pval < 0.05) txt <- paste(txt, "The Q-test was significant (p < 0.05), indicating evidence of heterogeneity.")
      else txt <- paste(txt, "The Q-test was not significant (p ≥ 0.05), suggesting no significant heterogeneity.")
    }
    return(txt)
  }

  interpret_tau2 <- function(tau2, I2) {
    if (is.na(tau2)) return("Tau² not available.")
    if (!is.na(I2) && I2 <= 1) I2 <- I2 * 100
    if (!is.na(I2)) {
      if (I2 < 25) level <- "very low"
      else if (I2 < 50) level <- "low"
      else if (I2 < 75) level <- "moderate"
      else level <- "high"
      return(sprintf("Between-study variance (τ²) was %.3f, which is considered %s given the observed heterogeneity.", tau2, level))
    } else {
      return(sprintf("Between-study variance (τ²) was %.3f.", tau2))
    }
  }

  interpret_prediction_interval <- function(pred_lower, pred_upper, null_val = 0) {
    if (is.na(pred_lower) || is.na(pred_upper)) return(NULL)
    if (pred_lower <= null_val && pred_upper >= null_val) {
      return("The prediction interval included the null value, suggesting that future studies may show no effect.")
    } else {
      return("The prediction interval did not include the null value, suggesting that future studies are likely to show an effect in the same direction.")
    }
  }

  # ---------- سطح ۱: تفسیر آماری ----------
  statistical_interpretation <- function(res, analysis_type, sm = "MD") {
    msgs <- list()

    if (analysis_type == "Pairwise Meta-Analysis" && inherits(res, "meta")) {
      TE <- tryCatch(res$TE.random, error = function(e) NA)
      lower <- tryCatch(res$lower.random, error = function(e) NA)
      upper <- tryCatch(res$upper.random, error = function(e) NA)
      pval <- tryCatch(res$pval.random, error = function(e) NA)
      I2 <- tryCatch(res$I2, error = function(e) NA)
      Q_pval <- tryCatch(res$pval.Q, error = function(e) NA)
      tau2 <- tryCatch(res$tau2, error = function(e) NA)

      if (!is.na(TE) && !is.na(lower) && !is.na(upper) && !is.na(pval)) {
        null_val <- 0
        msgs$effect <- interpret_effect_size(TE, sm, pval)
        msgs$ci <- interpret_ci(lower, upper, null_val)
        msgs$pvalue <- interpret_pvalue(pval)
      } else {
        msgs$effect <- "Effect size could not be interpreted due to missing values."
      }

      if (!is.na(I2)) {
        msgs$heterogeneity <- interpret_heterogeneity(I2, Q_pval)
      } else {
        msgs$heterogeneity <- "Heterogeneity could not be calculated."
      }

      if (!is.na(tau2)) {
        msgs$tau2 <- interpret_tau2(tau2, I2)
      }

      pred_lower <- tryCatch(res$lower.predict, error = function(e) NA)
      pred_upper <- tryCatch(res$upper.predict, error = function(e) NA)
      if (!is.na(pred_lower) && !is.na(pred_upper)) {
        msgs$pred_interval <- interpret_prediction_interval(pred_lower, pred_upper, 0)
      }
    }

    else if (analysis_type == "Meta-Regression" && is.list(res) && !is.null(res$meta)) {
      meta <- res$meta
      if (inherits(meta, "meta")) {
        TE <- tryCatch(meta$TE.random, error = function(e) NA)
        lower <- tryCatch(meta$lower.random, error = function(e) NA)
        upper <- tryCatch(meta$upper.random, error = function(e) NA)
        pval <- tryCatch(meta$pval.random, error = function(e) NA)
        I2 <- tryCatch(meta$I2, error = function(e) NA)
        Q_pval <- tryCatch(meta$pval.Q, error = function(e) NA)
        tau2 <- tryCatch(meta$tau2, error = function(e) NA)

        if (!is.na(TE) && !is.na(lower) && !is.na(upper) && !is.na(pval)) {
          null_val <- 0
          msgs$effect <- interpret_effect_size(TE, sm, pval)
          msgs$ci <- interpret_ci(lower, upper, null_val)
          msgs$pvalue <- interpret_pvalue(pval)
        }
        if (!is.na(I2)) msgs$heterogeneity <- interpret_heterogeneity(I2, Q_pval)
        if (!is.na(tau2)) msgs$tau2 <- interpret_tau2(tau2, I2)
      }
    }

    else if (analysis_type == "Dose-Response Meta-Analysis" && is.list(res) && !is.null(res$model)) {
      model <- res$model
      if (inherits(model, "dosresmeta")) {
        p_overall <- tryCatch({
          coefs <- coef(model)
          vcov_mat <- vcov(model)
          if (is.null(coefs) || is.null(vcov_mat)) NA
          else {
            wald_stat <- t(coefs) %*% solve(vcov_mat) %*% coefs
            df <- length(coefs)
            pchisq(wald_stat, df = df, lower.tail = FALSE)
          }
        }, error = function(e) NA)
        msgs$overall <- if (!is.na(p_overall)) {
          paste("The overall dose-response relationship was",
                if (p_overall < 0.05) "statistically significant" else "not statistically significant",
                sprintf("(p = %.3f).", p_overall))
        } else "Overall test could not be performed."

        p_nonlin <- tryCatch({
          coefs <- coef(model)
          spline_terms <- grep("rcs|ns", names(coefs))
          if (length(spline_terms) < 2) NA
          else {
            vcov_mat <- vcov(model)
            idx <- spline_terms
            wald_stat <- t(coefs[idx]) %*% solve(vcov_mat[idx, idx]) %*% coefs[idx]
            df <- length(idx)
            pchisq(wald_stat, df = df, lower.tail = FALSE)
          }
        }, error = function(e) NA)
        msgs$nonlinearity <- if (!is.na(p_nonlin)) {
          if (p_nonlin < 0.05) {
            sprintf("A significant non-linear dose-response relationship was detected (p for non-linearity = %.3f).", p_nonlin)
          } else {
            sprintf("No evidence of non-linearity was found (p for non-linearity = %.3f); the relationship may be linear.", p_nonlin)
          }
        } else "Non-linearity could not be tested."
      } else {
        msgs$note <- "Invalid dose-response model object."
      }
    }

    else {
      msgs$note <- "Statistical interpretation is not available for this analysis type or incomplete results."
    }

    return(msgs)
  }

  methodological_interpretation <- function(res, analysis_type,
                                            egger_obj, begg_obj, trimfill_obj,
                                            loo_obj, small_obj, covariate_name = NULL) {
    msgs <- list()

    # Publication Bias
    bias_msgs <- c()
    if (!is.null(egger_obj)) {
      pval <- tryCatch(egger_obj$p.value, error = function(e) NA)
      if (!is.na(pval)) {
        bias_msgs <- c(bias_msgs,
                       if (pval < 0.05) "Egger's test suggested possible publication bias (p < 0.05)."
                       else "Egger's test showed no evidence of publication bias (p ≥ 0.05).")
      } else bias_msgs <- c(bias_msgs, "Egger's test could not be performed.")
    } else bias_msgs <- c(bias_msgs, "Egger's test not available.")

    if (!is.null(begg_obj)) {
      pval <- tryCatch(begg_obj$p.value, error = function(e) NA)
      if (!is.na(pval)) {
        bias_msgs <- c(bias_msgs,
                       if (pval < 0.05) "Begg's test suggested possible publication bias (p < 0.05)."
                       else "Begg's test showed no evidence of publication bias (p ≥ 0.05).")
      } else bias_msgs <- c(bias_msgs, "Begg's test could not be performed.")
    } else bias_msgs <- c(bias_msgs, "Begg's test not available.")

    if (!is.null(trimfill_obj)) {
      filled <- tryCatch(sum(trimfill_obj$filled, na.rm = TRUE), error = function(e) NA)
      if (!is.na(filled)) {
        if (filled == 0) {
          bias_msgs <- c(bias_msgs, "Trim and Fill did not impute any missing studies, suggesting robustness to publication bias.")
        } else {
          bias_msgs <- c(bias_msgs, sprintf("Trim and Fill imputed %d studies; the adjusted effect may differ from the original.", filled))
        }
      } else bias_msgs <- c(bias_msgs, "Trim and Fill results could not be interpreted.")
    } else bias_msgs <- c(bias_msgs, "Trim and Fill not performed.")

    msgs$publication_bias <- paste(bias_msgs, collapse = " ")

    # Leave-One-Out
    if (!is.null(loo_obj) && inherits(loo_obj, "metainf") && !is.null(res) && inherits(res, "meta")) {
      orig_ci_lower <- tryCatch(res$lower.random, error = function(e) NA)
      orig_ci_upper <- tryCatch(res$upper.random, error = function(e) NA)
      if (!is.na(orig_ci_lower) && !is.na(orig_ci_upper)) {
        te_vals <- tryCatch(loo_obj$TE, error = function(e) NULL)
        if (!is.null(te_vals) && length(te_vals) > 0) {
          influential <- any(te_vals < orig_ci_lower | te_vals > orig_ci_upper, na.rm = TRUE)
          if (!influential) {
            msgs$loo <- "Leave-One-Out analysis showed that the pooled estimate remained stable after sequential omission of each study."
          } else {
            idx <- which(te_vals < orig_ci_lower | te_vals > orig_ci_upper)
            study_labels <- tryCatch(res$studlab, error = function(e) NULL)
            if (is.null(study_labels) || length(study_labels) < max(idx)) {
              msgs$loo <- "Some studies had a substantial influence on the pooled estimate (details not available)."
            } else {
              msgs$loo <- sprintf("Study(ies) %s had a substantial influence on the pooled estimate; results should be interpreted with caution.",
                                  paste(study_labels[idx], collapse = ", "))
            }
          }
        } else msgs$loo <- "Leave-One-Out analysis could not be interpreted."
      } else msgs$loo <- "Original confidence intervals missing."
    } else msgs$loo <- "Leave-One-Out analysis not available."

    # Small study effect
    if (!is.null(small_obj) && inherits(small_obj, "meta") && !is.null(res) && inherits(res, "meta")) {
      orig_te <- tryCatch(res$TE.random, error = function(e) NA)
      small_te <- tryCatch(small_obj$TE.random, error = function(e) NA)
      if (!is.na(orig_te) && !is.na(small_te)) {
        if (abs(orig_te - small_te) > 0.1) {
          msgs$small_study <- "Excluding studies with SE > median changed the pooled estimate noticeably, suggesting small-study effects."
        } else {
          msgs$small_study <- "Excluding studies with SE > median did not substantially change the result, indicating robustness."
        }
      } else msgs$small_study <- "Small study analysis could not be interpreted."
    } else msgs$small_study <- "Small study analysis not performed."

    # Meta-regression
    if (analysis_type == "Meta-Regression" && is.list(res) && !is.null(res$metareg)) {
      metareg <- res$metareg
      pval <- tryCatch({
        p <- metareg$pval
        if (length(p) >= 2) p[2] else p[1]
      }, error = function(e) NA)
      if (!is.null(pval) && !is.na(pval)) {
        if (pval < 0.05) {
          msgs$metareg <- sprintf("The covariate '%s' significantly explained between-study heterogeneity (p = %.3f).",
                                  covariate_name, pval)
        } else {
          msgs$metareg <- sprintf("The covariate '%s' did not significantly explain heterogeneity (p = %.3f).",
                                  covariate_name, pval)
        }
        coef_val <- tryCatch(metareg$b, error = function(e) NA)
        ci_low <- tryCatch(metareg$ci.lb, error = function(e) NA)
        ci_up <- tryCatch(metareg$ci.ub, error = function(e) NA)
        if (!is.na(coef_val) && !is.na(ci_low) && !is.na(ci_up)) {
          msgs$metareg_coef <- sprintf("The regression coefficient was %.3f (95%% CI: %.3f to %.3f).", coef_val, ci_low, ci_up)
        }
      } else msgs$metareg <- "Meta-regression p-value not available."
    }

    # Dose-response model description
    if (analysis_type == "Dose-Response Meta-Analysis" && is.list(res) && !is.null(res$model)) {
      msgs$dose_model <- "Dose-response model was fitted using restricted cubic splines (rcs) with knots chosen based on exposure distribution."
    }

    msgs <- msgs[!sapply(msgs, is.null)]
    return(msgs)
  }

  writing_assistant <- function(res, analysis_type, sm = "MD",
                                statistical_msgs, methodological_msgs) {
    txt <- ""

    if (analysis_type == "Pairwise Meta-Analysis" && inherits(res, "meta")) {
      TE <- tryCatch(round(res$TE.random, 3), error = function(e) NA)
      lower <- tryCatch(round(res$lower.random, 3), error = function(e) NA)
      upper <- tryCatch(round(res$upper.random, 3), error = function(e) NA)
      pval <- tryCatch(res$pval.random, error = function(e) NA)
      I2 <- tryCatch(res$I2, error = function(e) NA)
      if (!is.na(I2) && I2 <= 1) I2 <- I2 * 100
      I2 <- round(I2, 1)
      effect_label <- if (sm == "MD") "Mean Difference (MD)" else if (sm == "SMD") "Standardized Mean Difference (SMD)" else sm

      if (!is.na(TE) && !is.na(lower) && !is.na(upper) && !is.na(pval)) {
        if (pval < 0.05) {
          txt <- sprintf("A random-effects meta-analysis demonstrated a statistically significant effect (%s = %.3f, 95%% CI: %.3f to %.3f, p = %.3f). ",
                         effect_label, TE, lower, upper, pval)
        } else {
          txt <- sprintf("A random-effects meta-analysis did not show a statistically significant effect (%s = %.3f, 95%% CI: %.3f to %.3f, p = %.3f). ",
                         effect_label, TE, lower, upper, pval)
        }
        if (!is.na(I2)) txt <- paste0(txt, sprintf("Heterogeneity was moderate (I² = %.1f%%). ", I2))
        if (!is.null(methodological_msgs$publication_bias)) {
          if (grepl("no evidence", methodological_msgs$publication_bias, ignore.case = TRUE)) {
            txt <- paste0(txt, "No evidence of publication bias was detected. ")
          } else if (grepl("possible publication bias", methodological_msgs$publication_bias, ignore.case = TRUE)) {
            txt <- paste0(txt, "Possible publication bias should be considered. ")
          }
        }
        if (!is.null(methodological_msgs$loo)) {
          if (grepl("stable", methodological_msgs$loo, ignore.case = TRUE)) {
            txt <- paste0(txt, "Sensitivity analysis indicated that the results were robust to the exclusion of individual studies. ")
          } else if (grepl("substantial influence", methodological_msgs$loo, ignore.case = TRUE)) {
            txt <- paste0(txt, "However, the results were sensitive to the exclusion of certain studies, so interpretation should be cautious. ")
          }
        }
        if (pval < 0.05) {
          txt <- paste0(txt, "These findings support a significant effect, but the moderate heterogeneity suggests some variability across studies.")
        } else {
          txt <- paste0(txt, "The lack of a significant effect and the presence of heterogeneity suggest that further research is needed.")
        }
      } else {
        txt <- "Incomplete meta-analysis results; cannot generate writing assistant."
      }
    }

    else if (analysis_type == "Meta-Regression" && is.list(res) && !is.null(res$metareg)) {
      meta <- res$meta
      if (inherits(meta, "meta")) {
        TE <- tryCatch(round(meta$TE.random, 3), error = function(e) NA)
        lower <- tryCatch(round(meta$lower.random, 3), error = function(e) NA)
        upper <- tryCatch(round(meta$upper.random, 3), error = function(e) NA)
        pval <- tryCatch(meta$pval.random, error = function(e) NA)
        I2 <- tryCatch(meta$I2, error = function(e) NA)
        if (!is.na(I2) && I2 <= 1) I2 <- I2 * 100
        I2 <- round(I2, 1)
        effect_label <- if (sm == "MD") "MD" else if (sm == "SMD") "SMD" else sm

        if (!is.na(TE) && !is.na(lower) && !is.na(upper) && !is.na(pval)) {
          if (pval < 0.05) {
            txt <- sprintf("The meta-analysis showed a significant pooled effect (%s = %.3f, 95%% CI: %.3f to %.3f, p = %.3f). ",
                           effect_label, TE, lower, upper, pval)
          } else {
            txt <- sprintf("The meta-analysis did not show a significant pooled effect (%s = %.3f, 95%% CI: %.3f to %.3f, p = %.3f). ",
                           effect_label, TE, lower, upper, pval)
          }
          if (!is.null(methodological_msgs$metareg)) {
            txt <- paste0(txt, methodological_msgs$metareg, " ")
          }
          if (!is.na(I2)) txt <- paste0(txt, sprintf("Heterogeneity was I² = %.1f%%. ", I2))
          txt <- paste0(txt, "These results suggest that the covariate may play a role in explaining between-study differences, but further investigation is warranted.")
        } else {
          txt <- "Incomplete meta-regression results; cannot generate writing assistant."
        }
      } else {
        txt <- "Invalid meta object in meta-regression."
      }
    }

    else if (analysis_type == "Dose-Response Meta-Analysis" && is.list(res) && !is.null(res$model)) {
      if (!is.null(statistical_msgs$overall) && !is.null(statistical_msgs$nonlinearity)) {
        txt <- paste0("The dose-response meta-analysis showed that ",
                      if (grepl("significant", statistical_msgs$overall)) "a significant relationship" else "no significant relationship",
                      " between exposure and outcome existed. ")
        txt <- paste0(txt, statistical_msgs$nonlinearity, " ")
        txt <- paste0(txt, "Further studies with a wider range of doses may help clarify the shape of the association.")
      } else {
        txt <- "Dose-response analysis could not be fully interpreted."
      }
    }

    else {
      txt <- "Writing assistant is not available for this analysis type or incomplete results."
    }

    return(txt)
  }

  interpretation_full <- function(res, analysis_type,
                                  egger_obj = NULL, begg_obj = NULL, trimfill_obj = NULL,
                                  loo_obj = NULL, small_obj = NULL,
                                  sm = "MD", covariate_name = NULL) {

    stat <- statistical_interpretation(res, analysis_type, sm)
    method <- methodological_interpretation(res, analysis_type,
                                            egger_obj, begg_obj, trimfill_obj,
                                            loo_obj, small_obj, covariate_name)
    writer <- writing_assistant(res, analysis_type, sm, stat, method)

    list(
      statistical = stat,
      methodological = method,
      writing_assistant = writer
    )
  }

  extrapolate_effect <- function(new_dose, dose_vec, effect_vec) {
    if (new_dose < min(dose_vec)) {
      x1 <- dose_vec[1]; x2 <- dose_vec[2]
      y1 <- effect_vec[1]; y2 <- effect_vec[2]
      slope <- (y2 - y1) / (x2 - x1)
      return(y1 + slope * (new_dose - x1))
    } else if (new_dose > max(dose_vec)) {
      n <- length(dose_vec)
      x1 <- dose_vec[n-1]; x2 <- dose_vec[n]
      y1 <- effect_vec[n-1]; y2 <- effect_vec[n]
      slope <- (y2 - y1) / (x2 - x1)
      return(y2 + slope * (new_dose - x2))
    } else {
      return(approx(dose_vec, effect_vec, xout = new_dose, rule = 1)$y)
    }
  }

  dr_model_storage <- reactiveVal(NULL)
  dr_newdata_storage <- reactiveVal(NULL)

  sheets <- shiny::reactive({
    shiny::req(input$file)
    readxl::excel_sheets(path = input$file$datapath)
  })

  output$sheet_select <- shiny::renderUI({
    shiny::req(sheets())
    shiny::selectInput("sheet", "Choose sheet:", choices = sheets())
  })

  data_raw <- shiny::reactive({
    shiny::req(input$file, input$sheet)
    df <- readxl::read_excel(path = input$file$datapath, sheet = input$sheet)
    as.data.frame(df)
  })

  output$subgroup_select <- shiny::renderUI({
    shiny::req(data_raw())
    df <- data_raw()
    valid_columns <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    if (length(valid_columns) == 0) return(NULL)
    shiny::selectInput("subgroup", "Select Subgroup:",
                       choices = c("None", valid_columns),
                       selected = "None")
  })

  output$covariate_select <- shiny::renderUI({
    shiny::req(data_raw())
    cols <- names(data_raw())
    numeric_cols <- cols[sapply(data_raw(), is.numeric)]
    if (length(numeric_cols) == 0) {
      return(shiny::helpText("No numeric column available for covariate."))
    }
    shiny::selectInput("covariate", "Choose Covariate (Moderator):",
                       choices = numeric_cols)
  })

  output$column_mapping_ui <- shiny::renderUI({
    shiny::req(data_raw())
    cols <- names(data_raw())
    if (input$analysis_type %in% c("Pairwise Meta-Analysis", "Meta-Regression")) {
      shiny::tagList(
        shiny::selectInput("col_author", "Study Label (Author):", choices = cols),
        shiny::selectInput("col_mean_int", "Mean (Intervention):", choices = cols),
        shiny::selectInput("col_sd_int", "SD (Intervention):", choices = cols),
        shiny::selectInput("col_n_int", "N (Intervention):", choices = cols),
        shiny::selectInput("col_mean_con", "Mean (Control):", choices = cols),
        shiny::selectInput("col_sd_con", "SD (Control):", choices = cols),
        shiny::selectInput("col_n_con", "N (Control):", choices = cols)
      )
    } else {
      shiny::tagList(
        shiny::selectInput("col_id", "Study ID:", choices = cols),
        shiny::selectInput("col_duration", "Exposure variable (e.g., Dose or Duration):", choices = cols),
        shiny::selectInput("col_mean", "Mean Outcome:", choices = cols),
        shiny::selectInput("col_sd", "SD:", choices = cols),
        shiny::selectInput("col_n", "Sample Size:", choices = cols)
      )
    }
  })

  observe({
    file_ok <- !is.null(input$file)
    sheet_ok <- !is.null(input$sheet)
    if (!file_ok || !sheet_ok) {
      shinyjs::disable("analyze")
      return()
    }
    req_type <- input$analysis_type
    if (req_type %in% c("Pairwise Meta-Analysis", "Meta-Regression")) {
      cols_needed <- c("col_author", "col_mean_int", "col_sd_int", "col_n_int",
                       "col_mean_con", "col_sd_con", "col_n_con")
      cols_ok <- all(sapply(cols_needed, function(x) !is.null(input[[x]]) && input[[x]] != ""))
      if (req_type == "Meta-Regression") {
        cov_ok <- !is.null(input$covariate) && input$covariate != ""
      } else {
        cov_ok <- TRUE
      }
      if (cols_ok && cov_ok) {
        shinyjs::enable("analyze")
      } else {
        shinyjs::disable("analyze")
      }
    }
    else if (req_type == "Dose-Response Meta-Analysis") {
      cols_needed <- c("col_id", "col_duration", "col_mean", "col_sd", "col_n")
      cols_ok <- all(sapply(cols_needed, function(x) !is.null(input[[x]]) && input[[x]] != ""))
      if (cols_ok) {
        shinyjs::enable("analyze")
      } else {
        shinyjs::disable("analyze")
      }
    }
  })

  # ================================================================
  # ================================================================
  result <- shiny::eventReactive(input$analyze, {
    df <- data_raw()
    analysis <- input$analysis_type

    shiny::withProgress(message = 'Running analysis...', value = 0.3, {
      if (analysis == "Pairwise Meta-Analysis") {
        shiny::incProgress(0.6, detail = "Fitting pairwise model")
        tryCatch({
          if (!is.null(input$subgroup) && input$subgroup != "None") {
            by_var <- df[[input$subgroup]]
            if (any(is.na(by_var))) {
              shiny::showNotification("Some subgroup values are NA. Those studies will be excluded from subgroup analysis.", type = "warning", duration = 5)
              by_var <- as.factor(by_var)
            } else {
              by_var <- as.factor(by_var)
            }
          } else {
            by_var <- NULL
          }
          fit <- meta::metacont(
            n.e = df[[input$col_n_int]],
            mean.e = df[[input$col_mean_int]],
            sd.e = df[[input$col_sd_int]],
            n.c = df[[input$col_n_con]],
            mean.c = df[[input$col_mean_con]],
            sd.c = df[[input$col_sd_con]],
            studlab = df[[input$col_author]],
            sm = input$effect_size,
            method.random.ci = "HK",
            data = df,
            byvar = by_var
          )
          list(status = "success", data = fit)
        }, error = function(e) {
          shiny::showNotification(paste("Pairwise meta-analysis failed:", e$message), type = "error", duration = 8)
          list(status = "error", message = e$message)
        })
      }

      else if (analysis == "Dose-Response Meta-Analysis") {
        shiny::incProgress(0.6, detail = "Fitting dose-response spline")
        tryCatch({
          exposure_vals <- suppressWarnings(as.numeric(as.character(df[[input$col_duration]])))
          if (all(is.na(exposure_vals))) stop("Exposure variable is not numeric.")
          df[[input$col_duration]] <- exposure_vals
          df <- df[!is.na(df[[input$col_duration]]), ]
          if (nrow(df) < 3) stop("At least 3 non-missing exposure values needed.")

          df[[input$col_mean]] <- as.numeric(as.character(df[[input$col_mean]]))
          df[[input$col_sd]]   <- as.numeric(as.character(df[[input$col_sd]]))
          df[[input$col_n]]    <- as.numeric(as.character(df[[input$col_n]]))

          unique_exp <- unique(df[[input$col_duration]])
          n_knots <- if (length(unique_exp) >= 4) 4 else min(3, length(unique_exp))
          if (n_knots < 3) stop("Need at least 3 distinct exposure values for spline.")
          if (n_knots < 4) {
            shiny::showNotification(paste("Only", length(unique_exp), "distinct exposure values. Using", n_knots, "knots."), type = "warning", duration = 5)
          }
          formula_str <- paste(input$col_mean, "~ rcs(", input$col_duration, ",", n_knots, ")")
          model <- dosresmeta::dosresmeta(
            formula = as.formula(formula_str),
            id = df[[input$col_id]],
            sd = df[[input$col_sd]],
            n = df[[input$col_n]],
            covariance = "md",
            method = "reml",
            proc = "1stage",
            data = df
          )
          newdata <- data.frame(
            seq(min(df[[input$col_duration]], na.rm = TRUE),
                max(df[[input$col_duration]], na.rm = TRUE), length.out = 200)
          )
          names(newdata)[1] <- input$col_duration
          predicted <- predict(model, newdata = newdata, ci.incl = TRUE, order = TRUE)

          dr_model_storage(model)
          dr_newdata_storage(newdata)

          list(status = "success", data = list(model = model, pred = predicted, newdata = newdata))
        }, error = function(e) {
          shiny::showNotification(paste("Dose-response model failed:", e$message), type = "error", duration = 8)
          list(status = "error", message = e$message)
        })
      }

      else if (analysis == "Meta-Regression") {
        shiny::incProgress(0.6, detail = "Running meta-regression")
        shiny::req(input$covariate)
        tryCatch({
          covar_values <- df[[input$covariate]]
          if (!is.numeric(covar_values)) {
            stop("Covariate must be numeric.")
          }
          na_idx <- is.na(covar_values)
          if (any(na_idx)) {
            shiny::showNotification(paste(sum(na_idx), "rows with missing covariate were removed."), type = "warning", duration = 5)
            df <- df[!na_idx, ]
            covar_values <- covar_values[!na_idx]
          }
          if (length(unique(covar_values)) < 2) {
            stop("Covariate must have at least two distinct values.")
          }
          meta_simple <- meta::metacont(
            n.e = df[[input$col_n_int]],
            mean.e = df[[input$col_mean_int]],
            sd.e = df[[input$col_sd_int]],
            n.c = df[[input$col_n_con]],
            mean.c = df[[input$col_mean_con]],
            sd.c = df[[input$col_sd_con]],
            studlab = df[[input$col_author]],
            sm = input$effect_size,
            method.random.ci = "HK",
            data = df
          )
          meta_simple$data$covar <- covar_values
          metareg_result <- meta::metareg(meta_simple, ~ covar)
          list(status = "success", data = list(meta = meta_simple, metareg = metareg_result))
        }, error = function(e) {
          shiny::showNotification(paste("Meta-regression failed:", e$message), type = "error", duration = 8)
          list(status = "error", message = e$message)
        })
      }
    })
  })

  interpretation <- reactive({
    tryCatch({
      res_obj <- result()

      if (!is.list(res_obj)) {
        return(list(
          statistical = list(note = "Invalid result object. Please re-run analysis."),
          methodological = list(note = "Invalid result object."),
          writing_assistant = "Analysis result is invalid."
        ))
      }

      if (res_obj$status == "error") {
        return(list(
          statistical = list(note = paste("Analysis error:", res_obj$message)),
          methodological = list(note = "Analysis error."),
          writing_assistant = "Analysis failed. Please check your data and selections."
        ))
      }

      if (res_obj$status != "success") {
        return(list(
          statistical = list(note = "Analysis not performed or failed."),
          methodological = list(note = "Analysis not performed."),
          writing_assistant = "No analysis to interpret."
        ))
      }

      res <- res_obj$data

      if (!is.list(res) && !inherits(res, "meta")) {
        return(list(
          statistical = list(note = "Invalid result data."),
          methodological = list(note = "Invalid result data."),
          writing_assistant = "The analysis result data is not valid for interpretation."
        ))
      }

      analysis <- input$analysis_type

      egger <- egger_test()
      begg <- begg_test()
      trimfill <- trimfill_res()
      loo <- leave_one_out()
      small <- small_study_meta()

      covar <- if (analysis == "Meta-Regression") input$covariate else NULL
      sm <- if (analysis %in% c("Pairwise Meta-Analysis", "Meta-Regression")) input$effect_size else "MD"

      interpretation_full(
        res = res,
        analysis_type = analysis,
        egger_obj = egger,
        begg_obj = begg,
        trimfill_obj = trimfill,
        loo_obj = loo,
        small_obj = small,
        sm = sm,
        covariate_name = covar
      )
    }, error = function(e) {
      return(list(
        statistical = list(note = paste("Interpretation error:", e$message)),
        methodological = list(note = "Error in interpretation."),
        writing_assistant = "Unable to generate interpretation due to an error."
      ))
    })
  })

  observeEvent(input$predict_dr_btn, {
    req(input$new_dose)
    res_obj <- result()
    if (!is.list(res_obj)) {
      showNotification("Invalid result object.", type = "error")
      output$predicted_effect <- renderPrint({ cat("Invalid result.") })
      return()
    }
    if (res_obj$status == "error") {
      showNotification("Dose-response model error.", type = "error")
      output$predicted_effect <- renderPrint({ cat("Model error.") })
      return()
    }
    if (is.null(res_obj) || res_obj$status != "success") {
      showNotification("Dose-response model not available. Please run analysis first.", type = "error")
      output$predicted_effect <- renderPrint({ cat("Model not available.") })
      return()
    }
    res_dr <- res_obj$data
    if (is.null(res_dr) || !is.list(res_dr) || is.null(res_dr$pred) || is.null(res_dr$newdata)) {
      showNotification("Dose-response model data incomplete.", type = "error")
      output$predicted_effect <- renderPrint({ cat("Model data incomplete.") })
      return()
    }

    dose_vec <- res_dr$newdata[, 1]
    effect_vec <- res_dr$pred[, "pred"]
    ci_lb_vec <- res_dr$pred[, "ci.lb"]
    ci_ub_vec <- res_dr$pred[, "ci.ub"]

    if (input$new_dose < min(dose_vec) || input$new_dose > max(dose_vec)) {
      showNotification("Dose is outside the observed range. Prediction is based on linear extrapolation from the curve's edge.", type = "warning", duration = 5)
    }

    pred_effect <- extrapolate_effect(input$new_dose, dose_vec, effect_vec)
    ci_lb <- extrapolate_effect(input$new_dose, dose_vec, ci_lb_vec)
    ci_ub <- extrapolate_effect(input$new_dose, dose_vec, ci_ub_vec)

    output$predicted_effect <- renderPrint({
      cat("** Dose:", input$new_dose, "\n")
      cat("Predicted Effect Size:", round(pred_effect, 4), "\n")
      cat("95% CI: [", round(ci_lb, 4), ",", round(ci_ub, 4), "]")
    })

    output$prediction_plot <- renderPlot({
      df_points <- data_raw()
      exposure_points <- suppressWarnings(as.numeric(as.character(df_points[[input$col_duration]])))
      mean_points <- suppressWarnings(as.numeric(as.character(df_points[[input$col_mean]])))
      valid_idx <- !is.na(exposure_points) & !is.na(mean_points)

      par(bg = "white", fg = "black", col.axis = "black", col.lab = "black",
          mar = c(5,5,4,2), cex.lab = 1.3, cex.axis = 1.1, cex.main = 1.5, font.main = 2)

      x_max_display <- max(dose_vec, input$new_dose)
      x_min_display <- min(dose_vec, input$new_dose)
      x_padding <- (x_max_display - x_min_display) * 0.1
      x_lim <- c(x_min_display - x_padding, x_max_display + x_padding)

      y_vals <- c(effect_vec, ci_lb_vec, ci_ub_vec,
                  mean_points[valid_idx], pred_effect, ci_lb, ci_ub)
      y_limits <- range(y_vals, na.rm = TRUE)
      y_padding <- diff(y_limits) * 0.1
      y_lim <- c(y_limits[1] - y_padding, y_limits[2] + y_padding)

      plot(dose_vec, effect_vec, type = "l",
           xlab = input$col_duration, ylab = "Effect Size (Mean Difference)",
           main = paste(input$col_duration, "-Response Curve with Prediction (Linear Extrapolation)"),
           lwd = 3, col = "#2c7fb8",
           ylim = y_lim, xlim = x_lim)
      lines(dose_vec, ci_lb_vec, lty = 2, col = "#2c7fb8", lwd = 2)
      lines(dose_vec, ci_ub_vec, lty = 2, col = "#2c7fb8", lwd = 2)
      points(exposure_points[valid_idx], mean_points[valid_idx],
             pch = 19, col = "#d95f02", cex = 1.5)
      abline(h = 0, lty = 3, col = "gray30", lwd = 2)
      points(input$new_dose, pred_effect, pch = 18, col = "#e7298a", cex = 3)
      segments(x0 = input$new_dose, y0 = ci_lb, x1 = input$new_dose, y1 = ci_ub,
               col = "#e7298a", lwd = 4)

      legend("topright",
             legend = c("Predicted curve (spline)", "95% CI", "Observed", "New prediction (linear extrapolation)", "Reference"),
             col = c("#2c7fb8", "#2c7fb8", "#d95f02", "#e7298a", "gray30"),
             lty = c(1, 2, NA, NA, 3),
             pch = c(NA, NA, 19, 18, NA),
             lwd = c(3, 2, NA, NA, 2),
             pt.cex = c(1,1,1.5,2,1),
             bty = "n", cex = 1.0)
    })
  })

  # ================================================================
  # ================================================================

  output$summary <- renderUI({
    tryCatch({
      res_obj <- tryCatch(result(), error = function(e) NULL)
      if (is.null(res_obj)) {
        return(HTML("<p style='color:gray;'>Please upload a file and run analysis.</p>"))
      }

      if (!is.list(res_obj)) {
        return(HTML("<div class='error-box'><p><b>Error:</b> Invalid result object. Please re-run the analysis.</p></div>"))
      }

      if (res_obj$status == "error") {
        return(HTML(sprintf("<div class='error-box'><p><b>Analysis error:</b> %s</p></div>", res_obj$message)))
      }

      if (res_obj$status != "success") {
        return(HTML("<p style='color:red;'>Analysis failed or not yet run. Please check your data and selections.</p>"))
      }

      res <- res_obj$data
      interp <- interpretation()

      html <- "<div style='font-family: Arial, sans-serif;'>"
      html <- paste0(html, "<h3>📊 Numerical Summary</h3>")

      if (inherits(res, "meta")) {
        TE <- round(res$TE.random, 3)
        lower <- round(res$lower.random, 3)
        upper <- round(res$upper.random, 3)
        pval <- res$pval.random
        I2 <- res$I2
        if (!is.na(I2) && I2 <= 1) I2 <- I2 * 100
        I2 <- round(I2, 1)
        tau2 <- round(res$tau2, 3)
        Q_pval <- tryCatch(res$pval.Q, error = function(e) NA)

        html <- paste0(html, "<table style='border-collapse: collapse; width: 100%; margin-bottom: 15px;'>")
        html <- paste0(html, "<tr style='background-color: #2c7fb8; color: white;'><th style='padding: 10px; border: 1px solid #ddd; text-align: left;'>Measure</th><th style='padding: 10px; border: 1px solid #ddd; text-align: left;'>Value</th></tr>")
        html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>Pooled Effect (Random)</td><td style='padding: 8px; border: 1px solid #ddd;'><b>%.3f</b> (95%% CI: %.3f to %.3f)</td></tr>", TE, lower, upper))
        html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>P-value</td><td style='padding: 8px; border: 1px solid #ddd;'>%.3f %s</td></tr>",
                                     pval, if (pval < 0.05) "🔴 Significant" else "🟢 Not significant"))
        html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>Heterogeneity (I²)</td><td style='padding: 8px; border: 1px solid #ddd;'>%.1f%%</td></tr>", I2))
        html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>Between-study variance (τ²)</td><td style='padding: 8px; border: 1px solid #ddd;'>%.3f</td></tr>", tau2))
        if (!is.na(Q_pval)) {
          html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>Q-test p-value</td><td style='padding: 8px; border: 1px solid #ddd;'>%.3f</td></tr>", Q_pval))
        }
        html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>Number of studies</td><td style='padding: 8px; border: 1px solid #ddd;'>%d</td></tr>", length(res$TE)))
        html <- paste0(html, "</table>")
        summary_txt <- paste(capture.output(print(summary(res))), collapse = "\n")
        html <- paste0(html, "<pre style='background:#1a1a2e; color:#ccc;
  padding:15px; border-radius:8px; font-size:12px;
  overflow-x:auto; white-space:pre;'>",
                       summary_txt, "</pre>")
      } else if (is.list(res) && !is.null(res$metareg)) {
        meta <- res$meta
        if (inherits(meta, "meta")) {
          TE <- round(meta$TE.random, 3)
          lower <- round(meta$lower.random, 3)
          upper <- round(meta$upper.random, 3)
          pval <- meta$pval.random
          I2 <- meta$I2
          if (!is.na(I2) && I2 <= 1) I2 <- I2 * 100
          I2 <- round(I2, 1)

          html <- paste0(html, "<table style='border-collapse: collapse; width: 100%; margin-bottom: 15px;'>")
          html <- paste0(html, "<tr style='background-color: #2c7fb8; color: white;'><th style='padding: 10px; border: 1px solid #ddd; text-align: left;'>Measure</th><th style='padding: 10px; border: 1px solid #ddd; text-align: left;'>Value</th></tr>")
          html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>Pooled Effect (Random)</td><td style='padding: 8px; border: 1px solid #ddd;'><b>%.3f</b> (95%% CI: %.3f to %.3f)</td></tr>", TE, lower, upper))
          html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>P-value</td><td style='padding: 8px; border: 1px solid #ddd;'>%.3f</td></tr>", pval))
          html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>Heterogeneity (I²)</td><td style='padding: 8px; border: 1px solid #ddd;'>%.1f%%</td></tr>", I2))

          metareg <- res$metareg
          if (!is.null(metareg$pval)) {
            html <- paste0(html, sprintf("<tr><td style='padding: 8px; border: 1px solid #ddd;'>Meta-regression p-value</td><td style='padding: 8px; border: 1px solid #ddd;'>%.3f</td></tr>", metareg$pval))
          }
          html <- paste0(html, "</table>")
        } else {
          html <- paste0(html, "<p>Meta-regression object invalid.</p>")
        }

      } else if (is.list(res) && !is.null(res$model)) {
        model <- res$model
        if (inherits(model, "dosresmeta")) {
          html <- paste0(html, "<h4>Dose-Response Model Summary</h4>")
          coefs <- tryCatch(coef(model), error = function(e) NULL)
          vcov_mat <- tryCatch(vcov(model), error = function(e) NULL)
          if (!is.null(coefs) && !is.null(vcov_mat)) {
            se <- sqrt(diag(vcov_mat))
            z <- coefs / se
            pvals <- 2 * (1 - pnorm(abs(z)))
            ci_low <- coefs - 1.96 * se
            ci_up <- coefs + 1.96 * se
            df_coef <- data.frame(
              Coefficient = names(coefs),
              Estimate = round(coefs, 4),
              SE = round(se, 4),
              Z = round(z, 4),
              `P-value` = round(pvals, 4),
              `95% CI Lower` = round(ci_low, 4),
              `95% CI Upper` = round(ci_up, 4)
            )
            coef_text <- paste(capture.output(print(df_coef, row.names = FALSE)), collapse = "\n")
            html <- paste0(html, "<pre>", coef_text, "</pre>")
          } else {
            html <- paste0(html, "<p>Coefficients not available.</p>")
          }

          interp_stat <- interp$statistical
          if (!is.null(interp_stat)) {
            html <- paste0(html, "<h4>Model Tests</h4>")
            for (msg in interp_stat) {
              html <- paste0(html, "<p>", msg, "</p>")
            }
          }
        } else {
          html <- paste0(html, "<p>Dose-response model summary not available.</p>")
        }
      } else {
        html <- paste0(html, "<p>No numerical summary available for this analysis type.</p>")
      }

      html <- paste0(html, "<hr>")

      html <- paste0(html, "<h3 style='color:#2c7fb8;'>📊 Statistical Interpretation</h3>")
      if (!is.null(interp$statistical) && length(interp$statistical) > 0) {
        for (txt in interp$statistical) {
          html <- paste0(html, "<p style='font-size:14px;'>", txt, "</p>")
        }
      } else {
        html <- paste0(html, "<p>No statistical interpretation available.</p>")
      }

      html <- paste0(html, "<h3 style='color:#2c7fb8;'>🔍 Methodological Interpretation</h3>")
      if (!is.null(interp$methodological) && length(interp$methodological) > 0) {
        for (key in names(interp$methodological)) {
          label <- switch(key,
                          publication_bias = "Publication Bias",
                          loo = "Leave-One-Out",
                          small_study = "Small Study Effect",
                          metareg = "Meta-Regression",
                          metareg_coef = "Meta-Regression Coefficient",
                          dose_model = "Dose-Response Model",
                          key)
          html <- paste0(html, "<p><b>", label, ":</b> ", interp$methodological[[key]], "</p>")
        }
      } else {
        html <- paste0(html, "<p>No methodological interpretation available.</p>")
      }

      html <- paste0(html, "<h3 style='color:#2c7fb8;'>✍️ Writing Assistant</h3>")
      html <- paste0(html, "<div style='background-color: #f9f9f9; border-left: 4px solid #18bc9c; padding: 15px; border-radius: 8px;'>")
      html <- paste0(html, "<p style='font-size:15px; color: #222;'>", interp$writing_assistant, "</p>")
      html <- paste0(html, "</div>")

      html <- paste0(html, "</div>")
      HTML(html)
    }, error = function(e) {
      HTML(sprintf("<div class='error-box'><p><b>Error in summary:</b> %s</p></div>", e$message))
    })
  })

  output$forest_interpretation <- renderUI({
    tryCatch({
      res_obj <- result()
      if (!is.list(res_obj) || res_obj$status != "success") {
        return(HTML("<div class='interpretation-box'><p>No forest plot interpretation available.</p></div>"))
      }
      res <- res_obj$data
      if (!inherits(res, "meta")) {
        return(HTML("<div class='interpretation-box'><p>No forest plot interpretation available.</p></div>"))
      }
      TE <- round(res$TE.random, 3)
      lower <- round(res$lower.random, 3)
      upper <- round(res$upper.random, 3)
      I2 <- res$I2
      if (!is.na(I2) && I2 <= 1) I2 <- I2 * 100
      I2 <- round(I2, 1)

      txt <- "<div class='interpretation-box'>"
      txt <- paste0(txt, "<h4>📌 Forest Plot Interpretation</h4>")
      txt <- paste0(txt, sprintf("<p>The forest plot displays individual study effects and the pooled estimate. The overall effect was <b>%.3f</b> (95%% CI: %.3f to %.3f).", TE, lower, upper))
      if (lower <= 0 && upper >= 0) {
        txt <- paste0(txt, " The confidence interval crosses the null line, indicating no statistically significant effect.")
      } else {
        txt <- paste0(txt, " The confidence interval does not cross the null line, indicating a statistically significant effect.")
      }
      txt <- paste0(txt, sprintf(" Heterogeneity was I² = %.1f%%.", I2))
      weights <- round(res$w.random, 1)
      if (length(weights) > 0) {
        max_weight <- max(weights)
        max_idx <- which.max(weights)
        study_label <- res$studlab[max_idx]
        txt <- paste0(txt, sprintf(" The study with the highest weight (%.1f%%) was '%s'.", max_weight, study_label))
      }
      txt <- paste0(txt, "</div>")
      HTML(txt)
    }, error = function(e) {
      HTML(sprintf("<div class='interpretation-box'><p>Error in forest interpretation: %s</p></div>", e$message))
    })
  })

  output$funnel_interpretation <- renderUI({
    tryCatch({
      res_obj <- result()
      if (!is.list(res_obj) || res_obj$status != "success") {
        return(HTML("<div class='interpretation-box'><p>No funnel plot interpretation available.</p></div>"))
      }
      res <- res_obj$data
      if (!inherits(res, "meta")) {
        return(HTML("<div class='interpretation-box'><p>No funnel plot interpretation available.</p></div>"))
      }
      txt <- "<div class='interpretation-box'>"
      txt <- paste0(txt, "<h4>📌 Funnel Plot Interpretation</h4>")
      egger <- egger_test()
      if (!is.null(egger)) {
        pval <- tryCatch(egger$p.value, error = function(e) NA)
        if (!is.na(pval)) {
          if (pval < 0.05) {
            txt <- paste0(txt, sprintf("<p>🔴 Egger's test suggests asymmetry (p = %.3f), indicating possible publication bias.</p>", pval))
          } else {
            txt <- paste0(txt, sprintf("<p>🟢 Egger's test does not suggest asymmetry (p = %.3f), indicating no evidence of publication bias.</p>", pval))
          }
        } else {
          txt <- paste0(txt, "<p>Egger's test could not be performed.</p>")
        }
      } else {
        txt <- paste0(txt, "<p>Egger's test not available.</p>")
      }
      txt <- paste0(txt, "<p>Visually, the funnel plot shows the distribution of studies. Asymmetry may indicate publication bias, small-study effects, or heterogeneity.</p>")
      txt <- paste0(txt, "</div>")
      HTML(txt)
    }, error = function(e) {
      HTML(sprintf("<div class='interpretation-box'><p>Error in funnel interpretation: %s</p></div>", e$message))
    })
  })

  output$dose_interpretation <- renderUI({
    tryCatch({
      res_obj <- result()
      if (!is.list(res_obj) || res_obj$status != "success") {
        return(HTML("<div class='interpretation-box'><p>No dose-response interpretation available.</p></div>"))
      }
      res <- res_obj$data
      if (is.null(res) || !is.list(res) || is.null(res$pred)) {
        return(HTML("<div class='interpretation-box'><p>No dose-response interpretation available.</p></div>"))
      }
      txt <- "<div class='interpretation-box'>"
      txt <- paste0(txt, "<h4>📌 Dose-Response Interpretation</h4>")
      interp <- interpretation()
      if (!is.null(interp$statistical)) {
        for (msg in interp$statistical) {
          txt <- paste0(txt, "<p>", msg, "</p>")
        }
      }
      txt <- paste0(txt, "<p><i>Use the prediction tool below to estimate effects for specific dose values.</i></p>")
      txt <- paste0(txt, "</div>")
      HTML(txt)
    }, error = function(e) {
      HTML(sprintf("<div class='interpretation-box'><p>Error in dose-response interpretation: %s</p></div>", e$message))
    })
  })

  output$bubble_interpretation <- renderUI({
    tryCatch({
      res_obj <- result()
      if (!is.list(res_obj) || res_obj$status != "success") {
        return(HTML("<div class='interpretation-box'><p>No meta-regression interpretation available.</p></div>"))
      }
      res <- res_obj$data
      if (is.null(res) || !is.list(res) || is.null(res$metareg)) {
        return(HTML("<div class='interpretation-box'><p>No meta-regression interpretation available.</p></div>"))
      }
      txt <- "<div class='interpretation-box'>"
      txt <- paste0(txt, "<h4>📌 Meta-Regression Interpretation</h4>")
      interp <- interpretation()
      if (!is.null(interp$methodological$metareg)) {
        txt <- paste0(txt, "<p>", interp$methodological$metareg, "</p>")
      }
      if (!is.null(interp$methodological$metareg_coef)) {
        txt <- paste0(txt, "<p>", interp$methodological$metareg_coef, "</p>")
      }
      txt <- paste0(txt, "<p>The bubble plot shows the relationship between the covariate and effect size. Each bubble's size represents the study's weight.</p>")
      txt <- paste0(txt, "</div>")
      HTML(txt)
    }, error = function(e) {
      HTML(sprintf("<div class='interpretation-box'><p>Error in meta-regression interpretation: %s</p></div>", e$message))
    })
  })

  output$loo_interpretation <- renderUI({
    tryCatch({
      res_obj <- result()
      loo <- leave_one_out()
      if (!is.list(res_obj) || res_obj$status != "success") {
        return(HTML("<div class='interpretation-box'><p>No Leave-One-Out interpretation available.</p></div>"))
      }
      res <- res_obj$data
      if (is.null(res) || is.null(loo) || !inherits(res, "meta")) {
        return(HTML("<div class='interpretation-box'><p>No Leave-One-Out interpretation available.</p></div>"))
      }
      txt <- "<div class='interpretation-box'>"
      txt <- paste0(txt, "<h4>📌 Leave-One-Out Interpretation</h4>")
      interp <- interpretation()
      if (!is.null(interp$methodological$loo)) {
        txt <- paste0(txt, "<p>", interp$methodological$loo, "</p>")
      }
      txt <- paste0(txt, "</div>")
      HTML(txt)
    }, error = function(e) {
      HTML(sprintf("<div class='interpretation-box'><p>Error in LOO interpretation: %s</p></div>", e$message))
    })
  })

  output$trimfill_interpretation <- renderUI({
    tryCatch({
      tf <- trimfill_res()
      if (is.null(tf)) {
        return(HTML("<div class='interpretation-box'><p>No Trim and Fill interpretation available.</p></div>"))
      }
      txt <- "<div class='interpretation-box'>"
      txt <- paste0(txt, "<h4>📌 Trim and Fill Interpretation</h4>")
      interp <- interpretation()
      if (!is.null(interp$methodological$publication_bias)) {
        txt <- paste0(txt, "<p>", interp$methodological$publication_bias, "</p>")
      }
      txt <- paste0(txt, "</div>")
      HTML(txt)
    }, error = function(e) {
      HTML(sprintf("<div class='interpretation-box'><p>Error in Trim & Fill interpretation: %s</p></div>", e$message))
    })
  })

  output$small_study_interpretation <- renderUI({
    tryCatch({
      res_obj <- result()
      filt <- small_study_meta()
      if (!is.list(res_obj) || res_obj$status != "success") {
        return(HTML("<div class='interpretation-box'><p>No small study interpretation available.</p></div>"))
      }
      res <- res_obj$data
      if (is.null(res) || is.null(filt) || !inherits(res, "meta")) {
        return(HTML("<div class='interpretation-box'><p>No small study interpretation available.</p></div>"))
      }
      txt <- "<div class='interpretation-box'>"
      txt <- paste0(txt, "<h4>📌 Small Study Analysis Interpretation</h4>")
      interp <- interpretation()
      if (!is.null(interp$methodological$small_study)) {
        txt <- paste0(txt, "<p>", interp$methodological$small_study, "</p>")
      }
      txt <- paste0(txt, "</div>")
      HTML(txt)
    }, error = function(e) {
      HTML(sprintf("<div class='interpretation-box'><p>Error in small study interpretation: %s</p></div>", e$message))
    })
  })

  output$forestPlot <- shiny::renderPlot({
    tryCatch({
      res_obj <- result()
      if (!is.list(res_obj) || res_obj$status != "success") {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "No valid result to display.", cex = 1.2, col = "red")
        return()
      }
      res <- res_obj$data
      meta_obj <- NULL
      if (inherits(res, "meta")) meta_obj <- res
      else if (is.list(res) && !is.null(res$meta)) meta_obj <- res$meta
      else {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Forest plot not available for this analysis type.", cex = 1.2, col = "red")
        return()
      }
      par(bg = "white", fg = "black", mar = c(5, 10, 4, 2) + 0.1)
      forest(meta_obj,
             leftlabs = c("Study", "TE", "seTE"),
             overall = TRUE,
             overall.hetstat = TRUE,
             col.diamond = "#2c7fb8",
             col.diamond.lines = "#2c7fb8",
             col.predict = "#e7298a",
             fontsize = 10,
             cex = 0.9,
             just = "left",
             col.study = "black",
             squaresize = 0.8)
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error in forest plot:", e$message), cex = 1.2, col = "red")
    })
  })

  output$funnelPlot <- shiny::renderPlot({
    tryCatch({
      res_obj <- result()
      if (!is.list(res_obj) || res_obj$status != "success") {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Funnel plot is only available for Pairwise Meta-Analysis.\nPlease run a pairwise analysis first.",
             cex = 1.2, col = "red")
        return()
      }
      res <- res_obj$data
      if (!inherits(res, "meta")) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Funnel plot is only available for Pairwise Meta-Analysis.\nPlease run a pairwise analysis first.",
             cex = 1.2, col = "red")
        return()
      }
      par(bg = "white", fg = "black", mar = c(5,5,3,3))
      funnel(res,
             contour = c(0.9, 0.95, 0.99),
             col.contour = c("gray85", "gray75", "gray65"),
             pch = 21, bg = "#2c7fb8", col = "#d95f02", cex = 1.0,
             xlab = "Effect Size", ylab = "Standard Error",
             main = "Funnel Plot with Contours (90%, 95%, 99%)")
      legend("topright",
             legend = c("Studies", "p < 0.10", "p < 0.05", "p < 0.01"),
             pch = c(21, NA, NA, NA),
             pt.bg = c("#2c7fb8", NA, NA, NA),
             col = c("#d95f02", "gray85", "gray75", "gray65"),
             lty = c(NA, 1, 1, 1),
             lwd = c(NA, 2, 2, 2),
             fill = c(NA, "gray85", "gray75", "gray65"),
             border = "black",
             bty = "n", cex = 0.9)
      mtext("Contours: 90% (light gray), 95% (medium gray), 99% (dark gray)",
            side = 1, line = 4, cex = 0.8)
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error in funnel plot:", e$message), cex = 1.2, col = "red")
    })
  })

  output$doseResponsePlot <- shiny::renderPlot({
    tryCatch({
      res_obj <- result()
      if (!is.list(res_obj) || res_obj$status != "success") {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Dose-response result not available or analysis failed.", cex = 1.2, col = "red")
        return()
      }
      res <- res_obj$data
      if (is.null(res) || !is.list(res) || is.null(res$pred) || is.null(res$newdata)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Dose-response result not available or analysis failed.", cex = 1.2, col = "red")
        return()
      }
      if (input$analysis_type == "Dose-Response Meta-Analysis") {
        df_points <- data_raw()
        exposure_points <- suppressWarnings(as.numeric(as.character(df_points[[input$col_duration]])))
        mean_points <- suppressWarnings(as.numeric(as.character(df_points[[input$col_mean]])))
        valid_idx <- !is.na(exposure_points) & !is.na(mean_points)
        if (!any(valid_idx)) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "No valid points for plotting.", cex = 1.2, col = "red")
          return()
        }

        x_range <- range(res$newdata[, 1], exposure_points[valid_idx], na.rm = TRUE)
        x_padding <- diff(x_range) * 0.1
        x_lim <- c(x_range[1] - x_padding, x_range[2] + x_padding)

        y_vals <- c(res$pred[, "pred"], res$pred[, "ci.lb"], res$pred[, "ci.ub"], mean_points[valid_idx])
        y_range <- range(y_vals, na.rm = TRUE)
        y_padding <- diff(y_range) * 0.1
        y_lim <- c(y_range[1] - y_padding, y_range[2] + y_padding)

        par(bg = "white", fg = "black", col.axis = "black", col.lab = "black",
            mar = c(5,5,4,2), cex.lab = 1.3, cex.axis = 1.1, cex.main = 1.5, font.main = 2)

        plot(res$newdata[, 1], res$pred[, "pred"], type = "l",
             xlab = input$col_duration, ylab = "Effect Size (Mean Difference)",
             main = paste(input$col_duration, "-Response Curve"), lwd = 3, col = "#2c7fb8",
             ylim = y_lim, xlim = x_lim)
        lines(res$newdata[, 1], res$pred[, "ci.lb"], lty = 2, col = "#2c7fb8", lwd = 2)
        lines(res$newdata[, 1], res$pred[, "ci.ub"], lty = 2, col = "#2c7fb8", lwd = 2)
        points(exposure_points[valid_idx], mean_points[valid_idx],
               pch = 19, col = "#d95f02", cex = 1.5)
        abline(h = 0, lty = 3, col = "gray30", lwd = 2)

        legend("topright", legend = c("Predicted (spline)", "95% CI", "Observed", "Reference"),
               col = c("#2c7fb8", "#2c7fb8", "#d95f02", "gray30"),
               lty = c(1, 2, NA, 3), pch = c(NA, NA, 19, NA),
               bty = "n", cex = 1.1)
      }
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error in dose-response plot:", e$message), cex = 1.2, col = "red")
    })
  })

  output$bubblePlot <- shiny::renderPlot({
    tryCatch({
      res_obj <- result()
      if (!is.list(res_obj) || res_obj$status != "success") {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Meta-regression result not available or analysis failed.", cex = 1.2, col = "red")
        return()
      }
      res <- res_obj$data
      if (is.null(res) || !is.list(res) || is.null(res$metareg)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Meta-regression result not available or analysis failed.", cex = 1.2, col = "red")
        return()
      }
      if (input$analysis_type == "Meta-Regression") {
        par(bg = "white", fg = "black")
        meta::bubble(res$metareg, xlab = input$covariate, ylab = "Effect Size",
                     main = "Meta-Regression Bubble Plot", col = "#2c7fb8", bg = "#d95f02", pch = 21)
      }
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error in bubble plot:", e$message), cex = 1.2, col = "red")
    })
  })

  leave_one_out <- shiny::reactive({
    res_obj <- result()
    if (!is.list(res_obj) || res_obj$status != "success") return(NULL)
    res <- res_obj$data
    if (inherits(res, "meta") && input$analysis_type == "Pairwise Meta-Analysis") {
      tryCatch(meta::metainf(res), error = function(e) NULL)
    } else NULL
  })

  trimfill_res <- shiny::reactive({
    res_obj <- result()
    if (!is.list(res_obj) || res_obj$status != "success") return(NULL)
    res <- res_obj$data
    if (inherits(res, "meta") && input$analysis_type == "Pairwise Meta-Analysis") {
      tryCatch(meta::trimfill(res), error = function(e) NULL)
    } else NULL
  })

  egger_test <- shiny::reactive({
    res_obj <- result()
    if (!is.list(res_obj) || res_obj$status != "success") return(NULL)
    res <- res_obj$data
    if (inherits(res, "meta") && input$analysis_type == "Pairwise Meta-Analysis") {
      tryCatch(meta::metabias(res, method.bias = "linreg", k.min = 3), error = function(e) NULL)
    } else NULL
  })

  begg_test <- shiny::reactive({
    res_obj <- result()
    if (!is.list(res_obj) || res_obj$status != "success") return(NULL)
    res <- res_obj$data
    if (inherits(res, "meta") && input$analysis_type == "Pairwise Meta-Analysis") {
      tryCatch(meta::metabias(res, method.bias = "Begg", k.min = 3), error = function(e) NULL)
    } else NULL
  })

  small_study_meta <- shiny::reactive({
    res_obj <- result()
    if (!is.list(res_obj) || res_obj$status != "success") return(NULL)
    res <- res_obj$data
    if (!inherits(res, "meta") || input$analysis_type != "Pairwise Meta-Analysis") return(NULL)
    seTE <- res$seTE
    if (length(seTE) < 2) return(NULL)
    keep <- which(seTE <= median(seTE, na.rm = TRUE))
    if (length(keep) < 2) return(NULL)
    df_f <- res$data[keep, ]
    tryCatch(
      meta::metacont(
        n.e = df_f[[input$col_n_int]], mean.e = df_f[[input$col_mean_int]], sd.e = df_f[[input$col_sd_int]],
        n.c = df_f[[input$col_n_con]], mean.c = df_f[[input$col_mean_con]], sd.c = df_f[[input$col_sd_con]],
        studlab = df_f[[input$col_author]], sm = input$effect_size, method.random.ci = "HK", data = df_f
      ), error = function(e) NULL
    )
  })

  output$egger_test_result <- shiny::renderPrint({
    test <- egger_test()
    if (is.null(test)) cat("Egger's test not available (run pairwise analysis first).")
    else { cat("Egger's test:\n"); print(test) }
  })

  output$begg_test_result <- shiny::renderPrint({
    test <- begg_test()
    if (is.null(test)) cat("Begg's test not available (run pairwise analysis first).")
    else { cat("Begg's rank correlation test:\n"); print(test) }
  })

  output$small_study_summary <- shiny::renderPrint({
    res_obj <- result()
    filt <- small_study_meta()
    if (!is.list(res_obj) || res_obj$status != "success") {
      cat("Analysis not available.")
      return()
    }
    orig <- res_obj$data
    if (is.null(orig) || !inherits(orig, "meta")) cat("Small study analysis not available.")
    else if (is.null(filt)) cat("Could not perform analysis.")
    else {
      cat("=== Comparison: Original vs Excluding studies with SE > median ===\n\n")
      cat("Original: TE =", round(orig$TE.random,4), "95% CI [", round(orig$lower.random,4), ",", round(orig$upper.random,4), "]\n")
      cat("Filtered: TE =", round(filt$TE.random,4), "95% CI [", round(filt$lower.random,4), ",", round(filt$upper.random,4), "]\n")
      cat("Studies excluded:", length(orig$seTE)-length(filt$seTE), "out of", length(orig$seTE), "\n")
    }
  })

  output$small_study_forest <- shiny::renderPlot({
    tryCatch({
      res_obj <- result()
      filt <- small_study_meta()
      if (!is.list(res_obj) || res_obj$status != "success") {
        plot(1,type="n",axes=F); text(1,1,"No comparison available",cex=1.2,col="red"); return()
      }
      orig <- res_obj$data
      if (is.null(orig) || is.null(filt)) {
        plot(1,type="n",axes=F); text(1,1,"No comparison available",cex=1.2,col="red"); return()
      }
      par(bg="white", mfrow=c(2,1), mar=c(2,8,3,2))
      forest(orig, overall=TRUE, hetstat=FALSE, col.diamond="#2c7fb8", main="Original", squaresize=0.8, cex=0.9, just="left")
      forest(filt, overall=TRUE, hetstat=FALSE, col.diamond="#2c7fb8", main="After exclusion", squaresize=0.8, cex=0.9, just="left")
      par(mfrow=c(1,1))
    }, error = function(e) {
      plot(1,type="n",axes=F); text(1,1,paste("Error:", e$message), cex=1.2, col="red")
    })
  })

  output$leave_one_out_ui <- shiny::renderUI({
    res_obj <- result()
    if (!is.list(res_obj) || res_obj$status != "success") {
      return(shiny::plotOutput("leaveOneOutPlot", height = "800px"))
    }
    res <- res_obj$data
    if (inherits(res, "meta") && input$analysis_type == "Pairwise Meta-Analysis") {
      n_studies <- nrow(res$data)
      height_px <- max(800, n_studies * 25)
      shiny::plotOutput("leaveOneOutPlot", height = paste0(height_px, "px"))
    } else {
      shiny::plotOutput("leaveOneOutPlot", height = "800px")
    }
  })

  output$leaveOneOutPlot <- shiny::renderPlot({
    tryCatch({
      loo <- leave_one_out()
      if (is.null(loo)) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Leave-one-out not available", cex = 1.2, col = "red")
        return()
      }
      te_vals <- loo$TE
      ci_lower <- loo$lower
      ci_upper <- loo$upper
      x_range <- range(c(te_vals, ci_lower, ci_upper), na.rm = TRUE)
      x_padding <- diff(x_range) * 0.25
      x_lim <- c(x_range[1] - x_padding, x_range[2] + x_padding)
      par(bg = "white", mar = c(5, 10, 4, 2) + 0.1)
      forest(loo, leftlabs = "Study omitted", overall = FALSE, hetstat = FALSE,
             col.diamond = "#2c7fb8", squaresize = 0.8, cex = 0.9,
             xlim = x_lim, xlab = "Effect Size (95% CI)")
    }, error = function(e) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error in LOO plot:", e$message), cex = 1.2, col = "red")
    })
  })

  output$trimfillSummary <- shiny::renderPrint({
    tf <- trimfill_res()
    if (is.null(tf)) cat("Trim & Fill not available.")
    else { cat("Trim and Fill Summary:\n"); print(summary(tf)) }
  })

  output$trimfillPlot <- shiny::renderPlot({
    tryCatch({
      tf <- trimfill_res()
      if (is.null(tf)) {
        plot(1,type="n",axes=F); text(1,1,"Trim & Fill not available",cex=1.2,col="red"); return()
      }
      par(bg="white", mar=c(5,5,4,2))
      if (!is.null(tf$filled)) {
        te <- tf$TE; se <- tf$seTE
        plot(te, se, type="n", xlab="Observed Outcome", ylab="Standard Error",
             xlim=range(te), ylim=rev(range(se)), main="Trim & Fill Funnel Plot")
        points(te[!tf$filled], se[!tf$filled], pch=16, col="#2c7fb8", cex=1)
        points(te[tf$filled], se[tf$filled], pch=1, col="#e7298a", cex=1.2, lwd=1.5)
        abline(v=tf$TE.fixed, lty=2, col="gray")
        legend("topright", legend=c("Observed","Filled"), pch=c(16,1), col=c("#2c7fb8","#e7298a"), bty="n")
      } else {
        funnel(tf, pch=c(16,1), col=c("#2c7fb8","#e7298a"), bg=c("#2c7fb8",NA), main="Trim & Fill Funnel Plot")
      }
    }, error = function(e) {
      plot(1,type="n",axes=F); text(1,1,paste("Error:", e$message), cex=1.2, col="red")
    })
  })

  output$download_summary <- downloadHandler(
    filename = function() paste0("summary_", Sys.Date(), ".txt"),
    content = function(file) {
      tryCatch({
        res_obj <- result()
        sink(file)
        if (!is.list(res_obj) || res_obj$status != "success") {
          cat("No analysis available or error.")
          sink()
          return()
        }
        res <- res_obj$data
        if (inherits(res, "meta")) {
          print(summary(res))
          cat("\n\n===== Interpretation =====\n")
          interp <- interpretation()
          if (!is.null(interp$statistical)) {
            cat("\nStatistical Interpretation:\n")
            for (txt in interp$statistical) cat(txt, "\n")
          }
          if (!is.null(interp$methodological)) {
            cat("\nMethodological Interpretation:\n")
            for (key in names(interp$methodological)) {
              cat(key, ":", interp$methodological[[key]], "\n")
            }
          }
          if (!is.null(interp$writing_assistant)) {
            cat("\nWriting Assistant:\n", interp$writing_assistant, "\n")
          }
        } else if (is.list(res) && !is.null(res$metareg)) {
          cat("*** Meta-Regression ***\n"); print(summary(res$metareg))
          cat("\n*** Overall Meta ***\n"); print(summary(res$meta))
        } else if (is.list(res) && !is.null(res$model)) {
          print(summary(res$model))
        } else {
          cat("No summary available.")
        }
        sink()
      }, error = function(e) {
        sink()
        cat("Error in downloading summary:", e$message)
      })
    }
  )

  output$download_forest <- downloadHandler(
    filename = function() paste0("forest_", Sys.Date(), ".png"),
    content = function(file) {
      tryCatch({
        res_obj <- result()
        if (!is.list(res_obj) || res_obj$status != "success") return()
        res <- res_obj$data
        meta_obj <- if (inherits(res, "meta")) res else if (is.list(res) && !is.null(res$meta)) res$meta else NULL
        if (is.null(meta_obj)) return()
        n_studies <- nrow(meta_obj$data)
        height_in <- max(6, n_studies * 0.4 + 2)
        png(file, width = 14, height = height_in, units = "in", res = 200)
        par(bg = "white", mar = c(5, 10, 4, 2) + 0.1)
        forest(meta_obj,
               leftlabs = c("Study", "TE", "seTE"),
               overall = TRUE,
               overall.hetstat = TRUE,
               col.diamond = "#2c7fb8",
               col.diamond.lines = "#2c7fb8",
               col.predict = "#e7298a",
               fontsize = 10,
               cex = 0.9,
               just = "left",
               col.study = "black",
               squaresize = 0.8)
        dev.off()
      }, error = function(e) {})
    }
  )

  output$download_funnel <- downloadHandler(
    filename = function() paste0("funnel_", Sys.Date(), ".png"),
    content = function(file) {
      tryCatch({
        res_obj <- result()
        if (!is.list(res_obj) || res_obj$status != "success") return()
        res <- res_obj$data
        png(file, width = 8, height = 8, units = "in", res = 200)
        if (inherits(res, "meta")) {
          par(bg = "white", mar = c(5, 5, 3, 3))
          funnel(res,
                 contour = c(0.9, 0.95, 0.99),
                 col.contour = c("gray85", "gray75", "gray65"),
                 pch = 21, bg = "#2c7fb8", col = "#d95f02", cex = 1.0,
                 xlab = "Effect Size", ylab = "Standard Error",
                 main = "Funnel Plot with Contours")
          legend("topright",
                 legend = c("Studies", "p < 0.10", "p < 0.05", "p < 0.01"),
                 fill = c(NA, "gray85", "gray75", "gray65"), bty = "n")
        }
        dev.off()
      }, error = function(e) {})
    }
  )

  output$download_dose_response <- downloadHandler(
    filename = function() paste0("dose_response_", Sys.Date(), ".png"),
    content = function(file) {
      tryCatch({
        res_obj <- result()
        if (!is.list(res_obj) || res_obj$status != "success") return()
        res <- res_obj$data
        png(file, width = 8, height = 6, units = "in", res = 300)
        if (is.list(res) && !is.null(res$pred)) {
          dfp <- data_raw()
          exposure <- as.numeric(as.character(dfp[[input$col_duration]]))
          mean_out <- as.numeric(as.character(dfp[[input$col_mean]]))
          valid <- !is.na(exposure) & !is.na(mean_out)

          x_range <- range(res$newdata[, 1], exposure[valid], na.rm = TRUE)
          x_padding <- diff(x_range) * 0.1
          x_lim <- c(x_range[1] - x_padding, x_range[2] + x_padding)
          y_vals <- c(res$pred[, "pred"], res$pred[, "ci.lb"], res$pred[, "ci.ub"], mean_out[valid])
          y_range <- range(y_vals, na.rm = TRUE)
          y_padding <- diff(y_range) * 0.1
          y_lim <- c(y_range[1] - y_padding, y_range[2] + y_padding)

          plot(res$newdata[, 1], res$pred[, "pred"], type = "l", lwd = 3, col = "#2c7fb8",
               xlab = input$col_duration, ylab = "Effect Size",
               main = paste(input$col_duration, "-Response Curve"),
               xlim = x_lim, ylim = y_lim)
          lines(res$newdata[, 1], res$pred[, "ci.lb"], lty = 2, col = "#2c7fb8")
          lines(res$newdata[, 1], res$pred[, "ci.ub"], lty = 2, col = "#2c7fb8")
          points(exposure[valid], mean_out[valid], pch = 19, col = "#d95f02")
          abline(h = 0, lty = 3, col = "gray30")
          legend("topright",
                 legend = c("Predicted", "95% CI", "Observed", "Reference"),
                 col = c("#2c7fb8", "#2c7fb8", "#d95f02", "gray30"),
                 lty = c(1, 2, NA, 3), pch = c(NA, NA, 19, NA), bty = "n")
        }
        dev.off()
      }, error = function(e) {})
    }
  )

  output$download_bubble <- downloadHandler(
    filename = function() paste0("bubble_", Sys.Date(), ".png"),
    content = function(file) {
      tryCatch({
        res_obj <- result()
        if (!is.list(res_obj) || res_obj$status != "success") return()
        res <- res_obj$data
        png(file, width = 8, height = 6, units = "in", res = 200)
        if (is.list(res) && !is.null(res$metareg)) {
          meta::bubble(res$metareg,
                       xlab = input$covariate,
                       ylab = "Effect Size",
                       main = "Meta-Regression Bubble Plot",
                       col = "#2c7fb8", bg = "#d95f02", pch = 21)
        }
        dev.off()
      }, error = function(e) {})
    }
  )

  output$download_loo <- downloadHandler(
    filename = function() paste0("leave_one_out_", Sys.Date(), ".png"),
    content = function(file) {
      tryCatch({
        loo <- leave_one_out()
        if (is.null(loo)) return()
        n_studies <- nrow(loo$data)
        height_in <- max(6, n_studies * 0.4 + 2)
        te_vals <- loo$TE
        ci_lower <- loo$lower
        ci_upper <- loo$upper
        x_range <- range(c(te_vals, ci_lower, ci_upper), na.rm = TRUE)
        x_padding <- diff(x_range) * 0.25
        x_lim <- c(x_range[1] - x_padding, x_range[2] + x_padding)
        png(file, width = 14, height = height_in, units = "in", res = 200)
        par(bg = "white", mar = c(5, 10, 4, 2) + 0.1)
        forest(loo,
               leftlabs = "Study omitted",
               overall = FALSE,
               col.diamond = "#2c7fb8",
               squaresize = 0.8, cex = 0.9,
               xlim = x_lim, xlab = "Effect Size (95% CI)")
        dev.off()
      }, error = function(e) {})
    }
  )

  output$download_trimfill_plot <- downloadHandler(
    filename = function() paste0("trimfill_", Sys.Date(), ".png"),
    content = function(file) {
      tryCatch({
        tf <- trimfill_res()
        png(file, width = 8, height = 8, units = "in", res = 200)
        if (!is.null(tf)) {
          if (!is.null(tf$filled)) {
            te <- tf$TE; se <- tf$seTE
            plot(te, se, type = "n",
                 xlab = "Observed Outcome", ylab = "Standard Error",
                 xlim = range(te), ylim = rev(range(se)),
                 main = "Trim & Fill Funnel Plot")
            points(te[!tf$filled], se[!tf$filled], pch = 16, col = "#2c7fb8", cex = 1)
            points(te[tf$filled], se[tf$filled], pch = 1, col = "#e7298a", cex = 1.2)
            abline(v = tf$TE.fixed, lty = 2, col = "gray")
            legend("topright", legend = c("Observed", "Filled"),
                   pch = c(16, 1), col = c("#2c7fb8", "#e7298a"), bty = "n")
          } else {
            funnel(tf, pch = c(16, 1), col = c("#2c7fb8", "#e7298a"),
                   bg = c("#2c7fb8", NA), main = "Trim & Fill Funnel Plot")
          }
        }
        dev.off()
      }, error = function(e) {})
    }
  )

  output$download_trimfill_summary <- downloadHandler(
    filename = function() paste0("trimfill_summary_", Sys.Date(), ".txt"),
    content = function(file) {
      tryCatch({
        tf <- trimfill_res()
        sink(file)
        if (!is.null(tf)) print(summary(tf)) else cat("Not available")
        sink()
      }, error = function(e) { sink(file); cat("Error:", e$message); sink() })
    }
  )

  output$download_small_study_forest <- downloadHandler(
    filename = function() paste0("small_study_forest_", Sys.Date(), ".png"),
    content = function(file) {
      tryCatch({
        res_obj <- result()
        filt <- small_study_meta()
        png(file, width = 12, height = 10, units = "in", res = 200)
        if (!is.list(res_obj) || res_obj$status != "success") { dev.off(); return() }
        orig <- res_obj$data
        if (!is.null(orig) && !is.null(filt)) {
          par(mfrow = c(2, 1), mar = c(2, 8, 3, 2))
          forest(orig, overall = TRUE, hetstat = FALSE,
                 col.diamond = "#2c7fb8",
                 main = "Original", squaresize = 0.8, cex = 0.9, just = "left")
          forest(filt, overall = TRUE, hetstat = FALSE,
                 col.diamond = "#2c7fb8",
                 main = "After Exclusion", squaresize = 0.8, cex = 0.9, just = "left")
          par(mfrow = c(1, 1))
        }
        dev.off()
      }, error = function(e) {})
    }
  )

  output$download_report <- shiny::downloadHandler(
    filename = function() { paste0("meta_analysis_report_", Sys.Date(), ".html") },
    content = function(file) {
      tryCatch({
        res_obj <- result()
        analysis_type <- input$analysis_type

        if (!is.list(res_obj) || res_obj$status != "success") {
          rmd_file <- tempfile(fileext = ".Rmd")
          writeLines(c(
            "---",
            "title: \"Meta-Analysis Report\"",
            "output: html_document",
            "date: \"`r Sys.Date()`\"",
            "---",
            "",
            "<p style='color:red;'>Analysis not available or error occurred.</p>"
          ), con = rmd_file)
          rmarkdown::render(rmd_file, output_file = file, envir = new.env(), quiet = TRUE)
          return()
        }

        res <- res_obj$data

        tmp_forest <- NULL
        if (!is.null(res) && (inherits(res, "meta") || (is.list(res) && !is.null(res$meta)))) {
          meta_obj <- if (inherits(res, "meta")) res else res$meta
          n_studies <- nrow(meta_obj$data)
          height_in <- max(6, n_studies * 0.4 + 2)
          tmp_forest <- tempfile(fileext = ".png")
          png(tmp_forest, width = 14, height = height_in, units = "in", res = 150)
          par(bg = "white", mar = c(5, 10, 4, 2) + 0.1)
          forest(meta_obj, leftlabs = c("Study", "TE", "seTE"), overall = TRUE,
                 col.diamond = "#2c7fb8", fontsize = 10, cex = 0.9, squaresize = 0.8, just = "left")
          dev.off()
        }

        tmp_funnel <- NULL
        if (inherits(res, "meta")) {
          tmp_funnel <- tempfile(fileext = ".png")
          png(tmp_funnel, width = 8, height = 8, units = "in", res = 150)
          par(bg = "white", mar = c(5, 5, 3, 3))
          funnel(res, contour = c(0.9, 0.95, 0.99), col.contour = c("gray85", "gray75", "gray65"),
                 pch = 21, bg = "#2c7fb8", col = "#d95f02", cex = 1.0,
                 xlab = "Effect Size", ylab = "Standard Error", main = "Funnel Plot with Contours")
          legend("topright", legend = c("Studies", "p < 0.10", "p < 0.05", "p < 0.01"),
                 fill = c(NA, "gray85", "gray75", "gray65"), bty = "n")
          dev.off()
        }

        tmp_dose <- NULL
        if (analysis_type == "Dose-Response Meta-Analysis" && is.list(res) && !is.null(res$pred)) {
          tmp_dose <- tempfile(fileext = ".png")
          png(tmp_dose, width = 8, height = 6, units = "in", res = 300)
          df_points <- data_raw()
          exposure <- as.numeric(as.character(df_points[[input$col_duration]]))
          mean_out <- as.numeric(as.character(df_points[[input$col_mean]]))
          valid <- !is.na(exposure) & !is.na(mean_out)

          x_range <- range(res$newdata[, 1], exposure[valid], na.rm = TRUE)
          x_padding <- diff(x_range) * 0.1
          x_lim <- c(x_range[1] - x_padding, x_range[2] + x_padding)
          y_vals <- c(res$pred[, "pred"], res$pred[, "ci.lb"], res$pred[, "ci.ub"], mean_out[valid])
          y_range <- range(y_vals, na.rm = TRUE)
          y_padding <- diff(y_range) * 0.1
          y_lim <- c(y_range[1] - y_padding, y_range[2] + y_padding)

          plot(res$newdata[, 1], res$pred[, "pred"], type = "l", lwd = 3, col = "#2c7fb8",
               xlab = input$col_duration, ylab = "Effect Size",
               main = paste(input$col_duration, "-Response Curve"),
               xlim = x_lim, ylim = y_lim)
          lines(res$newdata[, 1], res$pred[, "ci.lb"], lty = 2, col = "#2c7fb8")
          lines(res$newdata[, 1], res$pred[, "ci.ub"], lty = 2, col = "#2c7fb8")
          points(exposure[valid], mean_out[valid], pch = 19, col = "#d95f02")
          abline(h = 0, lty = 3, col = "gray30")
          legend("topright", legend = c("Predicted", "95% CI", "Observed", "Reference"),
                 col = c("#2c7fb8", "#2c7fb8", "#d95f02", "gray30"),
                 lty = c(1, 2, NA, 3), pch = c(NA, NA, 19, NA), bty = "n")
          dev.off()
        }

        tmp_bubble <- NULL
        if (analysis_type == "Meta-Regression" && is.list(res) && !is.null(res$metareg)) {
          tmp_bubble <- tempfile(fileext = ".png")
          png(tmp_bubble, width = 8, height = 6, units = "in", res = 150)
          par(bg = "white")
          meta::bubble(res$metareg, xlab = input$covariate, ylab = "Effect Size",
                       main = "Meta-Regression Bubble Plot", col = "#2c7fb8", bg = "#d95f02", pch = 21)
          dev.off()
        }

        tf <- trimfill_res()
        tmp_trimfill <- NULL
        if (!is.null(tf)) {
          tmp_trimfill <- tempfile(fileext = ".png")
          png(tmp_trimfill, width = 8, height = 8, units = "in", res = 150)
          par(bg = "white")
          if (!is.null(tf$filled)) {
            te <- tf$TE; se <- tf$seTE
            plot(te, se, type = "n", xlab = "Observed Outcome", ylab = "Standard Error",
                 xlim = range(te), ylim = rev(range(se)), main = "Trim & Fill Funnel Plot")
            points(te[!tf$filled], se[!tf$filled], pch = 16, col = "#2c7fb8", cex = 1)
            points(te[tf$filled], se[tf$filled], pch = 1, col = "#e7298a", cex = 1.2)
            abline(v = tf$TE.fixed, lty = 2, col = "gray")
            legend("topright", legend = c("Observed", "Filled"), pch = c(16, 1), col = c("#2c7fb8", "#e7298a"), bty = "n")
          } else {
            funnel(tf, pch = c(16, 1), col = c("#2c7fb8", "#e7298a"), bg = c("#2c7fb8", NA), main = "Trim & Fill")
          }
          dev.off()
        }

        # ----- Small Study Forest -----
        filt_small <- small_study_meta()
        tmp_small <- NULL
        if (!is.null(res) && !is.null(filt_small) && inherits(res, "meta")) {
          tmp_small <- tempfile(fileext = ".png")
          n_small <- nrow(res$data)
          height_small <- max(6, n_small * 0.4 + 2)
          png(tmp_small, width = 12, height = height_small, units = "in", res = 150)
          par(mfrow = c(2, 1), mar = c(2, 8, 3, 2))
          forest(res, overall = TRUE, hetstat = FALSE, col.diamond = "#2c7fb8",
                 main = "Original", squaresize = 0.8, cex = 0.9, just = "left")
          forest(filt_small, overall = TRUE, hetstat = FALSE, col.diamond = "#2c7fb8",
                 main = "After Exclusion", squaresize = 0.8, cex = 0.9, just = "left")
          par(mfrow = c(1, 1))
          dev.off()
        }

        # ----- Leave-One-Out -----
        loo <- leave_one_out()
        tmp_loo <- NULL
        if (!is.null(loo)) {
          n_loo <- nrow(loo$data)
          height_loo <- max(14, n_loo * 1.5 + 12)
          te_vals <- loo$TE
          ci_lower <- loo$lower
          ci_upper <- loo$upper
          x_range <- range(c(te_vals, ci_lower, ci_upper), na.rm = TRUE)
          x_padding <- diff(x_range) * 0.5
          x_lim <- c(x_range[1] - x_padding, x_range[2] + x_padding)
          tmp_loo <- tempfile(fileext = ".png")
          png(tmp_loo, width = 28, height = height_loo, units = "in", res = 200)
          par(bg = "white", mar = c(5, 24, 4, 2) + 0.1)
          forest(loo, leftlabs = "Study omitted", overall = FALSE, col.diamond = "#2c7fb8",
                 fontsize = 6, cex = 0.6, squaresize = 0.5, just = "left",
                 xlim = x_lim, xlab = "Effect Size (95% CI)")
          dev.off()
        }

        interp <- interpretation()
        interp_html <- ""
        if (is.list(interp) && length(interp) > 0) {
          interp_html <- "<h2>Statistical Interpretation</h2>"
          if (!is.null(interp$statistical)) {
            interp_html <- paste0(interp_html, "<h3>Statistical</h3>")
            for (txt in interp$statistical) {
              interp_html <- paste0(interp_html, "<p>", txt, "</p>")
            }
          }
          if (!is.null(interp$methodological)) {
            interp_html <- paste0(interp_html, "<h3>Methodological</h3>")
            for (key in names(interp$methodological)) {
              interp_html <- paste0(interp_html, "<p><b>", key, ":</b> ", interp$methodological[[key]], "</p>")
            }
          }
          if (!is.null(interp$writing_assistant)) {
            interp_html <- paste0(interp_html, "<h3>Writing Assistant</h3><p>", interp$writing_assistant, "</p>")
          }
        } else {
          interp_html <- "<p>No interpretation available.</p>"
        }

        summary_lines <- capture.output({
          if (is.null(res)) cat("Analysis not available.")
          else if (inherits(res, "meta")) print(summary(res))
          else if (is.list(res) && !is.null(res$metareg)) {
            cat("*** Meta-Regression Results ***\n\n")
            print(summary(res$metareg))
            cat("\n\n*** Overall Meta-Analysis (without covariate) ***\n")
            print(summary(res$meta))
          } else if (is.list(res) && !is.null(res$model)) {
            print(summary(res$model))
          } else cat("No summary available.")
        })
        summary_text <- paste(summary_lines, collapse = "\n")

        egger_lines <- capture.output({
          eg <- egger_test()
          if (is.null(eg)) cat("Egger's test not available (run pairwise analysis first).")
          else print(eg)
        })
        egger_text <- paste(egger_lines, collapse = "\n")

        begg_lines <- capture.output({
          bg <- begg_test()
          if (is.null(bg)) cat("Begg's test not available (run pairwise analysis first).")
          else print(bg)
        })
        begg_text <- paste(begg_lines, collapse = "\n")

        tf_lines <- capture.output({
          tfx <- trimfill_res()
          if (is.null(tfx)) cat("Trim and Fill analysis not available.")
          else print(summary(tfx))
        })
        tf_text <- paste(tf_lines, collapse = "\n")

        small_lines <- capture.output({
          if (!is.null(res) && !is.null(filt_small) && inherits(res, "meta")) {
            I2_res <- res$I2
            if (!is.na(I2_res) && I2_res <= 1) I2_res <- I2_res * 100
            I2_res <- round(I2_res, 1)
            cat("Original (all studies):\n")
            cat("  Pooled effect =", round(res$TE.random, 4),
                "95% CI [", round(res$lower.random, 4), ",", round(res$upper.random, 4), "]\n")
            cat("  Heterogeneity I² =", I2_res, "%\n")
            cat("After excluding small studies (SE > median):\n")
            cat("  Pooled effect =", round(filt_small$TE.random, 4),
                "95% CI [", round(filt_small$lower.random, 4), ",", round(filt_small$upper.random, 4), "]\n")
            cat("  Heterogeneity I² =", round(filt_small$I2, 1), "%\n")
            cat("Studies excluded:", length(res$seTE) - length(filt_small$seTE),
                "out of", length(res$seTE), "\n")
          } else {
            cat("Small study analysis not available (requires pairwise meta-analysis).")
          }
        })
        small_text <- paste(small_lines, collapse = "\n")

        rmd_file <- tempfile(fileext = ".Rmd")
        writeLines(c(
          "---",
          "title: \"Meta-Analysis Report\"",
          "output: html_document",
          "date: \"`r Sys.Date()`\"",
          "params:",
          "  summary_text: null",
          "  interp_html: null",
          "  forest_img: null",
          "  funnel_img: null",
          "  dose_img: null",
          "  bubble_img: null",
          "  egger_txt: null",
          "  begg_txt: null",
          "  tf_txt: null",
          "  tf_img: null",
          "  loo_img: null",
          "  small_txt: null",
          "  small_img: null",
          "---",
          "",
          "<style>",
          "body { background-color: white; color: black; font-family: 'Segoe UI', Arial, sans-serif; }",
          "pre { background-color: #f5f5f5; padding: 10px; border-radius: 5px; }",
          "img { max-width: 100%; height: auto; }",
          "</style>",
          "",
          "## Summary of Results",
          "",
          "```{r echo=FALSE, results='asis'}",
          "cat('<pre>')",
          "cat(params$summary_text)",
          "cat('</pre>')",
          "```",
          "",
          "## Statistical Interpretation",
          "```{r echo=FALSE, results='asis'}",
          "cat(params$interp_html)",
          "```",
          "",
          if (!is.null(tmp_forest)) c("## Forest Plot", "![](`r params$forest_img`)", "") else NULL,
          if (!is.null(tmp_funnel)) c("## Funnel Plot", "![](`r params$funnel_img`)", "") else NULL,
          if (!is.null(tmp_dose)) c("## ", paste(input$col_duration, "-Response Curve"), "![](`r params$dose_img`)", "") else NULL,
          if (!is.null(tmp_bubble)) c("## Meta-Regression Bubble Plot", "![](`r params$bubble_img`)", "") else NULL,
          "",
          "## Publication Bias and Sensitivity Analyses",
          "",
          "### Egger's Test",
          "```{r echo=FALSE, results='asis'}",
          "cat('<pre>')",
          "cat(params$egger_txt)",
          "cat('</pre>')",
          "```",
          "",
          "### Begg's Test",
          "```{r echo=FALSE, results='asis'}",
          "cat('<pre>')",
          "cat(params$begg_txt)",
          "cat('</pre>')",
          "```",
          "",
          "### Trim and Fill Method",
          "```{r echo=FALSE, results='asis'}",
          "cat('<pre>')",
          "cat(params$tf_txt)",
          "cat('</pre>')",
          "```",
          if (!is.null(tmp_trimfill)) c("![](`r params$tf_img`)", "") else NULL,
          "",
          "## Leave-One-Out Sensitivity Analysis",
          if (!is.null(tmp_loo)) c("![](`r params$loo_img`)") else "*Not available*",
          "",
          "### Small Study Analysis",
          "```{r echo=FALSE, results='asis'}",
          "cat('<pre>')",
          "cat(params$small_txt)",
          "cat('</pre>')",
          "```",
          if (!is.null(tmp_small)) c("#### Forest Plot Comparison", "![](`r params$small_img`)") else NULL
        ), con = rmd_file)

        rmarkdown::render(rmd_file, output_file = file,
                          params = list(
                            summary_text = summary_text,
                            interp_html = interp_html,
                            forest_img = if (!is.null(tmp_forest)) tmp_forest else "",
                            funnel_img = if (!is.null(tmp_funnel)) tmp_funnel else "",
                            dose_img = if (!is.null(tmp_dose)) tmp_dose else "",
                            bubble_img = if (!is.null(tmp_bubble)) tmp_bubble else "",
                            egger_txt = egger_text,
                            begg_txt = begg_text,
                            tf_txt = tf_text,
                            tf_img = if (!is.null(tmp_trimfill)) tmp_trimfill else "",
                            loo_img = if (!is.null(tmp_loo)) tmp_loo else "",
                            small_txt = small_text,
                            small_img = if (!is.null(tmp_small)) tmp_small else ""
                          ),
                          envir = new.env(),
                          quiet = TRUE)
      }, error = function(e) {

        rmd_file <- tempfile(fileext = ".Rmd")
        writeLines(c(
          "---",
          "title: \"Meta-Analysis Report (Error)\"",
          "output: html_document",
          "date: \"`r Sys.Date()`\"",
          "---",
          "",
          sprintf("<p style='color:red;'>Error generating report: %s</p>", e$message)
        ), con = rmd_file)
        rmarkdown::render(rmd_file, output_file = file, envir = new.env(), quiet = TRUE)
      })
    }
  )

}
