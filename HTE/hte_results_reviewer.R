# =============================================================================
# View HTE sample size search results
# Loads every row_*.rds file written by run_grid_row() and summarises them.
# =============================================================================

library(ggplot2)

results_dir <- "HTE Results"

# -----------------------------------------------------------------------------
# 1. Load all result files
# -----------------------------------------------------------------------------
files <- list.files(results_dir, pattern = "^row_[0-9]+_.*\\.rds$", full.names = TRUE)
if (length(files) == 0) stop("No result files found in '", results_dir, "'.")

results <- lapply(files, readRDS)
row_ids <- vapply(results, function(r) as.numeric(r$summary$Row), numeric(1))
results <- results[order(row_ids)]

summary_df <- do.call(rbind, lapply(results, `[[`, "summary"))
rownames(summary_df) <- NULL
summary_df$Time_Mins <- round(summary_df$Time_Secs / 60, 1)

# pmsims objects, in the same order as summary_df
sim_res_list <- lapply(results, `[[`, "sim_res")
names(sim_res_list) <- sprintf("%03d | %s | %s | %s", summary_df$Row,
                               summary_df$Data_Gen, summary_df$Model, summary_df$Metric)

# -----------------------------------------------------------------------------
# 2. Overview
# -----------------------------------------------------------------------------
cat("Loaded", nrow(summary_df), "result files from", normalizePath(results_dir), "\n\n")
print(table(Status = summary_df$Status))

no_n <- summary_df$Status == "Success" & is.na(summary_df$Estimated_N)
if (any(no_n)) {
  cat("\n", sum(no_n), " successful run(s) returned no minimum sample size (rows: ",
      paste(summary_df$Row[no_n], collapse = ", "), ")\n", sep = "")
}

cat("\n=== Full summary ===\n")
print(summary_df[, c("Row", "Data_Gen", "Model", "Metric", "Target",
                     "Estimated_N", "Status", "Time_Mins")])

# -----------------------------------------------------------------------------
# 3. Failed runs and their error messages
# -----------------------------------------------------------------------------
failed <- summary_df[summary_df$Status == "Failed", ]
if (nrow(failed) > 0) {
  cat("\n=== Failed runs ===\n")
  for (k in seq_len(nrow(failed))) {
    cat(sprintf("[%03d] %s | %s | %s\n      %s\n",
                failed$Row[k], failed$Data_Gen[k], failed$Model[k],
                failed$Metric[k], failed$Error[k]))
  }
}

# -----------------------------------------------------------------------------
# 4. Estimated N as a model-by-metric table, one per data generator
# -----------------------------------------------------------------------------
cat("\n=== Estimated minimum N (rows = model, columns = metric) ===\n")
for (dg in unique(summary_df$Data_Gen)) {
  sub <- summary_df[summary_df$Data_Gen == dg, ]
  wide <- tapply(sub$Estimated_N,
                 list(Model = sub$Model, Metric = sub$Metric),
                 function(x) x[1])
  cat("\nData generator:", dg, "\n")
  print(wide)
}

# -----------------------------------------------------------------------------
# 5. Plots
# -----------------------------------------------------------------------------
plot_df <- summary_df[!is.na(summary_df$Estimated_N), ]

if (nrow(plot_df) > 0) {
  p_n <- ggplot(plot_df, aes(x = Estimated_N, y = Model, colour = Model)) +
    geom_point(size = 3) +
    geom_text(aes(label = scales::comma(round(Estimated_N))),
              hjust = -0.3, size = 3, show.legend = FALSE) +
    scale_x_log10(labels = scales::comma, expand = expansion(mult = c(0.05, 0.25))) +
    facet_grid(Metric ~ Data_Gen) +
    labs(title = "Estimated minimum sample size",
         x = "Estimated N (log scale)", y = NULL) +
    theme_bw() +
    theme(legend.position = "none", strip.text.y = element_text(angle = 0))
  print(p_n)
}

p_time <- ggplot(summary_df, aes(x = Time_Mins, y = Model, colour = Status)) +
  geom_point(size = 3) +
  facet_grid(Metric ~ Data_Gen) +
  labs(title = "Run time per grid row", x = "Minutes", y = NULL) +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0))
print(p_time)

# -----------------------------------------------------------------------------
# 6. Inspect an individual pmsims object
# -----------------------------------------------------------------------------
show_sim_res <- function(row) {
  idx <- match(row, summary_df$Row)
  if (is.na(idx)) stop("No result loaded for row ", row)
  x <- sim_res_list[[idx]]
  cat("\n=== ", names(sim_res_list)[idx], " ===\n", sep = "")
  
  if (is.null(x)) {
    cat("No pmsims object (run failed): ", summary_df$Error[idx], "\n", sep = "")
    return(invisible(NULL))
  }
  
  cat("Class:", paste(class(x), collapse = ", "), "\n")
  str(x, max.level = 1, give.attr = FALSE)
  
  # Use pmsims' own plot method if it has one
  has_plot <- any(vapply(class(x), function(cl)
    !is.null(utils::getS3method("plot", cl, optional = TRUE)), logical(1)))
  if (has_plot) plot(x)
  
  invisible(x)
}

# Example: inspect the first loaded row
show_sim_res(summary_df$Row[1])

# -----------------------------------------------------------------------------
# 7. Save the combined summary
# -----------------------------------------------------------------------------
write.csv(summary_df, file.path(results_dir, "summary_all_rows.csv"), row.names = FALSE)
cat("\nSummary saved to", file.path(results_dir, "summary_all_rows.csv"), "\n")
