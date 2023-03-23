plot.pmsims <- function(results) {
  train_size <- results$train_size
  performance <- results$summaries
  quant_performance <- performance$quant20_performance
  median_performance <- performance$median_performance
  quant5_performance <- performance$quant5_performance
  quant95_performance <- performance$quant95_performance
  
  # Plot
  plot(
    train_size,
    quant_performance,
    type = "l",
    lty = 2,
    col = "red",
    main = "AUC by train size with  20th percentile, 5th percentile, & 95th percentile"
  )
  graphics::lines(train_size, median_performance, col = "black")
  graphics::lines(train_size,
                  quant5_performance,
                  col = "grey",
                  lty = 1)
  graphics::lines(train_size,
                  quant95_performance,
                  col = "grey",
                  lty = 1)
  abline(h = results$target_performance, col = 3)
  graphics::abline(h = results$target,
                   col = "green",
                   lty = 3)
  graphics::abline(v = results$min_n,
                   col = "green",
                   lty = 3)
  graphics::legend(
    "bottomright",
    legend = c(
      "AUC median",
      "AUC 5th to 95th percentile",
      "AUC 20th percentile",
      "Acceptable AUC"
    ),
    col = c("black", "grey", "red", "green"),
    lty = c(1, 1, 2, 3)
  )
}
