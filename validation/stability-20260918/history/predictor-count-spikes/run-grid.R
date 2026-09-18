source('validation/predictor-count-spikes/common.R')
cli_args <- commandArgs(trailingOnly = TRUE)
id <- cli_args[1]
n <- as.integer(cli_args[2])
seed <- as.integer(cli_args[3])
args <- readRDS(raw_path(paste0(id, '-prepared.rds')))$args
result <- evaluate_values(args, n, seed, paste0(id, ' N=', n))
saveRDS(result, raw_path(paste0(id, '-grid-n', n, '.rds')))
row <- cbind(
  data.frame(
    id = id,
    n = n,
    validation_seed = seed,
    errors = length(result$errors),
    seconds = result$seconds
  ),
  result$summary
)
write.csv(row, result_path(paste0(id, '-grid-n', n, '.csv')), row.names = FALSE)
print(row)
