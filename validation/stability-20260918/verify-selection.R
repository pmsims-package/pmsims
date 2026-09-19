#!/usr/bin/env Rscript
# Optional check against the original sibling cache-build grid.
root <- 'validation/stability-20260918'
source('../pmsims-chatbot/cache/scenarios_grid.R')
s <- jsonlite::fromJSON(
  file.path(root, 'scenarios.json'),
  simplifyVector = FALSE
)
g <- get_grid('1f')
rows <- lapply(s, function(x) {
  z <- g[[x$outcome]]
  idx <- which(
    z$model == x$model &
      Reduce(
        `&`,
        lapply(names(x$inputs), function(n) abs(z[[n]] - x$inputs[[n]]) < 1e-8)
      )
  )
  data.frame(
    id = x$id,
    cache_seed = x$cache_seed,
    grid_seed = idx,
    matched = identical(as.integer(idx), as.integer(x$cache_seed))
  )
})
v <- do.call(rbind, rows)
write.csv(v, file.path(root, 'selection-verification.csv'), row.names = FALSE)
stopifnot(all(v$matched))
print(v)
