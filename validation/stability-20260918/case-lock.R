# Study scheduling only: no RNG or simulation changes.
# Atomic directory creation prevents duplicate writers when runners meet.
stability_case_lock <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 3L) {
    stop('Expected scenario ID, seed and role')
  }
  root <- 'validation/stability-20260918'
  name <- paste0(args[1], '-', args[3], '-seed', args[2])
  out <- file.path(root, 'results', name)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, 'locks'), showWarnings = FALSE)
  lock <- file.path(root, 'locks', name)
  repeat {
    if (file.exists(file.path(out, 'DONE'))) {
      quit(status = 0)
    }
    if (dir.create(lock, showWarnings = FALSE)) {
      writeLines(as.character(Sys.getpid()), file.path(lock, 'owner.txt'))
      return(invisible(lock))
    }
    owner <- file.path(lock, 'owner.txt')
    if (file.exists(owner)) {
      pid <- suppressWarnings(as.integer(readLines(owner, warn = FALSE)[1]))
      if (is.finite(pid) && !tools::pskill(pid, signal = 0L)) {
        unlink(lock, recursive = TRUE)
        next
      }
    }
    Sys.sleep(1)
  }
}
stability_case_lock()
