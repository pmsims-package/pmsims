# Execution notes

The protocol and historical evidence were committed at 8ea765f before completed neighbour results were available. The package's production R files remain those of d00a137 throughout.

An initial instrumentation pilot failed before running the adaptive search because a study data wrapper omitted the generator's predictor-count formals. That failed pilot directory was discarded. The wrapper was corrected to preserve formals and attributes before the real pilot and study began. Final verification compares guarded and original generator values and RNG states.

The first four running jobs had already read a version of run-case.R that wrote a zero GP-rep count from the wrong returned-object field. Subsequent jobs use result$data. analyse.R recomputes every case's GP-rep count from the saved search object and writes the corrected individual and aggregate CSV summaries. This is a reporting correction, not a rerun or change of search decisions.

Editing run-case.R while those four processes were executing caused an end-of-file parser error in their terminals after the completed summaries and DONE marker were written. Completion is therefore audited from the raw objects, three full independent draw sets, CSV summaries and final verification, rather than relying on those initial process exit codes. The fitting and validation functions had been parsed before the edit, and their recorded draws are preserved. The final frozen script is used by later jobs and reproducible reruns.

The initial run reserved three fitting slots for run-study.py and one for the separate pilot. After the pilot completed, that fourth slot ran the two GLM seed-failure cases ahead of their position at the end of the queue. The runner skips them once DONE exists. All cases retain the same prespecified inputs and seeds; there are at most four active study fitting processes. A separate three-fit timing probe estimated cost and was not used for performance inference. Future full runs use the default four-worker runner without the external-pilot environment variable.

No returned sample size is corrected. Search errors or the prespecified administrative training-row limit remain reported outcomes. Raw saved draws and package closures stay local; final CSV summaries, session information, plots, diagnostics and verification are committed.

After the two GLM jobs finished, a fourth study lane worked from the end of the remaining queue. Atomic per-case directory locks prevent duplicate writers when the original queue reaches an ahead-of-queue case. The worker entry script was replaced atomically so already-running R processes retain their open original file. This scheduling change consumes no RNG and leaves the fitting and validation function bodies unchanged. The standard four-worker runner reproduces the same jobs without the extra lane.

The user then prioritised predictor-count peaks over exact target coverage. Future runs retain the same GP searches but use one returned-N diagnostic; already-running cases finish their three checks. PROTOCOL.md records this amendment. Reported-N validation seeds remain unchanged. The worker file was again replaced atomically.
