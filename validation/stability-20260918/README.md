# Stability validation, 18 September 2026

Start with [REPORT.md](REPORT.md) for results, [FIXES-AND-PROVENANCE.md](FIXES-AND-PROVENANCE.md) for the repair and consolidated history, and [PROTOCOL.md](PROTOCOL.md) for the prespecified methods.

The tested package is Ridwan's dev commit d00a137; historical repair branches are archived as evidence without applying their code. The scenarios and job manifests pin inputs, historical cache Ns and RNG seeds.

From the repository root:

```sh
python3 validation/stability-20260918/run-study.py
Rscript validation/stability-20260918/analyse.R
Rscript validation/stability-20260918/verify.R
Rscript validation/stability-20260918/review-diagnostics.R
```

The runner resumes completed cases and uses at most four independent R processes. Each successful case has a search summary, adaptive trace, three 1,000-replicate independent checks, and session information. Raw RDS objects and progress logs remain local and are ignored by Git; scripts, inputs, CSV summaries, figures and reports are committed. Historical files are indexed by history/manifest.json. See PROTOCOL.md for the training-row administrative ceiling and interpretation of unsupported or below-target answers.
