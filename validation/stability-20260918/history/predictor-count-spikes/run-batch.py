#!/usr/bin/env python3
"""Run an explicit JSON job list with at most three independent R processes."""
import concurrent.futures, json, pathlib, subprocess, sys
root = pathlib.Path('validation/predictor-count-spikes')
jobs = json.loads(pathlib.Path(sys.argv[1]).read_text())
def run(job):
    logfile = root / 'logs' / (job['name'] + '.log')
    with logfile.open('w') as log:
        result = subprocess.run(['Rscript', str(root / job['script']), *map(str, job['args'])],
                                stdout=log, stderr=subprocess.STDOUT)
    print(job['name'], 'exit', result.returncode, flush=True)
    return result.returncode
with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
    statuses = list(pool.map(run, jobs))
if any(statuses):
    raise SystemExit(1)
