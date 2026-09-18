#!/usr/bin/env python3
"""Keep at most four R jobs active including the concurrently running baselines."""
import concurrent.futures, json, pathlib, subprocess, threading
root = pathlib.Path('validation/predictor-count-spikes')
jobs = json.loads((root/'reference-grid-jobs.json').read_text())
condition = threading.Condition()
active = 0
def slots():
    completed = len(list((root/'results').glob('*-baseline-search.csv')))
    return 4 - min(3, 6-completed)
def run(job):
    global active
    expected = root/'results'/(job['name']+'.csv')
    if expected.exists():
        print(job['name'], 'already complete', flush=True)
        return 0
    with condition:
        while active >= slots():
            condition.wait(timeout=1)
        active += 1
    try:
        with (root/'logs'/(job['name']+'.log')).open('w') as log:
            result = subprocess.run(['Rscript',str(root/job['script']),*map(str,job['args'])],
                                    stdout=log,stderr=subprocess.STDOUT)
        print(job['name'],'exit',result.returncode,flush=True)
        return result.returncode
    finally:
        with condition:
            active -= 1
            condition.notify_all()
with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
    statuses = list(pool.map(run,jobs))
if any(statuses):
    raise SystemExit(1)
