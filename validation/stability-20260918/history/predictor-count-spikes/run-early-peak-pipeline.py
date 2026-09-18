#!/usr/bin/env python3
"""Run two supported peak-case ablations while the broader reference grid completes."""
import concurrent.futures, json, pathlib, subprocess
root=pathlib.Path('validation/predictor-count-spikes')
searches=json.loads((root/'common-searches-jobs.json').read_text())
checks=json.loads((root/'answer-checks-jobs.json').read_text())
ids=['p15-seed1725','p15-seed48']
jobs=[]
for id in ids:
    jobs.append([next(j for j in searches if j['args'][0]==id),
                 next(j for j in checks if j['args'][0]==id and j['args'][1]=='common-bounds')])
(root/'early-peak-jobs.json').write_text(json.dumps(jobs,indent=2)+'\n')
def run(pair):
    for job in pair:
        with (root/'logs'/(job['name']+'.log')).open('w') as log:
            code=subprocess.run(['Rscript',str(root/job['script']),*map(str,job['args'])],
                                stdout=log,stderr=subprocess.STDOUT).returncode
        print(job['name'],'exit',code,flush=True)
        if code:return code
    return 0
with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
    codes=list(pool.map(run,jobs))
if any(codes):raise SystemExit(1)
