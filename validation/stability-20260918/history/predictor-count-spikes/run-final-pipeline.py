#!/usr/bin/env python3
"""Finish ablations and held-out checks, sharing four fitting slots with references."""
import json, pathlib, subprocess, time
root = pathlib.Path('validation/predictor-count-spikes')
reference = json.loads((root/'reference-grid-jobs.json').read_text())
checks = json.loads((root/'answer-checks-jobs.json').read_text())
wide = json.loads((root/'common-searches-jobs.json').read_text())
extras = [dict(name=id+'-grid-n8000',script='run-grid.R',args=[id,8000,7300000+i*10000])
          for i,id in enumerate(['p10-seed1077','p10-seed48'])]
(root/'extra-reference-jobs.json').write_text(json.dumps(extras,indent=2)+'\n')
heavy_order = {'p15-seed1725':0,'p10-seed48':1}
base_checks = sorted([j for j in checks if j['args'][1]=='baseline'],
                     key=lambda j: heavy_order.get(j['args'][0],2))
for job in extras: job['priority']=0
for job in base_checks: job['priority']=2
pending = extras + base_checks
active = []
added_wide = False
failures = []
def output(job):
    suffix = '-search' if job['script']=='run-search.R' else ''
    return root/'results'/(job['name']+suffix+'.csv')
def complete(job): return output(job).exists()
def ready(job):
    if job['script']=='run-check.R':
        id,mode,_ = job['args']
        return (root/'raw'/(id+'-'+mode+'.rds')).exists()
    return True
while pending or active or not added_wide:
    for item in active[:]:
        job,proc,log = item
        code = proc.poll()
        if code is not None:
            log.close()
            active.remove(item)
            print(job['name'],'exit',code,flush=True)
            if code: failures.append(job['name'])
    remaining_refs = sum(not complete(j) for j in reference)
    if not added_wide and remaining_refs==0 and all(complete(j) for j in extras):
        with (root/'logs/select-bounds.log').open('w') as log:
            selected = subprocess.run(['Rscript',str(root/'select-bounds.R')],stdout=log,stderr=subprocess.STDOUT)
        if selected.returncode:
            failures.append('select-bounds')
            break
        for job in wide: job['priority']=1
        extra_checks = [j for j in checks if j['args'][1]=='common-bounds']
        for job in extra_checks: job['priority']=3
        pending = wide + pending + extra_checks
        added_wide = True
        print('Common domain selected; ablations scheduled.',flush=True)
    available = 4-min(4,remaining_refs)
    pending.sort(key=lambda j:j['priority'])
    for job in pending[:]:
        if complete(job):
            pending.remove(job)
            continue
        if len(active)>=available:
            break
        if not ready(job):
            continue
        log = (root/'logs'/(job['name']+'.log')).open('w')
        proc = subprocess.Popen(['Rscript',str(root/job['script']),*map(str,job['args'])],stdout=log,stderr=subprocess.STDOUT)
        active.append((job,proc,log))
        pending.remove(job)
        print('Started',job['name'],flush=True)
    if failures:
        break
    time.sleep(1)
if failures:
    # Finish only already launched study children before reporting failure.
    for job,proc,log in active:
        proc.wait();log.close()
    raise SystemExit('Failed jobs: '+', '.join(failures))
print('All ablations and answer checks complete.',flush=True)
