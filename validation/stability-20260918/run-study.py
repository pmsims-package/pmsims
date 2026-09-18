#!/usr/bin/env python3
"""Run from repository root. At most four independent R fitting processes."""
import concurrent.futures,json,os,pathlib,subprocess,time
root=pathlib.Path('validation/stability-20260918');(root/'logs').mkdir(exist_ok=True)
cases=json.loads((root/'scenarios.json').read_text()); jobs=[]
for s in cases:
    jobs.append(dict(id=s['id'],seed=s['cache_seed'],role='cache'))
    jobs.append(dict(id=s['id'],seed=1 if s['slice']=='binary_glm_seed_failure' else 48,role='shared'))
(root/'jobs.json').write_text(json.dumps(jobs,indent=2)+'\n')
def run(job):
    name=f"{job['id']}-{job['role']}-seed{job['seed']}"
    if (root/'results'/name/'DONE').exists():return name,'cached'
    env=dict(os.environ,OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1')
    with (root/'logs'/(name+'.log')).open('w') as log:
        code=subprocess.call(['Rscript',str(root/'run-case.R'),job['id'],str(job['seed']),job['role']],stdout=log,stderr=subprocess.STDOUT,env=env)
    return name,code
# The first pilot may be running in a separate terminal, using one fitting slot.
pilot=root/'results/binary_ridge_reported_p5-cache-seed429/DONE'
external_pilot=os.environ.get('PMSIMS_EXTERNAL_PILOT')=='1'
if external_pilot:jobs=jobs[1:]
with concurrent.futures.ThreadPoolExecutor(max_workers=3 if external_pilot else 4) as pool:
    futures=[pool.submit(run,j) for j in jobs]
    for f in concurrent.futures.as_completed(futures): print(*f.result(),flush=True)
