#!/usr/bin/env python3
"""Generate deterministic study jobs; actual results remain separate."""
import csv, json, pathlib
root = pathlib.Path('validation/predictor-count-spikes')
cases = list(csv.DictReader((root/'results/cases.csv').open()))
baseline = [dict(name=c['id']+'-baseline', script='run-search.R', args=[c['id'], 'baseline'])
            for c in sorted(cases, key=lambda c: (c['seed_role'] != 'cache', int(c['p'])))]
grid = []
for i,c in enumerate(cases):
    ns = [1000,2000,4000] if c['seed_role']=='cache' else [1000,4000]
    for n in ns:
        seed = 7200002 if c['id']=='p15-seed1725' and n==2000 else 7210000 + i*10000 + n
        grid.append(dict(name=c['id']+'-grid-n'+str(n), script='run-grid.R', args=[c['id'], n, seed]))
checks = [dict(name=c['id']+'-'+mode+'-heldout', script='run-check.R',
               args=[c['id'],mode,8100000+i*10000+j*1000])
          for i,c in enumerate(cases) for j,mode in enumerate(['baseline','common-bounds'])]
wide = [dict(name=c['id']+'-common-bounds', script='run-search.R', args=[c['id'],'common-bounds']) for c in cases]
for name,jobs in [('baseline',baseline),('reference-grid',grid),('answer-checks',checks),('common-searches',wide)]:
    (root/(name+'-jobs.json')).write_text(json.dumps(jobs, indent=2)+'\n')
