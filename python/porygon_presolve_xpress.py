# -*- coding: utf-8 -*-
"""
Copyright (c) 2020-2022, RTE (www.rte-france.com)
See AUTHORS.txt
SPDX-License-Identifier: MPL-2.0
This file is part of the Porygon project.
"""

import os
# Update this if environment variables for Xpress could not be set
#os.environ['XPRESSDIR'] = ''
#os.environ['PATH'] = os.environ['PATH']
#os.environ['XPRESS'] = ''

import pulp
import pandas
import timeit
from multiprocessing import Pool, Manager
import xpress as xp #### Note that xpress version must align with license (currently <=8.12)

EPSILON = 0.01

POOL_SIZE = 6
UPDATE_RATE = 100

CORE_HUBS = ['AT', 'BE', 'CZ', 'DE', 'FR', 'HR', 'HU', 'NL', 'PL', 'RO', 'SI', 'SK']

def test_cnec_glpk(local_cnec_data, core_hubs, ahc_hubs, filtered_cnecs, chunk_size, chunk_no):
    all_hubs = core_hubs + ahc_hubs
    
    start_index = chunk_size * chunk_no
    end_index = min(start_index + chunk_size, len(local_cnec_data.index))
    print('Starting chunk', chunk_no, '[', start_index,'-', end_index,']', flush=True)
    presolved = []
    last_index = 0
    # Optimisation problem creation
    prob = pulp.LpProblem("Flowbased_presolved", pulp.LpMaximize)
    
     
    hubs = pulp.LpVariable.dicts("country", all_hubs)
    alegro = pulp.LpVariable("alegro")
    
    # Nul balance in CORE (constraint #1)
    prob += pulp.lpSum(hubs[i] for i in core_hubs) == 0
    
    # Nul total balance (constraint #2)
    prob += pulp.lpSum(hubs[i] for i in all_hubs) == 0
    
    # CNECs constraints
    for index, row in local_cnec_data.iterrows(): 
          # cnec equation
          prob += pulp.lpSum(hubs[i]*row[i+'.hub'] for i in core_hubs)\
                  + pulp.lpSum(hubs[i]*row[i] for i in ahc_hubs)\
                  + alegro * row['ALEGRO.hub'] <= row['Margin']    
    
    # filtering cnec subset    
    tmp_filtered = set()
    start = timeit.default_timer()
    for index, row in local_cnec_data.iloc[start_index:end_index].iterrows():
        constraint = prob.constraints.pop("_C" + str(index+3))
        
        if sum([abs(row[i+".hub"]) for i in core_hubs]) + sum([abs(row[i]) for i in ahc_hubs]) + abs(row['ALEGRO.hub']) < 1e-5:
            continue
        
        prob.setObjective(pulp.lpSum(hubs[i]*row[i+'.hub'] for i in core_hubs)\
                          + pulp.lpSum(hubs[i]*row[i] for i in ahc_hubs)\
                          + alegro * (row['ALEGRO.hub']))     
        
        #prob.writeLP("Flow-based_presolved.lp")
        
        prob.solve(pulp.GLPK(msg=False, options=["--dual"]))
        
        if (prob.status != pulp.LpStatusOptimal):
            print(index,"- Status:", pulp.LpStatus[prob.status])
            prob.writeLP("Flow-based_presolved_"+index+".lp")
            return
        
        max_transit = pulp.value(prob.objective)
        
        if max_transit > row['Margin'] - EPSILON:
            prob += constraint
            presolved_cnec = (row['id'], row['Contingency Name'], row['Direction'])
            presolved.append(presolved_cnec)
            print("Found limiting CNEC", index, ":", presolved_cnec, '[', max_transit, ">" , row["Margin"], ']')
        else:
            tmp_filtered.add(index)
            
        if index % UPDATE_RATE == 0:
            print(index, "/", end_index, "(avg time =" , round(timeit.default_timer()-start), "s.)",flush=True)   
            newly_filtered = [k for k in filtered_cnecs[last_index:]]
            filtered_cnecs.extend(tmp_filtered)
            last_index = len(filtered_cnecs)
            
            print("Chunk", chunk_no, ": ", len(newly_filtered), "newly filtered CNECs")
            
            for i in newly_filtered:
                constraint = prob.constraints.pop("_C" + str(i+3), None)
            tmp_filtered.clear()
            start = timeit.default_timer()
          
    return presolved

def test_cnec_xpress(local_cnec_data, core_hubs, ahc_hubs, filtered_cnecs, chunk_size, chunk_no):
    all_hubs = core_hubs + ahc_hubs
    
    start_index = chunk_size * chunk_no
    end_index = min(start_index + chunk_size, len(local_cnec_data.index))
    print('Starting chunk', chunk_no, '[', start_index,'-', end_index,']', flush=True)
    presolved = []
    last_index = 0
    # Optimisation problem creation
    prob = xp.problem("Flowbased_presolve")
    prob.setControl({'outputlog':0})
    
     
    
    hubs = {hub:xp.var(lb=-xp.infinity,
                       name="country_" + hub) for hub in all_hubs}
    prob.addVariable(hubs)
    
    alegro = xp.var(lb=-xp.infinity,name="alegro")
    prob.addVariable(alegro)
        
    # Nul balance in CORE (constraint #1)
    core_bal = xp.constraint(xp.Sum(hubs[i] for i in core_hubs)==0,
                             name="CORE_balance")
    prob.addConstraint(core_bal)
    
    # Nul total balance (constraint #2)
    total_bal = xp.constraint(xp.Sum(hubs[i] for i in all_hubs)==0,
                              name="Total_balance")
    prob.addConstraint(total_bal)
    
    # CNECs constraints
    cnec_constraints = {}
    for index, row in local_cnec_data.iterrows(): 
        # cnec equation
        cnec_constraints[index] = xp.constraint(
                xp.Sum(hubs[i]*row[i+'.hub'] for i in core_hubs)\
                + xp.Sum(hubs[j]*row[j] for j in ahc_hubs)\
                + alegro * row['ALEGRO.hub'] <= row['Margin'],
                name = "CNEC_" + str(index))
        prob.addConstraint(cnec_constraints[index])
          
          
    
    # filtering cnec subset    
    tmp_filtered = set()
    start = timeit.default_timer()
    for index, row in local_cnec_data.iloc[start_index:end_index].iterrows():
        prob.delConstraint(cnec_constraints[index])
        
        
        if sum([abs(row[i+".hub"]) for i in core_hubs]) + sum([abs(row[i]) for i in ahc_hubs]) + abs(row['ALEGRO.hub']) < 1e-5:
            continue
        
        prob.setObjective(
                xp.Sum(hubs[i]*row[i+'.hub'] for i in core_hubs)\
                + xp.Sum(hubs[j]*row[j] for j in ahc_hubs)\
                + alegro*(row['ALEGRO.hub']),
                sense = xp.maximize)    
        
        #prob.writeLP("Flow-based_presolved.lp")
        
        prob.solve()
        
        if (prob.getProbStatusString() != 'lp_optimal'):
            print(index,"- Status:", prob.getProbStatusString())
            prob.writeLP("Flow-based_presolved_"+str(index)+".lp")
            return
        
        max_transit = prob.getObjVal()
        
        if max_transit > row['Margin'] - EPSILON:
            prob.addConstraint(cnec_constraints[index])
            presolved_cnec = (row['id'], row['Contingency Name'], row['Direction'])
            presolved.append(presolved_cnec)
            print("Found limiting CNEC", index, ":", presolved_cnec, '[', max_transit, ">" , row["Margin"], ']')
        #else:
        #    tmp_filtered.add(index)
            
        if index % UPDATE_RATE == 0:
            print(index, "/", end_index, "(avg time =" , round(timeit.default_timer()-start), "s.)",flush=True)   
            #newly_filtered = [k for k in filtered_cnecs[last_index:]]
            #filtered_cnecs.extend(tmp_filtered)
            #last_index = len(filtered_cnecs)
            
            #print("Chunk", chunk_no, ": ", len(newly_filtered), "newly filtered CNECs")
            
            #for i in newly_filtered:
            #    prob.delConstraint(cnec_constraints[i+1])
            #tmp_filtered.clear()
            start = timeit.default_timer()
    
    
    return presolved



def fb_domain_presolve(cnec_data, core_hubs=CORE_HUBS, ahc_hubs=[], processes=POOL_SIZE,solver="xpress"):   

    # reorder index
    cnec_data_work = cnec_data.copy().sort_values(by=['Margin'], ascending=False).reset_index(drop=True)
    
    # Add ALEGRO PTDF if missing
    if 'ALEGRO.hub' not in cnec_data_work.columns:
        if 'BEDE.hub' in cnec_data_work.columns:
            cnec_data_work['ALEGRO.hub'] = cnec_data_work['BEDE.hub'] - cnec_data_work['DEBE.hub']
        else:
            cnec_data_work['ALEGRO.hub'] = 0
    
    if solver == "glpk":
        start = timeit.default_timer()
        chunk_size = round(len(cnec_data.index)/processes)+1
        print('Chunk size:', chunk_size)
        presolved_cnecs = []
        with Manager() as manager: 
            filtered = manager.list()
            with Pool(POOL_SIZE) as p:
                presolved_cnecs.extend(p.starmap(test_cnec_glpk, ((cnec_data_work, core_hubs, ahc_hubs, filtered, chunk_size, i) for i in range(processes))))
        #print(presolved_cnecs)
        presolved_cnecs_flattened = [item for sublist in presolved_cnecs if sublist != None for item in sublist ]        
        print(len(presolved_cnecs_flattened), "presolved CNEC found", "(Time:", round(timeit.default_timer()-start), "s.)")
        print(presolved_cnecs_flattened)
    
    elif solver == "xpress":
        ## Note that with xpress, I have not implemented the parallelization 
        ## For all the tests that I ran the parallelization was slower 
        ## It's possible that if we have a ton of lines, it could be useful to add back in
        start = timeit.default_timer()
        chunk_size = len(cnec_data.index)
        i = 0
        presolved_cnecs = test_cnec_xpress(cnec_data_work, core_hubs, ahc_hubs, [], chunk_size, i)
        
        presolved_cnecs_flattened = presolved_cnecs
        print(len(presolved_cnecs_flattened), "presolved CNEC found", "(Time:", round(timeit.default_timer()-start), "s.)")
        print(presolved_cnecs_flattened)
    
    cnec_data['Presolved'] = False
    for item in presolved_cnecs_flattened:
        cnec_data.loc[(cnec_data['id'] == item[0]) & (cnec_data['Contingency Name'] == item[1]) & (cnec_data['Direction'] == item[2]),
                      'Presolved'] = True
    
    return cnec_data
    
    
if __name__ == '__main__':
    PTDF_FILE = "path_to_csv_domain_file"    
    cnec_data = pandas.read_csv(PTDF_FILE, sep=';', decimal=',')
    cnec_data.fillna(0, inplace=True)
    print(cnec_data) 
    filepath = os.path.splitext(PTDF_FILE)
    fb_domain_presolve(cnec_data).to_csv(filepath[0]+'_presolved_xpress'+filepath[1], sep=';', header=True, index=False, decimal=',')
