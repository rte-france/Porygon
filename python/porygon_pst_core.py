# -*- coding: utf-8 -*-
"""
Copyright (c) 2020-2022, RTE (www.rte-france.com)
See AUTHORS.txt
SPDX-License-Identifier: MPL-2.0
This file is part of the Porygon project.
"""

import pulp
import porygon_ptdf
import pandas
import timeit
import pypowsybl as gp
import pypowsybl.network

# Constants
EPSILON = 1e-5
MIN_SENSI = 0.0001 
PST_COST = 0.1
HVDC_COST = 0.001
NB_RESULTS = 10
PST_ALLOWED_RANGE = 1/3 # range allowed in both side for optmisation

HVDC_DATA = {
    'ALEGRO' : {'node1' : 'XLI_OB1B_ALEGRO_Or', 'node2' : 'XLI_OB1A_ALEGRO_Ex', 'country1' : 'BE', 'country2' : 'DE', 'maxP' : 1000}
}


def pst_optimisation(n, cnecs_df, borders, contingencies_df=None, psts=[], hvdcs=[], mode='acer'):
    
    if len(psts) + len(hvdcs) == 0:
        return cnecs_df
    
    cnecs_df.reset_index(inplace=True)
    
    # ##################
    #  Retrieve PST data
    # ##################
    pst_data = n.get_phase_tap_changers().filter(items=psts, axis=0)[['tap']].rename(columns={'tap':'nb_taps'})
    pst_steps = n.get_phase_tap_changer_steps()
    pst_steps = pst_steps[pst_steps.index.isin(PSTS, level=0)]
    pst_data['max_phase_shift'] = (pst_steps.groupby(level=[0])['alpha'].max() - pst_steps.groupby(level=[0])['alpha'].min()).round(1)
    pst_data = pst_data.to_dict(orient='index')
    
    # ##################
    #  Retrieve HVDC data
    # ##################
    hvdc_data = n.get_hvdc_lines().filter(items=hvdcs, axis=0)[['max_p', 'converter_station1_id', 'converter_station2_id']].\
        rename(columns={'converter_station1_id':'node1', 'converter_station2_id':'node2'}).\
               to_dict(orient='index')
    
    # ##################
    #  Sensi computation
    # ##################
    hubs = set([i for l in borders for i in l]) 
    
    print('Computing PST sensitivity...', flush=True)
    hvdc_elements = [hvdc_data[i]['node1'] for i in hvdcs] + [hvdc_data[i]['node2'] for i in hvdcs]
    elements = psts + hvdc_elements
    elements = [i for i in elements if i not in cnecs_df.columns]
    
    pst_sensitivities = porygon_ptdf.compute_ptdf(n, cnecs_df, contingencies_df, other_elements_id=elements).\
        drop(columns=['Margin', 'F0'])
        
    pst_sensitivities = pandas.merge(cnecs_df, pst_sensitivities, 
                                     on=['CNEC Name', 'id', 'Contingency Name', 'Direction', 'Fmax (MW)', 'FRM (MW)'])
    
    # Combine HVDC elements sensitivities
    for i in hvdcs:
        pst_sensitivities[i] = pst_sensitivities[hvdc_data[i]['node1']] - pst_sensitivities[hvdc_data[i]['node2']]
    
    # Temporary hack for ALEGRO
    pst_sensitivities['BEDE.hub'] = pst_sensitivities['XLI_OB1B_ALEGRO_Or'] - pst_sensitivities['XLI_OB1A_ALEGRO_Ex'] + pst_sensitivities['DE.hub'] 
    pst_sensitivities['DEBE.hub'] = pst_sensitivities['XLI_OB1A_ALEGRO_Ex'] - pst_sensitivities['XLI_OB1B_ALEGRO_Or'] + pst_sensitivities['BE.hub']

    porygon_ptdf.update_margin(pst_sensitivities)
    
    # ###################
    #    OPTIMISATION
    # ###################
    
    # Creation du probleme
    print("Optimisation using", len(pst_sensitivities), "CNECs, mode='"+mode+"'", flush=True)
    prob = pulp.LpProblem("PST_optimisation", pulp.LpMaximize)
      
    pst_vars_pos = pulp.LpVariable.dicts("POS", psts, 0, 0)
    pst_vars_neg = pulp.LpVariable.dicts("NEG", psts, 0, 0)
    
    hvdc_vars_pos = pulp.LpVariable.dicts("POS", hvdcs, 0, 0)
    hvdc_vars_neg = pulp.LpVariable.dicts("NEG", hvdcs, 0, 0)
    
    for i in psts:
        max_phase_shift = pst_data[i]['max_phase_shift']*PST_ALLOWED_RANGE/2 # assumes PST is on neutral step
        pst_vars_pos[i].bounds(0, max_phase_shift)
        pst_vars_neg[i].bounds(0, max_phase_shift)
        
    for i in hvdcs:
        max_power = hvdc_data[i]['max_p'] # assumes HVDC is initial at 0
        hvdc_vars_pos[i].bounds(0, max_power)
        hvdc_vars_neg[i].bounds(0, max_power)
        
    min_margin = pulp.LpVariable("min_margin")
    
    # objective function
    prob += min_margin\
            - pulp.lpSum(PST_COST * pst_vars_pos[i] for i in psts)\
            - pulp.lpSum(PST_COST * pst_vars_neg[i] for i in psts)\
            - pulp.lpSum(HVDC_COST * hvdc_vars_pos[i] for i in hvdcs)\
            - pulp.lpSum(HVDC_COST * hvdc_vars_neg[i] for i in hvdcs)\
    
    for index, row in pst_sensitivities.iterrows():
        #print("Processing constraint for ", row['Quadripole']) 
        if mode == 'acer':
            max_ptdf = sum([abs(row[x[0]+".hub"]-row[x[1]+".hub"]) for x in borders])
            if (row['Margin'] < 0):
                max_ptdf = 1
        elif mode == 'max_ptdf':
            max_ptdf = max([abs(row[x+".hub"]) for x in hubs])
        elif mode == 'max_border':
            max_ptdf = max([abs(row[x[0]+".hub"]-row[x[1]+".hub"]) for x in borders])
        else:
            print("Error: unknown PST optimisation mode '" + mode + "'")
            raise SystemExit
                
        #print(max_ptdf)
        if max_ptdf > EPSILON:
            prob += min_margin * max_ptdf - row['Margin']\
                + pulp.lpSum((pst_vars_pos[i]-pst_vars_neg[i])*row[i] for i in psts)\
                + pulp.lpSum((hvdc_vars_pos[i]-hvdc_vars_neg[i])*row[i] for i in hvdcs) <= 0
                
    # debug LP file
    #prob.writeLP("Flow-based.lp")
    
    # Sove with default solver
    prob.solve()
    
    # Status
    print("Status:", pulp.LpStatus[prob.status])
    
    if prob.status != pulp.LpStatusOptimal:
        print("Error during PST optimisation")
        prob.writeLP("Flow-based_PST_optimisation.lp")
        raise SystemExit
    
    # Optimal variables values
    for i in psts:
        phase_shift = pst_vars_pos[i].varValue - pst_vars_neg[i].varValue 
        print(i, "=", phase_shift)
        cnecs_df["F0"] = cnecs_df["F0"] + pst_sensitivities[i].multiply(phase_shift) # Update CNEC Fref
    print()
    
    for i in hvdcs:
        power_shift = hvdc_vars_pos[i].varValue - hvdc_vars_neg[i].varValue 
        print(i, "=", power_shift)
    print()

    print("\nResult:") 
    print(min_margin.name, "=", min_margin.varValue) 
    
    print("Top", NB_RESULTS, "most limiting CNECs:")
    print(cnecs_df.sort_values(by=["Margin"]).head(NB_RESULTS)[["id", "Contingency Name", "Margin"]])
    
    cnecs_df["F0"] = cnecs_df["F0"].round()
    cnecs_df["Margin"] = cnecs_df["Margin"].round()
    
    return cnecs_df


if __name__ == '__main__':
    
    NETWORK_FILE = 'path_to_network_file'
    
    PTDF_FILE = "path_to_csv_domain_file"
    CONTINGENCY_FILE = "path_to_csv_contingency_description_file"
    RESULT_FILE = "path_to_optimised_domain_csv_file"
    
    PSTS = ['list of psts ids to use in optimisation']
    HVDCS = ['ALEGRO']
    
    CORE_BORDERS = [['AT', 'DE'], ['AT', 'HU'], ['AT', 'SI'],
                    ['BE', 'FR'], ['BE', 'NL'], ['BE', 'BEDE'],
                    ['CZ', 'DE'], ['CZ', 'PL'], ['CZ', 'SK'],
                    ['DE', 'FR'], ['DE', 'NL'], ['DE', 'PL'], ['DE', 'DEBE'],
                    ['HR', 'HU'], ['HR', 'SI'],
                    ['HU', 'RO'], ['HU', 'SI'], ['HU', 'SK'],
                    ['PL', 'SK']]

    print('Reading network...', flush=True)
    start = timeit.default_timer()
    n = gp.network.load(NETWORK_FILE)
    print("Time:", round(timeit.default_timer() - start),"s.", flush=True)
    
    cnecs_df = pandas.read_csv(PTDF_FILE, sep=';', decimal=',')
    contingencies_df = pandas.read_csv(CONTINGENCY_FILE, sep=';', decimal=',')
   
    pst_optimisation(n, cnecs_df, CORE_BORDERS, contingencies_df, psts=PSTS, hvdcs=HVDCS, mode='acer').\
        to_csv(RESULT_FILE, sep=';', header=True, index=False, decimal=',')

       

