# -*- coding: utf-8 -*-
"""
Copyright (c) 2020-2022, RTE (www.rte-france.com)
See AUTHORS.txt
SPDX-License-Identifier: MPL-2.0
This file is part of the Porygon project.
"""

import pypowsybl as gp

import timeit
import pandas

gp.set_debug_mode(False)

PRECISION = 5                 

# ######### FUNCTIONS #############
 
def compute_amr(cnecs_df, min_ram_percent=0.2):
    cnecs_df['AMR_tmp'] = cnecs_df['Fmax (MW)'].multiply(min_ram_percent - 1) + cnecs_df['FRM (MW)'] + cnecs_df['F0']
    if 'AMR' not in cnecs_df.columns:
        cnecs_df['AMR'] = 0
    cnecs_df['AMR'] = cnecs_df[['AMR', 'AMR_tmp']].max(axis=1).round(0)
    cnecs_df.drop(columns=['AMR_tmp'], inplace=True)
    return cnecs_df 


def update_margin(cnecs_df):
    cnecs_df['Margin'] = cnecs_df['Fmax (MW)'] - cnecs_df['FRM (MW)'] - cnecs_df['F0']
    if 'AMR' in cnecs_df.columns:
        cnecs_df['Margin'] = cnecs_df['Margin'] + cnecs_df['AMR']
    return cnecs_df

def update_reference_flows(n, cnecs_df, hub_balances, hvdc_elements={}, xnodes_elements={}):
    balances = hub_balances.copy()
    
    hvdc_df = n.get_hvdc_lines()
    hvdc_df_or = hvdc_df.loc[hvdc_df['converter_station1_id'].isin(hvdc_elements), ['converter_station1_id', 'converters_mode', 'target_p']]
    hvdc_df_ext = hvdc_df.loc[hvdc_df['converter_station2_id'].isin(hvdc_elements), ['converter_station2_id', 'converters_mode', 'target_p']]
    or_inverter = hvdc_df_or['converters_mode'] == 'SIDE_1_RECTIFIER_SIDE_2_INVERTER'
    hvdc_df_or.loc[or_inverter, 'target_p'] = hvdc_df_or.loc[or_inverter, 'target_p'].multiply(-1)
    ext_inverter = hvdc_df_ext['converters_mode'] == 'SIDE_1_INVERTER_SIDE_2_RECTIFIER'
    hvdc_df_ext.loc[ext_inverter, 'target_p'] = hvdc_df_ext.loc[ext_inverter, 'target_p'].multiply(-1)
    hvdc_df_full = pandas.concat([hvdc_df_or, hvdc_df_ext.rename(columns={'converter_station2_id' : 'converter_station1_id'})], axis=0)
    hvdc_balances = dict(hvdc_df_full[['converter_station1_id', 'target_p']].values)
     
    for hvdc in hvdc_balances:
        #print(hvdc, hvdc_elements[hvdc], hvdc_balances[hvdc])
        cnecs_df['F0'] = cnecs_df['F0'] - hvdc_balances[hvdc] * cnecs_df[hvdc]
    
    load_balances = dict(n.get_loads().loc[xnodes_elements, 'p0'])
    
    for load in load_balances:
        #print(load, xnodes_elements[load], load_balances[load])
        cnecs_df['F0'] = cnecs_df['F0'] + load_balances[load] * cnecs_df[load]
        
    for country in balances:
        #print(country, balances[country])
        cnecs_df['F0'] = cnecs_df['F0'] - balances[country] * cnecs_df[country + ".hub"]

    cnecs_df['F0'] = cnecs_df['F0'].round(0)
    return cnecs_df


def add_contingencies(sa, contingencies_df_chunk):
    # N-1 contingencies
    for index, row in contingencies_df_chunk.groupby(['Contingency Name'])\
                                            .filter(lambda x: len(x) == 1)\
                                            .iterrows():   
        sa.add_single_element_contingency(row['id'], row['Contingency Name'])
            
    # N-k contingencies
    for index, row in contingencies_df_chunk.groupby(['Contingency Name'])\
                                            .filter(lambda x: len(x) > 1)\
                                            .drop_duplicates(subset=['Contingency Name']).iterrows():
        outage_elements = list(contingencies_df_chunk[contingencies_df_chunk['Contingency Name'] == row['Contingency Name']]['id'].unique())
        sa.add_multiple_elements_contingency(outage_elements, row['Contingency Name'])


def compute_ptdf(n, cnecs_df, contingencies_df=None, countries=[], other_elements_id=[], gsk = gp.sensitivity.ZoneKeyType.GENERATOR_TARGET_P):    
    # SENSIBILITY COMPUTATION    
    parameters = gp.loadflow.Parameters(distributed_slack=True, balance_type=gp.loadflow.BalanceType.PROPORTIONAL_TO_LOAD) # On ne peut pas mettre la balance à None
    #print(parameters, flush=True)
    
    print("Sensibility elements:", len(countries), 'countries,', len(other_elements_id), 'other elements')
    
    outage_list = list(cnecs_df['Contingency Name'].unique())
    print("Contingencies:", len(outage_list))
    #print(outage_list, flush=True)
    
    ptdfs = pandas.DataFrame()
    column_names = ['CNEC Name', 'id', 'Contingency Name', 'Direction', 'Fmax (MW)', 'FRM (MW)']
    ptdf_columns = ['reference_flows'] + countries + other_elements_id
    start_all = timeit.default_timer()
    
    zones = [gp.sensitivity.create_country_zone(n, i, key_type = gsk) for i in countries] 
    
    lines_ids = list(cnecs_df[cnecs_df['Contingency Name'].isin(outage_list)]['id'].unique())
    sa = gp.sensitivity.create_dc_analysis()
    sa.set_zones(zones)
    sa.set_branch_flow_factor_matrix(lines_ids, [z.id for z in zones] + other_elements_id) 

    if contingencies_df is not None: 
        contingencies_df_chunk = contingencies_df[contingencies_df['Contingency Name'].isin(outage_list)]
        add_contingencies(sa, contingencies_df_chunk)
                                              
    print('Computing sensitivity for', len(lines_ids),'lines and', len(outage_list), 'outages on',
          len(countries) , 'countries and ', len(other_elements_id), 'elements', flush=True)
    start = timeit.default_timer()
    r = sa.run(n, parameters)
    print("Sensitivity computation time:", round(timeit.default_timer() - start),"s.", flush=True)
        
    # PTDF COMPUTATION    
    print("Computing PTDF...", flush=True)
    for outage in outage_list:
        #print('-', outage, flush=True)
        cnecs = cnecs_df[cnecs_df['Contingency Name'] == outage][column_names]
        line_ids = list(cnecs['id'].unique())
        
        cid = None if outage == 'BASECASE' else outage
        m = (r.get_branch_flows_sensitivity_matrix(cid).T).loc[line_ids,]
        tmp_ptdfs = (r.get_reference_flows(cid).T).loc[line_ids,]     
        tmp_ptdfs = pandas.concat([tmp_ptdfs, m[countries+other_elements_id]], axis=1)
        tmp_ptdfs = tmp_ptdfs.merge(cnecs, how='right', right_on='id', left_index=True)
        
        # invert PTDF in OPPOSITE direction
        row_ids = tmp_ptdfs['Direction'] == 'OPPOSITE'
        tmp_ptdfs.loc[row_ids, ptdf_columns] = tmp_ptdfs[row_ids][ptdf_columns].multiply(-1)
        ptdfs = pandas.concat([ptdfs, tmp_ptdfs], axis=0)

    print("Overall time:", round(timeit.default_timer() - start_all),"s.", flush=True)
    print(ptdfs)
    #raise SystemExit
    
    ptdfs = ptdfs.reset_index(drop=True)
    
    ptdfs[ptdf_columns] = ptdfs[ptdf_columns].round(PRECISION)
    ptdfs["reference_flows"] = ptdfs["reference_flows"].round(0)
    ptdfs.rename(columns={"reference_flows" : "F0"}, inplace=True)
    ptdfs.rename(columns={i:i+'.hub' for i in countries}, inplace=True)
    ptdfs["Fmax (MW)"] = ptdfs["Fmax (MW)"].astype(float).round(0)
    update_margin(ptdfs)
    
    return ptdfs[column_names + ['F0', 'Margin'] + [i+'.hub' for i in countries] + other_elements_id]

