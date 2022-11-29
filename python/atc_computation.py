# -*- coding: utf-8 -*-
"""
Copyright (c) 2020-2022, RTE (www.rte-france.com)
See AUTHORS.txt
SPDX-License-Identifier: MPL-2.0
This file is part of the Porygon project.
"""

import os
import pandas as pd
import numpy as np

pd.options.mode.use_inf_as_na = True

def compute_id_atc(ptdfs, net_positions, borders, debug=False):    
    data = ptdfs.copy()
    for i in net_positions:
       data['Margin'] += net_positions[i] * data[i+'.hub']  
    
    return compute_atc(data, borders, debug)

def compute_atc(ptdfs, borders, debug=False):
    data = ptdfs.copy()
    nb_shares = len({item for sublist in borders for item in sublist}) - 1 # DEBE and BEDE
    
    if 'ALEGRO.hub' in data.columns:
        # combine ALEGRO PTDF
        data['BEDE.hub'] = data['ALEGRO.hub'] + data['DE.hub']
        data['DEBE.hub'] = data['BE.hub'] - data['ALEGRO.hub']
    else:
        data['BEDE.hub'] = data['DEBE.hub'] = 0
    
    #check negative margin CNECs
    negative_margin_cnecs = data[data['Margin'] < 0]
    if (len(negative_margin_cnecs.index) > 0):
        print("WARNING : There are", len(negative_margin_cnecs.index),"CNEC with an initial negative margin:")
        print(negative_margin_cnecs[['CNEC Name', 'id', 'Contingency Name', 'Margin']].to_string(index=False))
        print()
    
    if 'Presolved' in data.columns:
        data = data[data['Presolved'] == True]
    
    #print(data)
    #data.to_csv('debug.csv', sep=';', header=True, index=False, decimal=',')
    #raise SystemExit
    
    # compute borders ptdf
    hub_names = []
    for hub in borders:
        hub1 = hub[0]
        hub2 = hub[1]
        hub_name = hub1 + " - " + hub2
        data[hub_name] = data[hub1 + ".hub"] - data[hub2 + ".hub"]
        hub_names.append(hub_name)

    data = data[["id", "Contingency Name", "Margin"] + hub_names].copy() 
    data[data[hub_names] < 0] = np.nan
    data.dropna(axis=0, how='all', subset=hub_names, inplace=True)
    data['RAM'] = data['Margin']
    data.loc[data['RAM']<0, 'RAM'] = 0

    # atc computation loop
    sum_delta_atc = 9999
    atc = np.zeros(1, dtype={'names':hub_names, 'formats':[np.float32]*len(hub_names)})
    while sum_delta_atc > 0.1:
        data['RAMshare'] = data['RAM'] / nb_shares
        sum_delta_atc = 0
        for hub_name in hub_names:
            delta_atc = max(0, (data['RAMshare']/data[hub_name]).dropna().min())
            atc[hub_name] = atc[hub_name] + delta_atc
            data['RAM'] = np.where(data[hub_name].isna(), data['RAM'], data['RAM'] - data[hub_name] * delta_atc)
            sum_delta_atc = sum_delta_atc + delta_atc
        if debug:
            print(atc)
            print(data[["id", "Contingency Name", "Margin", "RAM"]].sort_values("RAM"))
            
    #data = pd.concat([data.drop('RAMshare', 1), pd.DataFrame(np.repeat(atc, len(data.index)), columns=hub_names)], axis=1)
    result = pd.DataFrame(atc, columns=hub_names).round(0)
    print(result)
    return result


if __name__ == "__main__":
    PTDF_FILE = 'path_to_csv_domain_file'
    ptdfs = pd.read_csv(PTDF_FILE, sep=';', decimal=',')
    
    CORE_BORDERS = [['AT', 'DE'], ['AT', 'HU'], ['AT', 'SI'],
                   ['BE', 'BEDE'], ['BE', 'FR'], ['BE','NL'],
                   ['CZ', 'DE'], ['CZ', 'PL'], ['CZ', 'SK'],
                   ['DE', 'AT'], ['DE', 'DEBE'], ['DE', 'CZ'], ['DE', 'FR'], ['DE', 'NL'], ['DE', 'PL'],
                   ['FR', 'BE'], ['FR', 'DE'],
                   ['HR', 'HU'], ['HR', 'SI'],
                   ['HU', 'AT'], ['HU', 'HR'], ['HU', 'RO'], ['HU', 'SI'], ['HU', 'SK'],
                   ['NL', 'BE'], ['NL', 'DE'],
                   ['PL', 'DE'], ['PL', 'SK'], ['PL', 'CZ'], 
                   ['RO', 'HU'], 
                   ['SI', 'AT'], ['SI', 'HR'], ['SI', 'HU'],
                   ['SK', 'CZ'], ['SK', 'HU'], ['SK', 'PL']]
    
    filepath = os.path.splitext(PTDF_FILE)
    compute_atc(ptdfs, CORE_BORDERS, debug=True).\
        to_csv(filepath[0]+'_ATC'+filepath[1], sep=';', header=True, index=False, decimal=',')
    
    net_positions = {'AT' : 3425, 'BE' : 2365, 'CZ' : 2937, 'DE' : 2648, 'FR' : -3450, 'HR' : 629,
                     'HU' : 1479, 'NL' : -2625, 'PL' : -1133, 'RO' : -1012, 'SI' : -2351, 'SK' : -2912, 'ALEGRO' : 1000}     
    compute_id_atc(ptdfs, net_positions, CORE_BORDERS)
    
