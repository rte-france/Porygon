# -*- coding: utf-8 -*-
"""
Copyright (c) 2020-2022, RTE (www.rte-france.com)
See AUTHORS.txt
SPDX-License-Identifier: MPL-2.0
This file is part of the Porygon project.
"""

import os
import pulp
import pandas 

MAX_EXCHANGE = 50000 

def max_bilateral_exchanges(data, borders, debug=True):
    
    cnecs_df = data.copy(deep=True)
        
    if 'Presolved' in cnecs_df.columns:
        cnecs_df = cnecs_df[cnecs_df['Presolved'] == True]
    
    # Variables
    core_hubs = set([i for border in borders for i in border])
    
    net_positions = pulp.LpVariable.dicts("np", core_hubs, -MAX_EXCHANGE, MAX_EXCHANGE)
    
    result = []
    
    for i in borders:
        # Optimisation problem creation
        prob = pulp.LpProblem("Flow-based-bilateral-exchanges", pulp.LpMaximize)
    
        #prob += net_positions['BEDE'] + net_positions['DEBE'] == 0
    
        for index, row in cnecs_df.iterrows():
            #print("Processing constraint for ", row['Quadripole']) 
            prob += pulp.lpSum(net_positions[j]*row[j+".hub"] for j in core_hubs) <= row['Margin']
    
        prob += pulp.lpSum(net_positions[j] for j in i) == 0 
        prob += pulp.lpSum(net_positions[j] for j in core_hubs if j not in i) == 0
        
        # Objective function for A -> B
        prob.setObjective(net_positions[i[0]])        
        # Solve
        prob.solve(pulp.GLPK(msg=False))
    
        # debug LP file
        #prob.writeLP("Flow-based.lp")
    
        # Resolution status
        #print("Status:", pulp.LpStatus[prob.status])
    
        # Optimal variable values
        # for v in prob.variables():
        #     print(v.name, "=", v.varValue)
    
        row = {}
        result.append(row)
        row['border'] = i
    
        if debug:
            print(i, ":", round(net_positions[i[0]].varValue))
        row['max'] = round(net_positions[i[0]].varValue)
    
        # Objective function for A -> B
        prob.setObjective(net_positions[i[1]])        
        # Solve
        prob.solve(pulp.GLPK(msg=False))
    
        if debug:
            print(i[::-1],":", round(net_positions[i[1]].varValue))
        row['min'] = -round(net_positions[i[1]].varValue)
    
    return pandas.DataFrame(result).set_index('border')


def max_net_positions(data, hubs, core_hubs=[], debug=False):
    cnecs_df = data.copy()
    
    if 'Presolved' in cnecs_df.columns:
        cnecs_df = cnecs_df[cnecs_df['Presolved'] == True]
    
    net_positions = pulp.LpVariable.dicts("np", hubs, -MAX_EXCHANGE, MAX_EXCHANGE)
    
    result = []
   
    # Optimisation problem creation
    prob = pulp.LpProblem("Flow-based-net-positions", pulp.LpMaximize)

    for index, row in cnecs_df.iterrows():
        #print("Processing constraint for ", row['Quadripole']) 
        prob += pulp.lpSum(net_positions[j]*row[j+".hub"] for j in hubs) <= row['Margin']

    prob += pulp.lpSum(net_positions[i] for i in core_hubs) == 0
    
    for i in hubs:
        # Objective function for A -> B
        prob.setObjective(net_positions[i])        
        # Solve
        prob.solve(pulp.GLPK(msg=False))
    
        # debug LP file
        #prob.writeLP("Flow-based.lp")
    
        # Resolution status
        #print("Status:", pulp.LpStatus[prob.status])
    
        # Optimal variable values
        # for v in prob.variables():
        #     print(v.name, "=", v.varValue)
    
        row = {}
        result.append(row)
        row['hub'] = i
    
        if debug:
            print(i, ":", round(net_positions[i].varValue))
        row['max'] = round(net_positions[i].varValue)
    
        # Objective function for A -> B
        prob.setObjective(-net_positions[i])        
        # Solve
        prob.solve(pulp.GLPK(msg=False))
    
        if debug:
            print(i, ":", round(net_positions[i].varValue))
        row['min'] = round(net_positions[i].varValue)
        
    return pandas.DataFrame(result).set_index('hub')


if __name__ == '__main__':
    
    CORE_BORDERS = [['AT', 'CZ'], ['AT', 'DE'], ['AT', 'HU'], ['AT', 'SI'],
                    ['BE', 'FR'], ['BE','NL'], ['BE', 'ALEGRO'],
                    ['CZ', 'DE'], ['CZ', 'PL'], ['CZ', 'SK'],
                    ['DE', 'FR'], ['DE', 'NL'], ['DE', 'PL'], ['DE', 'ALEGRO'],
                    ['HR', 'HU'], ['HR', 'SI'],
                    ['HU', 'RO'], ['HU', 'SI'], ['HU', 'SK'],
                    ['PL', 'SK']]

    core_hubs = list({i for border in CORE_BORDERS for i in border})

    PTDF_FILES = ['path_to_csv_domain_files',
                  'path_to_csv_domain_files'
                  ]
    
    # Limits on AHC links
    AHC_LIMITS = {'ptdfFR-IT.hub' : [4350, 1995], 
                  'ptdfAT-IT.hub' : [660, 490], 
                  'ptdfSI-IT.hub' : [285, 680],
                  'ptdfFR-ES.hub' : [2900, 2700], 
                  'ptdfDE-DK.hub' : [3500, 3500], 
                  'ptdfFR-CH.hub' : [3700, 1400], 
                  'ptdfDE-CH.hub' : [2600, 4000],
                  'ptdfAT-CH.hub' : [1200, 1200],  
                  'ptdfHR-RS.hub' : [200, 300], 
                  'ptdfHU-RS.hub' : [600, 600], 
                  'ptdfRO-RS.hub' : [1000, 1000],
                  'ptdfRO-BG.hub' : [2190, 2190], 
                  'ptdfHR-BA.hub' : [800, 900], 
                  'ptdfBE-NEMO.hub' : [1000, 1000], 
                  'ptdfFR-IFA1.hub' : [3000, 3000],
                  'ptdfFR-IFA2.hub' : [1000, 1000], 
                  'ptdfNL-BRITNED.hub' : [1000, 1000], 
                  'ptdfPL-SE_PL.hub' : [600, 600],
                  'ptdfDE-DKE_DE.hub' : [600, 585], 
                  'ptdfNL-DKW_NL.hub' : [700, 700], 
                  'ptdfNL-NO_NL.hub' : [700, 700],
                  'ptdfDE-SE_DE.hub' : [615, 615], 
                  'ptdfDE-NO_DE.hub' : [1400, 1400], 
                  'ptdfRO-UA_RO.hub' : [200, 200],
                  'ptdfSK-UA_SK.hub' : [495, 634], 
                  'ptdfPL-LT_PL.hub' : [492, 485], 
                  'ptdfHU-UA_HU.hub' : [450, 650],
                  'ptdfDE-UK_DE.hub' : [0, 0]
                  }
    
    AHC_LINKS = [['FR', 'IT'], 
                ['AT', 'IT'],
                ['SI', 'IT'],
                ['FR', 'ES'],
                ['DE', 'DK'],
                ['FR', 'CH'],
                ['DE', 'CH'],
                ['AT', 'CH'],
                ['HR', 'RS'],
                ['HU', 'RS'],
                ['RO', 'RS'],
                ['RO', 'BG'],
                ['HR', 'BA'],
                ['BE', 'NEMO'],
                ['FR', 'IFA1'],
                ['FR', 'IFA2'],
                ['NL', 'BRITNED'],
                ['PL', 'SE_PL'],
                ['DE', 'DKE_DE'],
                ['NL', 'DKW_NL'],
                ['NL', 'NO_NL'],
                ['DE', 'SE_DE'],
                ['DE', 'NO_DE'],
                ['RO', 'UA_RO'],
                ['SK', 'UA_SK'],
                ['PL', 'LT_PL'],
                ['HU', 'UA_HU'],
                ['DE', 'UK_DE']]
    
    bilateral_exchanges_results = pandas.DataFrame()
    net_positions_results = pandas.DataFrame()
    for i in PTDF_FILES:
        print(i)
        filepath = os.path.splitext(i)
        cnecs_df = pandas.read_csv(i, sep=';', decimal=',')

        #cnecs_df.rename(columns={'ALEGRO.hub' : 'ptdfALEGRO.hub'}, inplace=True)

        # Adds ALEGRO limits
        ALEGRO_LIMIT = 1000
        cnecs_df = cnecs_df.append([pandas.DataFrame([['ALEGRO max', 1, ALEGRO_LIMIT, True], ['ALEGRO min', -1, ALEGRO_LIMIT, True]], columns=['CNEC Name', 'ALEGRO.hub', 'Margin', 'Presolved'])], ignore_index=True).fillna(0)    

        if frozenset({i for j in AHC_LINKS for i in j}) in cnecs_df.columns:
            for l in AHC_LINKS:
                cnecs_df['ptdf'+l[0]+'-'+l[1]+'.hub'] = cnecs_df[l[0]+'.hub'] - cnecs_df[l[1]+'.ahc']

            cnecs_df = cnecs_df.append([pandas.DataFrame([[i+' max', 1, AHC_LIMITS[i][0], True], [i+' min', -1, AHC_LIMITS[i][1], True]], columns=['CNEC Name', i, 'Margin', 'Presolved']) for i in AHC_LIMITS], ignore_index=True).fillna(0)    

        hubs = [i[:-4] for i in cnecs_df.columns if i.endswith('.hub')]
        core_hubs = list({i for l in CORE_BORDERS for i in l if not i == 'ALEGRO'})

        result = max_bilateral_exchanges(cnecs_df, CORE_BORDERS)  
        print(result)
        result.to_csv(filepath[0]+'_minmax_exchanges'+filepath[1], sep=';', header=True, decimal=',')
        result['domain'] = i
        bilateral_exchanges_results = bilateral_exchanges_results.append(result)
        result = max_net_positions(cnecs_df, hubs, core_hubs) 
        print(result)
        result.to_csv(filepath[0]+'_minmax_netpositions'+filepath[1], sep=';', header=True, decimal=',')
        result['domain'] = i
        net_positions_results = net_positions_results.append(result)

    bilateral_exchanges_results = bilateral_exchanges_results.reset_index() 
    bilateral_exchanges_results["border"] = bilateral_exchanges_results["border"].apply(lambda s: (s[0]+'-'+s[1]))
    net_positions_results = net_positions_results.reset_index() 
    
    # Plotly
    import plotly.graph_objects as go
    import plotly.io as pio

    #pio.renderers.default = 'svg'
    pio.renderers.default = 'browser'
    pandas.options.plotting.backend = "plotly"

    fig = go.Figure()
    for i in range(len(PTDF_FILES)):
        metrics = bilateral_exchanges_results[bilateral_exchanges_results['domain'] == PTDF_FILES[i]] 
        fig.add_trace(go.Box(x=metrics["border"],
                                 name=str(i),
                                 q1=metrics["min"],
                                 median=[0]*len(metrics["border"]),
                                 q3=metrics["max"],
                                 #marker_color=palette[i%NB_COLORS],
                                 showlegend=False))
    fig.update_layout(title="Bilateral exchanges", title_x=0.5, boxmode='group')
    fig.show()
            
    fig = go.Figure()
    for i in range(len(PTDF_FILES)):
        metrics = net_positions_results[net_positions_results['domain'] == PTDF_FILES[i]] 
        fig.add_trace(go.Box(x=metrics["hub"],
                                 name=str(i),
                                 q1=metrics["min"],
                                 median=[0]*len(metrics["hub"]),
                                 q3=metrics["max"],
                                 #marker_color=palette[i%NB_COLORS],
                                 showlegend=False))
    fig.update_layout(title="Net positions", title_x=0.5, boxmode='group')
    fig.show()
     

