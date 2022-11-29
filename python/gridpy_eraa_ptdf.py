# -*- coding: utf-8 -*-
"""
Copyright (c) 2020-2022, RTE (www.rte-france.com)
See AUTHORS.txt
SPDX-License-Identifier: MPL-2.0
This file is part of the Porygon project.
"""
import pypowsybl as gp

import porygon_ptdf
import porygon_pst_core
import porygon_presolve

import pandas
import numpy
import csv

import timeit

PRESOLVE = True
TRACES = False
REMOVE_DUPLICATE_ROWS = True
INCLUDE_AHC_CONSTRAINTS = False

CROSS_BORDER_CNE_ONLY = True
CNE_380_ONLY = False

CNE_VOLTAGE_LEVELS = {220, 225, 380, 400, 750}
ERAA_DIR = '../data/' # directory containing data
CNEC_FILE = ERAA_DIR + 'xlsx_cnec_definition_file' # name of merged CNEC file
CROSS_BORDER_FILE = ERAA_DIR + 'Co_dict_interco_CORE+AHC.xlsx'

FILE_NAME = "_ERAA_PTDF"

if CROSS_BORDER_CNE_ONLY:
    FILE_NAME = FILE_NAME + "_CBONLY"
if CNE_380_ONLY:
    FILE_NAME = FILE_NAME + "_380KV"
    CNE_VOLTAGE_LEVELS = {380, 400, 750}

# ###################
#  ERAA22 TYPICAL DAYS
# ###################
#TYPICAL_DAY_DATA = ['6169', 'Fmax_summer (MW)', '7_20010914_2300']
#TYPICAL_DAY_DATA = ['3957', 'Fmax_summer (MW)', '33_20010614_1900']
#TYPICAL_DAY_DATA = ['7182', 'Fmax_winter (MW)', '33_20011027_0400']
TYPICAL_DAY_DATA = ['7503', 'Fmax_winter (MW)', '33_20011109_1300']

TYPICAL_DAY = TYPICAL_DAY_DATA[0]
FMAX = TYPICAL_DAY_DATA[1]
WORKING_DIR = ERAA_DIR + 'year'+TYPICAL_DAY_DATA[2][:TYPICAL_DAY_DATA[2].find('_')]+'/'+TYPICAL_DAY+'_'+TYPICAL_DAY_DATA[2][TYPICAL_DAY_DATA[2].find('_')+1:]+'/'
NETWORK_FILE = WORKING_DIR + 'TYNDP2025_'+TYPICAL_DAY_DATA[2]+'.xiidm.gz'
BALANCE_FILE = WORKING_DIR + 'balance_CORE_timeId_'+TYPICAL_DAY+'.csv'


CORE_HUBS = ['AT', 'BE', 'CZ', 'DE', 'FR', 'HR', 'HU', 'NL', 'PL', 'RO', 'SI', 'SK']
CORE_BORDERS = [['AT', 'CZ'], ['AT', 'DE'], ['AT', 'HU'], ['AT', 'SI'],
                ['BE', 'FR'], ['BE','NL'], ['BE', 'BEDE'],
                ['CZ', 'DE'], ['CZ', 'PL'], ['CZ', 'SK'],
                ['DE', 'FR'], ['DE', 'NL'], ['DE', 'PL'], ['DE', 'DEBE'],
                ['HR', 'HU'], ['HR', 'SI'],
                ['HU', 'RO'], ['HU', 'SI'], ['HU', 'SK'],
                ['PL', 'SK']]


NON_CORE_HUBS = {'ba00' : 'BA', 'bg00' : 'BG', 'ch00' : 'CH', 'dkw1' : 'DK', 'es00' : 'ES', 'itn1' : 'IT', 'rs00' : 'RS'}
NON_CORE_HVDCS = {  #'XGU_HU1D_DESE_HANSA1_Or' : 'DE', # DESE_HANSA1
                    'XHW_KR1D_DESE_600_Or' : 'DE',    # DESE_600   
                    'XWI_TO1D_NORGER_1400_Or' : 'DE', # NORGER_1400
                    'XFE_GR1D_BRITGER_Ex' : 'DE',     # BRITGER    
                    'XBE_GB1B_NEMO_Ex' : 'BE',        # NEMO       
                    'XMA_SE11_IFA2000_A_Ex' : 'FR',   # IFA2000_A  
                    'XMA_SE13_IFA2000_B_Ex' : 'FR',   # IFA2000_B  
                    'XTO_CH11_IFA2__Ex' : 'FR',       # IFA2_      
                    'XMA_SE15_ELECLINK_Ex' : 'FR',    # ELECLINK   
                    'XGR_MA1N_BRITNED_Ex' : 'NL',     # BRITNED    
                    'XEE_FE1N_NORNED_700_Or' : 'NL',  # NORNED_700 
                    'XED_EE1N_COBRA_700_Or' : 'NL',  'XED_EE1D_COBRA_700_Ex' : 'DK',  # COBRA_700  
                    'XBW_BJ1D_DEDKE1_1000_Or' : 'DE', # DEDKE1_1000
                    'XSL_SW11_PLSE_VSC_Or' : 'PL' }   # PLSE_VSC   

NON_CORE_XLOADS = {'XEL_AL11_L' : 'PL', 'XEL_AL12_L' : 'PL', 'XDR_ZR1P_L' : 'PL',  # 'XEL_AL11', 'XEL_AL12', 'XDR_ZR1P' (LT - PL) 
                      '11_1_01c6acd2-016' : 'RO', #'XRO_MU11' (RO - UA)
                      '78210_99' : 'HU', '78431_99' : 'HU', '78988_99' : 'HU', '78556_99' : 'HU', # 'XKI_MU21', 'XTI_MU21', 'XBA_ZA01', 'XBA_MU11', (HU - UA)
                      '60_1_832e3fce-f57' : 'SK'} # 'XVK_MU11'] (SK - UA)

GSK_PARAMETERS = None
# GSK_PARAMETERS =  {'element_type' : gp.network.ElementType.LOAD, 
#                    'distribution_key' : 'p0',
#                    'nominal_voltages' : None}

PSTS = ['Trafo 421.2.I', 'CHRDT451', 'CHRDT454', 'CHRDT453', 'CHRDT452', 'PST_DIEL_442', 'PST_DIEL_441', 
        'EHPST_1d3e3cff-029b-', 'PST_Gronau_441', 'MIK-PF4', 'MIK-PF1', 'MIK-PF2', 'MIK-PF3', 'Mee-drtw', 
        'Mee-drtz', 'PST_VIE_443', 'PST_VIE_441', 'PST_ROE_442', 'PST_ROE_441', 'TAPST_TOOTon_P312', 
        'TEPST_db02036e-9237-', 'VANYK380 D 1 380/380', 'VANYK380 D 2 380/380', 'ZANDV380 D 2 380/380', 'ZANDV380 D 1 380/380']


ALEGRO_CONVERTORS = {'XLI_OB1B_ALEGRO_Or' : 'BE', 'XLI_OB1A_ALEGRO_Ex' : 'DE'} 

def ahc_constraints(ahc_id, hub_name, max_p, min_p = None):
    min_p = max_p if min_p == None else min_p
    return pandas.DataFrame([{'CNEC Name' : ahc_id+'_max', 'id' : ahc_id, 'Contingency Name' : 'BASECASE', 'Direction' : 'DIRECT', hub_name : 1, 'Fmax (MW)' : max_p},
                             {'CNEC Name' : ahc_id+'_min', 'id' : ahc_id, 'Contingency Name' : 'BASECASE', 'Direction' : 'OPPOSITE', hub_name : -1, 'Fmax (MW)' : min_p}])


def format_result(ptdf_df, mode='shc', include_ahc_constraints = False):
    columns = ['CNEC Name', 'id', 'Contingency Name', 'Direction', 'Presolved', 'Fmax (MW)', 'FRM (MW)', 'F0', 'Margin', 'AMR'] +\
                  [i+".hub" for i in CORE_HUBS] + ['ALEGRO.hub']
   
    # ALEGRO constraint
    ptdf_df['ALEGRO.hub'] = ptdf_df['XLI_OB1A_ALEGRO_Ex'] - ptdf_df['XLI_OB1B_ALEGRO_Or']
    if include_ahc_constraints:
        ptdf_df = pandas.concat([ptdf_df, ahc_constraints('ALEGRO', 'ALEGRO.hub', 1000)], axis=0, ignore_index=True).fillna(0)
    
    if mode == 'ahc':
        columns.extend(['NEMO.ahc', 'BRITNED.ahc', 'IFA1.ahc', 'IFA2.ahc', 
                        'BA.ahc', 'BG.ahc', 'CH.ahc', 'DK.ahc', 'ES.ahc', 'IT.ahc', 'RS.ahc', 
                        'UK_DE.ahc', 'SE_PL.ahc', 'DKW_NL.ahc', 'DKE_DE.ahc', 'SE_DE.ahc', 'NO_DE.ahc', 'NO_NL.ahc', 
                        'LT_PL.ahc', 'UA_RO.ahc', 'UA_HU.ahc', 'UA_SK.ahc'])
        
        # AC countries PTDF
        ptdf_df.rename(columns = {i+".hub" : i+".ahc" for i in NON_CORE_HUBS.values()}, inplace=True)
        
        # HVDC PTDF
        ptdf_df['NEMO.ahc'] = ptdf_df['XBE_GB1B_NEMO_Ex']
        ptdf_df['BRITNED.ahc'] = ptdf_df['XGR_MA1N_BRITNED_Ex']
        ptdf_df['IFA2.ahc'] = ptdf_df['XTO_CH11_IFA2__Ex']
        ptdf_df['IFA1.ahc'] = ptdf_df['XMA_SE11_IFA2000_A_Ex']
        ptdf_df['UK_DE.ahc'] = ptdf_df['XFE_GR1D_BRITGER_Ex']
        ptdf_df['SE_PL.ahc'] = ptdf_df['XSL_SW11_PLSE_VSC_Or']
        ptdf_df['DKE_DE.ahc'] = ptdf_df['XBW_BJ1D_DEDKE1_1000_Or']
        ptdf_df['DKW_NL.ahc'] = ptdf_df['XED_EE1N_COBRA_700_Or'] - ptdf_df['XED_EE1D_COBRA_700_Ex']
        ptdf_df['NO_NL.ahc'] = ptdf_df['XEE_FE1N_NORNED_700_Or']
        ptdf_df['SE_DE.ahc'] = ptdf_df['XHW_KR1D_DESE_600_Or']
        ptdf_df['NO_DE.ahc'] = ptdf_df['XWI_TO1D_NORGER_1400_Or']
       
        # XNodes PTDF
        ptdf_df['UA_RO.ahc'] = ptdf_df['11_1_01c6acd2-016']
        ptdf_df['UA_SK.ahc'] = ptdf_df['60_1_832e3fce-f57']
        ptdf_df['LT_PL.ahc'] = ptdf_df['XEL_AL11_L'].multiply(2970/7440) + ptdf_df['XEL_AL12_L'].multiply(2970/7440) + ptdf_df['XDR_ZR1P_L'].multiply(1500/7440)
        ptdf_df['UA_HU.ahc'] = ptdf_df['78210_99'].multiply(820/4640) + ptdf_df['78431_99'].multiply(820/4640) + ptdf_df['78988_99'].multiply(2000/4640) + ptdf_df['78556_99'].multiply(1000/4640)
        ptdf_df['LT_PL.ahc'].round(porygon_ptdf.PRECISION)
        ptdf_df['UA_HU.ahc'].round(porygon_ptdf.PRECISION)
        
        if include_ahc_constraints:
            ptdf_df = pandas.concat([ptdf_df, 
                                     ahc_constraints('NEMO', 'NEMO.ahc', 1000),
                                     ahc_constraints('BRITNED', 'BRITNED.ahc', 1000),
                                     ahc_constraints('IFA2', 'IFA2.ahc', 1000),
                                     ahc_constraints('IFA1', 'IFA1.ahc', 3000),
                                     ahc_constraints('SE_PL', 'SE_PL.ahc', 600),
                                     ahc_constraints('DKE_DE', 'DKE_DE.ahc', 600, 585),
                                     ahc_constraints('DKW_NL', 'DKW_NL.ahc', 700),
                                     ahc_constraints('NO_NL', 'NO_NL.ahc', 700),
                                     ahc_constraints('SE_DE', 'SE_DE.ahc', 615),
                                     ahc_constraints('NO_DE', 'NO_DE.ahc', 1400),
                                     ahc_constraints('UK_DE', 'UK_DE.ahc', 0),
                                     ahc_constraints('UA_RO', 'UA_RO.ahc', 200),
                                     ahc_constraints('UA_SK', 'UA_SK.ahc', 495, 634),
                                     ahc_constraints('LT_PL', 'LT_PL.ahc', 492, 485),
                                     ahc_constraints('UA_HU', 'UA_HU.ahc', 450, 650),
                                     ], axis=0, ignore_index=True).fillna(0)
        
    return ptdf_df[ptdf_df.columns.intersection(columns)]
    
    

if __name__ == '__main__':
    
    overall_start = timeit.default_timer()
    
    print('Reading TYNDP20 network...', flush=True)
    start = timeit.default_timer()
    n = gp.network.load(NETWORK_FILE)
    print("Time:", round(timeit.default_timer() - start),"s.", flush=True)
    
    print('Reading CORE balances...', flush=True)
    balances = pandas.read_csv(BALANCE_FILE, delimiter=';')
    maf2hub = {**NON_CORE_HUBS, **{i.lower() + '00' : i for i in CORE_HUBS}}
    balances['area'] = balances['area'].map(maf2hub)
    core_balances = {i : balances[balances['area'] == i]['Balance_fb'].iloc[0] for i in CORE_HUBS}
    ahc_balances = {i : balances[balances['area'] == i]['Balance_ahc'].iloc[0] for i in maf2hub.values()}
        
    mc_branches_ids = n.get_elements_ids(element_type=gp.network.ElementType.LINE, 
                                         nominal_voltages=CNE_VOLTAGE_LEVELS, 
                                         main_connected_component=True,
                                         not_connected_to_same_bus_at_both_sides=True) +\
                      n.get_elements_ids(element_type=gp.network.ElementType.TWO_WINDINGS_TRANSFORMER, 
                                         nominal_voltages=CNE_VOLTAGE_LEVELS, 
                                         main_connected_component=True,
                                         not_connected_to_same_bus_at_both_sides=True)
    print(len(mc_branches_ids), "branches in main-component")
    
    # rdf_ids missing in network file
    hvdc_rdfid_dict = {'_508bdfad-70cb-42f6-b5d4-f3e8e205ce2d' : 'ALEGRO',
                       '_968403df-b86a-4752-b527-f02261f20fad' : 'ALEGRO',
                       '_7bcbb68cb94e425bb88478cd7eb83588' : 'DC02 OSTRAT-PHILI'}
    
    hvdc_rdfid_df = pandas.DataFrame().from_dict(hvdc_rdfid_dict, orient='index', columns=['id'])
    hvdc_rdfid_df['main_component'] = True
    
    rdfid_dict = pandas.concat([n.get_lines(True)[['rdfId']],
                                n.get_2_windings_transformers(True)[['rdfId']]])
    
    rdfid_dict['id'] = rdfid_dict.index
    rdfid_dict['main_component'] = rdfid_dict['id'].isin(mc_branches_ids)
    rdfid_dict.set_index('rdfId', inplace=True)
    
    rdfid_dict = pandas.concat([rdfid_dict,hvdc_rdfid_df])
    
    print()
    print(len(rdfid_dict), "branches in dic")
    
    print('Reading CNECs data...', flush=True)
    cnec_data = pandas.read_excel(CNEC_FILE, sheet_name='CNECs')
    cnec_data['Contingency Name'] = cnec_data['Contingency Name'].str.strip()
    #print(cnec_data)
    
    contingency_data = pandas.read_excel(CNEC_FILE, sheet_name='Elements in contingency')
    contingency_data['Contingency Name'] = contingency_data['Contingency Name'].str.strip()
    #print(contingency_data)
    
    print('*********************************************')
    print('Performing some checks...')
    
    unknown_cnecs = cnec_data.loc[~cnec_data['CNE RDF ID'].isin(rdfid_dict.index)].drop_duplicates(subset=['CNE RDF ID'])
    print()
    print(len(unknown_cnecs), 'Unknown CNEs')
    print(unknown_cnecs[['Country', 'CNE']])
    if TRACES:
        unknown_cnecs.to_csv(ERAA_DIR+'Unknown_CNE.csv', sep=';', header=True, index=False, decimal='.')
    
    print()
    print('Contingencies with >1 elements:')
    contingency_data.groupby(['Country', 'Contingency Name']).filter(lambda x: len(x) > 1).drop_duplicates()
    
    unknown_contingencies = (cnec_data.loc[~cnec_data['Contingency Name'].isin(contingency_data['Contingency Name']),]['Contingency Name']).unique()
    unknown_contingencies = numpy.setdiff1d(unknown_contingencies, ['BASECASE'])
    print()
    print(len(unknown_contingencies), 'Unknown contingencies')
    print(cnec_data.loc[cnec_data['Contingency Name'].isin(unknown_contingencies), ['Country', 'Contingency Name']].drop_duplicates())
    
    duplicate_contigencies = pandas.merge(contingency_data[['Country', 'Contingency Name']], 
                                          contingency_data['Contingency Name'].value_counts().rename('count'),
                                          how='right', left_on='Contingency Name', right_index=True)
    print('\nDuplicate contingency definitions')
    print(duplicate_contigencies[duplicate_contigencies['count'] > 3].drop_duplicates())
    
    unknown_elements = contingency_data.loc[~contingency_data['RDF ID'].isin(rdfid_dict.index),['Country', 'Element Name', 'Contingency Name', 'RDF ID']].drop_duplicates()
    print()
    print(len(unknown_elements), 'Unknown contingency elements')
    print(unknown_elements)
    if TRACES:
        unknown_elements.to_csv(ERAA_DIR+'Unknown_contingencies.csv', sep=';', header=True, index=False, decimal='.')
    print('*********************************************')
    
    # Create 'id' column based on rdf-id
    #cnec_dict = pandas.DataFrame.from_dict(rdfid_dict, orient='index', columns=['id'])
    cnec_data = cnec_data.merge(rdfid_dict, left_on='CNE RDF ID', right_index=True)
    
    # Remove CNE which are not in the MCC
    open_cne = cnec_data[cnec_data['main_component'] == False][['Country', 'CNE', 'CNE Location']].drop_duplicates()
    cnec_data = cnec_data[cnec_data['main_component'] == True]
    print('CNE(s) not in main connected component (i.e. disconnected):', len(open_cne.index) ,flush=True)
    print(open_cne)
    print()
    
    print('Reading cross-border lines...', flush=True)
    cross_border_data = pandas.read_excel(CROSS_BORDER_FILE, sheet_name='CORE')
    print(cross_border_data)
    
    #contingency_data['id'] = contingency_data['RDF ID'].map(rdfid_dict)
    contingency_data = contingency_data.merge(rdfid_dict, left_on='RDF ID', right_index=True)
    contingency_data.dropna(inplace = True)
    
    cross_border_outage = contingency_data[(contingency_data['id'].isin(cross_border_data['Element Name'])) | 
                                           (contingency_data['id'].isin(cross_border_data['Element Name 2'])) | 
                                           (contingency_data['id'].isin(cross_border_data['Element Name 3']))]
    
    print("Cross-border lines in contingencies", len(cross_border_outage['id'].unique()))
    cross_border_dict = {'ALEGRO' : 'ALEGRO',
                         'DC02 OSTRAT-PHILI' : 'DC02 OSTRAT-PHILI'}
    cross_border_dict.update(dict(cross_border_data[['Element Name', 'Element Name']].values))
    cross_border_dict.update(dict(cross_border_data[['Element Name 2', 'Element Name']].values))
    cross_border_dict.update(dict(cross_border_data[['Element Name 3', 'Element Name']].dropna().values))
    print('After duplicate removal:', len(cross_border_outage['id'].map(cross_border_dict).unique()))
    print()
    
    cross_border_ahc_data = pandas.read_excel(CROSS_BORDER_FILE, sheet_name='AHC')
    cross_border_ahc_outage = contingency_data[(contingency_data['id'].isin(cross_border_ahc_data['Element Name'])) | 
                                               (contingency_data['id'].isin(cross_border_ahc_data['Element Name 2']))]
    
    print("AHC lines in contingencies:", len(cross_border_ahc_outage['id'].unique()))
    cross_border_ahc_dict = dict(cross_border_ahc_data[['Element Name', 'Element Name']].values)
    cross_border_ahc_dict.update(dict(cross_border_ahc_data[['Element Name 2', 'Element Name']].values))
    print('After duplicate removal:', len(cross_border_ahc_outage['id'].map(cross_border_ahc_dict).unique()))
    print()
    
    
    cnec_data_ahc = cnec_data[(cnec_data['id'].isin(cross_border_ahc_data['Element Name'])) | 
                              (cnec_data['id'].isin(cross_border_ahc_data['Element Name 2']))]
    print("AHC lines in CNE:", len(cnec_data_ahc['id'].unique()))
    print()
    
    cnec_data_cross_border = cnec_data[(cnec_data['id'].isin(cross_border_data['Element Name'])) | 
                                       (cnec_data['id'].isin(cross_border_data['Element Name 2'])) | 
                                       (cnec_data['id'].isin(cross_border_data['Element Name 3']))]
    print('Unique cross-border lines in CNE:', len(cnec_data_cross_border['id'].unique()))
    print('After duplicate removal:', len(cnec_data_cross_border['id'].map(cross_border_dict).unique()))
    print()
    
    print('\n----------------------')
    print('Before CNEC filtering:')
    print('----------------------')
    print('Unique CNEs:', len(cnec_data['CNE RDF ID'].unique()))
    print('Unique contingencies:', len(contingency_data['Contingency Name'].unique()))
    print('Nb CNECs:', len(cnec_data['CNE RDF ID']))
    print('----------------------\n')
    
    # #################################
    #          CNEC FILTERING
    # #################################
    
    print('Removing duplicates contingencies')
    #print('Before:', contingency_data.groupby(['Contingency Name', 'id']).filter(lambda x: len(x) > 1))
    contingency_data.drop_duplicates(subset=['Contingency Name', 'id'], keep="first", inplace=True)
    #print('After:', contingency_data.groupby(['Contingency Name', 'id']).filter(lambda x: len(x) > 1))
    print('Nb Contingencies:', len(contingency_data['Contingency Name'].unique()), 
                            '(n-k : ', len(contingency_data.groupby(['Contingency Name']).filter(lambda x: len(x) > 1)), ')')
    
    print('Removing cross-border duplicates from contingencies')
    cross_border_outage = contingency_data[(contingency_data['id'].isin(cross_border_data['Element Name'])) | 
                                           (contingency_data['id'].isin(cross_border_data['Element Name 2'])) | 
                                           (contingency_data['id'].isin(cross_border_data['Element Name 3']))]
    contingency_data.loc[cross_border_outage.index, 'id'] = contingency_data.loc[cross_border_outage.index]['id'].map(cross_border_dict)
    contingency_data.drop_duplicates(subset=['Contingency Name', 'id'], keep="first", inplace=True)
    
    raw_contigency_dict = {'BASECASE' : 'BASECASE'}
    raw_contigency_dict.update(dict(contingency_data[['Contingency Name', 'id']].values))
    revert_contigency_dict = {v: k for k, v in raw_contigency_dict.items()}
    contigency_dict = {k: revert_contigency_dict[v] for k, v in raw_contigency_dict.items()}
    contingency_data['Contingency Name'] = contingency_data['Contingency Name'].map(contigency_dict)
    contingency_data.drop_duplicates(subset=['Contingency Name', 'id'], keep="first", inplace=True)
    print('Nb Contingencies:', len(contingency_data['Contingency Name'].unique()), 
                            '(n-k : ', len(contingency_data.groupby(['Contingency Name']).filter(lambda x: len(x) > 1)), ')')
    
    
    print('Removing AHC lines from CNEC')
    cnec_data_ahc = cnec_data[(cnec_data['id'].isin(cross_border_ahc_data['Element Name'])) | 
                              (cnec_data['id'].isin(cross_border_ahc_data['Element Name 2']))]
    cnec_data.drop(cnec_data_ahc.index, inplace=True)
    print('Nb CNECs:', len(cnec_data))
    
    print('Removing cross-border duplicates from CNEC')
    cross_border_cne = cnec_data[(cnec_data['id'].isin(cross_border_data['Element Name'])) | 
                                 (cnec_data['id'].isin(cross_border_data['Element Name 2'])) | 
                                 (cnec_data['id'].isin(cross_border_data['Element Name 3']))]
    cnec_data.loc[cross_border_cne.index, 'id'] = cnec_data.loc[cross_border_cne.index]['id'].map(cross_border_dict)
    cnec_data.drop_duplicates(subset=['id', 'Contingency Name', 'Direction', 'Fmax_winter (MW)', 'Fmax_summer (MW)', 'Fmax_interseasonal (MW)',	'FRM (MW)'], 
                              keep="first", inplace=True)
    print('Nb CNECs:', len(cnec_data))
    
    if CROSS_BORDER_CNE_ONLY:
        print('Keeping cross-border CNECs only')
        cnec_data = cnec_data[cnec_data['id'].isin(cross_border_dict.values())]
        print('Nb CNECs:', len(cnec_data))
    
    print('Expanding "BOTH" direction and removing duplicate contingencies from CNEC')
    cnec_data['Contingency Name'] = cnec_data['Contingency Name'].map(contigency_dict)
    cnec_data.dropna(subset = ['Contingency Name'], inplace = True)
    
    # duplicate CNEC with BOTH direction
    cnec_data['Direction'] = cnec_data['Direction'].str.upper()
    row_ids = cnec_data['Direction'] == 'BOTH'
    cnec_data_both = cnec_data.loc[row_ids,].copy()
    cnec_data_both['Direction'] = 'OPPOSITE'
    cnec_data.loc[row_ids, 'Direction'] = 'DIRECT'
    cnec_data = pandas.concat([cnec_data, cnec_data_both], axis=0)
    cnec_data.drop_duplicates(subset=['id', 'Contingency Name', 'Direction', 'Fmax_winter (MW)', 'Fmax_summer (MW)', 'FRM (MW)'], 
                              keep="first", inplace=True)
    print('Nb CNECs:', len(cnec_data))
    
    print('\n----------------------')
    print('After CNEC filtering:')
    print('----------------------')
    print('Unique CNEs:', len(cnec_data['id'].unique()))
    print('Unique contingencies:', len(contingency_data['Contingency Name'].unique()))
    print('Nb CNECs:', len(cnec_data))
    print('----------------------\n')
    
    print(contingency_data.groupby(['Contingency Name']).filter(lambda x: len(x) > 1))
    
    if TRACES:
        with open(ERAA_DIR+"ERAA_CNE.txt", "w") as f:
            for item in cnec_data['id'].drop_duplicates():
                f.write("'%s',\n" % item)
        cnec_data.to_csv(ERAA_DIR+'CNEs.csv', sep=';', header=True, index=False, decimal='.')

        with open(ERAA_DIR+"ERAA_Contingencies.txt", "w") as f:
            for item in contingency_data['id'].drop_duplicates():
                f.write("'%s',\n" % item)
        contingency_data.to_csv(ERAA_DIR+'Contigencies.csv', sep=';', header=True, index=False, decimal='.')
    
    #raise SystemExit
    
    # #################################
    #         PTDF COMPUTATION
    # #################################    
    cnec_data['Fmax (MW)'] = cnec_data[FMAX] # winter point
    # drop duplicates
    cnec_data['Fmax-FRM'] = cnec_data['Fmax (MW)'] - cnec_data['FRM (MW)']
    cnec_data.sort_values(by=['Fmax-FRM'], ascending=True, inplace=True)
    cnec_data.drop_duplicates(subset=['id', 'Contingency Name', 'Direction'], keep="first", inplace=True)
    cnec_data.drop(columns=['Fmax-FRM'], inplace=True)
    
    if TRACES:    
        cnec_data.to_csv('cnec_data.csv', sep=';', header=True, index=False, decimal=',')
        contingency_data.to_csv('contingency_data.csv', sep=';', header=True, index=False, decimal=',')
    
    print()
    
    other_elements = list(ALEGRO_CONVERTORS.keys()) + list(NON_CORE_HVDCS.keys()) + list(NON_CORE_XLOADS.keys())
    result = porygon_ptdf.compute_ptdf(n, cnec_data, contingency_data, list(maf2hub.values()), other_elements)
    
    print("Raw domain")    
    print(result)
    if TRACES:
        result.to_csv(WORKING_DIR+TYPICAL_DAY+FILE_NAME+'_raw.csv', sep=';', header=True, index=False, decimal=',')

    # Results before optimisation
    raw_netpos0 = porygon_ptdf.update_reference_flows(n, result.copy(), core_balances, ALEGRO_CONVERTORS)
      
    # PST optimisation
    print("PST optimisation")    
    result_optimised = porygon_pst_core.pst_optimisation(n, raw_netpos0.copy(), CORE_BORDERS, contingency_data, PSTS, mode="acer")

    print("\nMACZT computation") 
    # raw domain
    if TRACES:
        raw_maczt = porygon_ptdf.update_reference_flows(n, raw_netpos0.copy(), ahc_balances, NON_CORE_HVDCS, NON_CORE_XLOADS)
        min_ram = pandas.concat([porygon_ptdf.compute_amr(raw_netpos0)['AMR'], 
                                 porygon_ptdf.compute_amr(raw_maczt, 0.7)['AMR'].rename('AMR_MACZT')], axis=1)
        min_ram['AMR'] = min_ram[['AMR', 'AMR_MACZT']].max(axis=1)
        raw_netpos0['AMR'] = min_ram['AMR']
        porygon_ptdf.update_margin(format_result(raw_netpos0)).to_csv(WORKING_DIR+TYPICAL_DAY+FILE_NAME+'_minram20.csv', sep=';', header=True, index=False, decimal=',') 
  
    # optimised domain
    result_maczt = porygon_ptdf.update_reference_flows(n, result_optimised.copy(), ahc_balances, NON_CORE_HVDCS, NON_CORE_XLOADS)
    min_ram = pandas.concat([porygon_ptdf.compute_amr(result_optimised)['AMR'], 
                             porygon_ptdf.compute_amr(result_maczt, 0.7)['AMR'].rename('AMR_MACZT')], axis=1)
    min_ram['AMR'] = min_ram[['AMR', 'AMR_MACZT']].max(axis=1)
    result_optimised['AMR'] = min_ram['AMR']
    result_maczt['AMR'] = min_ram['AMR']
    
    result_optimised = porygon_ptdf.update_margin(format_result(result_optimised, include_ahc_constraints=INCLUDE_AHC_CONSTRAINTS))
    result_maczt = porygon_ptdf.update_margin(format_result(result_maczt, "ahc", INCLUDE_AHC_CONSTRAINTS))
    
    if REMOVE_DUPLICATE_ROWS:
        length_before = len(result_optimised.index)
        result_optimised.drop_duplicates(subset=['Margin'] + [i for i in result_maczt.columns if i.endswith('.hub')], 
                              keep="first", inplace=True)
        print('Final duplicate removal removed', length_before-len(result_optimised.index), 'rows in SHC')
        length_before = len(result_maczt.index)
        result_maczt.drop_duplicates(subset=['Margin'] + [i for i in result_maczt.columns if i.endswith('.hub')] + [i for i in result_maczt.columns if i.endswith('.ahc')], 
                              keep="first", inplace=True) 
        print('Final duplicate removal removed', length_before-len(result_maczt.index), 'rows in AHC')

    if not PRESOLVE or TRACES:
        print("\nExport unpresolved results") 
        result_optimised.to_csv(WORKING_DIR+TYPICAL_DAY+FILE_NAME+'_shc.csv', sep=';', header=True, index=False, decimal=',')
        result_maczt.to_csv(WORKING_DIR+TYPICAL_DAY+FILE_NAME+'_ahc.csv', sep=';', header=True, index=False, decimal=',') 

    if PRESOLVE:
        print("\nPresolve SHC results") 
        porygon_presolve.fb_domain_presolve(result_optimised)\
            .to_csv(WORKING_DIR+TYPICAL_DAY+FILE_NAME+'_shc_presolved.csv', sep=';', header=True, index=False, decimal=',')
        print("\nPresolve AHC results") 
        result_maczt = porygon_presolve.fb_domain_presolve(result_maczt, ahc_hubs=[i for i in result_maczt.columns if i.endswith('.ahc')])\
            .to_csv(WORKING_DIR+TYPICAL_DAY+FILE_NAME+'_ahc_presolved.csv', sep=';', header=True, index=False, decimal=',')
    
    print("Overall time:", round((timeit.default_timer() - overall_start)/60, 1),"min.", flush=True)
