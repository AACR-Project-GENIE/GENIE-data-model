"""
Description: validate python functions to extract drug NCIT code to drug name mappings.
Author: Haley Hunter-Zinck
Date: November 1, 2021
genisp command: python -m geniesp --staging NSCLC ~/cbioportal 1.1-consortium
"""

import synapseclient
import pandas as pd

syn = synapseclient.Synapse()
syn.login()

def _get_synid_dd(syn, cohort, synid_table_prissmm = "syn22684834"):
    """Get Synapse ID of the most current PRISSMM non-PHI data dictionary for the BPC cohort."""

    query = 'SELECT id FROM ' + synid_table_prissmm + ' WHERE cohort = \'' + cohort + '\' ORDER BY name DESC LIMIT 1'
    query_results = syn.tableQuery(query)
    
    synid_folder_prissmm = query_results.asDataFrame()['id'][0]
    
    synid_prissmm_children = syn.getChildren(synid_folder_prissmm)
    
    for child in synid_prissmm_children:
        if child['name'] == "Data Dictionary non-PHI":
            return child['id'] 
    return None

def get_drug_mapping(syn, cohort, synid_file_grs = 'syn24184523'):
    """
    Get a mapping between drug short names and NCIT code from BPC data dictionary
    and BPC global response set for a given BPC cohort.
    
    Returns:
      dictionary: map where keys are BPC drug short names and value is the 
        corresponding NCIT drug code
    """
    
    mapping = {}
    var_names = []

    synid_file_dd = _get_synid_dd(syn, cohort)

    dd = pd.read_csv(syn.get(synid_file_dd).path, encoding = 'unicode_escape')
    grs = pd.read_csv(syn.get(synid_file_grs).path, encoding = 'unicode_escape')
    grs.columns = ['Variable / Field Name', 'Choices, Calculations, OR Slider Labels']

    for i in ['1','2','3','4','5']:
        var_names.append('drugs_drug_' + i)
        var_names.append('drugs_drug_oth' + i)

    for obj in dd, grs:

        for var_name in var_names:
            
            if var_name in obj['Variable / Field Name'].unique():
                choice_str = obj[obj['Variable / Field Name'] == var_name]['Choices, Calculations, OR Slider Labels'].values[0]
                choice_str = choice_str.replace('"', '')
            
                for pair in choice_str.split('|'):
                    if (pair.strip() != ""): 
                        code = pair.split(',')[0].strip()
                        value = pair.split(',')[1].strip()
                        label = value.split('(')[0].strip()
                        mapping[label] = code
    return(mapping)

def get_regimen_abbr(regimen, mapping):
    """ 
    Given a BPC regimen and mapping between drug names and NCIT codes,
    return the regimen abbreviation consisting of NCIT codes
    """
    abbr = ''
    drugs = regimen.split(',')
    for drug in drugs:
        if drug == drugs[0]:
            abbr = mapping[drug.strip()]
        else:
            abbr = abbr + '_' + mapping[drug.strip()]
    return(abbr)

# parameters
cohort = 'Prostate'
abbrs = []
top_x_regimens=20

for cohort in 'BrCa', 'CRC', 'NSCLC', 'Prostate', 'PANC':
    abbrs = []
    # get regimen abbreviations
    mapping = get_drug_mapping(syn, cohort)
    # mapping of drug names to NCIT codes
    regimen_synid = "syn22296818"
    regimens_to_exclude = ["Investigational Drug"]
    regimen_ent = syn.get(regimen_synid)
    regimendf = pd.read_csv(regimen_ent.path)
    # Get only NSCLC cohort
    regimendf = regimendf[regimendf['cohort'] == cohort]
    # Use redcap_ca_index == Yes
    regimendf = regimendf[regimendf['redcap_ca_index'] == "Yes"]
    # Exclude regimens
    regimendf = regimendf[~regimendf['regimen_drugs'].isin(regimens_to_exclude)]
    regimendf = regimendf[
        ~regimendf['regimen_drugs'].str.contains("Investigational Drug")
    ]
    # Exclude all regimens with "Other"
    regimendf = regimendf[~regimendf['regimen_drugs'].str.contains("Other")]
    # sort file by regimen_number and drop rest of duplicates
    # (not all duplicates), if duplicated keep the first regimen
    regimendf.sort_values('regimen_number', inplace=True)
    regimendf.drop_duplicates(["record_id", "regimen_drugs"], inplace=True)
    
    count_of_regimens = regimendf['regimen_drugs'].value_counts()
    # Obtain top X number of regimens
    to_include_regimens = count_of_regimens[:top_x_regimens].index.tolist()
    
    subset_regimendf = regimendf[
        regimendf['regimen_drugs'].isin(to_include_regimens)
    ]
    regimen_groups = subset_regimendf.groupby("regimen_drugs")
    new_regimen_info = pd.DataFrame()
    # Create regimen clinical headers
    final_regimendf = pd.DataFrame()
    for regimen, df in regimen_groups:
        # Create regimen drug abbreviations
        abbrs.append(get_regimen_abbr(regimen, mapping))
        
        #print(regimen)
        #print(get_regimen_abbr(regimen, mapping))
    
    print(cohort)
    print(abbrs)

