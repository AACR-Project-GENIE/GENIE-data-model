"""
Exclude derived variables from NTRK study
1. Take this excel file and download: https://www.synapse.org/#!Synapse:syn22294851.163
   and change into csv: data_dict.csv
2. 
"""
import pandas as pd
import synapseclient

data_dict_df = pd.read_csv("data_dict.csv")

# Exclude derived variable tables
exclude_cols = data_dict_df['VARNAME'][data_dict_df['TYPE'] == 'Derived']

syn = synapseclient.login()
redcap_exports_files = syn.getChildren("syn32026176")

for redcap_export_file in redcap_exports_files:
    if not redcap_export_file['name'].endswith(".xlsx"):
        ent = syn.get(redcap_export_file['id'])
        ent_df = pd.read_csv(ent.path)
        # only keep columns that aren't part of derived variable list
        subset_df = ent_df[ent_df.columns[~ent_df.columns.isin(exclude_cols)]]
        subset_df.to_csv(ent.name, index=False)
        syn.store(
            synapseclient.File(ent.name, parentId="syn26529348"),
            executed="https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-06-23_ntrk_exclude_derived_variables.py",
            used=["syn22294851", ent.id]
        )
