"""
NTRK - Use BPC NSCLC/CRC/BrCa scope of release
to create release files
"""
import os
import pandas
import synapseclient

syn = synapseclient.login()

sor = pandas.read_excel(syn.get('syn22294851').path, sheet_name="Data Dictionary")
sor.columns = sor.columns.str.lower()
sor = sor.filter(regex='^varname|^type|shared')
sor.rename(columns={'varname': 'variable', 
                     'type': 'dataType'}, 
           inplace=True)
sor.dataType.replace(['project genie tier 1 data', 'tumor registry'],'curated',inplace=True)
sor.variable = sor.variable.str.strip()
sor.drop_duplicates('variable',inplace=True)
derived_index = sor[sor['dataType']=="Derived"].index
sor.drop(derived_index, inplace=True)
sor_filtered = sor[['variable','shared for nsclc 2.0-public release','shared for crc v2.0 public release','shared for brca public release']]
sor_filtered.rename(columns={'shared for nsclc 2.0-public release': 'nsclc', 
                             'shared for crc v2.0 public release': 'crc',
                             'shared for brca public release': 'brca'}, 
                    inplace=True)
sor_filtered = sor_filtered.applymap(lambda s: s.lower() if type(s) == str else s)
yes_value_list = ["yes","always","index cancer only","non-index cancer only"]
sor_filtered.query('nsclc in @yes_value_list or crc in @yes_value_list or brca in @yes_value_list',inplace=True)
var_to_release = list(set(sor_filtered['variable']))
var_to_release = var_to_release + ['redcap_repeat_instrument','redcap_repeat_instance']
temp_retraction = syn.tableQuery("SELECT genie_id, site FROM syn49097276 WHERE project='NTRK'").asDataFrame()

info = {"PROV":"syn49090867", "UCSF":"syn49090888","VICC":"syn49090681"}
out_folder_id = "syn48376328"

for site in info:
    print("Create release file for", site)
    print("Download laballed data...")
    syn_obj = syn.get(info[site])
    out_fname = syn_obj.name+"_release.csv"
    df = pandas.read_csv(syn_obj.path,dtype=str)
    df_release = df[df.columns[df.columns.isin(var_to_release)]]
    print("Get the list of retracted patients...")
    temp_retraction_site = list(temp_retraction.query("site == @site")['genie_id'])
    if temp_retraction_site:
        df_release.query("record_id not in @temp_retraction_site",inplace=True)
    df_release.to_csv(out_fname, index=False)
    syn.store(
            synapseclient.File(out_fname, parentId=out_folder_id),
            executed="https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-12-08_ntrk_filter_by_sor_create_release.py",
            used=["syn22294851", info[site], 'syn49097276']
    )
    os.remove(out_fname)
