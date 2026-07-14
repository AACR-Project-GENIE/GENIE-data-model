"""
NTRK - Use list provided by AACR
to create release files
"""
import os
import pandas
import synapseclient

syn = synapseclient.login()

sor = pandas.read_excel(syn.get('syn49769483').path, sheet_name="Sheet1")
sor.columns = sor.columns.str.lower()
#sor = sor.filter(regex='^variables')
sor.rename(columns={'variables to be included in upload': 'variable'},
           inplace=True)
sor.variable = sor.variable.str.strip()
#sor_filtered = sor.drop_duplicates('variable',inplace=True)
#sor_filtered = sor[['variable','release']]
#sor_filtered = sor_filtered.applymap(lambda s: s.lower() if type(s) == str else s)
#yes_value_list = ["yes"]
#sor_filtered.query('release in @yes_value_list',inplace=True)
var_to_release = list(set(sor['variable']))
var_to_release = var_to_release + ['redcap_repeat_instance']
temp_retraction = syn.tableQuery("SELECT genie_id, site FROM syn49097276 WHERE project='NTRK'").asDataFrame()

info = {"PROV":"syn49090867", "UCSF":"syn49090888","VICC":"syn49090681","DFCI":"syn49770249","MSK":"syn50913015", "CHOP":"syn50941169"}
out_folder_id = {"PROV":"syn49091082","UCSF":"syn49089325", "VICC":"syn49091348","DFCI":"syn49688680", "MSK":"syn50913052", "CHOP":"syn49377214"}
staging_id = {"PROV":"syn50118024","UCSF":"syn50118020", "VICC":"syn50118032","DFCI":"syn50117991","MSK":"syn50913017", "CHOP":"syn50118000"}

for site in info:
    print("Create release file for", site)
    print("Download labelled data...")
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
            synapseclient.File(out_fname, parentId=staging_id[site]),
            executed="https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-12-16_ntrk_filter_by_list.py",
            used=["syn49769483", info[site], 'syn49097276']
    )
    os.remove(out_fname)
