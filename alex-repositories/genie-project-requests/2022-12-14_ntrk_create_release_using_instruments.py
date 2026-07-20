# !/usr/bin/python
import argparse
import logging
from pydoc import describe
import sys
import os
import re

import pandas
import synapseclient

from synapseclient import File, Activity

def get_csv_file_by_id(syn, file_synid):
    """Download csv file from Synapse
    
    Args:
        syn (Object): Synapse credential
        file_synid (String): Synapse ID of a csv file
    
    Returns:
        Dataframe: data
    """
    data = pandas.read_csv(syn.get(file_synid).path,dtype=str,low_memory=False)
    return(data)

def create_file_obj(file_path, file_name, parent_id):
    """Create Synapse File Object
    
    Args:
        file_path (String): Local file path
        file_name (String): File name on Synapse
        parent_id (String): Synapse ID of folder or project for the file 
    
    Returns:
        Synapse File Object
    """   
    file_obj = File(file_path, name=file_name, parent=parent_id)
    return(file_obj)

def create_act_obj(used, executed, description):
    """Create Synapse Activity Object
    
    Args:
        used (list): List of Synapse Entities and links that were used
        executed (String): Link of the executed code
        description (String): text description
    
    Returns:
        Synapse Activity Object
    """    
    act_obj = Activity(name=description)
    act_obj.used(used)
    act_obj.executed(executed)
    return(act_obj)

def create_synapse_obj(file_path, file_name, args, description):
    """Upload file to Synapse
    
    Args:
        file_path (String): local path of the file
        file_name (String): Name of the file in Synapse
        args (list): List of the arguments passed in
        description (String): text description of the activity
       
    Returns:
        List of Objects 
    """
    file_obj = create_file_obj(file_path, file_name, args.output)
    used = [args.input, args.file]
    executed = "https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-12-14_ntrk_create_release_using_instruments.py"
    act_obj = create_act_obj(used, executed, description)
    return([file_obj, act_obj])

def upload_to_synapse(syn, file_obj, act_obj):
    """Upload file to Synapse
    
    Args:    
        syn (Object): Synapse credential
        file_obj (Object): Synapse Object of the file
        act_obj (Object): Synapse Object of the activity for the file
    
    Returns:
        Synapse Entity   
    """
    file_ent = syn.store(file_obj, activity=act_obj)
    return(file_ent)

def check_empty_row(row, cols_to_skip):
    """
    Check if the row of data is empty with given columns to skip
    """
    return row.drop(cols_to_skip).isnull().all()


def split_data_by_dd(label_data, data_dict, logger):
    """ Split labelled data according to the instruments in Data dictionary
    
    Args:
        label_data (Dataframe)
        data_dict (Dataframe)
      
    Returns:
        Dictinary of Dataframe: instrument as key; data as value
    """
    label_data_by_instrument = dict()
    label_data_cols = label_data.columns
    default_cols = ['record_id','redcap_repeat_instance']
    data_dict.rename(columns={'Variable / Field Name':'variable','Form Name':'instrument'},inplace=True)
    data_dict = data_dict[['variable', 'instrument']]
    data_dict.drop(data_dict.loc[data_dict['variable']=='record_id'].index, inplace=True)
    instrument_groups = data_dict.groupby('instrument')
    # group labelled data into instrument group
    for instrument, df in instrument_groups:
        logger.info(f'Searching column names for instument {instrument}...')
        col_list = list()
        variable_list = list(df.variable)
        for col in label_data_cols:
            if col in variable_list or re.sub(r"___\d+","",col) in variable_list:
                col_list.append(col)
        if col_list:
            temp_data = label_data[default_cols+col_list]
            # drop inrelevent rows
            rows_to_drop = temp_data.index[temp_data.apply(lambda row: check_empty_row(row,default_cols),axis=1)]
            temp_data.drop(index=rows_to_drop,inplace=True)
            # drop redcap_repeat_instance if empty - it is a non-repeating form
            check_repeat_df = temp_data[['redcap_repeat_instance']]
            if check_repeat_df.dropna().empty:
                temp_data.drop(columns=['redcap_repeat_instance'], inplace=True)
            label_data_by_instrument[instrument] = temp_data
    return(label_data_by_instrument)

def setup_custom_logger(name):
    """Set up customer logger
    Args:
        name (String): Name of the logger 
    
    Returns:
       logger
    """
    formatter = logging.Formatter(fmt='%(asctime)s %(levelname)-8s %(message)s',
                                  datefmt='%Y-%m-%d %H:%M:%S')
    handler = logging.FileHandler('log.txt', mode='w')
    handler.setFormatter(formatter)
    screen_handler = logging.StreamHandler(stream=sys.stdout)
    screen_handler.setFormatter(formatter)
    logger = logging.getLogger(name)
    logger.setLevel(logging.DEBUG)
    logger.addHandler(handler)
    logger.addHandler(screen_handler)
    return(logger)

def synapse_login(synapse_config):
    """Log into Synapse
    Args:
        synapse_config (String): File path to the Synapse config file
        
    Returns:
        Synapse object
    """
    try:
        syn = synapseclient.login(silent=True)
    except Exception:
        syn = synapseclient.Synapse(configPath=synapse_config, silent=True)
        syn.login()
    return(syn)

def main():
    parser = argparse.ArgumentParser(
        description='Split REDCap labelled data into multiple files according to the instruments')
    parser.add_argument(
        "-i", "--input",
        required=True,
        help="Synapse ID of the REDCap labelled data file"
    )
    parser.add_argument(
        "-o", "--output",
        help="Synapse ID of the output folder"
    )
    parser.add_argument(
        "-f", "--file",
        required=True,
        help="Synapse ID of the REDCap Data Dictionary"
    )
    parser.add_argument(
        "-c", "--synapse_config",
        default=synapseclient.client.CONFIG_FILE,
        help="Synapse credentials file"
    )
    parser.add_argument(
        "-d", "--dry_run",
        action="store_true",
        help="dry run flag"
    )

    args = parser.parse_args()
    input_id = args.input
    dd_id = args.file
    dry_run = args.dry_run
    #login to synapse
    syn = synapse_login(args.synapse_config)

    #create logger
    logger_name = "testing" if dry_run else "production"
    logger = setup_custom_logger(logger_name)
    
    logger.info('Download the labelled data...')
    label_data = get_csv_file_by_id(syn, input_id)
    
    logger.info('Download data dictionary...')
    data_dict_file = get_csv_file_by_id(syn, dd_id)
    
    logger.info('Split the labelled data by data dictioanry instruments...')
    df_dict = split_data_by_dd(label_data, data_dict_file, logger)
    
    logger.info('Write to local directory...')
    for instrument in df_dict:
        file_name = instrument+".csv"
        file_path = os.path.join(os.getcwd(),file_name)
        df_dict[instrument].to_csv(file_path, index=False)
        if not dry_run:
            logger.info('Upload %s to Synapse...',file_name)
            description = "Split REDCap labelled data by instruments"
            file_obj, act_obj = create_synapse_obj(file_path, file_name, args, description)
            upload_to_synapse(syn, file_obj, act_obj)
            os.remove(file_path)

if __name__ == "__main__":
    main()
