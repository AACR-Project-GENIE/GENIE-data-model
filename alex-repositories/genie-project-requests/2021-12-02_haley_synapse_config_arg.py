# Description: login into Synapse with config file in a user specified path.  
# Author: Haley Hunter-Zinck
# Date: 2021-12-02

import argparse
import synapseclient

# command line arguments
parser = argparse.ArgumentParser()
parser.add_argument('--config', dest = 'config', help = 'path to .synapseConfig file')
args = parser.parse_args()

syn = synapseclient.Synapse(configPath = args.config)

syn.login()

