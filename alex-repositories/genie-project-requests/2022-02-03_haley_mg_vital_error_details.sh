# Description: create all detail error logs
# Author: Haley Hunter-Zinck
# Date: 2022-02-03

# run locally
Rscript 2022-02-03_mg_vital_error_details.R -i syn20446601 -c CHOP -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn18693147 -c COLU -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn12121308 -c CRUK -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn21265748 -c DFCI -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn17023617 -c DUKE -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn8514264 -c GRCC -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn7821249 -c JHU -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn7224475 -c MSK -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn7245149 -c NKI -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn26986939 -c PROV -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn17017899 -c SCI -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn13310072 -c UCHI -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn26718092 -c UCSF -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn7229580 -c UHN -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn16969648 -c VHIO -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn7395712 -c VICC -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn23679302 -c WAKE -v
Rscript 2022-02-03_mg_vital_error_details.R -i syn12979114 -c YALE -v

# check errors 
for file in $(ls *errors_details.csv)
{
  echo START $file '----------------------------'
  echo '------'
  cut -d ',' -f 3,4,5,8 $file | sort | uniq
  echo END $file '----------------------------'
  echo
}
for file in $(ls *errors_details.csv)
{
  wc -l $file
}
for file in $(ls *errors_details.csv)
{
  echo START $file '----------------------------'
  head -n 2 $file | tail -n 1 | cut -f 2 -d ','
  echo END $file '----------------------------'
  echo
}

# load to synapse
Rscript 2022-02-03_mg_vital_error_details.R -i syn20446601 -c CHOP -v -o syn11703496
Rscript 2022-02-03_mg_vital_error_details.R -i syn18693147 -c COLU -v -o syn11703487
Rscript 2022-02-03_mg_vital_error_details.R -i syn12121308 -c CRUK -v -o syn11727016
Rscript 2022-02-03_mg_vital_error_details.R -i syn21265748 -c DFCI -v -o syn5016917
Rscript 2022-02-03_mg_vital_error_details.R -i syn17023617 -c DUKE -v -o syn13363328
Rscript 2022-02-03_mg_vital_error_details.R -i syn8514264 -c GRCC -v -o syn5016904
Rscript 2022-02-03_mg_vital_error_details.R -i syn7821249 -c JHU -v -o syn5016900
Rscript 2022-02-03_mg_vital_error_details.R -i syn7224475 -c MSK -v -o syn5548760
Rscript 2022-02-03_mg_vital_error_details.R -i syn7245149 -c NKI -v -o syn5016919
Rscript 2022-02-03_mg_vital_error_details.R -i syn26986939 -c PROV -v -o syn11703431
Rscript 2022-02-03_mg_vital_error_details.R -i syn17017899 -c SCI -v -o syn11703426
Rscript 2022-02-03_mg_vital_error_details.R -i syn13310072 -c UCHI -v -o syn11703501
Rscript 2022-02-03_mg_vital_error_details.R -i syn26718092 -c UCSF -v -o syn11703469
Rscript 2022-02-03_mg_vital_error_details.R -i syn7229580 -c UHN -v -o 	syn5016894
Rscript 2022-02-03_mg_vital_error_details.R -i syn16969648 -c VHIO -v -o syn11727025
Rscript 2022-02-03_mg_vital_error_details.R -i syn7395712 -c VICC -v -o syn5016921
Rscript 2022-02-03_mg_vital_error_details.R -i syn23679302 -c WAKE -v -o syn11703505
Rscript 2022-02-03_mg_vital_error_details.R -i syn12979114 -c YALE -v -o syn11703407
