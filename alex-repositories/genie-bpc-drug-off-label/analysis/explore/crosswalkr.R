install.packages('crosswalkr')
library(crosswalkr)
library(dplyr)
library(labelled)
library(haven)

df <- data.frame(state = c('Kentucky','Tennessee','Virginia'),
                 fips = c(21,47,51),
                 region = c('South','South','South'))
df


cw <- data.frame(old_name = c('state','fips'),
                 new_name = c('stname','stfips'),
                 label = c('Full state name', 'FIPS code'))
cw

df1 <- renamefrom(
  .data = df, 
  cw_file = cw, 
  raw = old_name, 
  clean = new_name
  # label is optional:
  # label = label
)
df1

df2 <- renamefrom(
  df, 
  cw_file = cw, 
  raw = old_name, 
  clean = new_name, 
  name_label = TRUE # uses the old names as labels
)

df3 <- renamefrom(
  df, 
  cw_file = cw, 
  raw = old_name, 
  clean = new_name, 
  drop_extra = FALSE # keeps the unmatched names in the data (region)
)
df3 


df <- data.frame(state = c('Kentucky','Tennessee','Virginia'),
                 stfips = c(21,47,51),
                 cenregnm = c('South','South','South'))
df

cw <- get(data(stcrosswalk))
cw

# Creates a new column:
df %<>%
  as_tibble(.) %>% 
  mutate(
    state2 = encodefrom(
      df, 
      var = state, 
      cw_file = cw, 
      raw = stname, 
      clean = stfips,
      label = stabbr
    )
  )
df

## convert to tbl_df
df <- tibble::as_tibble(df)
df$state3 <- encodefrom(df, var = state, cw_file = cw, raw = stname, clean = stfips, label = stabbr)


