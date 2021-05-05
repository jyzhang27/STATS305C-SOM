import os
import pandas as pd

# global variable names
src_data_dir = "../../deps/transfers/data"
dest_data_dir = "../"
dest_data_name = 'all_transfer_data.csv'
dest_data_no_nan_name = 'all_transfer_data_no_nan.csv'

# read all dataframes from years 2010 and above
# drop transfer_period since it is not common to all data files
df_out = pd.DataFrame()
for subdir, dirs, files in os.walk(src_data_dir):
    for f in files:
        if int(subdir[-4:]) >= 2010:
            f_path = os.path.join(subdir, f)
            df_new = pd.read_csv(f_path)
            if 'transfer_period' in df_new.columns:
                df_new.drop('transfer_period', axis=1, inplace=True)
            df_out = pd.concat([df_out, df_new])

# save raw combined data
df_out.reset_index(drop=True, inplace=True)
df_out.to_csv(os.path.join(dest_data_dir, dest_data_name), index=False)

# save raw combined data dropped NaNs
df_out.dropna(inplace=True)
df_out.to_csv(os.path.join(dest_data_dir, dest_data_no_nan_name), index=False)
