
import csv

import pandas as pd
df = pd.read_csv('scripts/data_Happiness_combined_cleaned.csv', sep=',', encoding='utf-8', header=None, index_col=False)
#print(df.head())

df_transposed = df.transpose()
df_transposed[0] = df_transposed[0].astype(int)
print(df_transposed.head())
df_transposed[0] = df_transposed[0].astype(str) + "-12-31 23:00:00"

print(df_transposed.head())
df_transposed.to_csv('data/happy_transposed2.csv', index=False, sep=';', encoding='utf-8')
