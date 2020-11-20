
import csv

import pandas as pd
df = pd.read_csv('data/AsylgesuchePerNation1986_noCode3.csv', sep=';', encoding='utf-8', header=None, index_col=False)
#print(df.head())
df_transposed = df.transpose()
#print(df_transposed.head())
df_transposed[0] = df_transposed[0].astype(str)+"-12-31 23:00:00"

#print(df_transposed.head())
df_transposed.to_csv('data/appli_transposed2.csv', index=False, sep=';', encoding='utf-8')
