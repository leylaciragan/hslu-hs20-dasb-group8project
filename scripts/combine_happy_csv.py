import os
import glob
import pandas as pd



os.chdir("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness")

pilot = pd.read_excel("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/Correlation/happiness_correlation_excel_2.xlsx")

pilot1 = pd.DataFrame(pilot)


pilot1.to_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/CSV_File_65.csv")





