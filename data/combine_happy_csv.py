import os
import glob
import pandas as pd
os.chdir("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness")

fünfzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/2015_sorted.csv")
sechzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/2016_sorted.csv")
siebzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/2017_sorted.csv")
achtzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/2018_sorted.csv")
neunzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/2019_sorted.csv")


fünfzehn1 = pd.DataFrame(fünfzehn)
sechzehn1 = pd.DataFrame(sechzehn)
siebzehn1 = pd.DataFrame(siebzehn)
achtzehn1 = pd.DataFrame(achtzehn)
neunzehn1 = pd.DataFrame(neunzehn)

HSsechzehn = sechzehn1['Happiness Score16']
HSsiebzehn = siebzehn1['Happiness.Score17']
HSachtzehn = achtzehn1['Score18']
HSneunzehn = neunzehn1['Score19']

fünfzehn2 = fünfzehn1.join(HSsechzehn).join(HSsiebzehn).join(HSachtzehn).join(HSneunzehn)


fünfzehn2.to_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/combined.csv")






