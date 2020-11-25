import os
import glob
import pandas as pd
os.chdir("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness")

fünfzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/happy_2015.csv")
sechzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/happy_2016.csv")
siebzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/happy_2017.csv")
achtzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/happy_2018.csv")
neunzehn = pd.read_csv("/Users/urs/Festplatte/03_HSLU/00_exercises/dasb_team08/data/Happiness/happy_2019.csv")

fünfzehn = fünfzehn.drop(['Region', 'Happiness Rank', 'Standard Error', 'Economy (GDP per Capita)', 'Family', 'Health (Life Expectancy)',
                       'Trust (Government Corruption)', 'Dystopia Residual', 'Freedom', 'Generosity'], axis=1)
sechzehn = sechzehn.drop(['Region', 'Happiness Rank', 'Lower Confidence Interval', 'Upper Confidence Interval', 'Economy (GDP per Capita)', 'Family', 'Health (Life Expectancy)',
                       'Freedom', 'Trust (Government Corruption)', 'Generosity', 'Dystopia Residual'], axis=1)
siebzehn = siebzehn.drop(['Happiness.Rank', 'Whisker.high', 'Whisker.low', 'Economy..GDP.per.Capita.', 'Family', 'Health..Life.Expectancy.',
                       'Freedom', 'Generosity', 'Trust..Government.Corruption.', 'Dystopia.Residual'], axis=1)
achtzehn = achtzehn.drop(['Overall rank', 'GDP per capita', 'Social support', 'Healthy life expectancy', 'Freedom to make life choices',
                       'Perceptions of corruption', 'Generosity'], axis=1)
neunzehn = neunzehn.drop(['Overall rank', 'GDP per capita', 'Social support', 'Healthy life expectancy', 'Freedom to make life choices', 'Generosity',
                       'Perceptions of corruption'], axis=1)

fünfzehn1 = pd.DataFrame(fünfzehn)
sechzehn1 = pd.DataFrame(sechzehn)
siebzehn1 = pd.DataFrame(siebzehn)
achtzehn1 = pd.DataFrame(achtzehn)
neunzehn1 = pd.DataFrame(neunzehn)

fünfzehn2 = fünfzehn1.sort_values(by=['Country'])
sechzehn2 = sechzehn1.sort_values(by=['Country'])
siebzehn2 = siebzehn1.sort_values(by=['Country'])
achtzehn2 = achtzehn1.sort_values(by=['Country or region'])
neunzehn2 = neunzehn1.sort_values(by=['Country or region'])












