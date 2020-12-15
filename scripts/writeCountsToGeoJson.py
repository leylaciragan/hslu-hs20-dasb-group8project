# import packages to read csv file and write json file.
import csv, json

# the file we want to extract information from.
csvFilePath = 'data/AsylgesuchePerNation1986.csv'

# the file we want to write the information from above into.
jsonFilePath = 'sources/data_countries.geo.json'

# create a dictionary that will contain the country code as key, and another dict as value
# the value dict will contain year (key) and no. of applications (value) of the respective country.
asylappli = {}

# read csv
with open(csvFilePath, newline='') as csvFile:
    csvReader = csv.reader(csvFile, delimiter=';')

    # read the first line and save it as our header.
    header = next(csvReader)

    # extract the 3 letter country code from each row.
    for row in csvReader: 
        code = row[0]

        # a dict to hold the year (key) and no. of applications (value).
        yearAndNumberDict = {}

        # walk through each column, extracting year and number, storing them in the dict.
        for i in range(2,len(row)):
            year = header[i]
            value = int(row[i])
            yearAndNumberDict[year] =  value
        
        # now that we have all the years and their values for one row (country) in the yearAndNumberDict:
        # create a new dict entry in the asylappli dict using the country code as key. As value, add the yearAndNumberDict.
        asylappli[code] = yearAndNumberDict
        
        
# now we have all the info we need in the format we want. Ready to write our JSON.
# open geo.json file and add property for each country
with open(jsonFilePath, "r", encoding='utf-8', newline='') as jsonFile:
    data = json.load(jsonFile)
    for feature in data['features']:
        country_code = feature['properties']['sov_a3']
        if country_code in asylappli:
            feature['properties']['asyl_application'] = asylappli[country_code]
            
    with open('countries2.geo.json', 'w') as outfile:
        # indent is added for pretty print. increases file size (factor 3). remove for machine readable version.
        json.dump(data, outfile, indent=4)