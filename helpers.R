# TODO why does it not find the files here?
#applications <- read.csv('data/AsylgesuchePerNation1986.csv', sep=";", encoding="UTF-8")
#countries <- geojson_read("data/countries.geo.json", what = "sp")

head(applications)

append_properties <- function(countries, applications) {
  for(element in rownames(applications)) 
    if (applications$X.U.FEFF.Code3 == countries$sov_a3) {
      # print(countries$sov_a3)
      # TODO could anyone make this work: 
      # for each country in applications, each year andno. of applications should be added to countries-json as a nested property
    }
}

# run the function
append_properties(countries, applications)
