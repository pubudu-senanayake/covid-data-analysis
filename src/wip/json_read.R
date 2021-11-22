library(httr)
library(jsonlite)

## Analyze cases with strigency data ---------------------------------------


# Pull data from stringency index API -------------------------------------

get_test <- GET(url = "https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range/2021-02-10/2021-06-15")

unwrap <- fromJSON("https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range/2021-02-10/2021-06-15")

get_test_txt <- content(get_test, "text")

get_test_json <- fromJSON(get_test_txt, flatten = TRUE)

get_test_df <- as.data.frame(get_test_json$data$`2021-02-10`)

get_test_json[["data"]][["2021-02-10"]][["GBR"]][["confirmed"]]
