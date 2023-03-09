load(here::here("datos", "election_2008_2016.RData"))

data <- as.data.frame(X)
colnames(data) <- c(
  "percent_population_change", 
  "percent_people_over_65", 
  "percent_african_american", 
  "percent_hispanic", 
  "percent_hs_grad", 
  "percent_bachelor",
  "rate_homeownership", 
  "median_house_value", 
  "median_hh_income", 
  "percent_poverty"
)
data$percent_vote_difference <- Y

readr::write_csv(data, here::here("datos", "rep_2012_2016.csv"))


# population_percent_change: Population, percent change - April 1, 2010 to July 1, 2014
# percent_people_over_65:    Persons 65 years and over, percent, 2014
# percent_african_american:  Black or African American alone, percent, 2014
# percent_hispanic:          Hispanic or Latino, percent, 2014
# percent_hs_grad:           High school graduate or higher, percent of persons age 25+, 2009-2013
# percent_bachelor:          Bachelor's degree or higher, percent of persons age 25+, 2009-2013
# rate_homeownership:        Homeownership rate, 2009-2013
# median_house_value:        Median value of owner-occupied housing units, 2009-2013
# median_hh_income:          Median household income, 2009-2013
# percent_poverty:           Persons below poverty level, percent, 2009-2013
# percent_vote_difference:   Vote different for republican candidate, 2012-2016
