import pandas as pd
from datetime import datetime
import logging
from scipy import stats
from scipy import mean

#############################
## Logging global settings ##
#############################

dir = "venv"

logging.basicConfig(filename=f"{dir}/out/output.log",
                            filemode='a',
                            format='COVID SCRAPE (WORLD): %(asctime)s, %(name)s %(levelname)s %(message)s',
                            datefmt='%H:%M:%S',
                            level=logging.DEBUG)



################################################
## Generate recent vaccination data from OWID ##
################################################
def get_vacs(latlong):
    vaccs = pd.read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
    vaccs = vaccs[["date", "location", "people_vaccinated_per_hundred"]]
    vaccs = vaccs[vaccs.people_vaccinated_per_hundred.notnull()]
    vaccs["date"] = pd.to_datetime(vaccs['date'], format='%Y-%m-%d')
    newvac = pd.DataFrame(columns=vaccs.columns)

    countries = vaccs.location.unique()
    countries = [x for x in countries if x in latlong["location"].values]
    for country in countries:
        dates = vaccs.loc[vaccs["location"] == country, "date"].values
        latest = max(dates)

        tvph = vaccs.loc[(vaccs["location"] == country) & (vaccs["date"] == latest), "people_vaccinated_per_hundred"].values[0]
        ls = [latest, country, tvph]
        row = pd.Series(ls, index=newvac.columns)
        newvac = newvac.append(row, ignore_index=True)

    return newvac
#########################################
## Generate recent test data from OWID ##
#########################################

def get_tests(latlong):
    tests = pd.read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")


    tests = tests[["date", "location", "new_cases_per_million"]]
    tests = tests[tests.new_cases_per_million.notnull()]
    tests["date"] = pd.to_datetime(tests['date'], format='%Y-%m-%d')
    newtest = pd.DataFrame(columns=["testdate", "location", "new_cases_per_million", "direction"])

    countries = tests.location.unique()
    countries = [x for x in countries if x  in latlong["location"].values]


    for country in countries:
        dates = tests.loc[tests["location"] == country, "date"].values
        dates.sort()
        #Build Regression
        latest = dates[-7:]
        reg_t = tests.loc[(tests["location"] == country) & (tests["date"].isin(latest)), ["new_cases_per_million", "date"]]
        xv = reg_t["new_cases_per_million"].values
        xd = reg_t["date"].values
        def convert(x, start):
            return (x-start).days

        reg_t["date"] = reg_t.apply(lambda x: convert(x["date"], dates[-7]), axis=1)
        #find gradient
        slope, intercept, r, p, std_err = stats.linregress(reg_t["date"].values, reg_t["new_cases_per_million"].values)
        nc = mean(reg_t["new_cases_per_million"].values)


        # latest data
        #nc = tests.loc[(tests["location"] == country) & (tests["date"] == dates[-1]), "new_cases_per_million",].values[0]



        if slope > 0:
            direction = "up"
        if slope < 0:
            direction = "down"
        if nc == 0:
            direction = "zero"
            nc = 0.1
        newtest = newtest.append({'location': country, 'testdate': dates[-1], 'new_cases_per_million': nc, 'direction': direction}, ignore_index=True)

    return newtest


####################################################################
## Combine vaccinations, tests and tier data to create final data ##
####################################################################

if __name__ == "__main__":
    # Import basic info
    latlong = pd.read_csv(f"{dir}/geospatial/latlong_world.csv")
    redlist = pd.read_csv(f"{dir}/fixed_data_misc/tierdata_world.csv")
    print("Running OWID scrape")
    logging.info("Running ...")

    # get vaccs
    newvac = get_vacs(latlong)


    # get tests
    newtest = get_tests(latlong)

    #join data
    newdata = newtest.join(newvac.set_index('location'), on='location')
    newdata = newdata.join(redlist.set_index('location'), on='location')
    newdata = newdata.join(latlong.set_index('location'), on='location')

    #output
    newdata.to_csv(f"{dir}/out/covid_data_world.csv", index=False)
    logging.info("Completed ...")
    print("Completed OWID scrape")


