import pandas as pd
from covid_api import *
import logging

####################
## Logging config ##
####################


logging.basicConfig(filename="../out/output.log",
                            filemode='a',
                            format='COVID SCRAPE (World): %(asctime)s, %(name)s %(levelname)s %(message)s',
                            datefmt='%H:%M:%S',
                            level=logging.DEBUG)

def adjust_for_pop(df):
    def adj(name, val):
        pops = {
            'Wales' : 2450353,
            'Scotland' : 4315637,
            'Northern Ireland' : 1408228,
            'England' : 43004640
        }
        return  (val / pops[name]) * 100

    if df["areaType"].iloc[0] == "nation":
        not_inc =["areaCode", "areaName", "areaType", "date"]
        cols_to_adj = df.columns.symmetric_difference(not_inc)
        for col in cols_to_adj:
            newname = col + "_adj"
            df[newname] = df.apply(lambda x: adj(x['areaName'], x[col]), axis=1)

    return  df






if __name__ == "__main__":
    dir = "venv"
    # load files
    logging.info("Running ...")
    print("Running Dashboard scrape ...")
    latlong = pd.read_csv(f"{dir}/geospatial/latlong_lta.csv")

    #Set "global" variables
    format = "csv"
    area = "ltla"


    ## Get Case data
    logging.info("Collecting case data")
    metrics = ["newCasesBySpecimenDateRollingRate", "newCasesBySpecimenDateChangePercentage"]
    response = get_covid_data(area, metrics, format, "newCasesBySpecimenDateRollingRate")
    if not response.empty:
        covid_out = response.join(latlong.set_index('lad19nm'), on='areaName')
        covid_out.to_csv(f"{dir}/out/casedata_uk_lta.csv", index=False)




    ##Get Vaccination data (lta level)
    logging.info("Collecting Vaccine data (lta)")
    metrics = ["cumPeopleVaccinatedCompleteByPublishDate", "cumPeopleVaccinatedFirstDoseByPublishDate", "cumVaccinationFirstDoseUptakeByPublishDatePercentage", "cumVaccinationSecondDoseUptakeByPublishDatePercentage", "cumVaccinesGivenByPublishDate"]
    response = get_covid_data(area, metrics, format, "cumPeopleVaccinatedFirstDoseByPublishDate")
    if not response.empty:
        covid_out = response.join(latlong.set_index('lad19nm'), on='areaName')
        covid_out.to_csv(f"{dir}/out/vaccine_uk_lta.csv", index=False)

    ## Get Hospitalisation data (UK/Nation only)
    area = "overview"
    logging.info("Collecting hopsitalisation data")
    metrics = ["newAdmissions", "newAdmissionsChangePercentage", "newAdmissionsRollingRate"]
    response = get_covid_data(area, metrics, format, "newAdmissions")
    if not response.empty:
        covid_out = adjust_for_pop(response)
        covid_out.to_csv(f"{dir}/out/hospdata_uk_nat.csv", index=False)

    ##Get Vaccination data (whole UK )
    logging.info("Collecting Vaccine data (UK)")
    area = "overview"
    metrics = ["cumPeopleVaccinatedCompleteByPublishDate", "cumPeopleVaccinatedFirstDoseByPublishDate", "cumVaccinationFirstDoseUptakeByPublishDatePercentage", "cumVaccinationSecondDoseUptakeByPublishDatePercentage", "cumVaccinesGivenByPublishDate"]
    response = get_covid_data(area, metrics, format, "cumPeopleVaccinatedFirstDoseByPublishDate")
    if not response.empty:
        covid_out = adjust_for_pop(response)
        covid_out.to_csv(f"{dir}/out/vaccine_uk_ov.csv", index=False)

    logging.info("Collecting Vaccine data (UK)")
    area = "nation"
    metrics = ["cumPeopleVaccinatedCompleteByPublishDate", "cumPeopleVaccinatedFirstDoseByPublishDate", "cumVaccinationFirstDoseUptakeByPublishDatePercentage", "cumVaccinationSecondDoseUptakeByPublishDatePercentage", "cumVaccinesGivenByPublishDate"]
    response = get_covid_data(area, metrics, format, False)
    if not response.empty:
        covid_out = adjust_for_pop(response)
        covid_out.to_csv(f"{dir}/out/vaccine_uk_nat.csv", index=False)

    logging.info("Completed ...")
    print("Completed dashboard scrape ...")




