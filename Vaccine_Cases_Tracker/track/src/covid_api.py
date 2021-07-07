import pandas as pd
from requests import get
from urllib.parse import urlencode
import logging

logging.basicConfig(filename="venv/out/output.log",
                            filemode='a',
                            format='FILE REQUEST: %(asctime)s, %(name)s %(levelname)s %(message)s',
                            datefmt='%H:%M:%S',
                            level=logging.DEBUG)

def ensure_latest(df, type_f):
    if type_f != "overview":
        df['date'] = pd.to_datetime(df['date'], format='%Y-%m-%d')
        df = df.sort_values('date').groupby('areaName').tail(1)
        return df
    else:
        return df



def get_covid_data(type_f, metrics_f, format_f, latest_by_f):
    endpoint = "https://api.coronavirus.data.gov.uk/v2/data?"
    type = [("areaType", type_f)]
    mlist = []
    for metric in metrics_f:
        mlist.append(("metric", metric))
    if latest_by_f:
        query = [("format", format_f), ("latest_by", latest_by_f)]
    else:
        query = [("format", format_f)]
    url = endpoint + str(urlencode(type+mlist+query))
    try:
        response = pd.read_csv(url)
    except Exception:
        logging.exception("Detail: ")
        logging.info("Attempting Status Code detection:")
        response = get(url, timeout=10)
        if response.status_code >= 400:
            logging.error(f"Request failed: {response.text}")
        else:
            logging.info("Request succeeded")

        response = pd.DataFrame()
    if latest_by_f:
        return ensure_latest(response, type_f)
    else:
        return response







