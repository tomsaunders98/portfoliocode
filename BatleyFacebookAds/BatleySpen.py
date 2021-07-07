from fb_ads_library_api import FbAdsLibraryTraversal
import pandas as pd
import numpy as np
import json
import urllib.parse as urlparse
from urllib.parse import parse_qs




if __name__ == "__main__":
    #Set some "global" variables
    access_token = ""
    country = "GB"
    fields = ["ad_delivery_start_time", "ad_delivery_stop_time", "ad_snapshot_url", "demographic_distribution","region_distribution","spend", "funding_entity", "page_name", "impressions", "ad_creative_body", "id"]
    search_term = "."


    api = FbAdsLibraryTraversal(
        access_token, ",".join(fields), search_term, country, after_date="2021-03-01"
    )
    t = 0
    generator_ad_archives = api.generate_ad_archives()
    add_info = pd.DataFrame(columns = ["date", "end", "funder", "person", "spend", "impact", "url", "demos", "elec", "id", "l_spend", "u_spend", "u_impact", "l_impact"])
    by_elecs = ["batley", "amersham", "williams", "mortimer", "fleet", "hartelpool", "natasa", "stephenson", "leadbeater", ]
    with open("bspen.json", "w+") as file:
        file.write("[")
        for ads in generator_ad_archives:
            for ad in ads:
                if "funding_entity" in ad:
                    if ad["funding_entity"] == "The Labour Party" or ad["funding_entity"] == "The Conservative Party" or ad["funding_entity"] == "Jill Mortimer":
                        for elec in by_elecs:
                            if "ad_creative_body" not in ad:
                                ad["ad_creative_body"] = "none"
                            if (elec in ad["page_name"].lower() or elec in ad["ad_creative_body"].lower() ) and ad["id"] not in add_info["id"].values:
                                if ad["page_name"] != "Tewkesbury Labour" or ad["page_name"] != "Samuel P. S. Williams": #edge cases
                                    spend = np.mean([int(ad["spend"]["lower_bound"]), int(ad["spend"]["upper_bound"])])
                                    imp =  np.mean([int(ad["impressions"]["lower_bound"]), int(ad["impressions"]["upper_bound"])])
                                    if "ad_delivery_stop_time" not in ad:
                                        ad["ad_delivery_stop_time"] = str(np.nan)


                                    detail = {
                                            "date": ad["ad_delivery_start_time"],
                                            "end": ad["ad_delivery_stop_time"],
                                            "funder": ad["funding_entity"],
                                            "person": ad["page_name"],
                                            "l_spend" : ad["spend"]["lower_bound"],
                                            "m_spend": str(spend),
                                            "u_spend" : ad["spend"]["upper_bound"],
                                            "impact": str(imp),
                                            "u_impact" :ad["impressions"]["upper_bound"],
                                            "l_impact" : ad["impressions"]["lower_bound"],
                                            "url": ad["ad_snapshot_url"],
                                            "demos": ad["demographic_distribution"],
                                            "elec" : elec,
                                            "id" : ad["id"]

                                    }
                                    file.write(json.dumps(detail))
                                    file.write(",\n")
                                    add_info = add_info.append(detail, ignore_index=True)
            t = t + len(ads)
            if t%1000 == 0:
                print(f"Scraped {t} ads: {str(ad['ad_delivery_start_time'])}")
        file.write("]")
    add_info.to_csv("bspen.csv", index=False)
