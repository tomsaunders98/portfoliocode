import pandas as pd
import re
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from bs4 import BeautifulSoup
import numpy as np
from datetime import datetime


def conv_perc(x):
    xv = x
    if "%" in str(xv):
        xv = int(re.sub("[^\d+]+", "", xv))/100
        val = xv
    elif str(xv) == "?":
        val = np.nan
    else:
        val = x
    return val


def date_col(x, i):
    try:
        datestring = re.findall("(\d+)", str(x))
        mlist = re.findall("([a-zA-z]+)", x)
        if len(mlist) > 1:
            if i == 1:
                month = mlist[0]
            if i == 2:
                month = mlist[-1]
        if len(mlist) == 1:
            month = mlist[0]
        if i == 1:
            date = datestring[0]
        if i == 2 and len(datestring) == 1:
            date = datestring[0]
        if i == 2 and len(datestring) > 1:
            date = datestring[1]
        ds = str(date) + "_" + month + "_2021"
        ds_f = datetime.strptime(ds, "%d_%b_%Y")
        return(f"{ds_f.day}/{ds_f.month}/{ds_f.year}")
    except:
        return np.nan


def cycle_table(table):
    #Initiate table
    constructeddf = pd.read_html(table.get_attribute("outerHTML"), header=0, skiprows=[1])[0]

    #Create Columns
    constructeddf["Crosstab"] = np.nan
    constructeddf["url"] = np.nan

    #Compile Regex
    ftype = re.compile("((\.pdf)|(\.xlsx))$", re.MULTILINE)
    perc_re = re.compile("(?<=%)(.*?)(?=%)", re.DOTALL)
    split = re.compile("(.+(?=on))|(\d+)")

    #Find Rows
    rows = table.find_elements_by_xpath(".//tr")

    #Other
    rowindex = 0

    #Cycler
    for row in rows:
        cells = row.find_elements_by_xpath(".//td")
        if len(cells) == 0:
            continue
        i = 1
        for cell in cells:
            if (i == 1):
                inner_text = cell.get_attribute("innerHTML")
                soup = BeautifulSoup(inner_text, features="lxml")
                if soup.get_text() == "":
                    break
                url = cell.find_element_by_xpath(".//a").get_attribute("href")
                constructeddf.loc[rowindex, ["url"]] = str(url)
                crosstab = re.search(ftype, url)
                if crosstab != None:
                    constructeddf.loc[rowindex, ["Crosstab"]] = "Crosstab"

            if i == 11: #Expand table
                text = cell.get_attribute("innerHTML")
                soup = BeautifulSoup(text, features="lxml")
                finds = re.findall(perc_re, soup.get_text())
                if len(finds) > 0:
                    for find in finds:
                        vals = re.findall(split, find)
                        party = vals[0][0].strip()
                        perc = vals[1][1].strip()
                        if party in constructeddf.columns:
                            constructeddf.loc[rowindex, [party]] = int(perc)/100
                        else:
                            constructeddf[party] = np.nan
                            constructeddf.loc[rowindex, [party]] = int(perc)/100
            i += 1
        rowindex += 1
    return constructeddf


if __name__ == "__main__":

    print("Scraping polls...")
    #Set Chrome Options
    opts = Options()
    opts.add_argument("--disable-extensions")
    opts.add_argument("--disable-gpu")
    opts.add_argument("--headless")

    #Initiate Chrome
    driver = webdriver.Chrome("/mnt/sda1/chromedriver", options=opts)
    print("Chrome initiated, scraping table")
    # Once Chrome is started, ensure closure on error
    try:
        # Get table
        driver.get("https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_United_Kingdom_general_election")
        table = driver.find_element_by_xpath("//h3[contains(.,'2021')]/..//table//th[contains(.,'Pollster')]/../../..")

        #Cycle
        constructeddf = cycle_table(table)
        driver.quit()

    except Exception as e:
        print(e)
        driver.quit()

    print("Table retrieved")
    #Final Wrangles

    #Get rid of non-rows
    constructeddf = constructeddf[constructeddf['url'].notna()]

    #Convert dateranges
    constructeddf["StartDate"] = constructeddf.apply(lambda x: date_col(x["Datesconducted"], 1), axis=1)
    constructeddf["EndDate"] = constructeddf.apply(lambda x: date_col(x["Datesconducted"], 2), axis=1)

    #Convert percs to ints
    constructeddf = constructeddf.applymap(conv_perc)

    #Cosmetic stuff
    constructeddf = constructeddf.drop(["Datesconducted"], axis=1)
    constructeddf.rename(columns={'Others': 'Minor'}, inplace=True)
    constructeddf = constructeddf[['StartDate', 'EndDate'] + [c for c in constructeddf if c not in ['StartDate', 'EndDate']]]


    #Push polls
    constructeddf.to_csv("polls.csv", index=False)
    print("Polls Built")