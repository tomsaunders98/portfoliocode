import requests, re, json, pandas as pd, numpy as np, sys, logging, os, random, lxml, html5lib, time
from datetime import datetime

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

def installdata(area, number):
    spread = pd.read_csv("data.csv")
    today = datetime.today()
    datestring = f'{today:%d}/{today:%m}/{today.year}'
    spread.loc[spread["date"] == datestring, area] = number
    spread.to_csv("data.csv", index=False)


class Browse:
    def __init__(self, filepath=None):
        self.opts = webdriver.ChromeOptions()
        #self.opts.headless = True
        self.opts.add_argument("--no-sandbox")
        self.filepath = filepath
        if self.filepath == None:
            self.filepath = ""

    def start(self):
        self.browser = webdriver.Chrome(executable_path="driver/chromedriver", options=self.opts)
    def stop(self):
        return self.browser.quit()

    def ClickLink(self, element):
        #element = self.browser.find_element_by_xpath('//a[@href="' + href + '"]')
        return self.browser.execute_script("arguments[0].click();", element)  # Only way to click w/ Python?
    def ClickButton(self, deet):
        element = self.browser.find_element_by_class_name(deet)
        return self.browser.execute_script("arguments[0].click();", element)
    def wait(self):
        clock = random.randint(2, 20)
        return self.browser.implicitly_wait(clock)

    def GetTables(self, table):
        href = table[1]
        name = table[0]
        self.browser.get(href)
        Browse.ClickButton(self, "_2N9lzTO4qyj6nRyj1sxFHp")
        Browse.wait(self)
        time.sleep(2)

        ##Check for extra pages
        try:
            pagelinks = self.browser.find_element_by_xpath('//div[@data-test="pagination-controls"]')
        except:
            pagelinks = None
            finalpage = 3
        if pagelinks != None:
            pagenumbers = pagelinks.find_elements_by_xpath('.//li[@data-test="pagination-link"]')
            finalpage = int(pagenumbers[-1].text)

        ##Find Number of Booking, number of restaurants, cycle thu pages
        books = 0
        rest = 0

        for page in range(2, finalpage):
            Browse.wait(self)
            time.sleep(5)
            ##Load Page
            lenOfPage = self.browser.execute_script(
                "window.scrollTo(0, 1000);var lenOfPage=document.body.scrollHeight;return lenOfPage;")
            scrolltimes = int(lenOfPage/1000)
            for i in range(0,scrolltimes):
                self.browser.execute_script("window.scrollBy(0, 1000);")
                self.browser.implicitly_wait(0.5)
                time.sleep(0.5)
            TableList = self.browser.find_elements_by_class_name("_3v-BAA6RD6_J0v34nZo2Qv")
            for table in TableList:
                TimeSlot = re.search("You're in luck! We still have \d+ timeslots left", table.text)
                if table.text == "Top special" or TimeSlot != None:
                    continue
                BookedNumber = int(re.findall(r'\d+', table.text)[0])
                rest = rest + 1
                books = BookedNumber + books
            #Load Next Page
            pagelinks = self.browser.find_elements_by_xpath('//li[@data-test="pagination-link"]')
            endpage = True
            # for link in pagelinks:
            #     if page == int(link.text):
            #         button = link.find_element_by_xpath('.//a')
            #         self.browser.execute_script("arguments[0].click();", button)
            #         break
            #     endpage = True
            if endpage == True:
                break
        ##SaveData
        print(int(books/rest))
        val = int(books/rest)
        installdata(name, val)


if __name__ == "__main__":
    Chrome = Browse()
    Chrome.start()
    LondonTables = [ ['WestEndLondon', 'https://www.opentable.com/g/london/west-end-london-restaurants'], ['CityOfLondon', 'https://www.opentable.com/g/london/city-of-london-restaurants'], ['EastLondon', 'https://www.opentable.com/g/london/east-london-restaurants'], ['WestLondon', 'https://www.opentable.com/g/london/west-london-restaurants'], ['DocklandsCanaryWharf', 'https://www.opentable.com/g/london/docklands-and-canary-wharf-restaurants'], ['NorthLondon', 'https://www.opentable.com/g/london/north-london-restaurants'], ['NorthWestLondon', 'https://www.opentable.com/g/london/north-west-london-restaurants'], ['SouthEastLondon', 'https://www.opentable.com/g/london/south-east-london-restaurants'], ['SouthWestLondon', 'https://www.opentable.com/g/london/south-west-london-restaurants']]
    #Add Today Row
    today = datetime.today()
    datestring = f'{today:%d}/{today:%m}/{today.year}'
    spread = pd.read_csv("data.csv")
    spread = spread.append({"date": datestring, "WestEndLondon": 0, "CityOfLondon": 0, "EastLondon": 0, "WestLondon": 0, "DocklandsCanaryWharf": 0, "NorthLondon": 0, "NorthWestLondon": 0, "SouthEastLondon" : 0, "SouthWestLondon" : 0},
                                   ignore_index=True)
    spread.to_csv("data.csv", index=False)
    for table in LondonTables:
        print(table[0])
        Chrome.GetTables(table)
    Chrome.stop()
