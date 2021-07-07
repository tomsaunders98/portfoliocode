import json

percs = json.loads("bspen.json")


for ad in percs:
    print(ad["demos"]["percs"])