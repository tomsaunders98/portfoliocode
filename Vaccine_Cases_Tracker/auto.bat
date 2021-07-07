@echo off
C:\Users\Tom\Inews\Covid_data_col\venv\Scripts\python.exe C:\Users\Tom\Inews\Covid_data_col\venv\src\scrape_covid_world.py %*
C:\Users\Tom\Inews\Covid_data_col\venv\Scripts\python.exe C:\Users\Tom\Inews\Covid_data_col\venv\src\scrape_covid_uk_lta.py %*
"C:\Program Files\R\R-3.6.2\bin\Rscript.exe" C:\Users\Tom\Inews\Covid_data_col\img_gen\main_gen.R
robocopy C:\Users\Tom\Inews\Covid_data_col\venv\out "C:\Users\Tom\Dropbox\InewsRepo"
robocopy C:\Users\Tom\Inews\Covid_data_col\venv\out\img "C:\Users\Tom\Dropbox\InewsRepo\img"
