# Discharge-editing
Calculate summary statistics from 5 min, hourly and daily discharge time-series.

By Maartje Korver

5 min, hourly and daily discharge data for the seven watersheds of the Watersheds program can be downloaded from the Hakai Institute data portal.

These scripts were created to:
1) format the layout of the 5min, hourly and daily timeseries sheets ('5min.R', 'hourly.R', 'daily.R') so it is better readable and easier to process.
2) calculate average and total monthly, wateryearly and yearly discharge data and summarize the QC flags ('5min_to_monthly.R', '5min_to_wty.R', '5min_to_yearly.R')
