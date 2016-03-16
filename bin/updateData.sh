R -e "source('~/Documents/Rprojects/Trading/src/update_data_in_h2.R')" --quiet --vanilla --slave
R -e "source('~/Documents/Rprojects/Trading/src/create_daily_report.R');go.markdown()" --quiet --vanilla --slave
