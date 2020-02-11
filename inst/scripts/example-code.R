library(desurvdata)
library(ggplot2)

diseases <- get_diseases()
weekly_noro <- get_weekly_timeseries(disease = "Noroviral gastroenteritis")
weekly_rsv <- get_weekly_timeseries(disease = "RSV-Infection")

ggplot(weekly_noro, aes(x=date, y=value)) + 
    geom_line() +
    facet_wrap(variable~.)


ggplot(weekly_rsv, aes(x=date, y=value)) + 
    geom_line() +
    facet_wrap(variable~.)
