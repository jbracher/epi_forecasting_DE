library(desurvdata)
library(ggplot2)

diseases <- get_diseases()
weekly_noro <- get_weekly_timeseries(disease = "Noroviral gastroenteritis", years=2010:2020)
weekly_lyme <- get_weekly_timeseries(disease = "Lyme disease", years=2010:2020)
weekly_camp <- get_weekly_timeseries(disease = "Campylobacteriosis", years=2014:2020)
weekly_salmon <- get_weekly_timeseries(disease = "Salmonellosis", years=2014:2020)
weekly_flu <- get_weekly_timeseries(disease = "Influenza", years=2010:2020)

ggplot(weekly_noro, aes(x=date, y=value)) + 
    geom_line() +
    facet_wrap(state~.) +
    xlab(NULL)

ggplot(weekly_flu, aes(x=date, y=value)) + 
    geom_line() +
    facet_wrap(state~.) +
    xlab(NULL)

ggplot(weekly_lyme, aes(x=date, y=value)) + 
    geom_line() +
    facet_wrap(state~.) +
    xlab(NULL)

ggplot(weekly_camp, aes(x=date, y=value)) + 
    geom_line() +
    facet_wrap(state~.) +
    xlab(NULL)

ggplot(weekly_salmon, aes(x=date, y=value)) + 
    geom_line() +
    facet_wrap(state~.) +
    xlab(NULL)
