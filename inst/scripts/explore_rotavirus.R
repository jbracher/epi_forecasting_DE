# Some preliminary descriptive analyses of rotavirus

library(desurvdata)
library(RColorBrewer)

weekly_rota <- get_weekly_timeseries(disease = "Rotavirus gastroenteritis", years=2010:2019)
# save(weekly_rota, file = "weekly_rota.rda")

head(weekly_rota)

# get sums per calendar week over the 10 years:
means_per_calendar_week <- aggregate(weekly_rota$value,
                                    by = list(weekly_rota$week),
                                    FUN = mean, na.rm = TRUE)
colnames(means_per_calendar_week) <- c("calendar week", "mean")
plot(means_per_calendar_week$mean, xlab = "calendar week", ylab = "total cases")
# season start in week 40 is probably more reasonable, but for now just use calendar years

# sums per calendar year:
yearly_sums <- 
    aggregate(weekly_rota$value, by = list(weekly_rota$year, weekly_rota$state), 
              FUN = sum, na.rm = TRUE)
colnames(yearly_sums) <- c("year", "state", "total")


# find peaks per calendar year:
yearly_peaks <- 
    aggregate(weekly_rota$value, by = list(weekly_rota$year, weekly_rota$state), FUN = which.max)
colnames(yearly_peaks) <- c("year", "land", "peak_week")

plot(table(yearly_peaks$peak_week), xlab = "peak week", ylab = "Frequency",
     main = "Distribution of peak weeks (across states and years)")
# there is actually some variation in the peak timing

# overlay seasons for a given state:
state <- "Bayern"
cols_season <- brewer.pal(10, "Paired"); names(cols_season) <- as.character(2010:2019)

par(mfrow = 1:2, las = 1)

plot(2010:2019, yearly_sums$total[yearly_sums$state == state], type = "h", 
     col = cols_season, ylim = c(0, 8000), lwd = 3, xlab = "year", 
     ylab = "total cases", main = paste("Yearly totals,", land))


plot(NULL, xlim = c(1, 52), ylim = c(0, 500), 
     xlab = "calendar week", ylab = "cases",
     main = paste("Yearly epidemic curves,", state))
sapply(as.character(2010:2019), 
       function(y){
           ts_season <- weekly_rota$value[weekly_rota$year == y&
                                              weekly_rota$state == state]
           col_season <- cols_season[y]
           lines(ts_season, col = col_season)
           points(which.max(ts_season), max(ts_season), pch = 15, col = col_season)
       }
)


