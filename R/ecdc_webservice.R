
library(httr)
library(dplyr)
library(ggplot2)

# http://atlas.ecdc.europa.eu/public/index.aspx 
# 
# http://atlas.ecdc.europa.eu/Public/AtlasService/rest/help/operations/GetMeasuresResults

# dataset <- GET("http://atlas.ecdc.europa.eu/public/AtlasService/rest/GetMeasuresResults?healthTopicId=29&datasetId=27&measureIds=&measureTypes=I,Q&timeCodes=&geoCodes=AT,BE,CZ,DE,DK,ES,FR,IE,IT,LT,LU,LV,MT,NL,NO,PL,PT,RO,SE,SI,SK,UKG13182,UKL,UKM,UKN,CY,EE,HU,EL,BG,FI,IS,HR&geoLevel=1&timeUnit=Week&isPivotByTimePeriod=false&measurePopulation=")
# 
# 
# dataset <- GET("http://atlas.ecdc.europa.eu/public/AtlasService/rest/GetMeasuresResults?healthTopicId=29&datasetId=27&measureIds=395195&measureTypes=I,Q&timeCodes=2020-W02&geoCodes=AT,BE,CZ,DE,DK,ES,FR,IE,IT,LT,LU,LV,MT,NL,NO,PL,PT,RO,SE,SI,SK,UKG13182,UKL,UKM,UKN,CY,EE,HU,EL,BG,FI,IS,HR&geoLevel=1&timeUnit=Week&isPivotByTimePeriod=false&measurePopulation=Influenza%20activity")

dataset <- GET("http://atlas.ecdc.europa.eu/public/AtlasService/rest/GetMeasuresResults?healthTopicId=71&datasetId=27&measureIds=395333&measureTypes=I,Q&timeCodes=&geoCodes=DE,EU31_UKX,FI,IE,NL,PL,SI,UKL,UKN,PT,EE,CZ,EL,ES,FR,LV,UKG13182,UKM,IT,BG,HU,CY,LT,LU,RO,BE,DK,SK&geoLevel=1&timeUnit=Week&isPivotByTimePeriod=false&measurePopulation=RSV%20Sentinel")

dataset <- content(dataset)$MeasureResults
dataset_df <- data.frame(do.call(rbind, dataset))

#result <- dataset_df %>% filter(MeasureLabel=="Intensity") %>% dplyr::select(TimeCode, GeoLabel, XValue)
result <- dataset_df %>% filter(MeasureLabel=="RSV detection") %>% dplyr::select(TimeCode, GeoLabel, N)
result$XValue <- sapply(result$N, function(x) ifelse(is.null(x), 0, x))
result$GeoLabel <- as.factor(unlist(result$GeoLabel))
result$TimeCode <- unlist(result$TimeCode)
result$Date <- as.Date(paste0(result$TimeCode,"-1"), format = "%Y-W%U-%u")
result$Year <- factor(format(result$Date, "%Y"))
result$Week <- as.numeric((format(result$Date, "%U")))

ggplot(result, aes(x=Week, y=XValue)) + geom_line(aes(color=factor(Year))) + facet_wrap(~GeoLabel, scales = "free_y")

ggplot(result, aes(x=Date, y=XValue)) + geom_line() + facet_wrap(~GeoLabel, scales = "free_y")

