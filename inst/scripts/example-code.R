weekly_timeseries <- get_weekly_timeseries()

# cube <- "SurvStat"
# language <- "English"
# hierarchies <- getHierarchies(cube, language) %>% dplyr::select(HierarchyCaption)
# hierarchy <- hierarchies[7,"HierarchyCaption"]
# facet <- hierarchies[18,"HierarchyCaption"]
# filter <- hierarchies[3,"HierarchyCaption"]
# members <- getHierarchyMembers(cube, language, filter) %>% dplyr::select(Caption)
# filterValue <- members[65, "Caption"]
# filter2 <- hierarchies[5,"HierarchyCaption"]
# members2 <- getHierarchyMembers(cube, language, filter2) %>% dplyr::select(Caption)
# filterValue2 <- members2[2, "Caption"]
# 
# data <- getOlapData(cube, language, hierarchy, facet, filter, filterValue, filter2, filterValue2)
# 
# if(hierarchy=="Year and week of notification"){
#   data$Year <- substr(data$Categories,1,4)
#   data$Week <- substr(data$Categories,8,9)
#   data$date <- as.Date(paste0(data$Year,"-",data$Week,"-1"), "%Y-%U-%u")
#   
#   ggplot(data, aes(x=date, y=value, color=variable)) + geom_line()
# }