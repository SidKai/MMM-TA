getwd()
setwd("C:/Users/siddharth.kaithw/MMX Modelling Project/Data")



# imputing null values in clicks with 0
dig_media$Clicks[is.na(dig_media$Clicks)] <- 0


dig_media$Type[is.na(dig_media$Type)] <- 'New_type'