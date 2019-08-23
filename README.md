library(RODBC)
require(lubridate)
require(ggplot2)
require(dplyr)
library(splines)
require(reshape2)
library(grid)
library(gridExtra)
library(polynom)
library(RColorBrewer)
library(ggplot2)

options(dplyr.print_max = 10)

source("S:\\OandT\\OptRisk\\Energy_Requirements\\09 - Demand forecasting (Forecasting team)\\Renewable Generation Capacity\\R function\\running 32 bit query function.R")

root_dir <- "S:/OandT/OptRisk/Energy_Requirements/19 - Wind Energy/WPFS/Wind farm models"
#check for 32 bit set up
ifelse(.Machine$sizeof.pointer == 4, "Congrats you are running 32bit", "Caution - Its a trap 64bit")

root_dir <- "S:/OandT/OptRisk/Energy_Requirements/19 - Wind Energy/WPFS/Wind farm models"
dev_dir <- "S:/OandT/OptRisk/Energy_Requirements/19 - Wind Energy/WPFS/Wind farm models/Physical models/development"

files_name_only <- list.files(path=paste0(root_dir, "/Historic_data"), pattern="*.RDS", recursive=FALSE)
files_name_only <- gsub("wind_id_", "", files_name_only)
files_name_only <- as.numeric(gsub(".RDS", "", files_name_only))

#set up connection args to DEAF
connection_args <-  list("DEAFP", "tde", "control1")

query <- "select  gen_id from wind_generator where category = 'BMU' and (end_date > sysdate or end_date is NULL) order by gen_id"

#set up connection to DEAF
wind_farm_list <- tbl_df(RODBC_query(connection_args, query))
#set up connection to NED

#NED <-odbcDriverConnect("Driver={Oracle in OraClient11g_home1};Dbq=NEDP;Uid=caplinj;Pwd=caplinj;")
connection_args_NED <-  list("NEDP", "caplinj", "caplinj")

#wind_farm_list <- (sqlQuery(myconn, "select  gen_id from wind_generator where category = 'BMU' and end_date > sysdate order by gen_id"))$GEN_ID

wind_farm_list <- c(3, 11, 21, 44, 743, 91403, 91553)

i <- 3

for (i in wind_farm_list) {

wind_farm_list[i]

##which Gen_id to study
Gen_id <- i
GEN_IDs_FROM_FILE <- Gen_id

##which models to check
Model_id <- c(1,5)

##find the date that the cubic spline was last changed and assign to start_date
query_date_range_by_cubic <- "select unique(TSTAMP) from DEAFDBA.GEN_1D_COEFFICIENTS_DATA where GEN_ID = Gen_id;"
query_date_range_by_cubic <- gsub("Gen_id", Gen_id, query_date_range_by_cubic)

#start_date <- tbl_df(sqlQuery(myconn, query_date_range_by_cubic, as.is = 1))

start_date <- tbl_df(RODBC_query(connection_args, query_date_range_by_cubic))

start_date <- start_date %>% mutate(TSTAMP = ymd_hms(TSTAMP, tz = "GMT")) %>% mutate(TSTAMP = format(as.Date(TSTAMP), "%Y-%m-%d"))

start_date_simple <- start_date %>% mutate(TSTAMP = format(as.Date(TSTAMP), "%Y%m%d"))

start_date_simple <- unique(start_date_simple)

start_date <- paste0(start_date, " ", "00:00:00")


##assign end date as today
end_date <- Sys.Date()
end_date <- paste0(end_date, " ", "00:00:00")


##recylced Andrews Code for obtaining actual wind speeds and metering for that Gen_id
for (i in Gen_id) {
  tryCatch(
    {#create sql
      #change GEN_ID as appropriate
      GEN_ID_NUMBER <- Gen_id
      
      ##Identify location id and station id, if it exists
      ##If station id does not exist then best we have for wind speed is the most recent forecast
      qlocation <- "select l.loc_id, s.station_id
      from gen_frcstr_loc_map l left join spf_frcstr_loc_station_map s
      on l.loc_id = s.loc_id
      where l.gen_id = GEN_ID_NUMBER"
      qlocation <- gsub("GEN_ID_NUMBER", GEN_ID_NUMBER, qlocation)
      
      #location <- sqlQuery(myconn, qlocation)
      location <- tbl_df(RODBC_query(connection_args, qlocation))
      
      locid <- location$LOC_ID
      stid <- location$STATION_ID
      
      query_wind_gen <- "SELECT GEN_NAME, GEN_FULL_NAME, capacity from wind_generator where GEN_ID = GEN_ID_NUMBER;"
      
      query_wind_name <- gsub("GEN_ID_NUMBER", GEN_ID_NUMBER, query_wind_gen)
      
      #obtain all metering for a GEN_ID
      #gen_name_capacity <- sqlQuery(myconn, query_wind_name)
      
      gen_name_capacity <- tbl_df(RODBC_query(connection_args, query_wind_name))
      
      gen_name_BMU <- gen_name_capacity$GEN_NAME
      capacity  <- gen_name_capacity$CAPACITY
      gen_full_name  <- gen_name_capacity$GEN_FULL_NAME
      
      if (!is.na(stid)) {
        query_windspeed <- "select gdate, gtime, 0.514444*ws as ws from vactweather 
        where stnum = stid and gdate >= start_date_simple
        order by gdate, gtime"
        query_windspeed <- gsub("stid", stid, query_windspeed)
        query_windspeed <- gsub("start_date_simple", start_date_simple, query_windspeed)
        #wind <- tbl_df(sqlQuery(myconn, query_windspeed))
        
        wind <- tbl_df(RODBC_query(connection_args, query_windspeed))
        
        wind <- wind %>% mutate(datetime = ymd_hm(10000*GDATE + GTIME, tz = 'GMT')) %>%
          select(datetime, WS)
      } else {
        query_windspeed <- "select target_datetime, frcst_datetime, F_WS_MEAN as WS
        from weather_frcst_data where loc_id = locid and 
        target_datetime >= to_date('start_date', 'YYYY-MM-DD HH24:MI:SS')
        and target_datetime > frcst_datetime and target_datetime < frcst_datetime + 1/4
        order by target_datetime"
        query_windspeed <- gsub("start_date", start_date, query_windspeed)
        query_windspeed <- gsub("locid", locid, query_windspeed)
        #wind <- tbl_df(sqlQuery(myconn, query_windspeed, as.is = 1:2))
        wind <- tbl_df(RODBC_query(connection_args, query_windspeed, as.is = 1:2))
        
        wind <- wind  %>% 
          mutate(datetime =  ymd_hms(TARGET_DATETIME, tz = "GMT"), FRCST_DATETIME =  ymd_hms(FRCST_DATETIME, tz = "GMT")) 
        wind_latest <- wind %>% group_by(datetime) %>% summarise(FRCST_DATETIME = max(FRCST_DATETIME))
        wind <- inner_join(wind, wind_latest, by = c("datetime", "FRCST_DATETIME")) %>%
          select(datetime, WS)
      }
      
      #obtain all metering for a GEN_ID
      query_wind_gen <- "select METERED_DATETIME as datetime, GEN_MW
      from  GEN_METERED_DATA 
      where GEN_ID = GEN_ID_NUMBER
      and METERED_DATETIME  >= to_date('start_date', 'YYYY-MM-DD HH24:MI:SS');"
      
      query_wind_gen <- gsub("start_date", start_date, query_wind_gen)
      query_wind_gen <- gsub("GEN_ID_NUMBER", GEN_ID_NUMBER, query_wind_gen)
      
      #wind_gen <- tbl_df(sqlQuery(myconn, query_wind_gen, as.is = 1))
      
      wind_gen <- tbl_df(RODBC_query(connection_args, query_wind_gen, as.is = 1))
      
      wind_gen <- wind_gen %>% mutate(datetime = ymd_hms(DATETIME, tz = "GMT"), GEN_MW = pmax(GEN_MW, 0)) %>%
        select(datetime, GEN_MW)
      
      ##Combine wind and generation
      
      metered <- inner_join(wind, wind_gen, by = "datetime") %>% mutate(gen_id = GEN_ID_NUMBER, capacity = capacity)
      
      ##If there's duplicated data: I know there's duplicated data in vactweather
      metered <- metered %>% mutate(dummy = 1:nrow(metered))
      metered_pick <- metered %>% group_by(datetime) %>% summarise (dummy = max(dummy)) 
      metered <- inner_join(metered, metered_pick, by = c("datetime", "dummy")) %>% select(-dummy)
      
      #use diff function to compare consectutive gen_mw and remove if 0, unless it's at maximum capacity
      metered <- metered[c(TRUE, (abs(diff(metered$GEN_MW))>0 | (metered$GEN_MW == capacity)[-1])), ]
      
      ##Acquire BOA data
      query_BOA <- "select gt.gmt_prd_dtm as datetime, bm.bmu_id
      from baar_bidoffer_acceptance bo inner join bm_units bm
      on bo.bmu_id = bm.bmu_id
      inner join general_date_times gt on gt.sett_date = bo.sett_date and gt.sett_period = bo.sett_period
      where bm.fuel_i = 'WIND' and bm.bmu_id = 'gen_name_BMU' and gt.gmt_prd_dtm between bm.datetime_from and bm.datetime_to
      and gt.gmt_prd_dtm >= to_date('start_date', 'YYYY-MM-DD HH24:MI:SS')
      and qab < 0;"
      
      query_BOA <- gsub("start_date", start_date, query_BOA)
      query_BOA <- gsub("gen_name_BMU", gen_name_BMU, query_BOA)
      query_BOA <- gsub("\n", " ", query_BOA)
      
      #obtain all metering for a GEN_ID
      #BOA_data <- tbl_df(sqlQuery(NED, query_BOA, as.is = 1))
      
      BOA_data <- tbl_df(RODBC_query(connection_args_NED, query_BOA))
      
      if (nrow(BOA_data) > 0) {
        BOA_data <- BOA_data %>% mutate(datetime = ymd_hms(DATETIME, tz = "GMT") + minutes(30)) %>% select(datetime)
        #join- remove all entries that have a datetime in the BOA table
        metered <- anti_join(metered, BOA_data, by = "datetime")
      }
      
      
      print(i)
    }, error = function(e){cat("Gen_ID", i , "not extracted", conditionMessage(e), "\n")})
}

#check plot
p <- ggplot(metered, aes(x = WS, y = GEN_MW)) + geom_point(size = 0.1)
p


##obtain forecast using physical model (mod1)

query_meter_per_model <- "select GEN_ID, MODEL_ID, FRCST_DATETIME, TARGET_DATETIME, F_MW_MEAN, TSTAMP 
from POWER_FRCST_DATA 
WHERE GEN_ID =Gen_id and  MODEL_ID = Model_id
and target_datetime >= to_date('start_date', 'YYYY-MM-DD HH24:MI:SS') and
frcst_datetime < to_date('end_date', 'YYYY-MM-DD HH24:MI:SS')
and target_datetime > frcst_datetime and target_datetime < frcst_datetime + 1/4
order by target_datetime ;"
query_meter_per_model <- gsub("Gen_id", Gen_id, query_meter_per_model)
query_meter_per_model <- gsub("start_date", start_date, query_meter_per_model)
query_meter_per_model <- gsub("end_date", end_date, query_meter_per_model)
query_meter_per_model <- gsub("Model_id", Model_id[1], query_meter_per_model)

#wind_mod_1 <- tbl_df(sqlQuery(myconn, query_meter_per_model))

wind_mod_1 <- tbl_df(RODBC_query(connection_args, query_meter_per_model))

names(wind_mod_1) <- c("GEN_ID",  "Model_id", "FRCST_DATETIME", "datetime", "physical", "TSTAMP")


##obtain forecast using cubic splinel model (mod5)

query_meter_per_model <- "select GEN_ID, MODEL_ID, FRCST_DATETIME, TARGET_DATETIME, F_MW_MEAN, TSTAMP 
from POWER_FRCST_DATA 
WHERE GEN_ID =Gen_id and  MODEL_ID = Model_id
and target_datetime >= to_date('start_date', 'YYYY-MM-DD HH24:MI:SS') and
frcst_datetime < to_date('end_date', 'YYYY-MM-DD HH24:MI:SS')
and target_datetime > frcst_datetime and target_datetime < frcst_datetime + 1/4
order by target_datetime ;"
query_meter_per_model <- gsub("Gen_id", Gen_id, query_meter_per_model)
query_meter_per_model <- gsub("start_date", start_date, query_meter_per_model)
query_meter_per_model <- gsub("end_date", end_date, query_meter_per_model)
query_meter_per_model <- gsub("Model_id", Model_id[2], query_meter_per_model)

#wind_mod_5 <- tbl_df(sqlQuery(myconn, query_meter_per_model))

wind_mod_5 <- tbl_df(RODBC_query(connection_args, query_meter_per_model))

names(wind_mod_5) <- c("GEN_ID",  "Model_id", "FRCST_DATETIME", "datetime", "cubic", "TSTAMP")


final <- inner_join(metered, wind_mod_1, by = "datetime")
final <- left_join(final, wind_mod_5, by = "datetime")
final <- final %>% select(datetime, WS, GEN_MW, physical, cubic, capacity)
final <- final %>% mutate(GEN_MW = GEN_MW/capacity)
final <- final %>% mutate(physical = physical/capacity)
final <- final %>% mutate(cubic = cubic/capacity)
final <- final %>% mutate(WS_PU = WS/max(final$WS)) 
final <- final %>% mutate(phy_error = abs(GEN_MW-physical))
final <- final %>% mutate(cub_error = abs(GEN_MW-cubic))

physical_error <- sum(abs(final$physical - final$GEN_MW))
cubic_error <- sum(abs(final$cubic - final$GEN_MW))
model_diff <- cubic_error-physical_error
model_diff_percent <- ((model_diff/physical_error)*100)*-1

text_for_graph <- if(model_diff_percent<0){
  paste0(signif(model_diff_percent,1), "%. Better with Cubic")
} else {
  paste0(signif(model_diff_percent,1), "%. Worse with Cubic")
}

test <- split(final, cut(final$WS_PU, seq(0, 1, 0.1)))
test2 <- lapply(test,function(my_table) { colSums(my_table[, c("phy_error", "cub_error")]) })
test3 <- do.call(rbind, test2)
test4 <- data.frame(test3, pu = seq(0, .9, 0.1))


#check plot
p2 <- ggplot(test4, aes(x = pu)) + 
  geom_line(aes(y = phy_error, colour = "Physical Model"))+
  geom_line(aes(y = cub_error, colour = "Cubic Model"))+
  theme(legend.title=element_blank(), axis.title.x=element_blank())+ ylab("Sum of Errors Per Bucket")+
  ggtitle(paste0(gen_full_name, " - ", start_date," to " ,end_date))
p2

test4_melt <- melt(data = test4, id.vars = "pu", measure.vars = c("phy_error", "cub_error"))

p5 <- ggplot(data=test4_melt, aes(x=pu, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())
p5 <- p5 + theme(legend.title = element_blank())
p5 <- p5 + ggtitle("Forecast Compared to Models per Bucket") +
  ylab("Error (MW)") + xlab("Buckets (pu)")
p5 <- p5 +theme(legend.position="bottom")
p5

###############################################################################################################################
##Extracting physical parameters
##Acquire existing physical model parameters
##create sqlquery
full_parameter <- "select 
GTM.GEN_ID, 
GTM.TRBNE_ID, 
TT.SCALE, 
TT.MULT, 
TT.SENS,
TT.CUTOFF_WS,
WG.CAPACITY
from VTURBINE_TYPES TT
LEFT JOIN VGEN_TRBNE_MAP GTM on GTM.TRBNE_ID=TT.TRBNE_ID
LEFT JOIN WIND_GENERATOR WG on GTM.GEN_ID= WG.GEN_ID
where GTM.GEN_ID in (GEN_IDs_FROM_FILE);"

full_parameter <- gsub("GEN_IDs_FROM_FILE", GEN_IDs_FROM_FILE, full_parameter)
full_parameter <- gsub("\n", " ", full_parameter)

##run sql
#full_parameter <- sqlQuery(myconn, full_parameter)

full_parameter <- tbl_df(RODBC_query(connection_args, full_parameter))

##Inputting into equation
WS <- c(0:26)

test <- full_parameter$SCALE/(1+full_parameter$MULT*exp((0-full_parameter$SENS)*(WS)))

##Create new data frame
df <- data.frame(Wind_Speed=WS, MW_Output=test)

###############################################################################################################################
###############################################################################################################################



root_dir <- "S:/OandT/OptRisk/Energy_Requirements/19 - Wind Energy/WPFS/Wind farm models"

metered_BOA_removed <- readRDS(paste0(root_dir, "/Historic_data/wind_id_", Gen_id, ".RDS"))

capacity <- unique(metered_BOA_removed$capacity)

metered_BOA_removed<- metered_BOA_removed%>% mutate(LF = GEN_MW/capacity)

##Re-formatting the date column as numerics

metered_BOA_removed["Date"] <- as.numeric(format(metered_BOA_removed$datetime, format='%Y%m%d'))

##Adding a Weighting column with higher values given to more recent dates

metered_BOA_removed["Weighting"] <- (max(metered_BOA_removed$Date, na.rm=TRUE) - min(metered_BOA_removed$Date, na.rm=TRUE)) - (max(metered_BOA_removed$Date, na.rm=TRUE) - metered_BOA_removed$Date)

##Plots a graph with a colour scale: low weighting = red, high weighting = green

p <- ggplot(data = metered_BOA_removed, aes(x = WS, y = LF)) + geom_point(aes(color=metered_BOA_removed$Weighting)) + scale_color_gradient(low="red", high="green")
p <- p + xlim(0, 30) + ylim(0,1)
p

wind_data <- metered_BOA_removed

wind_data_clean <- wind_data %>% filter(LF <=  0.16*WS, LF <= 1, WS <= 26)

p <- ggplot(data = wind_data_clean, aes(x = WS, y = LF)) + 
  geom_point(aes(color=wind_data_clean$Weighting)) + 
  scale_color_gradient(low="red", high="green")
p <- p + xlim(0, 30) + ylim(0,1)
p

##Additional columns for points (0,0) and (26,0), given maximum weighting and nominal date,
##to ensure graph crosses origin and cut off point

wind_data_clean <- rbind(wind_data_clean, 
                         data_frame(WS = 0, LF = 0, 
                                    datetime = max(wind_data_clean$datetime), 
                                    GEN_MW=rep(0,12), 
                                    gen_id=Gen_id, 
                                    capacity=capacity, 
                                    Date=20190101, 
                                    Weighting=max(wind_data_clean$Weighting)))

##Additional points added at (26,0) to pull graph down without distorting colour grading

wind_data_clean <- rbind(wind_data_clean, 
                         data_frame(WS = 26, LF=0,
                                    datetime = seq(max(wind_data_clean$datetime), max(wind_data_clean$datetime)+days(11), by = "days"),
                                    GEN_MW=0, 
                                    gen_id=Gen_id, 
                                    capacity=capacity,
                                    Date=20190101, 
                                    Weighting=max(wind_data_clean$Weighting)))

p <- ggplot(data = wind_data_clean, aes(x = WS, y = LF)) + geom_point(aes(color=wind_data_clean$Weighting)) + scale_color_gradient(low="red", high="green")
p

##Play around with knot points until you are happy with the shape
##Break points at 5,10, 13, 15, 20

knots_EFS <- "select WS_MIN from GEN_1DBRKPOINT_DATA where GEN_ID = GEN_IDs_FROM_FILE;"

knots_EFS_q <- gsub("GEN_IDs_FROM_FILE", GEN_IDs_FROM_FILE, knots_EFS)
knots_EFS_q <- gsub("\n", " ", knots_EFS_q)

##run sql
#knots_EFS_output <- sqlQuery(myconn, knots_EFS_q)
knots_EFS_output <- tbl_df(RODBC_query(connection_args, knots_EFS_q))


knots_EFS_output <- knots_EFS_output[(knots_EFS_output<26),]

knots_EFS_output <- knots_EFS_output$WS_MIN[-1]

knot_v <- knots_EFS_output

####################
mod1 <- lm(data = wind_data_clean, LF ~ ns(WS, knots = knot_v), weights=Weighting)

WSd <- seq(0, 28, by = 0.1)

new_d1 = data_frame(WS = WSd)
LFp = predict(mod1, new_d1)
new_d1 <- new_d1 %>% mutate(LF = LFp)

p1 <- p + geom_line(data = new_d1, aes(x = WS, y = LF), size = 2, colour = 'blue')
p1 <- p1 + geom_line(data = df, aes(x=Wind_Speed, y=MW_Output), colour="red", size = 2)
p1 <- p1 + theme(legend.position="none")
p1 <- p1 + annotate("text", x = 6, y = -.05, label = paste0(text_for_graph))

########################################################################################################

p6 <- grid.arrange(p1, p5, ncol = 2, top = paste0(gen_full_name, " - ", format(as.Date(start_date), "%Y/%m/%d")," to " ,format(as.Date(end_date), "%Y/%m/%d")))

png(filename = paste0("S:\\OandT\\OptRisk\\Energy_Requirements\\19 - Wind Energy\\WPFS\\Wind farm models\\Cubic Physical Comparisons\\images\\", gen_full_name, ".png"),width=9.25,height=7.25,units="in",res=600)

plot(p6)
dev.off()

}

