####################################################################
#
# DEBUGGING CODE
#
####################################################################

{
  ## CONTRACT = Prices per tonne
  kr_tonne <- 313 # at reference pol of 17% (30,1 EUR)
  kr_renhet <- 2 # extra kr per %-point over ref (89.5%), at 17% pol%
  kr_pol <- 9 # extra % per %-point over ref of 17% pol
  kr_TT <- 5 # for the use of TopTex for at least 7 days prior to delivery, for delivery after 15 November
  kr_vol <- 5 # extra per tonne if area increases 10%. This is SEK (actual bonus is 0,5 EUR/t)
  date_full <- seq(as.POSIXct("2021-09-10", tz = "UTC", format = "%Y-%m-%d"), length.out = 172, by = "1 day")
  price_early <- c(37,37,37,37,37,36,33,30,27,24,21,19,16,14,12,10,8,6,4,2,1,rep(0,151))
  price_late <- c(rep(0,82),seq(1,31,by=1),seq(32.5,120,by=1.5))
  price_TT <- c(rep(0,66),rep(5,8), rep(10,8), rep(15,90))
  price_vol <- rep(0,172)
  price_tab_contract <- data.frame(date_full,price_early, price_late, price_TT, price_vol)
  
  ## Reference values
  ref_hardness <- 50
  ref_pol <- 0.17
  ref_renhet <- 0.895
  ref_TT_1 <- as.POSIXct("2021-11-15", tz = "UTC", format = "%Y-%m-%d") # 5kr/tn beets
  ref_TT_2 <- as.POSIXct("2021-11-22", tz = "UTC", format = "%Y-%m-%d") # 10kr/tn beets
  ref_TT_3 <- as.POSIXct("2021-12-01", tz = "UTC", format = "%Y-%m-%d") # 15kr/tn beets
  ref_early <- as.POSIXct("2021-09-30", tz = "UTC", format = "%Y-%m-%d") # last day the early payment is made for
  ref_late_1 <- as.POSIXct("2021-12-01", tz = "UTC", format = "%Y-%m-%d") #first day you get money for late delivery
  ref_late_2 <- as.POSIXct("2021-01-01", tz = "UTC", format = "%Y-%m-%d") #first day you get 1.5 x money for late delivery
  cum_temp <- seq(1:1500)
  cum_temp <- c(0,cum_temp)
  ref_medel_linear <- cum_temp*0.0188
  ref_medel_discont <- ifelse(cum_temp <= 270, cum_temp*0.013, 270*0.013+(cum_temp-270)*0.042)
  ref_medel_quad <- (cum_temp*-0.0043 + 0.000064*cum_temp^2)
  ref_loss_data <- data.frame(cum_temp,ref_medel_linear, ref_medel_discont, ref_medel_quad)
  ref_temp <- 5
  first_day <- min(price_tab_contract$date_full)
  last_day <- max(price_tab_contract$date_full)
  days_full <- round(as.numeric(difftime(last_day, first_day, units="days")+1))
  
clamp_size <- 8
cover_date <- "2021-11-15"
data_restrict <- T
delivery_cost <- 10000
delivery_date <- "2021-12-25"
delivery_distance <- 5
delivery_loads <- 50
factor <- 1.2
field_size <- 100
harvest_date <- "2021-11-01"
harvester_cleaning <- 50
late_moisture <- 100
loss_model <- 1
moisture <- 100
pol <- 19
price <- 270
renhet <- 90
ref_renhet <- 0.895
ref_temp <- 5
root_yield_p <-100
temp_air_yr <- "yr2020"
temp_clamp_model <- 1
variety_hardness <- 50
vol <- F
late_potent <- 1
mass_loss <- 0.02
root_tip_break_pc <- 20
prod_data_date <- "2021-12-25"

yr2016 <- c(16.8,18.2,17.0,19.8,19.4,18.4,17.6,14.7,12.8,13.9,15.1,13.3,12.5,14.7,14.5,15.1,15.8,14.2,17.2,16.4,13.8,13.6,12.1,11.7,8.3,7.2,8.9,9.3,8.2,7.7,8.2,7.8,7.9,7.9,8.7,9.4,7.1,8.4,8.7,10.0,8.8,7.9,6.0,6.1,7.2,6.6,5.3,10.4,10.7,8.7,6.5,8.3,9.5,4.4,2.1,5.8,6.6,3.3,-0.7,-2.2,0.5,-0.4,-1.1,-2.3,-0.7,3.0,5.7,6.4,6.3,6.9,6.0,6.4,9.2,8.1,6.9,4.8,4.7,7.0,3.4,-0.6,4.3,6.0,6.7,1.7,0.1,3.4,5.2,1.2,3.3,9.3,7.9,8.7,6.9,0.7,4.6,1.3,-0.5,1.7,1.9,5.0,3.9,3.1,3.0,5.0,5.5,5.4,6.1,6.4,5.4,1.1,4.1,3.1,6.0,5.5,0.2,3.0,1.1,-6.8,-7.7,-0.7,-1.0,1.7,0.8,1.7,3.1,0.8,0.1,-1.0,-2.9,-1.5,1.7,3.1,4.4,2.0,2.0,1.8,2.5,1.7,0.3,0.6,0.1,0.9,2.2,2.1,1.2,0.4,1.0,2.4,2.2,1.0,-1.9,-2.7,-3.9,-1.1,-1.2,-2.1,-5.0,-3.6,-1.5,0.9,3.1,2.7,3.8,5.6,5.9,5.7,4.0,0.3,1.1,5.1,7.2,5.5)
yr2017 <- c(14.0,14.4,15.2,13.2,12.6,13.4,12.6,10.2,10.3,13.0,12.9,11.9,10.9,14.1,15.7,15.0,15.5,15.2,14.2,14.2,12.9,12.7,12.6,13.2,11.1,10.6,11.1,10.3,8.1,9.1,9.3,12.3,12.3,12.4,14.6,13.8,13.7,13.4,12.4,11.0,11.6,11.5,10.4,8.4,8.7,12.6,11.5,9.5,10.9,7.2,4.3,6.4,11.1,9.9,7.3,9.6,10.0,7.0,3.7,5.6,7.6,7.3,6.4,5.9,1.2,5.3,6.9,6.0,6.5,5.9,3.9,-0.2,0.4,6.6,8.9,6.8,2.9,4.9,5.3,5.5,3.3,1.1,1.6,2.0,4.3,3.6,6.9,8.0,6.6,4.6,3.7,1.8,0.6,1.2,3.1,3.1,3.1,1.9,-0.5,1.8,2.6,4.0,5.6,5.1,7.9,8.6,7.5,5.8,4.4,4.1,3.9,1.8,3.6,6.2,4.9,3.9,3.5,2.7,1.0,-2.3,-0.3,2.0,3.0,3.3,2.1,0.4,-0.7,0.2,2.5,1.6,1.5,1.1,1.5,-0.6,0.3,1.1,7.0,5.8,3.8,4.3,5.8,6.3,4.7,4.0,3.4,2.2,-2.7,-1.0,-5.6,-4.8,-1.5,-0.1,-0.9,-1.2,0.5,2.6,1.2,0.4,0.2,2.3,1.6,2.0,1.2,1.0,0.6,-0.9,-2.0,-2.7,-4.5,-4.5,-6.5,-8.2)
yr2018 <- c(16.9,15.7,14.3,14.4,15.4,14.3,14.1,17.1,18.7,18.1,18.3,16.7,12.2,11.7,9.1,10.3,13.5,15.1,10.9,10.9,12.6,10.1,9.3,9.6,11.1,13.2,12.0,9.0,11.7,13.2,12.7,14.6,14.3,15.5,14.1,13.1,12.4,11.5,9.9,7.0,10.0,9.7,10.5,10.7,7.2,10.6,9.0,4.8,1.8,3.9,10.2,8.5,8.4,9.0,6.9,6.1,9.5,8.8,7.9,6.5,7.9,8.9,9.6,10.3,9.5,9.6,9.0,5.4,4.5,4.9,4.1,3.8,4.0,4.2,2.1,2.0,1.3,-2.6,-3.9,-1.1,1.9,2.7,5.2,5.5,8.2,5.8,4.0,4.1,8.3,6.8,6.1,4.7,3.7,1.5,0.1,1.2,1.5,0.6,0.1,1.8,3.5,3.6,4.4,0.6,-0.7,-1.2,5.2,5.9,7.4,6.0,6.3,4.2,3.8,6.5,1.7,1.4,4.3,2.8,0.5,4.1,4.1,0.5,-1.3,3.8,2.9,5.0,1.3,3.7,5.4,2.1,-1.1,0.0,0.1,0.7,0.3,-2.5,-2.3,-3.2,-1.5,1.1,2.3,1.0,0.6,0.9,0.8,1.5,0.8,1.3,3.9,2.8,3.7,5.0,5.5,4.6,3.0,2.4,5.9,5.8,3.4,4.6,5.9,4.7,5.1,5.8,5.8,0.7,2.4,4.4,4.9,5.5,6.1,4.9)
yr2019 <- c(16.1,15.7,16.0,15.3,14.4,14.7,13.4,10.7,10.7,10.4,11.6,13.1,12.8,12.3,12.9,13.3,13.8,14.4,13.6,13.6,12.2,9.6,9.1,6.8,6.8,5.7,3.6,2.3,8.5,11.8,11.3,12.2,12.5,12.2,12.3,12.2,11.7,12.2,12.8,12.2,12.0,11.2,12.2,11.1,11.1,12.7,13.6,10.8,8.0,2.7,3.1,7.2,5.8,7.7,9.6,8.3,4.2,2.5,3.4,6.7,5.4,4.8,5.5,5.4,4.5,4.9,6.2,8.5,6.9,9.0,7.2,7.8,8.4,8.4,6.7,6.7,5.9,5.8,7.4,8.4,2.8,2.8,3.0,2.6,3.0,6.9,6.5,7.0,6.9,7.3,6.3,2.4,5.0,4.3,4.1,4.1,5.2,6.0,6.6,6.6,5.1,5.9,6.2,5.1,6.1,6.0,4.7,2.6,0.1,-1.7,3.2,5.8,5.8,4.2,2.0,4.8,4.2,3.0,5.5,5.2,6.2,5.2,6.2,4.8,5.6,5.3,6.1,8.6,5.7,4.8,5.7,4.1,4.9,6.6,4.9,3.5,6.1,5.6,4.4,4.5,5.2,4.6,5.9,7.1,7.6,5.4,4.4,4.0,2.8,6.3,4.2,4.9,7.1,6.0,3.7,4.3,3.9,3.4,5.6,8.2,6.8,5.8,4.8,4.9,5.8,6.1,4.7,4.6,4.5,1.8,0.9,3.3)
yr2020 <- c(14.1,13.7,15.7,16.0,17.3,18.1,14.4,10.1,10.5,11.1,11.3,12.9,13.4,14.5,15.1,12.8,15.0,18.1,13.7,12.9,12.7,12.6,15.4,15.8,14.8,12.9,13.3,13.3,12.6,12.5,10.3,9.9,7.3,7.9,7.4,6.0,6.5,7.3,8.5,6.9,9.2,12.1,13.9,11.2,11.9,13.0,11.3,10.7,11.1,9.9,8.8,11.8,12.3,14.6,10.9,9.7,10.0,11.1,9.0,7.2,9.1,8.2,6.7,7.0,8.2,8.6,10.5,10.6,10.4,11.0,8.3,4.5,7.2,8.5,7.4,8.1,7.1,7.5,0.8,-0.5,0.1,1.3,3.8,3.4,1.4,2.9,5.2,6.0,6.5,5.3,4.3,3.0,1.8,4.3,4.1,4.7,6.7,6.7,6.6,7.2,4.7,5.9,6.4,6.2,4.5,2.1,-0.3,1.7,3.5,3.4,3.8,4.2,4.1,2.4,1.0,1.5,0.0,0.8,1.1,0.7,0.7,0.8,1.3,3.4,2.2,1.7,-0.2,-5.1,-5.3,-1.7,0.3,2.2,4.4,5.8,5.1,2.7,1.2,1.4,0.8,1.2,-1.3,-3.2,-2.7,0.4,0.0,-1.0,-2.0,-4.6,-6.8,-6.9,-5.4,-3.6,-3.7,-4.9,-7.2,-7.4,-4.6,-4.5,-2.8,-0.2,0.9,1.6,3.0,3.3,4.2,3.7,6.1,7.3,8.3,5.2,3.8,4.2)

temp_tab_historical <- data.frame(yr2016,yr2017,yr2018,yr2019,yr2020)

root_tip_break_perc <- seq(0,100,5)
harvest_loss_tn <- c(rep(0.5,5),rep(1,4),rep(2,4),rep(3,4),rep(4,4))
harvest_loss_tab <- data.frame(root_tip_break_perc,harvest_loss_tn)

input <- data.frame(harvest_date, delivery_date, cover_date, root_yield_p, field_size, delivery_distance, delivery_cost, delivery_loads,
                    ref_temp, pol, clamp_size, moisture, factor, renhet, vol, price, data_restrict, loss_model, variety_hardness,
                    temp_clamp_model, temp_air_yr, late_moisture, harvester_cleaning, late_potent, mass_loss, root_tip_break_pc,
                    prod_data_date)
}

####################################################################

# LOCATION OF BEETS TABLE
  harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
  delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
  location <- "Clamp"
  
  # Full table of location of beets
  loc_tab <- data.frame(date_full, location)
  
  loc_tab$location[which(loc_tab$date_full <= harvest_date)] <- "Field"
  loc_tab$location[which(loc_tab$date_full == harvest_date)] <- "Harvest"
  loc_tab$location[which(loc_tab$date_full >= delivery_date)] <- "Factory"
  
  loc_tab


# LATE SEASON GROWTH
  LSG_pot <- input$late_potent
  
  LSG_pol <- rep(0.02, length(date_full))
  
  LSG_root_day <- seq(1,length(date_full)+5)
  LSG_mass_loss_pc_daily <- 1.5735e-06*LSG_root_day^2-2.8177e-04*LSG_root_day+0.01244
  LSG_mass_loss_pc_daily <- c(0.01244,LSG_mass_loss_pc_daily)
  LSG_mass_loss_pc_daily[which(LSG_mass_loss_pc_daily <= 0)] <- 0
  LSG_mass_loss_pc_daily[100:length(date_full)] <-0
  LSG_mass_loss_pc_daily <- LSG_mass_loss_pc_daily[1:length(date_full)]
  LSG_mass_loss_pc_daily <- LSG_mass_loss_pc_daily*LSG_pot
  
  LSG_mass_loss_pc_cum <- cumsum(LSG_mass_loss_pc_daily)
  LSG_pol_loss_pp_cum <- cumsum(LSG_pol)
  
  LSG_tab <- data.frame(date_full,LSG_mass_loss_pc_daily,LSG_mass_loss_pc_cum,LSG_pol_loss_pp_cum)
  
  LSG_tab


# CLAMP TEMP TABLE. Table of daily temperature inthe clamp given the chosen inputs
  # Air temp for given period taken from contant table
  temp_air <- temp_tab_historical[,input$temp_air_yr]
  
  # The minimum temperature the clamp can be
  temp_ref <- input$ref_temp
  
  # The clamp_temp = f(air_temp) model chosen
  temp_clamp_model <- input$temp_clamp_model
  
  # Clamp size
  temp_clamp_size <- (input$clamp_size - 7)/20+1
  
  # Implementation of clamp_temp = f(air_temp) model (Add new versions here and to input$temp_clamp model)
  ## basic => 3 day weighted lag, and min temp = ref temp.
  if(temp_clamp_model == 1){
    temp_clamp <- temp_air
    temp_clamp <- temp_clamp * temp_clamp_size
    temp_clamp <- WMA(temp_clamp,n=3,wts=c(0.2,0.3,0.5))
    temp_clamp <- c(temp_air[1],temp_air[1]/3+2*temp_air[2]/3,temp_clamp[-(1:2)])
    temp_clamp <- replace(temp_clamp, temp_clamp < temp_ref, temp_ref)
  }
  ## min_ref => min temp = ref_temp
  if(temp_clamp_model == 2){
    temp_clamp <- temp_air
    temp_clamp <- temp_clamp * temp_clamp_size
    temp_clamp <- replace(temp_clamp, temp_clamp < temp_ref, temp_ref)
  }
  ## air => clamp temp = air temp
  if(temp_clamp_model == 3){
    temp_clamp <- temp_air
    temp_clamp <- temp_clamp * temp_clamp_size
  }
  ## ventilation
  if(temp_clamp_model == 4){
    temp_clamp <- temp_air
    temp_clamp <- temp_clamp * temp_clamp_size
    temp_clamp <- WMA(temp_clamp,n=3,wts=c(0.2,0.3,0.5))
    temp_clamp <- c(temp_air[1],temp_air[1]/3+2*temp_air[2]/3,temp_clamp[-(1:2)])
    temp_clamp <- pmin(temp_clamp, temp_air)
    temp_clamp <- replace(temp_clamp, temp_clamp < temp_ref, temp_ref)
  }
  
  # Full table of daily temperatures compilation
  temp_tab <- data.frame(date_full, temp_air, temp_clamp)
  
  # Make the full table the output
  temp_tab


# SUGAR LOSS FACTOR
# Get model factor



# SUGAR LOSS MODEL TABLE
  # Write ref_medel series
  if(input$loss_model == 1) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_discont 
  if(input$loss_model == 2) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_linear 
  if(input$loss_model == 3) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_quad
  
  
  # Write actual loss rate as function of "damage factor"
  ref_loss_data$clamp_pol_loss_pc_cum <- ref_loss_data$ref_medel*factor
  
  #Write table
  loss_tab <- ref_loss_data

  
# MASS LOSS
  mass_loss_p <- input$mass_loss
  
  cum_temp <- seq(1,1000)
  cum_temp <- c(0,cum_temp)
  
  clamp_mass_loss_pc_cum <- mass_loss_p * cum_temp
  
  mass_loss_tab <- data.frame(cum_temp, clamp_mass_loss_pc_cum)
  
  mass_loss_tab
  

# HARVEST LOSS
  harvest_loss_p <- input$root_tip_break_perc
  
  harvest_loss_tn_summary <- harvest_loss_tab$harvest_loss_tn[which(harvest_loss_tab$root_tip_break_perc == harvest_loss_p)]
  
  summary_harvest_loss <- matrix(c(harvest_loss_tn_summary))
  colnames(summary_harvest_loss) <- c("Harvest loss (tn/ha)")
  
  summary_harvest_loss


# Delivery cost table
  


# FULL RESULTS TABLE

  # input from all previous tables
  temp_tab_p <- data.frame(temp_tab)
  loss_tab_p <- data.frame(loss_tab)
  mass_loss_tab_p <- data.frame(mass_loss_tab)
  loss_tab_p <- loss_tab_p[,c("cum_temp","clamp_pol_loss_pc_cum")]
  loc_tab_p <- data.frame(loc_tab)
  LSG_tab_p <- data.frame(LSG_tab)
  
  # input from required inputs
  harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
  delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
  cover_date <- as.POSIXct(input$cover_date, tz = "UTC", format = "%Y-%m-%d")
  day0 <-  as.POSIXct(input$prod_data_date, tz = "UTC", format = "%Y-%m-%d")
  kr_tonne <- input$price
  renhet <- input$renhet/100
  pol_p <- input$pol
  root_yield_p <- input$root_yield
  vol <- input$vol
  field_size <- input$field_size
  root_harvest <- root_yield_p*field_size
  root_tip_break_pc_p <- input$root_tip_break_pc
  
  # calculate a few key parameters
  days_h_s <- round(as.numeric(difftime(delivery_date, harvest_date, units="days")+1))
  days_p_h <- round(as.numeric(difftime(last_day, harvest_date, units="days")+1))
  
  # bind the foundation of the table together
  temp_clamp_p <- data.frame(date_full,temp_tab_p$temp_clamp)
  names(temp_clamp_p)[2] <- "temp_clamp_p"
  full_tab <- merge(price_tab_contract, temp_clamp_p, by="date_full")
  full_tab <- merge(full_tab, loc_tab_p, by="date_full")
  
  # start adding columns
  ## Cumulative temp
  full_tab$cum_temp <- c(rep(0, (days_full - days_p_h)), cumsum(full_tab$temp_clamp_p[which(full_tab$date_full>=harvest_date)]))
  full_tab$cum_temp <- round(full_tab$cum_temp)
  
  # POL LOSS
  ## Clamp Pol Loss for given temp 
  full_tab <- merge(full_tab, loss_tab_p, by="cum_temp")
  full_tab <- full_tab[,c("date_full","location","price_early","price_late","price_TT","price_vol","temp_clamp_p","cum_temp","clamp_pol_loss_pc_cum")]
  full_tab$clamp_pol_loss_pp_cum <- full_tab$clamp_pol_loss_pc_cum*pol_p/100
  clamp_pol_loss_pp_cum_ref <- full_tab$clamp_pol_loss_pp_cum[which(full_tab$date_full==day0)]*(-1)
  full_tab$clamp_pol_loss_pp_rel_day0 <- clamp_pol_loss_pp_cum_ref+full_tab$clamp_pol_loss_pp_cum
  
  ## Late Season Growth Pol gain
  full_tab <- merge(full_tab, LSG_tab_p, by="date_full")
  full_tab$LSG_pol_loss_pp_cum <- ifelse(full_tab$date_full <= harvest_date, full_tab$LSG_pol_loss_pp_cum, full_tab$LSG_pol_loss_pp_cum[which(full_tab$date_full == harvest_date)])
  full_tab$LSG_pol_loss_pp_cum <-   full_tab$LSG_pol_loss_pp_cum * (-1)
  LSG_pol_loss_pp_cum_ref <- full_tab$LSG_pol_loss_pp_cum[which(full_tab$date_full==day0)]*(-1)
  full_tab$LSG_pol_loss_pp_rel_day0 <- LSG_pol_loss_pp_cum_ref+full_tab$LSG_pol_loss_pp_cum
  
  ## Total daily pol change (pp)
  full_tab$pol_loss_pp_cum <- full_tab$clamp_pol_loss_pp_cum + full_tab$LSG_pol_loss_pp_cum
  full_tab$pol_loss_pp_cum <- full_tab$pol_loss_pp_cum - full_tab$pol_loss_pp_cum[which(full_tab$date_full==harvest_date)]
  full_tab$pol_loss_pp_rel_day0 <- full_tab$clamp_pol_loss_pp_rel_day0 + full_tab$LSG_pol_loss_pp_rel_day0
  full_tab$pol_cum <- pol_p - full_tab$pol_loss_pp_rel_day0
  full_tab$pol_loss_pc_cum <- full_tab$pol_loss_pp_cum / full_tab$pol_cum[which(full_tab$date_full==harvest_date)]
  
  ## Pol factor across whole period
  full_tab$pol_factor <- (full_tab$pol_cum - ref_pol*100)*kr_pol
  
  # MASS LOSS
  ## Clamp Mass Loss for given temp
  full_tab <- merge(full_tab, mass_loss_tab_p, by="cum_temp")
  full_tab$clamp_mass_loss_kg_cum <- full_tab$clamp_mass_loss_pc_cum*root_yield_p/100
  clamp_mass_loss_kg_cum_ref <- full_tab$clamp_mass_loss_kg_cum[which(full_tab$date_full==day0)]
  full_tab$clamp_mass_loss_kg_rel_day0 <- full_tab$clamp_mass_loss_kg_cum - clamp_mass_loss_kg_cum_ref
  
  ## Mass loss at harvest
  harvest_loss <- harvest_loss_tab$harvest_loss_tn[which(harvest_loss_tab$root_tip_break_perc == root_tip_break_pc_p)]
  
  ## Mass gain under late season growth
  full_tab$LSG_mass_loss_pc_cum <- ifelse(full_tab$date_full <= harvest_date, full_tab$LSG_mass_loss_pc_cum, full_tab$LSG_mass_loss_pc_cum[which(full_tab$date_full == harvest_date)])
  full_tab$LSG_mass_loss_kg_cum <- full_tab$LSG_mass_loss_pc_cum*root_yield_p/100
  LSG_mass_loss_kg_cum_ref <- full_tab$LSG_mass_loss_kg_cum[which(full_tab$date_full==day0)]
  full_tab$LSG_mass_loss_kg_rel_day0 <- full_tab$LSG_mass_loss_kg_cum - LSG_mass_loss_kg_cum_ref
  
  ## Total daily mass change (kg)
  full_tab$mass_loss_kg_rel_day0 <- full_tab$clamp_mass_loss_kg_rel_day0 + full_tab$LSG_mass_loss_kg_rel_day0
  full_tab$mass_kg_cum <- root_yield_p - full_tab$mass_loss_kg_rel_day0
  
  ## SUGAR YIELD
  full_tab$cum_sug <- full_tab$pol_cum/100*full_tab$cum_mass
  
  #TT bonus
  full_tab$price_TT[full_tab$date_full < as.POSIXct(cover_date)+7] <- 0
  
  #Volume bonus
  if (vol == T) full_tab$price_vol = kr_vol
  
  #renhet bonus
  renhet_diff <- (renhet - ref_renhet)*100
  price_renhet <- renhet_diff * kr_renhet
  
  # Clean prices
  full_tab$price_base_clean <- kr_tonne+kr_tonne*full_tab$pol_factor/100
  full_tab$price_bonus_clean <- (full_tab$price_early + full_tab$price_late + full_tab$price_TT + full_tab$price_vol + price_renhet)
  full_tab$price_clean <- full_tab$price_base_clean +  full_tab$price_bonus_clean
  
  # Delivered prices
  full_tab$price_base_delivered <- full_tab$price_base_clean*renhet
  full_tab$price_bonus_delivered <- full_tab$price_bonus_clean*renhet
  full_tab$price_delivered <- full_tab$price_clean*renhet
  
  # Ha prices
  full_tab$price_base_ha <- full_tab$price_base_delivered*full_tab$cum_mass
  full_tab$price_bonus_ha <- full_tab$price_bonus_delivered*full_tab$cum_mass
  full_tab$price_ha <- full_tab$price_delivered*full_tab$cum_mass
  
  # Field prices
  full_tab$price_base_field <- full_tab$price_base_ha*field_size
  full_tab$price_bonus_field <- full_tab$price_bonus_ha*field_size
  full_tab$price_field <- full_tab$price_ha*field_size
  
  full_tab
  

# SUMMARY (VISUALISED) RESULTS
##!!! This should really just restrict the full results table within the parameters given.

  # input from required previous tables
  summary_tab <- data.frame(full_tab)
  
  # input from required inputs
  summary_tab_cols <- input$summary_tab_show
  summary_tab_start <- input$data_restrict_start
  summary_tab_end <- input$data_restrict_end
  harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
  delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
  
  # Define summary table - columns
  summary_tab_show <- c("date_full", "location", "temp_clamp_p", "cum_temp", "pol_loss_pp_rel_day0", "pol_cum","cum_mass","cum_sug")
  if("CL" %in% summary_tab_cols) summary_tab_show <- c(summary_tab_show, "price_base_clean","price_bonus_clean","price_clean")
  if("DE" %in% summary_tab_cols) summary_tab_show <- c(summary_tab_show, "price_base_delivered","price_bonus_delivered","price_delivered")
  if("HA" %in% summary_tab_cols) summary_tab_show <- c(summary_tab_show, "price_base_ha","price_bonus_ha","price_ha")
  if("FI" %in% summary_tab_cols) summary_tab_show <- c(summary_tab_show, "price_base_field","price_bonus_field","price_field")
  summary_tab <- summary_tab[, summary_tab_show]
  # Define summary table - length (ie rows)
  if(summary_tab_start) first_day <- harvest_date
  if(summary_tab_end) last_day <- delivery_date
  summary_tab <- summary_tab[which(summary_tab$date_full >= first_day),]
  summary_tab <- summary_tab[which(summary_tab$date_full <= last_day),]
  
  # Extract a little info from summary table for later graphing
  delivery_date <<- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
  harvest_date <<- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
  cum_loss_delivery <<- summary_tab$cum_temp[summary_tab$date_full==delivery_date]
  loss_max <- max(summary_tab$pol_loss_pc_cum)
  pol_max <- max(summary_tab$pol_cum)
  pol_min <- min(summary_tab$pol_cum)
  pol_diff <- pol_max - pol_min
  amplify_factor <- 0.5
  amplify <<- loss_max/pol_diff*amplify_factor
  move <<- pol_max*amplify - loss_max*0.85
  
  summary_tab

summary_tab_output = reactive({
  summary_tab_output <- data.frame(summary_tab())
  
  # input from required inputs
  summary_tab_cols <- input$summary_tab_show
  
  summary_tab_names <- c("Date", "Location", "Temperature (C)", "Cum. Temp (Cd)", "Cum. % loss", "Pol","Root Yield","Sugar Yield")
  if("CL" %in% summary_tab_cols) summary_tab_names <- c(summary_tab_names, "Base price - clean tn","Bonus - clean tn","Payment - clean tn")
  if("DE" %in% summary_tab_cols) summary_tab_names <- c(summary_tab_names, "Base price - delivered tn","Bonus - delivered tn","Payment - delivered tn")
  if("HA" %in% summary_tab_cols) summary_tab_names <- c(summary_tab_names, "Base price - ha","Bonus - ha","Payment - ha")
  if("FI" %in% summary_tab_cols) summary_tab_names <- c(summary_tab_names, "Base price - field","Bonus - field","Payment - field")
  
  #Formatting
  summary_tab_output$date_full <- format(summary_tab_output$date_full,'%Y-%m-%d')
  
  colnames(summary_tab_output) <- summary_tab_names
  
  summary_tab_output
  
})

# Summary Table of the bottom line
summary_final_tab = reactive({
  delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
  summary_final_tab <- data.frame(full_tab())
  summary_final_tab <- summary_final_tab[which(summary_final_tab$date_full == delivery_date),]
  summary_final_tab <- matrix(summary_final_tab[c("price_base_ha","price_bonus_ha","price_ha",
                                                  "price_base_field","price_bonus_field","price_field",
                                                  "price_base_delivered","price_bonus_delivered","price_delivered",
                                                  "price_base_clean","price_bonus_clean","price_clean")],
                              byrow=T,nrow=4)
  rownames(summary_final_tab) <- c("Hectare", "Field","Tonne - delivered", "Tonne - clean")
  colnames(summary_final_tab) <- c("Base price","Bonuses","Total payment")
  
  summary_final_tab
  
})

# Root harvest yield table (CAN THIS SIT IN THE FULL_TAB REACTIVE?)
full_tab_p <- full_tab
root_yield_p <- input$root_yield 
field_size_p <- input$field_size
harvest_date_p <- input$harvest_date

root_mass_harvest <- full_tab_p$cum_mass[which(full_tab_p$date_full == as.POSIXct(harvest_date_p))] 
root_mass_grown <- full_tab_p$cum_mass[which(full_tab_p$date_full == (as.POSIXct(harvest_date_p) - 86400))]

root_mass_factory_field <- root_yield_p*field_size_p
root_mass_harvest_field <- root_mass_harvest*field_size_p
root_mass_grown_field <- root_mass_grown*field_size_p

root_harvest_tab <- matrix(c(root_mass_grown, root_mass_harvest, root_yield_p,
                             root_mass_grown_field, root_mass_harvest_field, root_mass_factory_field), byrow=F, nrow=3) 
colnames(root_harvest_tab) <- c("Ha","Field")
rownames(root_harvest_tab) <- c("Grown","Harvested", "Factory")

root_harvest_tab


###############
# VISUALS

#Summary root harvest
output$root_harvest_tab <- renderTable({
  root_harvest_tab()
}, rownames = T)

# Summary price table
output$summary_tab_output = renderTable({
  summary_tab_output()
}, digits=1)

# Summary outputs table
output$summary_final_tab = renderTable({
  summary_final_tab()
},rownames = T, digits=0)



## CHARTS

# Late season growth
#output$LSG_chart
  ggplot(LSG_tab(), aes(x=date_full)) + 
    geom_line(aes(y = LSG_root_daily), color = "darkred") + 
    ylab("Daily growth (%)") + 
    xlab("Date") +
    labs(title = "Late Season Growth Potential")


# Plot of temperatures (Air and Clamp)
output$temp_graph <- plotly::renderPlotly({
  
  ggplot(temp_tab(), aes(x=date_full)) + 
    geom_line(aes(y = temp_clamp), color = "darkred") + 
    geom_line(aes(y = temp_air), color="steelblue", linetype="twodash") +
    ylab("Temperature (C)") + 
    xlab("Date") +
    labs(title = "Clamp temperature model")
})

# Plot of sugar loss
output$loss_Cd <- plotly::renderPlotly({
  
  ggplot(loss_tab(), aes(x=cum_temp)) + 
    geom_line(aes(y = clamp_pol_loss_pc_cum), color = "darkred") + 
    geom_line(aes(y = ref_medel), color="steelblue", linetype="twodash") +
    xlim(0,500) +
    ylim(0,15) +
    ylab("Sugar loss (%)") + 
    xlab("Accumulated temperature (Cd)") +
    labs(title = "Storage loss model")
})


output$summary_graph_temp <- plotly::renderPlotly({
  ggplot(summary_tab(), aes(x=date_full)) + 
    geom_line(aes(y=cum_temp, color = "Cum. temperature")) +
    geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
    geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
    geom_hline(yintercept = cum_loss_delivery, linetype="dotted") +
    scale_colour_manual("", 
                        breaks = c("Cum. temperature"),
                        values = c("Cum. temperature"="red3")) +
    ylab("Cumulative temperature (Cd)") + 
    xlab("Date") +
    labs(title = "CLAMP TEMPERATURE") +
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})

output$summary_graph_pol <- plotly::renderPlotly({
  ggplot(summary_tab(), aes(x=date_full)) + 
    geom_line(aes(y = pol_loss_pp_cum, color = "Cum. % loss")) + 
    geom_line(aes(y = pol_cum * amplify - move, color = "Pol")) +
    geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
    geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
    scale_y_continuous(sec.axis = sec_axis(~(. + move) / amplify, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Cum. % loss", "Pol"),
                        values = c("Cum. % loss"="red3", "Pol"="blue3")) +
    ylab("Sugar loss (%)") + 
    xlab("Date") +
    labs(title = "SUGAR CONTENT") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})

output$summary_graph_mass <- plotly::renderPlotly({
  ggplot(summary_tab(), aes(x=date_full)) + 
    geom_line(aes(y=cum_mass, color = "Cum. mass")) +
    geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
    geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
    scale_colour_manual("", 
                        breaks = c("Cum. mass"),
                        values = c("Cum. mass"="red3")) +
    ylab("Root yield (tn)") + 
    xlab("Date") +
    labs(title = "ROOT YIELD") +
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})

output$summary_graph_sug <- plotly::renderPlotly({
  ggplot(summary_tab(), aes(x=date_full)) + 
    geom_line(aes(y=cum_sug, color = "Cum. sug")) +
    geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
    geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
    scale_colour_manual("", 
                        breaks = c("Cum. sug"),
                        values = c("Cum. sug"="red3")) +
    ylab("Sugar yield (tn)") + 
    xlab("Date") +
    labs(title = "SUGAR YIELD") +
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})

output$summary_graph_price_ha <- plotly::renderPlotly({
  ggplot(full_tab(), aes(x=date_full)) + 
    geom_line(aes(y = price_ha, colour = "Total payment")) +
    geom_line(aes(y = price_base_ha, colour = "Base payment")) + 
    geom_line(aes(y = price_bonus_ha, colour = "Bonus payment")) +
    geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
    geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Price (kr)") + 
    xlab("Date") +
    labs(title = "INCOME PER HECTARE") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})

output$summary_graph_price_fi <- plotly::renderPlotly({
  ggplot(full_tab(), aes(x=date_full)) + 
    geom_line(aes(y = price_field, colour = "Total payment")) +
    geom_line(aes(y = price_base_field, colour = "Base payment")) + 
    geom_line(aes(y = price_bonus_field, colour = "Bonus payment")) +
    geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
    geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Price (kr)") + 
    xlab("Date") +
    labs(title = "INCOME PER FIELD") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})

#output$summary_graph_price_cl
  ggplot(full_tab(), aes(x=date_full)) + 
    geom_line(aes(y = price_clean, colour = "Total payment")) +
    geom_line(aes(y = price_base_clean, colour = "Base payment")) + 
    geom_line(aes(y = price_bonus_clean, colour = "Bonus payment")) +
    geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
    geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Price (kr)") + 
    xlab("Date") +
    labs(title = "INCOME PER CLEAN TONNE") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  

#output$summary_graph_price_de
  ggplot(full_tab(), aes(x=date_full)) + 
    geom_line(aes(y = price_delivered, colour = "Total payment")) +
    geom_line(aes(y = price_base_delivered, colour = "Base payment")) + 
    geom_line(aes(y = price_bonus_delivered, colour = "Bonus payment")) +
    geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
    geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Price (kr)") + 
    xlab("Date") +
    labs(title = "INCOME PER DELIVERED TONNE") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")

