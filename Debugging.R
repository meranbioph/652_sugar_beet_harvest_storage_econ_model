####################################################################
#
# DEBUGGING CODE
#
####################################################################

{
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
root_yield <-100
temp_air_yr <- "yr2020"
temp_clamp_model <- 1
variety_hardness <- 50
vol <- F

input <- data.frame(harvest_date, delivery_date, cover_date, root_yield, field_size, delivery_distance, delivery_cost, delivery_loads,
                    ref_temp, pol, clamp_size, moisture, factor, renhet, vol, price, data_restrict, loss_model, variety_hardness,
                    temp_clamp_model, temp_air_yr, late_moisture, harvester_cleaning)
}

####################################################################

# Root harvest yield table 

  root_harvest <- input$root_yield * input$field_size
  root_harvest_tab <- matrix(c(root_harvest), byrow=F, nrow=1) 
  colnames(root_harvest_tab) <- c("Root harvest (tonne)")

# TEMP TABLE. Table of daily temperature given the chosen inputs

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
  # Full table of daily temperatures compilation
  temp_tab <- data.frame(date_full, temp_air, temp_clamp)


# LOSS FACTOR
# Get model factor
  data_late_moisture <- (input$late_moisture * 0.5)
  example.1 <- fuzzy_inference(model, list(harvester_cleaning = input$harvester_cleaning, 
                                           late_moisture = data_late_moisture,
                                           variety  = input$variety_hardness))
  # get the "damage factor"
  factor <- (gset_defuzzify(example.1, "centroid")/100+0.5)


# LOSS MODEL TABLE
  # Write ref_medel series
  if(input$loss_model == 1) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_discont 
  if(input$loss_model == 2) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_linear 
  if(input$loss_model == 3) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_quad
  
  # Write actual loss rate as function of "damage factor"
  ref_loss_data$actual <- ref_loss_data$ref_medel*factor
  loss_tab <- ref_loss_data
  
# Delivery cost table
  root_harvest <- input$root_yield * input$field_size
  delivery_cost_field_clean <- input$delivery_cost / (input$renhet/100)
  delivery_cost_field_soil <- input$delivery_cost * (1 - (input$renhet/100))
  delivery_tn_truck <- root_harvest / input$delivery_loads
  delivery_cost_truck <- input$delivery_cost / input$delivery_loads
  delivery_cost_truck_clean <- delivery_cost_truck / (input$renhet/100)
  delivery_cost_truck_soil <- delivery_cost_truck* (1 - (input$renhet/100))
  delivery_tn_mil <- input$root_yield / input$delivery_distance
  delivery_cost_mil <- input$delivery_cost / input$delivery_distance
  delivery_cost_mil_clean <- delivery_cost_mil / (input$renhet/100)
  delivery_cost_mil_soil <- delivery_cost_mil * (1 - (input$renhet/100))
  delivery_cost_tn <- input$delivery_cost / root_harvest
  delivery_cost_tn_clean <- delivery_cost_tn / (input$renhet/100)
  delivery_cost_tn_soil <- delivery_cost_tn * (1 - (input$renhet/100))
  delivery_cost_tab <- matrix(c(root_harvest, input$delivery_cost, delivery_cost_field_clean, delivery_cost_field_soil,
                                delivery_tn_truck, delivery_cost_truck, delivery_cost_truck_clean, delivery_cost_truck_soil,
                                delivery_tn_mil, delivery_cost_mil, delivery_cost_mil_clean, delivery_cost_mil_soil,
                                1, delivery_cost_tn, delivery_cost_tn_clean, delivery_cost_tn_soil), byrow=T, nrow=4) 
  rownames(delivery_cost_tab) <- c("Field", "Truck", "Mil", "Tonne")
  colnames(delivery_cost_tab) <- c("Tonnes per...","Cost per...","Cost per clean tonne", "Cost of soil transport")

# FULL RESULTS TABLE

  # input from all previous tables
  temp_tab_p <- data.frame(temp_tab)
  loss_tab_p <- data.frame(loss_tab)
  loss_tab_p <- loss_tab_p[,c("cum_temp","actual")]
  root_harvest_tab_p <- data.frame(root_harvest_tab)
  
  # input from required inputs
  harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
  delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
  cover_date <- as.POSIXct(input$cover_date, tz = "UTC", format = "%Y-%m-%d")
  kr_tonne <- input$price
  renhet <- input$renhet/100
  pol <- input$pol
  root_yield <- input$root_yield
  vol <- input$vol
  
  # calculate a few key parameters
  days_h_s <- round(as.numeric(difftime(delivery_date, harvest_date, units="days")+1))
  days_p_h <- round(as.numeric(difftime(last_day, harvest_date, units="days")+1))
  
  # bind the foundation of the table together
  temp_clamp_p <- data.frame(date_full,temp_tab_p$temp_clamp)
  names(temp_clamp_p)[2] <- "temp_clamp_p"
  price_tab <- merge(price_tab_contract, temp_clamp_p, by="date_full")
  
  # start adding columns
  ## Cumulative temp
  price_tab$cum_temp <- c(rep(0, (days_full - days_p_h)), cumsum(price_tab$temp_clamp_p[which(price_tab$date_full>=harvest_date)]))
  price_tab$cum_temp <- round(price_tab$cum_temp)
  ## Loss for given temp 
  price_tab <- merge(price_tab, loss_tab_p, by="cum_temp")
  names(price_tab)[names(price_tab)=="actual"] <- "cum_percent_loss"
  price_tab <- price_tab[,c("date_full","price_early","price_late","price_TT","price_vol","temp_clamp_p","cum_temp","cum_percent_loss")]
  ## 
  cum_percent_loss_max <- max(price_tab$cum_percent_loss[which(price_tab$date_full<=delivery_date)])
  price_tab$cum_sug <- pol + pol*(cum_percent_loss_max/100) - (price_tab$cum_percent_loss/100)*pol
  price_tab$pol_factor <- (price_tab$cum_sug - ref_pol*100)*kr_pol
  
  #TT bonus
  price_tab$price_TT[price_tab$date_full < as.POSIXct(cover_date)+7] <- 0
  
  #Volume bonus
  if (vol == T) price_tab$price_vol = kr_vol
  
  #renhet bonus
  renhet_diff <- (renhet - ref_renhet)*100
  price_renhet <- renhet_diff * kr_renhet
  
  # Clean prices
  price_tab$price_base_clean <- kr_tonne+kr_tonne*price_tab$pol_factor/100
  price_tab$price_bonus_clean <- (price_tab$price_early + price_tab$price_late + price_tab$price_TT + price_tab$price_vol + price_renhet)
  price_tab$price_clean <- price_tab$price_base_clean +  price_tab$price_bonus_clean
  
  # Delivered prices
  price_tab$price_base_delivered <- price_tab$price_base_clean*renhet
  price_tab$price_bonus_delivered <- price_tab$price_bonus_clean*renhet
  price_tab$price_delivered <- price_tab$price_clean*renhet
  
  # Ha prices
  price_tab$price_base_ha <- price_tab$price_base_delivered*root_yield
  price_tab$price_bonus_ha <- price_tab$price_bonus_delivered*root_yield
  price_tab$price_ha <- price_tab$price_delivered*root_yield
  
  # Field prices
  price_tab$price_base_field <- price_tab$price_base_ha*field_size
  price_tab$price_bonus_field <- price_tab$price_bonus_ha*field_size
  price_tab$price_field <- price_tab$price_ha*field_size
  
# SUMMARY (VISUALISED) RESULTS
##!!! This should really just restrict the full results table within the parameters given.

  # input from required previous tables
  summary_tab <- data.frame(price_tab)
  
  # input from required inputs
  summary_tab_cols <- input$summary_tab_show
  summary_tab_length <- input$data_restrict
  
  # Define summary table - columns
  summary_tab_show <- c("date_full", "temp_clamp_p", "cum_temp", "cum_percent_loss", "cum_sug", "pol_factor","price_base_clean","price_bonus_clean","price_clean")
  if("DE" %in% summary_tab_cols) summary_tab_show <- c(summary_tab_show, "price_base_delivered","price_bonus_delivered","price_delivered")
  if("HA" %in% summary_tab_cols) summary_tab_show <- c(summary_tab_show, "price_base_ha","price_bonus_ha","price_ha")
  if("FI" %in% summary_tab_cols) summary_tab_show <- c(summary_tab_show, "price_base_field","price_bonus_field","price_field")
  summary_tab <- summary_tab[, summary_tab_show]
  # Define summary table - length (ie rows)
  if(summary_tab_length) last_day <- delivery_date
  summary_tab <- summary_tab[which(summary_tab$date_full >= harvest_date),]
  summary_tab <- summary_tab[which(summary_tab$date_full <= last_day),]
  
  # Extract a little info from summary table for later graphing
  cum_loss_delivery <<- summary_tab$cum_temp[summary_tab$date_full==delivery_date]
  loss_max <- max(summary_tab$cum_percent_loss)
  pol_max <- max(summary_tab$cum_sug)
  pol_min <- min(summary_tab$cum_sug)
  pol_diff <- pol_max - pol_min
  amplify_factor <- 0.5
  amplify <<- loss_max/pol_diff*amplify_factor
  move <<- pol_max*amplify - loss_max*0.85

# Summary Table Output  
  summary_tab_output <- summary_tab
  
  summary_tab_names <- c("Date", "Temperature (C)", "Cum. Temp (Cd)", "Cum. % loss", "Pol", "Pol factor","Base price - clean tn","Bonus - clean tn","Payment - clean tn")
  if("DE" %in% summary_tab_cols) summary_tab_names <- c(summary_tab_names, "Base price - delivered tn","Bonus - delivered tn","Payment - delivered tn")
  if("HA" %in% summary_tab_cols) summary_tab_names <- c(summary_tab_names, "Base price - ha","Bonus - ha","Payment - ha")
  if("FI" %in% summary_tab_cols) summary_tab_names <- c(summary_tab_names, "Base price - field","Bonus - field","Payment - field")
  colnames(summary_tab_output) <- summary_tab_names
  
# Summary Table of the bottom line
  summary_final_tab <- data.frame(price_tab)
  summary_final_tab <- summary_final_tab[which(summary_final_tab$date_full == delivery_date),]
  summary_final_tab <- matrix(summary_final_tab[c("price_base_delivered","price_bonus_delivered","price_delivered",
                                                  "price_base_clean","price_bonus_clean","price_clean",
                                                  "price_base_ha","price_bonus_ha","price_ha",
                                                  "price_base_field","price_bonus_field","price_field")],
                              byrow=T,nrow=4)
  rownames(summary_final_tab) <- c("Tonne - delivered", "Tonne - clean", "Hectare", "Field")
  colnames(summary_final_tab) <- c("Base price","Bonuses","Total payment")
  
###############
# VISUALS

# Plot of temperatures (Air and Clamp)
  ggplot(temp_tab, aes(x=date_full)) + 
    geom_line(aes(y = temp_clamp), color = "darkred") + 
    geom_line(aes(y = temp_air), color="steelblue", linetype="twodash") +
    ylab("Temperature (C)") + 
    xlab("Date")

# Plot of sugar loss
  ggplot(loss_tab, aes(x=cum_temp)) + 
    geom_line(aes(y = actual), color = "darkred") + 
    geom_line(aes(y = ref_medel), color="steelblue", linetype="twodash") +
    xlim(0,500) +
    ylim(0,15) +
    ylab("Sugar loss (%)") + 
    xlab("Accumulated temperature (Cd)")

# Plot of cumulative temperature
  ggplot(summary_tab, aes(x=date_full)) + 
    geom_line(aes(y=cum_temp)) +
    geom_vline(xintercept = as.POSIXct(delivery_date), linetype="dotted") +
    geom_hline(yintercept = cum_loss_delivery, linetype="dotted") +
    scale_colour_manual("", 
                        breaks = c("Cum. temperature"),
                        values = c("Cum. temperature"="red3")) +
    ylab("Cumulative temperature (Cd)") + 
    xlab("Date") +
    labs(title = "Temperature") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")

# Plot of cum loss
  ggplot(summary_tab, aes(x=date_full)) + 
    geom_line(aes(y = cum_percent_loss, color = "Cum. % loss")) + 
    geom_line(aes(y = cum_sug * amplify - move, color = "Pol")) +
    geom_vline(xintercept = as.POSIXct(delivery_date)) +
    scale_y_continuous(sec.axis = sec_axis(~(. + move) / amplify, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Cum. % loss", "Pol"),
                        values = c("Cum. % loss"="red3", "Pol"="blue3")) +
    ylab("Sugar loss (%)") + 
    xlab("Date") +
    labs(title = "Sugar loss") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")

# Plot of price series
  ggplot(summary_tab, aes(x=date_full)) + 
    geom_line(aes(y = price_clean, colour = "Total payment")) +
    geom_line(aes(y = price_base_clean, colour = "Base payment")) + 
    geom_line(aes(y = price_bonus_clean, colour = "Bonus payment")) +
    geom_vline(xintercept = as.POSIXct(delivery_date)) +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Price (kr)") + 
    xlab("Date") +
    labs(title = "Price per clean tonne") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")

# Plot of price series - delivered
  ggplot(summary_tab, aes(x=date_full)) + 
    geom_line(aes(y = price_delivered, colour = "Total payment")) +
    geom_line(aes(y = price_base_delivered, colour = "Base payment")) + 
    geom_line(aes(y = price_bonus_delivered, colour = "Bonus payment")) +
    geom_vline(xintercept = as.POSIXct(delivery_date)) +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Price (kr)") + 
    xlab("Date") +
    labs(title = "Price per delivered tonne") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")


para_tab = reactive({
  para_tab_factor <- factor()*0.018
  para_tab <- data.frame(input$pol,input$renhet, input$delivery_date, para_tab_factor, input$root_yield*input$field_size)
  colnames(para_tab) <- c("Pol", "Cleanness", "Delivery date", "Storage loss rate (%/Cd)", "Harvest")
  para_tab
})

observeEvent(input$comp_1, {
  comp_1_tab$data <- summary_tab()
  comp_1_tab$para <- para_tab()
  comp_1_tab$payment <- price_tab()
})

observeEvent(input$comp_2, {
  comp_2_tab$data <- summary_tab()
  comp_2_tab$para <- para_tab()
  comp_2_tab$payment <- price_tab()
})

output$comp_1_tab = renderTable({
  comp_1_tab$data
})

output$comp_1_para = renderTable({
  comp_1_tab$para
})

output$comp_2_tab = renderTable({
  comp_2_tab$data
})

output$comp_2_para = renderTable({
  comp_2_tab$para
})

# Summary price  
output$summary_tab = renderTable({
  summary_tab()
},rownames = T, digits=0)

# Summary outputs  
output$summary_final_tab = renderTable({
  summary_final_tab()
})

output$summary_graph_price_comp_1 = renderPlot({
  ggplot(comp_1_tab$data, aes(x=get("Date"))) + 
    geom_line(aes(y = get("Payment - clean tn"), colour = "Total payment")) +
    geom_line(aes(y = get("Base price - clean tn"), colour = "Base payment")) + 
    geom_line(aes(y = get("Bonus - clean tn"), colour = "Bonus payment")) +
    geom_vline(xintercept = as.POSIXct(delivery_date)) +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Price (kr)") + 
    xlab("Date") +
    labs(title = "Price per clean tonne") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})

output$summary_graph_payment_comp_1 = renderPlot({
  ggplot(comp_1_tab$payment, aes(x=date_full)) + 
    geom_line(aes(y = price_field, colour = "Total payment")) +
    geom_line(aes(y = price_base_field, colour = "Base payment")) + 
    geom_line(aes(y = price_bonus_field, colour = "Bonus payment")) +
    geom_vline(xintercept = as.POSIXct(delivery_date)) +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Payment (kr)") + 
    xlab("Date") +
    labs(title = "Payment per field") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})

output$summary_graph_price_comp_2 = renderPlot({
  ggplot(comp_2_tab$data, aes(x=get("Date"))) + 
    geom_line(aes(y = get("Payment - clean tn"), colour = "Total payment")) +
    geom_line(aes(y = get("Base price - clean tn"), colour = "Base payment")) + 
    geom_line(aes(y = get("Bonus - clean tn"), colour = "Bonus payment")) +
    geom_vline(xintercept = as.POSIXct(delivery_date)) +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Price (kr)") + 
    xlab("Date") +
    labs(title = "Price per clean tonne") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})

output$summary_graph_payment_comp_2 = renderPlot({
  ggplot(comp_2_tab$payment, aes(x=date_full)) + 
    geom_line(aes(y = price_field, colour = "Total payment")) +
    geom_line(aes(y = price_base_field, colour = "Base payment")) + 
    geom_line(aes(y = price_bonus_field, colour = "Bonus payment")) +
    geom_vline(xintercept = as.POSIXct(delivery_date)) +
    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
    scale_colour_manual("", 
                        breaks = c("Total payment", "Base payment", "Bonus payment"),
                        values = c("Total payment"="red3", "Base payment"="blue3", 
                                   "Bonus payment"="green3")) +
    ylab("Payment (kr)") + 
    xlab("Date") +
    labs(title = "Payment per field") + 
    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
})
