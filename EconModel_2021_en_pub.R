########################################
#
# Swedish sugar beets economic model
# Will English, 2021-03-18
# FIXES: Date in Summary Table. Per truck, per ha into the payment schedule.
#
########################################

library(shiny)
library(plotly)
library(sets)
library(ggplot2)
library(reshape2)
library(TTR)
library(shinyWidgets)


###############################################
# ---------------------------------------------
# THE MODEL
# ---------------------------------------------
###############################################

###############################################
#
# Model coefficients
#
# Building the model components, 
#  framework and base data
#
###############################################

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

# Create temperature series to test the model on
yr2016 <- c(16.8,18.2,17.0,19.8,19.4,18.4,17.6,14.7,12.8,13.9,15.1,13.3,12.5,14.7,14.5,15.1,15.8,14.2,17.2,16.4,13.8,13.6,12.1,11.7,8.3,7.2,8.9,9.3,8.2,7.7,8.2,7.8,7.9,7.9,8.7,9.4,7.1,8.4,8.7,10.0,8.8,7.9,6.0,6.1,7.2,6.6,5.3,10.4,10.7,8.7,6.5,8.3,9.5,4.4,2.1,5.8,6.6,3.3,-0.7,-2.2,0.5,-0.4,-1.1,-2.3,-0.7,3.0,5.7,6.4,6.3,6.9,6.0,6.4,9.2,8.1,6.9,4.8,4.7,7.0,3.4,-0.6,4.3,6.0,6.7,1.7,0.1,3.4,5.2,1.2,3.3,9.3,7.9,8.7,6.9,0.7,4.6,1.3,-0.5,1.7,1.9,5.0,3.9,3.1,3.0,5.0,5.5,5.4,6.1,6.4,5.4,1.1,4.1,3.1,6.0,5.5,0.2,3.0,1.1,-6.8,-7.7,-0.7,-1.0,1.7,0.8,1.7,3.1,0.8,0.1,-1.0,-2.9,-1.5,1.7,3.1,4.4,2.0,2.0,1.8,2.5,1.7,0.3,0.6,0.1,0.9,2.2,2.1,1.2,0.4,1.0,2.4,2.2,1.0,-1.9,-2.7,-3.9,-1.1,-1.2,-2.1,-5.0,-3.6,-1.5,0.9,3.1,2.7,3.8,5.6,5.9,5.7,4.0,0.3,1.1,5.1,7.2,5.5)
yr2017 <- c(14.0,14.4,15.2,13.2,12.6,13.4,12.6,10.2,10.3,13.0,12.9,11.9,10.9,14.1,15.7,15.0,15.5,15.2,14.2,14.2,12.9,12.7,12.6,13.2,11.1,10.6,11.1,10.3,8.1,9.1,9.3,12.3,12.3,12.4,14.6,13.8,13.7,13.4,12.4,11.0,11.6,11.5,10.4,8.4,8.7,12.6,11.5,9.5,10.9,7.2,4.3,6.4,11.1,9.9,7.3,9.6,10.0,7.0,3.7,5.6,7.6,7.3,6.4,5.9,1.2,5.3,6.9,6.0,6.5,5.9,3.9,-0.2,0.4,6.6,8.9,6.8,2.9,4.9,5.3,5.5,3.3,1.1,1.6,2.0,4.3,3.6,6.9,8.0,6.6,4.6,3.7,1.8,0.6,1.2,3.1,3.1,3.1,1.9,-0.5,1.8,2.6,4.0,5.6,5.1,7.9,8.6,7.5,5.8,4.4,4.1,3.9,1.8,3.6,6.2,4.9,3.9,3.5,2.7,1.0,-2.3,-0.3,2.0,3.0,3.3,2.1,0.4,-0.7,0.2,2.5,1.6,1.5,1.1,1.5,-0.6,0.3,1.1,7.0,5.8,3.8,4.3,5.8,6.3,4.7,4.0,3.4,2.2,-2.7,-1.0,-5.6,-4.8,-1.5,-0.1,-0.9,-1.2,0.5,2.6,1.2,0.4,0.2,2.3,1.6,2.0,1.2,1.0,0.6,-0.9,-2.0,-2.7,-4.5,-4.5,-6.5,-8.2)
yr2018 <- c(16.9,15.7,14.3,14.4,15.4,14.3,14.1,17.1,18.7,18.1,18.3,16.7,12.2,11.7,9.1,10.3,13.5,15.1,10.9,10.9,12.6,10.1,9.3,9.6,11.1,13.2,12.0,9.0,11.7,13.2,12.7,14.6,14.3,15.5,14.1,13.1,12.4,11.5,9.9,7.0,10.0,9.7,10.5,10.7,7.2,10.6,9.0,4.8,1.8,3.9,10.2,8.5,8.4,9.0,6.9,6.1,9.5,8.8,7.9,6.5,7.9,8.9,9.6,10.3,9.5,9.6,9.0,5.4,4.5,4.9,4.1,3.8,4.0,4.2,2.1,2.0,1.3,-2.6,-3.9,-1.1,1.9,2.7,5.2,5.5,8.2,5.8,4.0,4.1,8.3,6.8,6.1,4.7,3.7,1.5,0.1,1.2,1.5,0.6,0.1,1.8,3.5,3.6,4.4,0.6,-0.7,-1.2,5.2,5.9,7.4,6.0,6.3,4.2,3.8,6.5,1.7,1.4,4.3,2.8,0.5,4.1,4.1,0.5,-1.3,3.8,2.9,5.0,1.3,3.7,5.4,2.1,-1.1,0.0,0.1,0.7,0.3,-2.5,-2.3,-3.2,-1.5,1.1,2.3,1.0,0.6,0.9,0.8,1.5,0.8,1.3,3.9,2.8,3.7,5.0,5.5,4.6,3.0,2.4,5.9,5.8,3.4,4.6,5.9,4.7,5.1,5.8,5.8,0.7,2.4,4.4,4.9,5.5,6.1,4.9)
yr2019 <- c(16.1,15.7,16.0,15.3,14.4,14.7,13.4,10.7,10.7,10.4,11.6,13.1,12.8,12.3,12.9,13.3,13.8,14.4,13.6,13.6,12.2,9.6,9.1,6.8,6.8,5.7,3.6,2.3,8.5,11.8,11.3,12.2,12.5,12.2,12.3,12.2,11.7,12.2,12.8,12.2,12.0,11.2,12.2,11.1,11.1,12.7,13.6,10.8,8.0,2.7,3.1,7.2,5.8,7.7,9.6,8.3,4.2,2.5,3.4,6.7,5.4,4.8,5.5,5.4,4.5,4.9,6.2,8.5,6.9,9.0,7.2,7.8,8.4,8.4,6.7,6.7,5.9,5.8,7.4,8.4,2.8,2.8,3.0,2.6,3.0,6.9,6.5,7.0,6.9,7.3,6.3,2.4,5.0,4.3,4.1,4.1,5.2,6.0,6.6,6.6,5.1,5.9,6.2,5.1,6.1,6.0,4.7,2.6,0.1,-1.7,3.2,5.8,5.8,4.2,2.0,4.8,4.2,3.0,5.5,5.2,6.2,5.2,6.2,4.8,5.6,5.3,6.1,8.6,5.7,4.8,5.7,4.1,4.9,6.6,4.9,3.5,6.1,5.6,4.4,4.5,5.2,4.6,5.9,7.1,7.6,5.4,4.4,4.0,2.8,6.3,4.2,4.9,7.1,6.0,3.7,4.3,3.9,3.4,5.6,8.2,6.8,5.8,4.8,4.9,5.8,6.1,4.7,4.6,4.5,1.8,0.9,3.3)
yr2020 <- c(14.1,13.7,15.7,16.0,17.3,18.1,14.4,10.1,10.5,11.1,11.3,12.9,13.4,14.5,15.1,12.8,15.0,18.1,13.7,12.9,12.7,12.6,15.4,15.8,14.8,12.9,13.3,13.3,12.6,12.5,10.3,9.9,7.3,7.9,7.4,6.0,6.5,7.3,8.5,6.9,9.2,12.1,13.9,11.2,11.9,13.0,11.3,10.7,11.1,9.9,8.8,11.8,12.3,14.6,10.9,9.7,10.0,11.1,9.0,7.2,9.1,8.2,6.7,7.0,8.2,8.6,10.5,10.6,10.4,11.0,8.3,4.5,7.2,8.5,7.4,8.1,7.1,7.5,0.8,-0.5,0.1,1.3,3.8,3.4,1.4,2.9,5.2,6.0,6.5,5.3,4.3,3.0,1.8,4.3,4.1,4.7,6.7,6.7,6.6,7.2,4.7,5.9,6.4,6.2,4.5,2.1,-0.3,1.7,3.5,3.4,3.8,4.2,4.1,2.4,1.0,1.5,0.0,0.8,1.1,0.7,0.7,0.8,1.3,3.4,2.2,1.7,-0.2,-5.1,-5.3,-1.7,0.3,2.2,4.4,5.8,5.1,2.7,1.2,1.4,0.8,1.2,-1.3,-3.2,-2.7,0.4,0.0,-1.0,-2.0,-4.6,-6.8,-6.9,-5.4,-3.6,-3.7,-4.9,-7.2,-7.4,-4.6,-4.5,-2.8,-0.2,0.9,1.6,3.0,3.3,4.2,3.7,6.1,7.3,8.3,5.2,3.8,4.2)

temp_tab_historical <- data.frame(yr2016,yr2017,yr2018,yr2019,yr2020)

root_tip_break_perc <- seq(0,100,5)
#harvest_loss_tn <- c(rep(0.5,5),rep(1,4),rep(2,4),rep(3,4),rep(4,4))
harvest_loss_tn <- 1.902e-4*root_tip_break_perc^2 + 2.254e-2*root_tip_break_perc + 0.25
harvest_loss_tab <- data.frame(root_tip_break_perc,harvest_loss_tn)

###############################################
#
# Fuzzy loss model
#
# Guessing how much loss relative to 'normal'
#  the farmer can expect given harvest and 
#  clamp conditions
#
###############################################

sets_options("universe", seq(1, 100, 0.5))

variables <- set(
  variety = fuzzy_partition(varnames = c(hard = 100, normal = 40, soft = 0), 
                            sd = 20.0),
  harvester_cleaning = fuzzy_partition(varnames = c(gentle = 0, medium = 60,
                                                    hard = 100), sd = 17.5),
  late_moisture = fuzzy_partition(varnames = c(dry = 0, perfect = 60,
                                               wet = 100), sd = 17.5),
  loss_rate = fuzzy_partition(varnames = c(v.low = 10, low = 30, medium = 50, high = 70, v.high = 90),
                              FUN = fuzzy_cone, radius = 10)
)


rules <- set(
  fuzzy_rule(variety %is% hard && harvester_cleaning %is% gentle && late_moisture %is% perfect, loss_rate %is% v.low),
  fuzzy_rule(variety %is% normal && harvester_cleaning %is% medium && late_moisture %is% dry, loss_rate %is% medium),
  fuzzy_rule(variety %is% soft && harvester_cleaning %is% hard && late_moisture %is% wet, loss_rate %is% v.high),
  fuzzy_rule(harvester_cleaning %is% hard && variety %is% soft, loss_rate %is% v.high),
  fuzzy_rule(harvester_cleaning %is% hard, loss_rate %is% high),
  fuzzy_rule(harvester_cleaning %is% medium, loss_rate %is% medium),
  fuzzy_rule(harvester_cleaning %is% gentle, loss_rate %is% low),
  fuzzy_rule(late_moisture %is% wet, loss_rate %is% high),
  fuzzy_rule(late_moisture %is% dry, loss_rate %is% medium),
  fuzzy_rule(late_moisture %is% perfect, loss_rate %is% v.low),
  fuzzy_rule(variety %is% hard, loss_rate %is% v.low),
  fuzzy_rule(variety %is% normal, loss_rate %is% medium),
  fuzzy_rule(variety %is% soft, loss_rate %is% v.high)
)

model <- fuzzy_system(variables, rules)

###############################################
#
# Shiny user interface
#
# Determining inputs and viewing outputs
#
###############################################

ui <- fluidPage(
  #  setBackgroundColor(
  #    color = c("#F7FBFF", "#2171B5"),
  #    gradient = "linear",
  #    direction = "bottom"
  #  ),
  titlePanel("SUGAR BEET HARVEST AND STORAGE - 2021 SWEDEN"),
  tabsetPanel(
    tabPanel("INSTRUCTIONS", fluid = T,
             fluidRow(
               column(12,h4("WARNINGS / DISCLAIMER"),
                      "You should not use this model to make management decisions on your farm.",
                      "It's purpose is to help explore the factors that drive successful harvest and storage strategies.",
                      "The model is general and may not be suitable to apply to your own production system.",
                      br(),br(),
                      "This model is not able to refelct that variability in the production system that occur from day to day in reality.",
                      "It similarly is not able to reflect your willingness to accept these production risks.",
                      br(), br(),
                      "The 2021 price model for Sweden is applied. Prices are assumed fixed in SEK.",
                      br(), br()
               )
             ),
             fluidRow(
               column(12, h4("INSTRUCTIONS"),
                      "Basically, just work through the tabs.",
                      "The default values set are those that approximately reflect the industry averages.",
                      "If you are unsure what is an appropriate value, either take the default, or set the value to an extreme value and see what happens.",
                      br(),br(),
                      "While a model can give an indication for what might happen, one of the best uses for them is think about all the things that the model doesn't capture.",
                      "Similarly, when you can see the modelled outcome is different to the actual outcome, this is the time to ask 'why?' and hopefully find some real insight.",
                      br(),br(),
                      "Please note that the cost information in the Delivery tab is not well developed.",
                      br(),br()
               )
             ),
             fluidRow(
               column(12, h4("DEFINITIONS"),
                      "Clean (mass): The beet mass that is deemed processible.", 
                      "The term on which root yield is commonly defined.", 
                      "The mass is independent of sugar concentration.",
                      "Equals the weight of delivered beet x (1 - dirt-tare).",
                      br(),br(),
                      "Clean (prices): The price per tonne of clean beet mass, are adjusted to be for a pol of 17%.",
                      "The term on which prices are set.",
                      "The price is adjusted 9% per percentage point change in pol from the base of 17%.",
                      "The conversion to 17% is linear in the change in price per change in pol percentage point.",
                      "This means that the increase/ decrease in price is greater at lower sugar concentrations.",
                      "The contract price is for a pol of 17%.",
                      "If your pol is higher than this, then the prices per tonne clean given in this model will be higher than your contract price.",
                      br(),br(),
                      "Delivered: The mass of beet that falls out of the back of the truck at Örtofta.",
                      "A large fraction of the mass is soil and unprocessible beet material (dirt tare).",
                      "Is independent of sugar concentration.",
                      br(), br() 
               )
             ),
             fluidRow(
               column(12, h4("IMPROVEMENTS"),
                      "The following improvements are on the to-do list:", br(),
                      "- Introduce a tab in which scenarios can be compared", br(),
                      "- Allow the price schedule to change to reflect those of recent years.",br(),
                      "- Have a 'simplified version' tab, in which some outputs are just set to defaults and some outputs disappear.",br(),
                      "- Introduce a model of dirt-tare. This would probably be something like an industry average, with spikes after rainfall events.", br(),
                      "- Improve the cost side of the equation, starting with the Delivery tab.",br(),
                      "- Introduce more factors that we know matter (eg, irrigation, Ca).",
                      br(),br(),
                      "If you have others, please contact William English as we@nbrf.nu")
             )
             
    ),
    tabPanel("IN THE FIELD", fluid = T,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("field_size","Field size (ha)", min=0, max=200, step = 0.1, value = 10),
                 br(),
                 fluidRow(
                   column(10, h4("VARIETY STRENGTH")),
                   column(2, actionButton("help_variety_strength", "?"))
                 ),
                 selectInput("variety_type","Variety type", choices = list("Normal"=2, "High root yield"=1,"High sugar content"=3)),
                 sliderInput("variety_hardness", "Variety strength (relative)", min=0, max=100, value = 50),
                 br(),
                 fluidRow(
                   column(10, h4("LATE SEASON GROWTH POTENTIAL")),
                   column(2, actionButton("help_LSG", "?"))
                 ),
                 sliderInput("late_potent","Late season growth potential (relative to average (%))", min=50, max=125, step=5, value=100),
               ),
               mainPanel(
                 column(12,plotly::plotlyOutput("LSG_mass_daily_chart"),
                        plotly::plotlyOutput("LSG_mass_cum_chart"))
               )
             )
    ),
    tabPanel("HARVEST", fluid = T,
             sidebarLayout(
               sidebarPanel(
                 dateInput("harvest_date","Harvest date",value = "2021-11-15"),
                 br(),
                 fluidRow(
                   column(10, h4("HARVEST CONDITIONS")),
                   column(2, actionButton("help_harvest_conditions", "?"))
                 ),
                 sliderInput("late_moisture", "Soil moisture at harvest (percent of ideal (%))", min=0, max=200, value=100),
                 sliderInput("harvester_cleaning", "Cleaning intensity (relative)", min=0, max=100, value=40),
                 sliderInput("root_tip_break_pc","Roots with tip breakage > 2cm (%)", min=0, max=100, step=5, value=25)
               ),
               mainPanel(
                 column(12, h4("Harvest loss"),tableOutput("summary_harvest_loss"), style = "margin-top: 340px")
               )
             )
    ),
    tabPanel("STORAGE", fluid = T,
             sidebarLayout(
               fluidRow(
                 sidebarPanel(
                   dateInput("cover_date", "Date of cover with TopTex", value="2021-12-01"),
                   br(),
                   fluidRow(
                     column(10, h4("CLAMP TEMPERATURE MODEL")),
                     column(2, actionButton("help_storage_temp", "?"))
                   ),
                   selectInput("temp_clamp_model","Clamp temperature model", choices = list("Moving average with floor"=1, "Air with floor"=2, "Air"=3, "Ventilated" = 4)),
                   selectInput("temp_air_yr","Temperature data from which year?", choices = list("2020"="yr2020", "2019"="yr2019", "2018"="yr2018", "2017"="yr2017", "2016"="yr2016")),
                   sliderInput("clamp_size", "Clamp width at base (m)", step = 0.1, min=7, max=9, value=8),
                   sliderInput("ref_temp", "Clamp temperature floor (°C)", min=0, max=10, value=2)
                 ),
                 mainPanel(
                   column(12, plotly::plotlyOutput("temp_graph"), style = "margin-top: 125px")
                 ),
                 style = 'padding-left:15px'
               ),
               fluidRow(
                 sidebarPanel(
                   fluidRow(
                     column(10, h4("STORAGE LOSS MODEL")),
                     column(2, actionButton("help_storage_loss", "?"))
                   ),
                   selectInput("loss_model","Loss model", choices = list("Discontinuous"=1, "Linear"=2, "Quadratic"=3)),
                   sliderInput("mass_loss","Mass loss per degree day (%/t/°C)", min=0, max=0.05, value=0.01)
                 ),
                 mainPanel(
                   column(12, plotly::plotlyOutput("loss_Cd"))
                 ),
                 style = 'padding-left:15px'
               )
             )
    ),
    tabPanel("DELIVERY", fluid = T,
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(10, h4("DELIVERY")),
                   column(2,actionButton("help_delivery", "?"))
                 ),
                 dateInput("delivery_date","Delivery date",value = "2022-01-15"),
                 sliderInput("delivery_distance","Distance to deliver (mil)",min=1,max=20,step=0.1,value=5),
                 sliderInput("delivery_cost","Cost for delivery",min=1000,max=200000,value=5000)
               ),
               mainPanel(
                 column(12, h4("Delivery costs"), tableOutput("delivery_cost_tab"), style = "margin-top: 125px")
               )
             )
             
    ),
    tabPanel("PROD. / PAYMENT", fluid = T,
             sidebarLayout(
               fluidRow(
                 sidebarPanel(
                   fluidRow(
                     column(10, h4("PRODUCTION")),
                     column(2, actionButton("help_production", "?"))
                   ),
                   fluidRow(
                     column(6,dateInput("prod_data_date","Date payment schedule data from:",value = "2022-01-15")),
                     column(6,textOutput("prod_data_loc"),style = "margin-top: 30px")
                   ),
                   sliderInput("root_yield","Root yield (t/ha)", min=40, max=120, step = 1, value = 86),
                   sliderInput("pol", "Sugar content (% pol)", min=15, max=22, value=17, step = 0.1),
                   sliderInput("renhet", "Renhet (%)", min=78, max=100, value=89.5, step = 0.1)
                 ),
                 mainPanel(
                   column(12, h4("Production summary"), tableOutput("root_harvest_tab"))
                 ),
                 style = 'padding-left:15px'
               ),
               fluidRow(
                 sidebarPanel(
                   fluidRow(
                     column(10,h4("PAYMENT")),
                     column(2,actionButton("help_payment", "?"))
                   ),
                   numericInput("price", "Your contract price (SEK)", value=313),
                   checkboxInput("vol","Eligible for volume bonus?", value = F),
                   br(),
                 ),
                 mainPanel(
                   column(12, h4("Payment"), tableOutput("summary_final_tab"))
                 ),
                 style = 'padding-left:15px'
               )
             )
    ),
    tabPanel("SUMMARY TABLE", fluid = T,
             fluidRow(
               column(3, checkboxInput("data_restrict_start","Restrict data start date to harvest date?"),
                      checkboxInput("data_restrict_end","Restrict data end date to delivery date?"),
                      style = 'padding-left:15px'
               ),
               column(3,checkboxGroupInput("summary_tab_show","Show economy:",choices=c("Per ha"="HA", "Per field"="FI", "Per tonne clean" = "CL"," Per tonne delivered"="DE"),selected = )),
               column(6,)
             ),
             fluidRow(column(12,tableOutput("summary_tab_output")))
    ),
    tabPanel("GRAPHS - PRODUCTION", fluid = T,
             fluidRow(
               column(6,plotly::plotlyOutput("summary_graph_sug")),
               column(6,plotly::plotlyOutput("summary_graph_pol"))
             ),
             fluidRow(
               column(6,plotly::plotlyOutput("summary_graph_mass")),
               column(6,plotly::plotlyOutput("summary_graph_temp"))
             )
    ),
    tabPanel("GRAPHS - ECONOMY", fluid = T,
             fluidRow(
               column(6,plotly::plotlyOutput("summary_graph_price_ha")),
               column(6,plotly::plotlyOutput("summary_graph_price_fi"))
             ),
             fluidRow(
               column(6,plotly::plotlyOutput("summary_graph_price_cl")),
               column(6,plotly::plotlyOutput("summary_graph_price_de"))
             )
    ),
    tabPanel("COMPARISON", fluid = T,
             fluidRow(
               column(6, actionButton("comp_1", "Compare the current set-up")),
               column(6, actionButton("comp_2", "Compare the current set-up"))
             ),
             fluidRow(
               column(6,tableOutput("comp_1_tab"),
                      plotly::plotlyOutput("summary_graph_sug_comp_1")),
               column(6,tableOutput("comp_2_tab"),
                      plotly::plotlyOutput("summary_graph_sug_comp_2"))
             )
    )
  )
)

# shinyApp(ui=ui, server=server)


###############################################
#
# Shiny server 
# 
# Where the calculations are run
#
###############################################

server <- function(input, output, session){
  
  # Update hardness slider
  observe(updateSliderInput(session, "variety_hardness", 
                            value = ifelse(input$variety_type == 1, 30, ifelse(input$variety_type == 2, 50, 70))))
  
  # Root harvest yield table (CAN THIS SIT IN THE FULL_TAB REACTIVE?)
  
  root_harvest_tab <- reactive({
    full_tab_p <- full_tab()
    root_yield_p <- input$root_yield 
    field_size_p <- input$field_size
    harvest_date_p <- input$harvest_date
    delivery_date_p <- input$delivery_date
    
    root_mass_factory <- full_tab_p$mass_kg_cum[which(full_tab_p$date_full == as.POSIXct(delivery_date_p))]
    root_mass_harvest <- full_tab_p$mass_kg_cum[which(full_tab_p$date_full == as.POSIXct(harvest_date_p))] 
    root_mass_grown <- full_tab_p$mass_kg_cum[which(full_tab_p$date_full == (as.POSIXct(harvest_date_p) - 86400))]
    
    root_mass_factory_field <- root_mass_factory*field_size_p
    root_mass_harvest_field <- root_mass_harvest*field_size_p
    root_mass_grown_field <- root_mass_grown*field_size_p
    
    pol_factory <- full_tab_p$pol_cum[which(full_tab_p$date_full == as.POSIXct(delivery_date_p))]
    pol_harvest <- full_tab_p$pol_cum[which(full_tab_p$date_full == as.POSIXct(harvest_date_p))] 
    pol_grown <- full_tab_p$pol_cum[which(full_tab_p$date_full == (as.POSIXct(harvest_date_p) - 86400))]
    
    sug_factory <- full_tab_p$sug_cum[which(full_tab_p$date_full == as.POSIXct(delivery_date_p))]
    sug_harvest <- full_tab_p$sug_cum[which(full_tab_p$date_full == as.POSIXct(harvest_date_p))] 
    sug_grown <- full_tab_p$sug_cum[which(full_tab_p$date_full == (as.POSIXct(harvest_date_p) - 86400))]
    
    root_harvest_tab <- matrix(c(sug_grown, sug_harvest, sug_factory,
                                 pol_grown, pol_harvest, pol_factory,
                                 root_mass_grown, root_mass_harvest, root_mass_factory,
                                 root_mass_grown_field, root_mass_harvest_field, root_mass_factory_field
    ), byrow=F, nrow=3) 
    colnames(root_harvest_tab) <- c("Sugar yield (t/ha)","Pol (%)", "Root Yield (t/ha)","Root Yield (t/field)")
    rownames(root_harvest_tab) <- c("Grown","Harvested", "Delivery")
    
    root_harvest_tab
  })
  
  # LOCATION OF BEETS TABLE
  loc_tab <- reactive({
    
    harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
    delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
    location <- "Clamp"
    
    # Full table of location of beets
    loc_tab <- data.frame(date_full, location)
    
    loc_tab$location[which(loc_tab$date_full <= harvest_date)] <- "Field"
    loc_tab$location[which(loc_tab$date_full == harvest_date)] <- "Harvest"
    loc_tab$location[which(loc_tab$date_full >= delivery_date)] <- "Factory"
    loc_tab$location[which(loc_tab$date_full == delivery_date)] <- "Delivery"
    
    loc_tab
  })
  
  # LATE SEASON GROWTH
  LSG_tab <- reactive({
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
    
  })
  
  # CLAMP TEMP TABLE. Table of daily temperature inthe clamp given the chosen inputs
  
  temp_tab <- reactive({
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
  })
  
  # SUGAR LOSS FACTOR
  # Get model factor
  factor <- reactive({
    data_late_moisture <- (input$late_moisture * 0.5)
    
    example.1 <- fuzzy_inference(model, list(harvester_cleaning = input$harvester_cleaning, 
                                             late_moisture = data_late_moisture,
                                             variety  = input$variety_hardness))
    # get the "damage factor"
    factor <- (gset_defuzzify(example.1, "centroid")/100+0.5)
    
    factor
  })
  
  # SUGAR LOSS MODEL TABLE
  
  loss_tab <- reactive({
    
    # Write ref_medel series
    if(input$loss_model == 1) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_discont 
    if(input$loss_model == 2) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_linear 
    if(input$loss_model == 3) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_quad
    
    
    # Write actual loss rate as function of "damage factor"
    ref_loss_data$clamp_pol_loss_pc_cum <- ref_loss_data$ref_medel*factor()
    
    #Write table
    ref_loss_data
  })
  
  # MASS LOSS
  
  mass_loss_tab <- reactive({
    
    mass_loss_p <- input$mass_loss
    
    cum_temp <- seq(1,1000)
    cum_temp <- c(0,cum_temp)
    
    clamp_mass_loss_pc_cum <- mass_loss_p * cum_temp
    
    mass_loss_tab <- data.frame(cum_temp, clamp_mass_loss_pc_cum)
    
    mass_loss_tab
    
  })
  
  # HARVEST LOSS
  summary_harvest_loss <- reactive({
    root_tip_break_pc_p <- input$root_tip_break_pc
    root_harvest_p <- data.frame(root_harvest_tab())[3,3]
    
    harvest_loss_tn_summary <- harvest_loss_tab$harvest_loss_tn[which(harvest_loss_tab$root_tip_break_perc == root_tip_break_pc_p)]
    harvest_loss_pc_summary <- harvest_loss_tn_summary/root_harvest_p*100
    
    summary_harvest_loss <- matrix(c(harvest_loss_tn_summary,harvest_loss_pc_summary ), nrow = 1)
    colnames(summary_harvest_loss) <- c("Tonne per hectare", "Percent")
    
    summary_harvest_loss
  })
  
  # Delivery cost table
  
  output$delivery_cost_tab <- renderTable({
    root_harvest_tab_p <- data.frame(root_harvest_tab())
    root_harvest_p <- root_harvest_tab_p[3,4]
    root_yield_p <- root_harvest_tab_p[3,3]
    
    delivery_cost_field_clean <- input$delivery_cost / (input$renhet/100)
    delivery_cost_field_soil <- input$delivery_cost * (1 - (input$renhet/100))
    delivery_tn_mil <- root_yield_p / input$delivery_distance
    delivery_cost_mil <- input$delivery_cost / input$delivery_distance
    delivery_cost_mil_clean <- delivery_cost_mil / (input$renhet/100)
    delivery_cost_mil_soil <- delivery_cost_mil * (1 - (input$renhet/100))
    delivery_cost_tn <- input$delivery_cost / root_harvest_p
    delivery_cost_tn_clean <- delivery_cost_tn / (input$renhet/100)
    delivery_cost_tn_soil <- delivery_cost_tn * (1 - (input$renhet/100))
    delivery_cost_tab <- matrix(c(root_harvest_p, input$delivery_cost, delivery_cost_field_clean, delivery_cost_field_soil,
                                  delivery_tn_mil, delivery_cost_mil, delivery_cost_mil_clean, delivery_cost_mil_soil,
                                  1, delivery_cost_tn, delivery_cost_tn_clean, delivery_cost_tn_soil), byrow=T, nrow=3) 
    rownames(delivery_cost_tab) <- c("Field", "Mil", "Tonne")
    colnames(delivery_cost_tab) <- c("Tonnes per...","Cost per... (SEK)","Cost per clean tonne (SEK)", "Cost of soil transport (SEK)")
    delivery_cost_tab
  }, rownames = T, digits=1
  )
  
  # FULL RESULTS TABLE
  
  full_tab = reactive({
    # input from all previous tables
    temp_tab_p <- data.frame(temp_tab())
    loss_tab_p <- data.frame(loss_tab())
    mass_loss_tab_p <- data.frame(mass_loss_tab())
    loss_tab_p <- loss_tab_p[,c("cum_temp","clamp_pol_loss_pc_cum")]
    loc_tab_p <- data.frame(loc_tab())
    LSG_tab_p <- data.frame(LSG_tab())
    
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
    full_tab$pol_loss_pc_cum <- full_tab$pol_loss_pp_cum / full_tab$pol_cum[which(full_tab$date_full==harvest_date)]*100
    
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
    if(day0 >= harvest_date) full_tab$harvest_mass_loss <- ifelse(full_tab$date_full < harvest_date, harvest_loss, 0)
    if(day0 < harvest_date) full_tab$harvest_mass_loss <- ifelse(full_tab$date_full < harvest_date, 0, harvest_loss*(-1))
    
    ## Mass gain under late season growth
    full_tab$LSG_mass_loss_pc_cum <- ifelse(full_tab$date_full <= harvest_date, full_tab$LSG_mass_loss_pc_cum, full_tab$LSG_mass_loss_pc_cum[which(full_tab$date_full == harvest_date)])
    full_tab$LSG_mass_loss_kg_cum <- full_tab$LSG_mass_loss_pc_cum*root_yield_p/100*(-1)
    LSG_mass_loss_kg_cum_ref <- full_tab$LSG_mass_loss_kg_cum[which(full_tab$date_full==day0)]
    full_tab$LSG_mass_loss_kg_rel_day0 <- full_tab$LSG_mass_loss_kg_cum - LSG_mass_loss_kg_cum_ref
    
    ## Total daily mass change (kg)
    full_tab$mass_loss_kg_rel_day0 <- full_tab$clamp_mass_loss_kg_rel_day0 + full_tab$LSG_mass_loss_kg_rel_day0 - full_tab$harvest_mass_loss
    full_tab$mass_kg_cum <- root_yield_p - full_tab$mass_loss_kg_rel_day0
    
    ## SUGAR YIELD
    full_tab$sug_cum <- full_tab$pol_cum/100*full_tab$mass_kg_cum
    
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
    full_tab$price_base_ha <- full_tab$price_base_delivered*full_tab$mass_kg_cum
    full_tab$price_bonus_ha <- full_tab$price_bonus_delivered*full_tab$mass_kg_cum
    full_tab$price_ha <- full_tab$price_delivered*full_tab$mass_kg_cum
    
    # Field prices
    full_tab$price_base_field <- full_tab$price_base_ha*field_size
    full_tab$price_bonus_field <- full_tab$price_bonus_ha*field_size
    full_tab$price_field <- full_tab$price_ha*field_size
    
    full_tab
    
  })
  
  # SUMMARY (VISUALISED) RESULTS
  
  summary_tab = reactive({
    # input from required previous tables
    summary_tab <- data.frame(full_tab())
    
    # input from required inputs
    summary_tab_cols <- input$summary_tab_show
    summary_tab_start <- input$data_restrict_start
    summary_tab_end <- input$data_restrict_end
    harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
    delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
    
    # Define summary table - columns
    summary_tab_show <- c("date_full", "location", "temp_clamp_p", "cum_temp", "pol_loss_pc_cum", "pol_cum","mass_kg_cum","sug_cum")
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
    
  })
  
  summary_tab_output = reactive({
    summary_tab_output <- data.frame(summary_tab())
    
    # input from required inputs
    summary_tab_cols <- input$summary_tab_show
    
    summary_tab_names <- c("Date", "Location", "Temp. (°C)", "Cum. Temp (°Cd)", "Cum. pol loss (%)", "Pol","Root Yield","Sugar Yield")
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
  
  prod_data_loc = reactive({
    summary_tab_p <- summary_tab()
    prod_data_date_p <- as.POSIXct(input$prod_data_date, tz = "UTC", format = "%Y-%m-%d")
    
    prod_data_loc <- summary_tab_p$location[which(summary_tab_p$date_full == prod_data_date_p)]
    
    prod_data_loc
  })
  
  dates <- reactive({
    delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
    harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
  })
  ###############
  # VISUALS
  
  ## TEXT ##
  
  # Location that the production data applies to
  output$prod_data_loc <- renderText({
    paste("Location:", prod_data_loc())
  })
  
  ## TABLES ##
  
  # Summary harvest loss
  output$summary_harvest_loss = renderTable({
    summary_harvest_loss()
  }, digits = 1)
  
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
  
  ## CHARTS ##
  
  # Late season growth
  output$LSG_mass_daily_chart <- plotly::renderPlotly({
    summary_tab()
    ggplot(LSG_tab(), aes(x=date_full)) + 
      geom_line(aes(y = LSG_mass_loss_pc_daily), color = "darkred") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      ylab("Daily growth (%)") + 
      xlab("Date") +
      labs(title = "Late Season Growth Potential - daily")
  })
  
  # Late season growth
  output$LSG_mass_cum_chart <- plotly::renderPlotly({
    summary_tab()
    ggplot(LSG_tab(), aes(x=date_full)) + 
      geom_line(aes(y = LSG_mass_loss_pc_cum), color = "darkred") + 
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      ylab("Cumulative growth (%)") + 
      xlab("Date") +
      labs(title = "Late Season Growth Potential - cumulative")
  })
  
  # Plot of temperatures (Air and Clamp)
  output$temp_graph <- plotly::renderPlotly({
    summary_tab()
    ggplot(temp_tab(), aes(x=date_full)) + 
      geom_line(aes(y = temp_clamp, color = "Clamp temp")) + 
      geom_line(aes(y = temp_air, color="Air temperature"), linetype="dotdash") +
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      scale_colour_manual("", 
                          breaks = c("Clamp temp","Air temperature"),
                          values = c("Clamp temp"="darkred","Air temperature"="steelblue")) +
      ylab("Temperature (°C)") + 
      xlab("Date") +
      labs(title = "Clamp temperature model")
  })
  
  # Plot of sugar loss
  output$loss_Cd <- plotly::renderPlotly({
    
    ggplot(loss_tab(), aes(x=cum_temp)) + 
      geom_line(aes(y = clamp_pol_loss_pc_cum, color = "Clamp pol loss")) + 
      geom_line(aes(y = ref_medel, color="Ref pol loss"), linetype="dotdash") +
      scale_colour_manual("", 
                          breaks = c("Clamp pol loss","Ref pol loss"),
                          values = c("Clamp pol loss"="darkred","Ref pol loss"="steelblue")) +
      xlim(0,500) +
      ylim(0,15) +
      ylab("Sugar loss (% of original)") + 
      xlab("Accumulated temperature (°Cd)") +
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
      ylab("Cumulative temperature (°Cd)") + 
      xlab("Date") +
      labs(title = "CLAMP TEMPERATURE") +
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  output$summary_graph_pol <- plotly::renderPlotly({
    ggplot(summary_tab(), aes(x=date_full)) + 
      geom_line(aes(y = pol_cum, color = "Pol")) +
      geom_line(aes(y = pol_loss_pc_cum, color = "Cum. % loss")) + 
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      scale_y_continuous(sec.axis = sec_axis(~(. + move) / amplify, name = "Pol sugar")) +
      scale_colour_manual("", 
                          breaks = c("Pol","Cum. % loss"),
                          values = c("Pol"="red3","Cum. % loss"="blue3")) +
      ylab("Sugar (%)") + 
      xlab("Date") +
      labs(title = "SUGAR CONTENT") + 
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  output$summary_graph_mass <- plotly::renderPlotly({
    ggplot(summary_tab(), aes(x=date_full)) + 
      geom_line(aes(y=mass_kg_cum, color = "Cum. mass")) +
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      scale_colour_manual("", 
                          breaks = c("Cum. mass"),
                          values = c("Cum. mass"="red3")) +
      ylab("Root yield (t/ha)") + 
      xlab("Date") +
      labs(title = "ROOT YIELD") +
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  output$summary_graph_sug <- plotly::renderPlotly({
    ggplot(summary_tab(), aes(x=date_full)) + 
      geom_line(aes(y=sug_cum, color = "Cum. sug")) +
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      scale_colour_manual("", 
                          breaks = c("Cum. sug"),
                          values = c("Cum. sug"="red3")) +
      ylab("Sugar yield (t/ha)") + 
      xlab("Date") +
      labs(title = "SUGAR YIELD") +
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  output$summary_graph_price_ha <- plotly::renderPlotly({
    ggplot(full_tab(), aes(x=date_full)) + 
      geom_line(aes(y = price_base_ha, colour = "Base payment")) + 
      geom_line(aes(y = price_bonus_ha, colour = "Bonus payment")) +
      geom_line(aes(y = price_ha, colour = "Total payment")) +
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      scale_colour_manual("", 
                          breaks = c("Total payment", "Base payment", "Bonus payment"),
                          values = c("Total payment"="red3", "Base payment"="blue3", 
                                     "Bonus payment"="green3")) +
      ylab("Price (kr/ha)") + 
      xlab("Date") +
      labs(title = "INCOME PER HECTARE") + 
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  output$summary_graph_price_fi <- plotly::renderPlotly({
    ggplot(full_tab(), aes(x=date_full)) + 
      geom_line(aes(y = price_base_field, colour = "Base payment")) + 
      geom_line(aes(y = price_bonus_field, colour = "Bonus payment")) +
      geom_line(aes(y = price_field, colour = "Total payment")) +
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      scale_colour_manual("", 
                          breaks = c("Total payment", "Base payment", "Bonus payment"),
                          values = c("Total payment"="red3", "Base payment"="blue3", 
                                     "Bonus payment"="green3")) +
      ylab("Price (kr)") + 
      xlab("Date") +
      labs(title = "INCOME PER FIELD") + 
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  output$summary_graph_price_cl <- plotly::renderPlotly({
    ggplot(full_tab(), aes(x=date_full)) + 
      geom_line(aes(y = price_base_clean, colour = "Base payment")) + 
      geom_line(aes(y = price_bonus_clean, colour = "Bonus payment")) +
      geom_line(aes(y = price_clean, colour = "Total payment")) +
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      scale_colour_manual("", 
                          breaks = c("Total payment", "Base payment", "Bonus payment"),
                          values = c("Total payment"="red3", "Base payment"="blue3", 
                                     "Bonus payment"="green3")) +
      ylab("Price (kr/t)") + 
      xlab("Date") +
      labs(title = "INCOME PER CLEAN (17%) TONNE") + 
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  output$summary_graph_price_de <- plotly::renderPlotly({
    ggplot(full_tab(), aes(x=date_full)) + 
      geom_line(aes(y = price_base_delivered, colour = "Base payment")) + 
      geom_line(aes(y = price_bonus_delivered, colour = "Bonus payment")) +
      geom_line(aes(y = price_delivered, colour = "Total payment")) +
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
      scale_colour_manual("", 
                          breaks = c("Total payment", "Base payment", "Bonus payment"),
                          values = c("Total payment"="red3", "Base payment"="blue3", 
                                     "Bonus payment"="green3")) +
      ylab("Price (kr/t)") + 
      xlab("Date") +
      labs(title = "INCOME PER DELIVERED (17%) TONNE") + 
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  ###############################
  # HELP!
  
  observeEvent(input$help_variety_strength, {
    showModal(modalDialog(
      title = "In the field",
      "Strength values are used to guess the level of harvest damage that will occur.", 
      "Variety Strength is currently a relative value, from 0 (weakest) to 100 (strongest).",
      "Strength can be defined either on this scale, or by selecting the variety type.",
      "Weaker varieties will have greater rates of damage.",
      br(),br(), 
      "Sources: NBR Project 631 / COBRI trials and further literature",
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_LSG, {
    showModal(modalDialog(
      title = "LATE SEASON GROWTH POTENTIAL",
      "Late Season Growth Potential defines how much extra root yield will be added between early September and the end of November.",
      "It is defined as a daily percent increase in biomass.",
      "Choosing a Late Season Growth Potential of 100% (default value) will give a total increase in root yield over this period of approximately 40%.",
      "Late season growth potential is determined by issues like disease or water stress.",
      br(),br(),
      "Sugar content (pol) is estimated to increase by 0.02 percentage points per day.",
      br(),br(),
      "All varieties should have the same late-season growth potential.", 
      br(), br(),
      "Sources: NBR Project 417",
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_harvest_conditions, {
    showModal(modalDialog(
      title = "HARVEST CONDITIONS",
      "Harvest Conditions are used to guess the level of harvest damage that will occur.",
      "Harder harvest conditions, a higher roter speed, and weaker beet varieties (selected on the IN THE FIELD page) give higher damage.",
      "Higher damage means higher loss during storage.", 
      "The higher your damage score, the higher your rate of loss will be for each degree-day (shown on STORAGE tab).",
      br(), 
      "Sources: NBR Project ###.",
      br(),br(),
      "Roots With Tip Breakage > 2cm is used to guess the harvest loss per ha.",
      "At the low end, 20% or less gives 0.5t/ha. At the high end, 80% or more gives 4t/ha.",
      "To sample this, 20 representative beets should be measured in the clamp.",
      br(),
      "Source: IIRB",
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_storage_temp, {
    showModal(modalDialog(
      title = "STORAGE TEMPERATURE MODEL",
      "The Storage Temperature Model determine the clamp temperature in relation to the air temperature.", 
      br(), br(),
      "The different years give historical mean air temperatures for Borgeby.",
      br(), br(),
      "Clamp width determines a multiplication factor on the clamp temperature relative to the air temperature.",
      "A wider clamp will build more heat than a thinner one.", 
      "At 7m, this multiplication factor is 1.00. At 9m, it is 1.10",
      br(), br(),
      "For models with a temperature floor - a minimum temperature the clamp can go to - Clamp Temperature Floor sets this.",
      br(), br(),
      "The date the clamp is covered with TopTex is only used to determine the TopTex bonus.", 
      br(), br(),
      "Sources: just general observation...",
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_storage_loss, {
    showModal(modalDialog(
      title = "STORAGE LOSS MODEL",
      "The Loss Models are different ways of defining the relationship between storage loss and degree-days.",
      "Degree Day is the sum of the red line in the above graph, for the period your beets are in storage.",
      br(), br(),
      "The higher your damage score, the higher your rate of sugar loss will be for each degree-day.",
      "Your damage score is the sum of the Variety Strength (In the Field tab), and Harvest Conditions (Harvest tab).",
      "You can see this relationship in the Storage Loss Model graph (right) - your storage loss (red line) relative to the median loss (blue dashed line).", 
      br(), br(),
      "Mass loss per degree-day is not a metric we have good information for, but we guess it's about 3% over long-term storage (300 degree-days).",
      br(), br(),
      "Sources: Linear model - Jaggard et al. 1997. Discontinuous and Quadratics - Legrande and Wauters, 2012",
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_delivery, {
    showModal(modalDialog(
      title = "DELIVERY",
      "Only the delivery date information is used elsewhere in this model",
      br(),br(),
      "The delivery costs table is accurate, but I'm not sure it's useful information.",
      "If you have thoughts on this, please contact me at we@nbrf.nu",
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_production, {
    showModal(modalDialog(
      title = "PRODUCTION",
      "The date for which this data is stated to apply to is that which all other calculations are made around.",
      "It is therefore important to get this right.",
      br(),br(),
      "If using data from delivery, the date should be the same as the delivery date.",
      "If using data to forecast management options, the date is most likely from when the beets are in the field.",
      "The 'Location: ...' text next to the date box tells you where the beets are when the production data applied to.",
      "If forecasting, use the Production summary table to the right to see if the yield and pol at delivery are in line with expectations.",
      br(),br(),
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_payment, {
    showModal(modalDialog(
      title = "PAYMENT",
      "Please take this information from your contact and delivery data.",
      br(),br(),
      "A Clean Tonne is one tonne of sugar beet (only), with pol = 17%.",
      "To adjust to a delivered tonne (a tonne in the truck):", 
      br(), "First adjust to 17%.", 
      "Add 0.9% of your base price per 0.1 percentage point above pol = 17%.",
      "If your beets are less than 17%, subtract 0.9% of your base price per 0.1 percentage point below pol = 17%.",
      br(), "Then convert to a 'dirty' tonne by multiplying by your measured dirt-tare.",
      br(),br(),
      "Bonuses include TopTex bonus, early/late delivery bonus, volume bonus, and dirt-tare bonus/penalty.",
      "The TopTex bonus is 5/10/15 SEK per clean tonne when TopTex is used for at least 7 days prior a delivery after 15Nov/1 Dec/15 Dec.",
      "Late delivery bonus is 1 SEK per clean tonne per day through December, and 1.5 SEK per clean tonne per day from 1 January.",
      "The volume bonus is 5 SEK (0.5 EUR) per clean tonne if you have increased your area grown under sugar beet by 10% since 2019.",
      "Dirt-tare bonus/ penalty is 2 SEK per clean tonne per percentage point below/above 10.5% dirt-tare.",
      br(),br(),
      "Please note that there is no modelling of dirt-tare in this model - this is assumed constant over the entire growth and storage period",
      easyClose = T,
      footer = NULL
    ))
  })
  
  ###############################
  # COMPARISONS
  #
  #
  
  comp_1_tab <- eventReactive(input$comp_1,{
    summary_final_tab()
  })
  comp_1_tab_graph <- eventReactive(input$comp_1,{
    summary_tab()
  })
  
  output$comp_1_tab = renderTable({
    comp_1_tab()
  }, rownames = T)
  
  output$summary_graph_sug_comp_1 <- plotly::renderPlotly({
    ggplot(comp_1_tab_graph(), aes(x=date_full)) + 
      geom_line(aes(y=sug_cum, color = "Cum. sug")) +
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      scale_colour_manual("", 
                          breaks = c("Cum. sug"),
                          values = c("Cum. sug"="red3")) +
      ylab("Sugar yield (t/ha)") + 
      xlab("Date") +
      labs(title = "SUGAR YIELD") +
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  comp_2_tab <- eventReactive(input$comp_2,{
    summary_final_tab()
  })
  comp_2_tab_graph <- eventReactive(input$comp_2,{
    summary_tab()
  })
  
  output$comp_2_tab = renderTable({
    comp_2_tab()
  }, rownames = T)
  
  output$summary_graph_sug_comp_2 <- plotly::renderPlotly({
    ggplot(comp_2_tab_graph(), aes(x=date_full)) + 
      geom_line(aes(y=sug_cum, color = "Cum. sug")) +
      geom_vline(xintercept = as.numeric(delivery_date), linetype="dotted") +
      geom_vline(xintercept = as.numeric(harvest_date), linetype="dotted") +
      scale_colour_manual("", 
                          breaks = c("Cum. sug"),
                          values = c("Cum. sug"="red3")) +
      ylab("Sugar yield (t/ha)") + 
      xlab("Date") +
      labs(title = "SUGAR YIELD") +
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
}

###############################################
#
# Run the model
#
###############################################

shinyApp(ui=ui, server=server)
