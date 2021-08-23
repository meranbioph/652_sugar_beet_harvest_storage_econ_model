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
kr_tonne <- 275 # at reference pol of 17%
kr_renhet <- 2 # extra kr per %-point over ref, at 17% pol%
kr_pol <- 9 # extra % per %-point over ref of 17% pol
kr_TT <- 5 # for the use of TopTex for at least 7 days prior to delivery, for delivery after 15 November
kr_late <- 1 # extra payment per tonne for beets delivered late
kr_vol <- 5 # extra per tonne if area increases 10%
date_full <- seq(as.POSIXct("2021-09-10", tz = "UTC", format = "%Y-%m-%d"), length.out = 172, by = "1 day")
price_early <- c(37,37,37,37,37,36,33,30,27,24,21,19,16,14,12,10,8,6,4,2,1,rep(0,151))
price_late <- c(rep(0,82),seq(1,23,by=1),seq(24.5,123.5,by=1.5))
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
harvest_loss_tn <- c(rep(0.5,5),rep(1,4),rep(2,4),rep(3,4),rep(4,4))
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
  titlePanel("Sugar beet harvest and storage"),
  tabsetPanel(
    tabPanel("IN THE FIELD", fluid = T,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("field_size","Field size (ha)", min=0, max=200, step = 0.1, value = 10),
                 h4("VARIETY"),
                 selectInput("variety_type","Variety type", choices = list("Normal"=2, "High root yield"=1,"High sugar content"=3)),
                 sliderInput("variety_hardness", "Variety hardness", min=0, max=100, value = 50),
                 h4("LATE SEASON GROWTH POTENTIAL"),
                 sliderInput("late_potent","Late season growth potential (%)", min=50, max=125, step=5, value=100),
                 actionButton("help_field", "?")
               ),
               mainPanel(
                 plotly::plotlyOutput("LSG_chart")
               )
             )
    ),
    tabPanel("HARVEST", fluid = T,
             sidebarLayout(
               fluidRow(
                 sidebarPanel(
                   dateInput("harvest_date","Harvest date",value = "2021-11-15"),
                   sliderInput("late_moisture", "Soil moisture at harvest, as percent of ideal", min=0, max=200, value=100),
                   sliderInput("harvester_cleaning", "Cleaning intensity", min=0, max=100, value=40),
                   sliderInput("root_tip_break_perc","Roots with tip breakage > 2cm (%)", min=0, max=100, step=5, value=25),
                   actionButton("help_harvest", "?")
                 ),
                 mainPanel(
                   tableOutput("summary_harvest_loss")
                 ),
                 style = 'padding-left:15px'
               ),
               fluidRow(
                 sidebarPanel(
                   sliderInput("root_yield","Root yield (t/ha) - at delivery", min=40, max=120, step = 1, value = 86),
                   
                 ),
                 mainPanel(
                   tableOutput("root_harvest_tab")
                 ),
                 style = 'padding-left:15px'
               )
             )
    ),
    tabPanel("STORAGE", fluid = T,
             sidebarLayout(
               fluidRow(
                 sidebarPanel(
                   dateInput("cover_date", "Date of cover with TopTex", value="2021-12-01"),
                   h4("Temperature model"),
                   selectInput("temp_air_yr","Temperature data from which year?", choices = list("2020"="yr2020", "2019"="yr2019", "2018"="yr2018", "2017"="yr2017", "2016"="yr2016")),
                   selectInput("temp_clamp_model","Clamp temperature model", choices = list("Moving average with floor"=1, "Air with floor"=2, "Air"=3, "Ventilated" = 4)),
                   sliderInput("clamp_size", "Clamp width at base (m)", step = 0.1, min=7, max=9, value=8),
                   sliderInput("ref_temp", "Clamp temperature floor", min=0, max=10, value=5),
                   actionButton("help_storage_temp", "?")
                 ),
                 mainPanel(
                   plotly::plotlyOutput("temp_graph")
                 ),
                 style = 'padding-left:15px'
               ),
               fluidRow(
                 sidebarPanel(
                   h4("Loss model"),
                   selectInput("loss_model","Loss model", choices = list("Discontinuous"=1, "Linear"=2, "Quadratic"=3)),
                   sliderInput("mass_loss","Mass loss per degree day", min=0, max=0.05, value=0.01),
                   actionButton("help_storage_loss", "?")
                 ),
                 mainPanel(
                   plotly::plotlyOutput("loss_Cd")
                 ),
                 style = 'padding-left:15px'
               )
             )
    ),
    tabPanel("DELIVERY & PAYMENT", fluid = T,
             sidebarLayout(
               fluidRow(
                 sidebarPanel(
                   h4("Delivery"),
                   dateInput("delivery_date","Delivery date",value = "2022-01-15"),
                   sliderInput("delivery_distance","Distance to deliver (mil)",min=1,max=20,step=0.1,value=5),
                   sliderInput("delivery_cost","Cost for field",min=1000,max=200000,value=5000),
                   sliderInput("delivery_loads", "Number of loads from field", min=1, max=200, value=10)
                 ),
                 mainPanel(
                   tableOutput("delivery_cost_tab")
                 ),
                 style = 'padding-left:15px'
               ),
               fluidRow(
                 sidebarPanel(
                   h4("Payment"),
                   numericInput("price", "Your contract price", value=306.48),
                   checkboxInput("vol","Eligible for volume bonus?", value = F),
                   br(),
                   sliderInput("pol", "Sugar content", min=15, max=22, value=17, step = 0.1),
                   sliderInput("renhet", "Renhet %", min=78, max=100, value=89.5, step = 0.1)
                 ),
                 mainPanel(
                   tableOutput("summary_final_tab")
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
               column(3,checkboxGroupInput("summary_tab_show","Show:",choices=c("Per ha"="HA", "Per field"="FI", "Per tonne clean" = "CL"," Per tonne delivered"="DE"),selected = )),
               column(6,)
             ),
             fluidRow(column(12,tableOutput("summary_tab_output")))
    ),
    tabPanel("GRAPHS - PRODUCTION", fluid = T,
             fluidRow(
               column(6,plotly::plotlyOutput("summary_graph_temp")),
               column(6,plotly::plotlyOutput("summary_graph_pol"))
             ),
             fluidRow(
               column(6,plotly::plotlyOutput("summary_graph_mass")),
               column(6,plotly::plotlyOutput("summary_graph_sug"))
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
    )#,
    #tabPanel("Payment Comparison", fluid = T,
    #         fluidRow(
    #           column(7, actionButton("comp_1", "Compare the current set-up")),
    #           column(5, actionButton("comp_2", "Compare the current set-up"))
    #         ),
    #         fluidRow(
    #           column(5,tableOutput("comp_1_para")), # table summarising the last week of data for the case
    #           column(2,),
    #           column(5,tableOutput("comp_2_para"))
    #         ),
    #         fluidRow(
    #           column(5,plotOutput("summary_graph_price_comp_1")), # graph showing the payment graph
    #           column(2,),
    #           column(5,plotOutput("summary_graph_price_comp_2"))
    #           ),
    #         fluidRow(
    #           column(5,plotOutput("summary_graph_payment_comp_1")), # graph showing the payment graph
    #           column(2,),
    #           column(5,plotOutput("summary_graph_payment_comp_2"))
    #         ),
    #         fluidRow(
    #           column(5,tableOutput("comp_1_tab")),
    #           column(2,), # table summarising the case
    #           column(5,tableOutput("comp_2_tab"))
    #         )
    #)
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
    
    root_mass_harvest <- full_tab_p$cum_mass[which(full_tab_p$date_full == as.POSIXct(harvest_date_p))] 
    root_mass_grown <- full_tab_p$cum_mass[which(full_tab_p$date_full == (as.POSIXct(harvest_date_p) - 86400))]
    
    root_mass_delivered_field <- root_yield_p*field_size_p
    root_mass_harvest_field <- root_mass_harvest*field_size_p
    root_mass_grown_field <- root_mass_grown*field_size_p
    
    root_harvest_tab <- matrix(c(root_mass_grown, root_mass_harvest, root_yield_p,
                                 root_mass_grown_field, root_mass_harvest_field, root_mass_delivered_field), byrow=F, nrow=3) 
    colnames(root_harvest_tab) <- c("Ha","Field")
    rownames(root_harvest_tab) <- c("Grown","Harvested", "Delivered")
    
    root_harvest_tab
  })
  
  output$root_harvest_tab <- renderTable({
    root_harvest_tab()
  }, rownames = T)
  
  # LOCATION OF BEETS TABLE
  loc_tab <- reactive({
    
    harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
    delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
    location <- "clamp"
    
    # Full table of location of beets
    loc_tab <- data.frame(date_full, location)
    
    loc_tab$location[which(loc_tab$date_full <= harvest_date)] <- "field"
    loc_tab$location[which(loc_tab$date_full == harvest_date)] <- "harvest"
    loc_tab$location[which(loc_tab$date_full >= delivery_date)] <- "delivered"
    
    loc_tab
  })
  
  # LATE SEASON GROWTH
  LSG_tab <- reactive({
    LSG_pot <- input$late_potent
    
    LSG_pol <- rep(0.02, length(date_full))
    
    LSG_root_day <- seq(1,length(date_full)+5)
    LSG_root_daily <- 1.5735e-06*LSG_root_day^2-2.8177e-04*LSG_root_day+0.01244
    LSG_root_daily <- c(0.01244,LSG_root_daily)
    LSG_root_daily[which(LSG_root_daily <= 0)] <- 0
    LSG_root_daily[100:length(date_full)] <-0
    LSG_root_daily <- LSG_root_daily[1:length(date_full)]
    LSG_root_daily <- LSG_root_daily*LSG_pot
    
    LSG_root_cum <- cumsum(LSG_root_daily)
    LSG_pol_cum <- cumsum(LSG_pol)
    
    LSG_tab <- data.frame(date_full,LSG_root_daily,LSG_root_cum,LSG_pol_cum)
    
    LSG_tab
    
  })
  
  output$LSG_chart <- plotly::renderPlotly({
    ggplot(LSG_tab(), aes(x=date_full)) + 
      geom_line(aes(y = LSG_root_daily), color = "darkred") + 
      ylab("Daily growth (%)") + 
      xlab("Date")
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
      temp_clamp <- c(temp_air[1],temp_air[1]/3+2*temp_air[2]/3,temp_clamp[-1])
      temp_clamp <- temp_clamp[1:(length(temp_clamp)-1)]
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
      temp_clamp <- c(temp_air[1],temp_air[1]/3+2*temp_air[2]/3,temp_clamp[-1])
      temp_clamp <- temp_clamp[1:(length(temp_clamp)-1)]
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
    ref_loss_data$actual <- ref_loss_data$ref_medel*factor()
    
    #Write table
    ref_loss_data
  })
  
  # MASS LOSS
  
  mass_loss_tab <- reactive({
    
    mass_loss_p <- input$mass_loss
    
    cum_temp <- seq(1,1000)
    cum_temp <- c(0,cum_temp)
    
    cum_mass_loss <- mass_loss_p * cum_temp
    
    mass_loss_tab <- data.frame(cum_temp, cum_mass_loss)
    
    mass_loss_tab
    
  })
  
  # HARVEST LOSS
  summary_harvest_loss <- reactive({
    harvest_loss_p <- input$root_tip_break_perc
    
    harvest_loss_tn_summary <- harvest_loss_tab$harvest_loss_tn[which(harvest_loss_tab$root_tip_break_perc == harvest_loss_p)]
    
    summary_harvest_loss <- matrix(c(harvest_loss_tn_summary))
    colnames(summary_harvest_loss) <- c("Harvest loss (tn/ha)")
    
    summary_harvest_loss
  })
  
  # Summary harvest loss
  output$summary_harvest_loss = renderTable({
    summary_harvest_loss()
  }, digits = 1)
  
  # Delivery cost table
  
  output$delivery_cost_tab <- renderTable({
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
    delivery_cost_tab
  }, rownames = T, digits=1
  )
  
  # FULL RESULTS TABLE
  
  full_tab = reactive({
    # input from all previous tables
    temp_tab_p <- data.frame(temp_tab())
    loss_tab_p <- data.frame(loss_tab())
    mass_loss_tab_p <- data.frame(mass_loss_tab())
    loss_tab_p <- loss_tab_p[,c("cum_temp","actual")]
    loc_tab_p <- data.frame(loc_tab())
    LSG_tab_p <- data.frame(LSG_tab())
    
    # input from required inputs
    harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
    delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
    cover_date <- as.POSIXct(input$cover_date, tz = "UTC", format = "%Y-%m-%d")
    kr_tonne <- input$price
    renhet <- input$renhet/100
    pol <- input$pol
    root_yield <- input$root_yield
    vol <- input$vol
    field_size <- input$field_size
    root_harvest <- root_yield*field_size
    root_tip_break_perc <- input$root_tip_break_perc
    
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
    ## Loss for given temp 
    full_tab <- merge(full_tab, loss_tab_p, by="cum_temp")
    names(full_tab)[names(full_tab)=="actual"] <- "cum_percent_loss"
    full_tab <- full_tab[,c("date_full","location","price_early","price_late","price_TT","price_vol","temp_clamp_p","cum_temp","cum_percent_loss")]
    ## 
    cum_percent_loss_max <- max(full_tab$cum_percent_loss[which(full_tab$date_full<=delivery_date)])
    full_tab$cum_pol <- pol + pol*(cum_percent_loss_max/100) - (full_tab$cum_percent_loss/100)*pol
    full_tab$pol_factor <- (full_tab$cum_pol - ref_pol*100)*kr_pol
    
    ## Mass loss for given temp
    full_tab <- merge(full_tab, mass_loss_tab_p, by="cum_temp")
    cum_mass_loss_max <- max(full_tab$cum_mass_loss[which(full_tab$date_full<=delivery_date)])
    full_tab$cum_mass <- root_yield + root_yield*(cum_mass_loss_max/100) - root_yield*(full_tab$cum_mass_loss/100)
    
    ## Mass loss at harvest
    harvest_loss <- harvest_loss_tab$harvest_loss_tn[which(harvest_loss_tab$root_tip_break_perc == root_tip_break_perc)]
    
    ## Mass gain under late season growth
    full_tab <- merge(full_tab, LSG_tab_p, by="date_full")
    root_mass_harvest <- full_tab$cum_mass[which(full_tab$date_full == harvest_date)] 
    root_mass_grown <- root_mass_harvest + harvest_loss
    LSG_root_cum_max <- full_tab$LSG_root_cum[which(full_tab$date_full == harvest_date)]
    #full_tab$cum_mass[which(full_tab$date_full < harvest_date)] <-
    #  root_mass_grown - root_mass_grown*(LSG_root_cum_max/100) + root_mass_grown*(full_tab$LSG_root_cum/100)
    full_tab$cum_mass <- ifelse(full_tab$date_full < harvest_date, 
                                root_mass_grown - root_mass_grown*(LSG_root_cum_max/100) + root_mass_grown*(full_tab$LSG_root_cum/100), full_tab$cum_mass)
    
    
    ## Pol gain under late season growth
    pol_harvest <- full_tab$cum_pol[which(full_tab$date_full == harvest_date)]
    LSG_pol_cum_max <- full_tab$LSG_pol_cum[which(full_tab$date_full == harvest_date)]
    #full_tab$cum_pol[which(full_tab$date_full < harvest_date)] <-
    #  pol_harvest - pol_harvest*(LSG_pol_cum_max/100) + pol_harvest*(full_tab$LSG_pol_cum/100)
    full_tab$cum_pol <- ifelse(full_tab$date_full < harvest_date,
                               pol_harvest - pol_harvest*(LSG_pol_cum_max/100) + pol_harvest*(full_tab$LSG_pol_cum/100),full_tab$cum_pol)    
    
    ## SUGAR YIELD
    full_tab$cum_sug <- full_tab$cum_pol/100*full_tab$cum_mass
    
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
    
  })
  
  # SUMMARY (VISUALISED) RESULTS
  ##!!! This should really just restrict the full results table within the parameters given.
  
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
    summary_tab_show <- c("date_full", "location", "temp_clamp_p", "cum_temp", "cum_percent_loss", "cum_pol","cum_mass","cum_sug")
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
    loss_max <- max(summary_tab$cum_percent_loss)
    pol_max <- max(summary_tab$cum_pol)
    pol_min <- min(summary_tab$cum_pol)
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
  ###############
  # VISUALS
  
  # Summary price table
  output$summary_tab_output = renderTable({
    summary_tab_output()
  }, digits=1)
  
  # Summary outputs table
  output$summary_final_tab = renderTable({
    summary_final_tab()
  },rownames = T, digits=0)
  
  # Plot of temperatures (Air and Clamp)
  output$temp_graph <- plotly::renderPlotly({
    
    ggplot(temp_tab(), aes(x=date_full)) + 
      geom_line(aes(y = temp_clamp), color = "darkred") + 
      geom_line(aes(y = temp_air), color="steelblue", linetype="twodash") +
      ylab("Temperature (C)") + 
      xlab("Date")
  })
  
  # Plot of sugar loss
  output$loss_Cd <- plotly::renderPlotly({
    
    ggplot(loss_tab(), aes(x=cum_temp)) + 
      geom_line(aes(y = actual), color = "darkred") + 
      geom_line(aes(y = ref_medel), color="steelblue", linetype="twodash") +
      xlim(0,500) +
      ylim(0,15) +
      ylab("Sugar loss (%)") + 
      xlab("Accumulated temperature (Cd)")
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
      geom_line(aes(y = cum_percent_loss, color = "Cum. % loss")) + 
      geom_line(aes(y = cum_pol * amplify - move, color = "Pol")) +
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
  
  output$summary_graph_price_cl <- plotly::renderPlotly({
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
  })
  
  output$summary_graph_price_de <- plotly::renderPlotly({
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
  })
  
  ###############################
  # HELP!
  
  observeEvent(input$help_field, {
    showModal(modalDialog(
      title = "In the field",
      "All varieties should have the same late-season growth potential, measured as a daily percent increase in biomass", br(), br(),
      "Late season growth potential is determined by issues like disease or water stress.", br(), br(),
      "Sources: Joakim",
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_harvest, {
    showModal(modalDialog(
      title = "Harvest",
      "Harvest conditions determine the rate of damage.", br(), br(),
      "Harder harvest conditions, a higher roter speed, and weaker beet varieties (selected on the previous page) give higher damage.",
      "Higher damage means higher loss during storage.", br(), br(),
      "The higher your damage score, the higher your rate of loss will be for each degree-day.",
      "You can see this in the graph of damage per degree day (right) - your storage loss (red line) relative to the median loss (blue dashed line).", br(), br(),
      "The Loss Models are different ways of defining the relationship between storage loss and degree-days.",
      "Sources: Linear model - Jaggard et al. 1997. Discontinuous and Quadratics - Legrande and Wauters, 2012",
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_storage_temp, {
    showModal(modalDialog(
      title = "Storage temperature model",
      "Storage conditions determine the clamp temperature in relation to the air temperature.", br(), br(),
      "The date the clamp is covered with TopTex is only used to determine the TopTex bonus.", br(), br(),
      "Clamp width determines a multiplication factor on the clamp temperature relative to the air temperature -",
      "a wider clamp will build more heat than a thinner one.", 
      "At 7m, this multiplication factor is 1.00. At 9m, it is 1.10",br(), br(),
      "The difference years give historical mean temperatures for Borgeby.",br(), br(),
      "The difference Clamp Temp Models define different relationships between clamp temperature and air temperature.",
      "For models with a temperature floor - a minimum temperature the clamp can go to - Temperature Floor sets this.",br(), br(),
      "Sources: just general observation",
      easyClose = T,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_storage_loss, {
    showModal(modalDialog(
      title = "Storage loss model",
      "Storage conditions determine the clamp temperature in relation to the air temperature.", br(), br(),
      "The date the clamp is covered with TopTex is only used to determine the TopTex bonus.", br(), br(),
      "Clamp width determines a multiplication factor on the clamp temperature relative to the air temperature -",
      "a wider clamp will build more heat than a thinner one.", 
      "At 7m, this multiplication factor is 1.00. At 9m, it is 1.10",br(), br(),
      "The difference years give historical mean temperatures for Borgeby.",br(), br(),
      "The difference Clamp Temp Models define different relationships between clamp temperature and air temperature.",
      "For models with a temperature floor - a minimum temperature the clamp can go to - Temperature Floor sets this.",br(), br(),
      "Sources: just general observation",
      easyClose = T,
      footer = NULL
    ))
  })
  
  ###############################
  # COMPARISONS
  
  #para_tab = reactive({
  #  para_tab_factor <- factor()*0.018
  #  para_tab <- data.frame(input$pol,input$renhet, input$delivery_date, para_tab_factor, input$root_yield*input$field_size)
  #  colnames(para_tab) <- c("Pol", "Cleanness", "Delivery date", "Storage loss rate (%/Cd)", "Harvest")
  #  para_tab
  #  })
  #
  #observeEvent(input$comp_1, {
  #               comp_1_tab$data <- summary_tab()
  #               comp_1_tab$para <- para_tab()
  #               comp_1_tab$payment <- full_tab()
  #             })
  #  
  #observeEvent(input$comp_2, {
  #              comp_2_tab$data <- summary_tab()
  #              comp_2_tab$para <- para_tab()
  #              comp_2_tab$payment <- full_tab()
  #            })
  #
  #output$comp_1_tab = renderTable({
  #  comp_1_tab$data
  #})
  #
  #output$comp_1_para = renderTable({
  #  comp_1_tab$para
  #})
  #
  #output$comp_2_tab = renderTable({
  #  comp_2_tab$data
  #})
  #
  #output$comp_2_para = renderTable({
  #  comp_2_tab$para
  #})
  #
  #
  #output$summary_graph_price_comp_1 = renderPlot({
  #  ggplot(comp_1_tab$data, aes(x=get("Date"))) + 
  #    geom_line(aes(y = get("Payment - clean tn"), colour = "Total payment")) +
  #    geom_line(aes(y = get("Base price - clean tn"), colour = "Base payment")) + 
  #    geom_line(aes(y = get("Bonus - clean tn"), colour = "Bonus payment")) +
  #    geom_vline(xintercept = as.POSIXct(delivery_date)) +
  #    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
  #    scale_colour_manual("", 
  #                        breaks = c("Total payment", "Base payment", "Bonus payment"),
  #                        values = c("Total payment"="red3", "Base payment"="blue3", 
  #                                   "Bonus payment"="green3")) +
  #    ylab("Price (kr)") + 
  #    xlab("Date") +
  #    labs(title = "Price per clean tonne") + 
  #    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  #})
  #
  #output$summary_graph_payment_comp_1 = renderPlot({
  #    ggplot(comp_1_tab$payment, aes(x=date_full)) + 
  #      geom_line(aes(y = price_field, colour = "Total payment")) +
  #      geom_line(aes(y = price_base_field, colour = "Base payment")) + 
  #      geom_line(aes(y = price_bonus_field, colour = "Bonus payment")) +
  #      geom_vline(xintercept = as.POSIXct(delivery_date)) +
  #      #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
  #      scale_colour_manual("", 
  #                          breaks = c("Total payment", "Base payment", "Bonus payment"),
  #                          values = c("Total payment"="red3", "Base payment"="blue3", 
  #                                     "Bonus payment"="green3")) +
  #      ylab("Payment (kr)") + 
  #      xlab("Date") +
  #      labs(title = "Payment per field") + 
  #      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  #})
  #
  #output$summary_graph_price_comp_2 = renderPlot({
  #  ggplot(comp_2_tab$data, aes(x=get("Date"))) + 
  #    geom_line(aes(y = get("Payment - clean tn"), colour = "Total payment")) +
  #    geom_line(aes(y = get("Base price - clean tn"), colour = "Base payment")) + 
  #    geom_line(aes(y = get("Bonus - clean tn"), colour = "Bonus payment")) +
  #    geom_vline(xintercept = as.POSIXct(delivery_date)) +
  #    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
  #    scale_colour_manual("", 
  #                        breaks = c("Total payment", "Base payment", "Bonus payment"),
  #                        values = c("Total payment"="red3", "Base payment"="blue3", 
  #                                   "Bonus payment"="green3")) +
  #    ylab("Price (kr)") + 
  #    xlab("Date") +
  #    labs(title = "Price per clean tonne") + 
  #    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  #})
  #
  #output$summary_graph_payment_comp_2 = renderPlot({
  #  ggplot(comp_2_tab$payment, aes(x=date_full)) + 
  #    geom_line(aes(y = price_field, colour = "Total payment")) +
  #    geom_line(aes(y = price_base_field, colour = "Base payment")) + 
  #    geom_line(aes(y = price_bonus_field, colour = "Bonus payment")) +
  #    geom_vline(xintercept = as.POSIXct(delivery_date)) +
  #    #scale_y_continuous(sec.axis = sec_axis(~. * sec_y_max / loss_max, name = "Pol sugar")) +
  #    scale_colour_manual("", 
  #                        breaks = c("Total payment", "Base payment", "Bonus payment"),
  #                        values = c("Total payment"="red3", "Base payment"="blue3", 
  #                                   "Bonus payment"="green3")) +
  #    ylab("Payment (kr)") + 
  #    xlab("Date") +
  #    labs(title = "Payment per field") + 
  #    theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  #})
  
}

###############################################
#
# Run the model
#
###############################################

shinyApp(ui=ui, server=server)