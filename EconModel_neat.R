########################################
#
# Swedish sugar beets economic model
# Will English, 2021-03-18
# FIXES: Date in Summary Table. Per truck, per ha into the payment schedule.
#
########################################


###############################################
#
# Setup
#
###############################################

{
 # -------------------------------------------
 snapshot_date = "2021-03-18"
 options("repos" = paste0("https://mran.revolutionanalytics.com/snapshot/", snapshot_date))
 # -------------------------------------------

 # -------------------------------------------
 # sink options
 options(width = 150)
 # rJava memory option
 options(java.parameters = "-Xmx8000m")
 # -------------------------------------------

 # R packages
 # -------------------------------------------
 Rpackages_version = c("shiny_1.6.0", "plotly_4.9.3", "sets_1.0-18", 
                       "ggplot2_3.3.3", "reshape2_1.4.4", "TTR_0.24.2")
 path_Rpackages = "C:/R packages_404"
 # -------------------------------------------
 
 # -------------------------------------------
 sessionInfo()
 # -------------------------------------------

 # version check and load packages
 # -------------------------------------------
 # R version check
 if(sessionInfo()$R.version$version.string != "R version 4.0.4 (2021-02-15)") stop("R.version must be 4.0.4 (2021-02-15)")

 # install packages
 Rpack = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[1])
 Rpack_version = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[2])
 if(!all(Rpack %in% list.files(path_Rpackages))){
   loadRpackages <- Rpack[!Rpack %in% list.files(path_Rpackages)]
   for(i in loadRpackages) install.packages(i, lib = path_Rpackages, repos = options("repos"), dependencies = T)
 }

 # load packages
 for(i in Rpack) eval(parse(text = paste0("library(", i, ", lib.loc = '", path_Rpackages, "')")))

 # Version check
 loadedPackagesAndVersions = sapply(sessionInfo()$otherPkgs, FUN = function(x) paste(x$Package, x$Version, sep = "_"))
 Rpack = Rpack[!Rpackages_version %in% loadedPackagesAndVersions]
 if(length(Rpack) > 0){
   for(i in rev(Rpack)) try(eval(parse(text = paste0("detach('package:", i, "', unload = T)"))), silent = T)
   for(i in Rpack) install.packages(i, lib = path_Rpackages, repos = options("repos"), dependencies = T)
   for(i in Rpack) eval(parse(text = paste0("library(", i, ", lib.loc = '", path_Rpackages, "')")))
 }
}
 
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
## This should be updated with actual temperature series from these years!
random_16 <- runif(days_full, min =-4, max=4)
random_17 <- runif(days_full, min =-4, max=3)
random_18 <- runif(days_full, min =-3, max=4)
random_19 <- runif(days_full, min =-4, max=4)
random_20 <- runif(days_full, min =-5, max=3)
day_drop_1 <- seq(0, length=days_full, by=0.1)
day_drop_2 <- seq(0, length=days_full, by=0.11)
#yr2016 <- c(10,8,8,8,7,9,7,8,9,8,7,6,7,6,6,7,8,9,8,7,6,7,6,6,6,7,8,8,7,6,6,6,7,6,5,4,4,5,5,6,5,4,3,4,5,6,5,4,3,3,3,2,1,1,2,6,5,6,7,8,10,8,8,8,7,9,7,8,9,8,7,6,7,6,6,6,7,8,8,7,6,6,6,7,6,5,4,4,5,5,6,5,4,3,4,5,6,5,4,3,3,3,5,4,5,6,7,6,5,4,4,4,5,5,5,4,4,3,2,3,3,4,4,3,2,1,1,2,6,5,6,7,8,5,4,5,6,7,3,2,1,1,2,6,5,6,7,8,5,4,5,6,7,5,6,7,8,8,8,8,7,6,7,6,5,6,5,4,3,3,2,2)
yr2016 <- 15 - random_16 - day_drop_1
yr2017 <- 16 - random_17 - day_drop_2
yr2018 <- 15 - random_18 - day_drop_2
yr2019 <- 14 - random_19 - day_drop_1
yr2020 <- 15 - random_20 - day_drop_1
temp_tab_historical <- data.frame(yr2016,yr2017,yr2018,yr2019,yr2020)

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
    tabPanel("In the Field", fluid = T,
             sidebarLayout(
               fluidRow(
                 sidebarPanel(
                   selectInput("variety_type","Variety type", choices = list("N - normal"=2, "E - high root yield"=1,"Z - high pol type"=3)),
                   sliderInput("variety_hardness", "Variety hardness", min=0, max=100, value = 50)
                 ),
                 style = 'padding-left:15px'
               ),
               fluidRow(
                 sidebarPanel(
                   sliderInput("root_yield","Root yield (t/ha)", min=40, max=120, step = 1, value = 86),
                   sliderInput("field_size","Field size (ha)", min=0, max=200, step = 0.1, value = 10)
                 ),
                 mainPanel(
                   tableOutput("root_harvest_tab")
                 ),
                 style = 'padding-left:15px'
               )
             )
    ),
    tabPanel("Harvest and Storage", fluid = T,
             sidebarLayout(
               fluidRow(
                 sidebarPanel(
                   h4("HARVEST"),
                   dateInput("harvest_date","Harvest date",value = "2021-11-15"),
                   sliderInput("late_moisture", "Moisture late in the season, as percent of ideal", min=0, max=200, value=100),
                   sliderInput("harvester_cleaning", "Rotor speed", min=0, max=100, value=40),
                   h4("Loss model"),
                   selectInput("loss_model","Loss model", choices = list("Discontinuous"=1, "Linear"=2, "Quadratic"=3))
                   
                 ),
                 mainPanel(
                   plotly::plotlyOutput("loss_Cd")
                 ),
                 style = 'padding-left:15px'
               ),
               fluidRow(
                 sidebarPanel(
                   h4("STORAGE"),
                   sliderInput("clamp_size", "Clamp width at base (m)", step = 0.1, min=7, max=9, value=8),
                   dateInput("cover_date", "Date of cover with TopTex", value="2021-12-01"),
                   h4("Temperature model"),
                   selectInput("temp_air_yr","Temperature data from which year?", choices = list("2020"="yr2020", "2019"="yr2019", "2018"="yr2018", "2017"="yr2017", "2016"="yr2016")),
                   selectInput("temp_clamp_model","Clamp temperature model", choices = list("Moving average with floor"=1, "Air with floor"=2, "Air"=3)),
                   sliderInput("ref_temp", "Clamp temperature floor", min=0, max=10, value=5)
                   ),
                 mainPanel(
                   plotly::plotlyOutput("temp_graph")
                 ),
                 style = 'padding-left:15px'
               )
               #,
               #fluidRow(
              #   sidebarPanel(
               #    h4("Loss model"),
              #     selectInput("loss_model","Loss model", choices = list("Linear"=1, "Discontinuous"=2,"Quadratic"=3))
               #  ),
              #   style = 'padding-left:15px'
               #)
             )
    ),
    tabPanel("Delivery and Payment", fluid = T,
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
    tabPanel("Summary table", fluid = T,
             fluidRow(
               column(3, checkboxInput("data_restrict","Restrict data end date to delivery date?"),
                      style = 'padding-left:15px'
               ),
               column(3,checkboxGroupInput("summary_tab_show","Show:",choices=c("Per tonne delivered"="DE", "Per ha"="HA", "Per field"="FI"),selected = )),
               column(6,)
            ),
             fluidRow(
               column(12,tableOutput("summary_tab"))
             )
    ),
    tabPanel("Summary graphs", fluid = T,
             fluidRow(
               column(6,plotOutput("summary_graph_temp")),
               column(6,plotOutput("summary_graph_loss"))
             ),
             fluidRow(
               column(6,plotOutput("summary_graph_price")),
               column(6,)
             )
    ),
    tabPanel("Payment Comparison", fluid = T,
             fluidRow(
               column(7, actionButton("comp_1", "Compare the current set-up")),
               column(5, actionButton("comp_2", "Compare the current set-up"))
             ),
             fluidRow(
               column(5,tableOutput("comp_1_para")), # table summarising the last week of data for the case
               column(2,),
               column(5,tableOutput("comp_2_para"))
             ),
             fluidRow(
               column(5,plotOutput("summary_graph_price_comp_1")), # graph showing the payment graph
               column(2,),
               column(5,plotOutput("summary_graph_price_comp_2"))
               ),
             fluidRow(
               column(5,plotOutput("summary_graph_payment_comp_1")), # graph showing the payment graph
               column(2,),
               column(5,plotOutput("summary_graph_payment_comp_2"))
             ),
             fluidRow(
               column(5,tableOutput("comp_1_tab")),
               column(2,), # table summarising the case
               column(5,tableOutput("comp_2_tab"))
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
  
  # Root harvest yield table 
  
  root_harvest_tab <- reactive({
    root_harvest <- input$root_yield * input$field_size
    root_harvest_tab <- matrix(c(root_harvest), byrow=F, nrow=1) 
    colnames(root_harvest_tab) <- c("Root harvest (tonne)")
    root_harvest_tab
  })
  
  output$root_harvest_tab <- renderTable({
    root_harvest_tab()
  })
  
  # TEMP TABLE. Table of daily temperature given the chosen inputs
  
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
    
    # Full table of daily temperatures compilation
    temp_tab <- data.frame(date_full, temp_air, temp_clamp)
    
    # Make the full table the output
    temp_tab
  })
  
  # LOSS FACTOR
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
  
  # LOSS MODEL TABLE
  
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
  }, rownames = T
  )
  
  # FULL RESULTS TABLE

  price_tab = reactive({
    # input from all previous tables
    temp_tab_p <- data.frame(temp_tab())
    loss_tab_p <- data.frame(loss_tab())
    loss_tab_p <- loss_tab_p[,"actual"]
    root_harvest_tab_p <- data.frame(root_harvest_tab())
    
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
    
    price_tab
    
    })

  # SUMMARY (VISUALISED) RESULTS
  ##!!! This should really just restrict the full results table within the parameters given.
      
  summary_tab = reactive({
    # input from required previous tables
    summary_tab <- data.frame(price_tab())
    
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

    summary_tab_names <- c("Date", "Temperature (C)", "Cum. Temp (Cd)", "Cum. % loss", "Pol", "Pol factor","Base price - clean tn","Bonus - clean tn","Payment - clean tn")
    if("DE" %in% summary_tab_show_input) summary_tab_names <- c(summary_tab_names, "Base price - delivered tn","Bonus - delivered tn","Payment - delivered tn")
    if("HA" %in% summary_tab_show_input) summary_tab_names <- c(summary_tab_names, "Base price - ha","Bonus - ha","Payment - ha")
    if("FI" %in% summary_tab_show_input) summary_tab_names <- c(summary_tab_names, "Base price - field","Bonus - field","Payment - field")
    colnames(summary_tab) <- summary_tab_names
    
    # Extract a little info from summary table for later graphing
    cum_loss_delivery <- summary_tab()$cum_temp[summary_tab$date_full==delivery_date]
    loss_max <- max(summary_tab$cum_percent_loss)
    pol_max <- max(summary_tab$cum_sug)
    pol_min <- min(summary_tab$cum_sug)
    pol_diff <- pol_max - pol_min
    amplify_factor <- 0.5
    amplify <- loss_max/pol_diff*amplify_factor
    move <- pol_max*amplify - loss_max*0.85
          
    summary_tab
    
  })
  
  # Summary Table of the bottom line
  summary_final_tab = reactive({
    summary_final_tab <- data.frame(summary_tab())
    summary_final_tab <- summary_tab_final[which(summary_tab_final$date_full == delivery_date),]
    summary_final_tab <- matrix(summary_tab_final[c("price_base_delivered","price_bonus_delivered","price_delivered",
                                    "price_base_clean","price_bonus_clean","price_clean",
                                    "price_base_ha","price_bonus_ha","price_ha",
                                    "price_base_field","price_bonus_field","price_field")],
                        byrow=T,nrow=4)
    rownames(summary_final_tab) <- c("Tonne - delivered", "Tonne - clean", "Hectare", "Field")
    colnames(summary_final_tab) <- c("Base price","Bonuses","Total payment")
    
    summary_final_tab
    
    })
  ###############
  # VISUALS
  
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
      xlim(0,500)+
      ylab("Sugar loss (%)") + 
      xlab("Accumulated temperature (Cd)")
  })
  
  
  summary_graph_temp <- plotly::renderPlotly({
    ggplot(summary_tab()) + 
      geom_line(aes(x=date_full, y=cum_temp, color = "Cum. temperature")) + 
      geom_vline(xintercept = as.POSIXct(delivery_date), linetype="dotted") +
      geom_hline(yintercept = cum_loss_delivery, linetype="dotted") +
      scale_colour_manual("", 
                          breaks = c("Cum. temperature"),
                          values = c("Cum. temperature"="red3")) +
      ylab("Cumulative temperature (Cd)") + 
      xlab("Date") +
      labs(title = "Temperature") + 
      theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
  })
  
  summary_graph_loss <- plotly::renderPlotly({
    ggplot(summary_tab(), aes(x=date_full)) + 
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
  })
  
  summary_graph_price <- plotly::renderPlotly({
    ggplot(summary_tab(), aes(x=date_full)) + 
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
  })
  
  summary_graph_price_delivered <- plotly::renderPlotly({
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
  })
  
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
  
}

###############################################
#
# Run the model
#
###############################################

shinyApp(ui=ui, server=server)






####################################################################
#
# DEBUGGING CODE
#
####################################################################

harvest_date <- "2021-10-01"
delivery_date <- "2021-12-25"
cover_date <- "2021-11-15"
field_size <- 100
root_yield <-100
delivery_distance <- 5
delivery_cost <- 10000
delivery_loads <- 50
ref_temp <- 5
temp_tab_historical <- c(10,8,8,8,7,9,7,8,9,8,7,6,7,6,6,6,7,8,8,7,6,6,6,7,6,5,4,4,5,5,6,5,4,3,4,5,6,5,4,3,3,3,2,1,1,2,6,5,6,7,8,5,4,5,6,7,6,5,4,4,4,5,5,5,4,4,3,2,3,3,4,4,5,6,7,8,8,8,8,7,6,7,6,5,6,5,4,3,3,2,2)
factor <- 1.2
pol <- 19
clamp_size <- 8
price <- 270
moisture <- 100
factor <- 1.2
renhet <- 90
ref_renhet <- 0.895
vol <- F
data_restrict <- F
loss_model <- 1

input <- data.frame(harvest_date, delivery_date, cover_date, root_yield, field_size, delivery_distance, delivery_cost, delivery_loads,
                    ref_temp, pol, clamp_size, moisture, factor, renhet, vol, price, data_restrict, loss_model)

