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
                       "ggplot2_3.3.3", "reshape2_1.4.4")
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

## Prices per tonne
kr_tonne <- 275 # at reference pol of 17%
kr_renhet <- 2 # extra kr per %-point over ref, at 17% pol%
kr_pol <- 9 # extra % per %-point over ref of 17% pol
kr_TT <- 5 # for the use of TopTex for at least 7 days prior to delivery, for delivery after 15 November
kr_late <- 1 # extra payment per tonne for beets delivered late
kr_vol <- 5 # extra per tonne if area increases 10%
date_full <- seq(as.POSIXct("2020-09-10", tz = "UTC", format = "%Y-%m-%d"), length.out = 172, by = "1 day")
pris_early <- c(37,37,37,37,37,36,33,30,27,24,21,19,16,14,12,10,8,6,4,2,1,rep(0,151))
pris_late <- c(rep(0,82),seq(1,23,by=1),seq(24.5,123.5,by=1.5))
pris_TT <- c(rep(0,66),rep(5,8), rep(10,8), rep(15,90))
pris_vol <- rep(0,172)
kr_tab <- data.frame(date_full,pris_early, pris_late, pris_TT, pris_vol)

## Reference values
ref_hardness <- 50
ref_pol <- 0.17
ref_renhet <- 0.895
ref_TT_1 <- as.POSIXct("2020-11-15", tz = "UTC", format = "%Y-%m-%d") # 5kr/tn beets
ref_TT_2 <- as.POSIXct("2020-11-22", tz = "UTC", format = "%Y-%m-%d") # 10kr/tn beets
ref_TT_3 <- as.POSIXct("2020-12-01", tz = "UTC", format = "%Y-%m-%d") # 15kr/tn beets
ref_early <- as.POSIXct("2020-09-30", tz = "UTC", format = "%Y-%m-%d") # last day the early payment is made for
ref_late_1 <- as.POSIXct("2020-12-01", tz = "UTC", format = "%Y-%m-%d") #first day you get money for late delivery
ref_late_2 <- as.POSIXct("2021-01-01", tz = "UTC", format = "%Y-%m-%d") #first day you get 1.5 x money for late delivery
ref_Cd <- seq(1:500)
ref_medel_linear <- ref_Cd*0.0188
ref_medel_discont <- ifelse(ref_Cd <= 270, ref_Cd*0.013, 270*0.013+(ref_Cd-270)*0.042)
ref_medel_quad <- (ref_Cd*-0.0043 + 0.000064*ref_Cd^2)
ref_loss_data <- data.frame(ref_Cd,ref_medel_linear, ref_medel_discont, ref_medel_quad)
ref_temp <- 5

#ref_models <- melt(ref_loss_data, id = "ref_Cd")
#ggplot(ref_models, aes(x=ref_Cd, y=value, colour=variable, group=variable)) +
#  geom_line() + 
#  xlab("")

first_day <- min(kr_tab$date_full)
last_day <- max(kr_tab$date_full)
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
actual_temp <- data.frame(yr2016,yr2017,yr2018,yr2019,yr2020)

temp <- actual_temp[,"yr2018"]

#if(length(actual_temp) == 0) temp <- rep(ref_temp, days_full) else temp <- actual_temp[1:days_full]

# Data frame with all the base price parameters, and temperature
kr_tab <- data.frame(kr_tab, temp)

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
  clamp_size = fuzzy_partition(varnames = c(seven = 0, seven_ = 15, eight = 35, eight_ = 65, nine = 100),
                               sd = 10.0),
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
  fuzzy_rule(clamp_size %is% seven && variety %is% hard &&
               harvester_cleaning %is% gentle && late_moisture %is% perfect, loss_rate %is% v.low),
  fuzzy_rule(clamp_size %is% eight && variety %is% normal &&
               harvester_cleaning %is% medium && late_moisture %is% dry, loss_rate %is% medium),
  fuzzy_rule(clamp_size %is% nine && variety %is% soft &&
               harvester_cleaning %is% hard && late_moisture %is% wet, loss_rate %is% v.high),
  fuzzy_rule(harvester_cleaning %is% hard && variety %is% soft, loss_rate %is% v.high),
  fuzzy_rule(harvester_cleaning %is% hard, loss_rate %is% high),
  fuzzy_rule(harvester_cleaning %is% medium, loss_rate %is% medium),
  fuzzy_rule(harvester_cleaning %is% gentle, loss_rate %is% low),
  fuzzy_rule(late_moisture %is% wet, loss_rate %is% high),
  fuzzy_rule(late_moisture %is% dry, loss_rate %is% medium),
  fuzzy_rule(late_moisture %is% perfect, loss_rate %is% v.low),
  fuzzy_rule(clamp_size %is% seven, loss_rate %is% v.low),
  fuzzy_rule(clamp_size %is% eight, loss_rate %is% low),
  fuzzy_rule(clamp_size %is% nine, loss_rate %is% high),
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
                   h4("Harvest"),
                   dateInput("harvest_date","Harvest date",value = "2020-10-01"),
                   sliderInput("late_moisture", "Moisture late in the season, as percent of ideal", min=0, max=200, value=100),
                   sliderInput("harvester_cleaning", "Rotor speed", min=0, max=100, value=40)
                 ),
                 style = 'padding-left:15px'
               ),
               fluidRow(
                 sidebarPanel(
                   h4("Storage"),
                   sliderInput("clamp_size", "Clamp width at base (m)", step = 0.1, min=7, max=9, value=8),
                   dateInput("cover_date", "Date of cover with TopTex", value="2021-01-01"),
                   h4("Loss model"),
                   selectInput("loss_model","Loss model", choices = list("Discontinuous"=1, "Linear"=2, "Quadratic"=3))
                 ),
                 mainPanel(
                   plotly::plotlyOutput("loss_Cd")
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
                   dateInput("delivery_date","Delivery date",value = "2020-10-15"),
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
                   tableOutput("price_tab")
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
  
  observe(updateSliderInput(session, "variety_hardness", value = ifelse(input$variety_type == 1, 30, ifelse(input$variety_type == 2, 50, 70))))
  
  # Root harvest yield table 
  
  output$root_harvest_tab <- renderTable({
    root_harvest <- input$root_yield * input$field_size
    root_harvest_tab <- matrix(c(root_harvest), byrow=F, nrow=1) 
    colnames(root_harvest_tab) <- c("Root harvest (tonne)")
    root_harvest_tab
  })
  
  # Plot of sugar loss
  output$loss_Cd <- plotly::renderPlotly({
    data_clamp_size <- ((input$clamp_size - 7)*50)
    data_late_moisture <- (input$late_moisture * 0.5)
    
    example.1 <- fuzzy_inference(model, list(clamp_size = data_clamp_size, 
                                             harvester_cleaning = input$harvester_cleaning, 
                                             late_moisture = data_late_moisture,
                                             variety  = input$variety_hardness))
    
    if(input$loss_model == 1) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_discont 
    if(input$loss_model == 2) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_linear 
    if(input$loss_model == 3) ref_loss_data$ref_medel <- ref_loss_data$ref_medel_quad
    
    factor <- (gset_defuzzify(example.1, "centroid")/100+0.5)
    ref_loss_data$actual <- ref_loss_data$ref_medel*factor
    
    ggplot(ref_loss_data, aes(x=ref_Cd)) + 
      geom_line(aes(y = actual), color = "darkred") + 
      geom_line(aes(y = ref_medel), color="steelblue", linetype="twodash") +
      ylab("Sugar loss (%)") + 
      xlab("Accumulated temperature (Cd)")
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
  
  # Price table and Summary data

  pris_tab = reactiveVal()
  summary_tab = reactiveVal()  
  summary_graph_temp = reactiveVal()
  summary_graph_loss = reactiveVal()
  summary_graph_price = reactiveVal()
  summary_graph_price_delivered = reactiveVal()
  comp_1_tab <- reactiveValues(data = NULL)
  comp_2_tab <- reactiveValues(data = NULL)
  para_tab_factor <- reactiveVal()
    
  price_tab = reactive({
    pris_tab <- kr_tab
    harvest_date <- as.POSIXct(input$harvest_date, tz = "UTC", format = "%Y-%m-%d")
    delivery_date <- as.POSIXct(input$delivery_date, tz = "UTC", format = "%Y-%m-%d")
    cover_date <- as.POSIXct(input$cover_date, tz = "UTC", format = "%Y-%m-%d")
    kr_tonne <- input$price
    renhet <- input$renhet/100
    pol <- input$pol
    root_yield <- input$root_yield
    vol <- input$vol
    root_yield <- input$root_yield
    field_size <- input$field_size
    root_harvest <- root_yield*field_size
    summary_tab_show_input <- input$summary_tab_show
    
    data_clamp_size <- ((input$clamp_size - 7)*50)
    data_late_moisture <- (input$late_moisture * 0.5)
    example.1 <- fuzzy_inference(model, list(clamp_size = data_clamp_size, 
                                             harvester_cleaning = input$harvester_cleaning, 
                                             late_moisture = data_late_moisture,
                                             variety  = input$variety_hardness))
    factor <- (gset_defuzzify(example.1, "centroid")/100+0.5)
    para_tab_factor(
      factor*0.018
    )
    
    days_h_s <- round(as.numeric(difftime(delivery_date, harvest_date, units="days")+1))
    days_p_h <- round(as.numeric(difftime(last_day, harvest_date, units="days")+1))
    #date_h_s <- seq(as.Date(harvest_date, tz = "UTC", format = "%Y-%m-%d"), length.out = days_h_S, by = "1 day")
    pris_tab$cum_temp <- c(rep(0, (days_full - days_p_h)), cumsum(temp[which(pris_tab$date_full>=harvest_date)]))
    pris_tab$cum_percent_loss <- (pris_tab$cum_temp*0.0128 + 0.00002*pris_tab$cum_temp^2)*factor
    cum_percent_loss_max <- max(pris_tab$cum_percent_loss[which(pris_tab$date_full<=delivery_date)])
    pris_tab$cum_sug <- pol + pol*(cum_percent_loss_max/100) - (pris_tab$cum_percent_loss/100)*pol
    pris_tab$pol_factor <- (pris_tab$cum_sug - ref_pol*100)*kr_pol
    
    #TT bonus
    pris_tab$pris_TT[pris_tab$date_full < as.POSIXct(cover_date)+7] <- 0
    
    #Volume bonus
    vol <- input$vol
    if (vol == T) pris_tab$pris_vol = kr_vol
    
    #renhet bonus
    renhet_diff <- (renhet - ref_renhet)*100
    pris_renhet <- renhet_diff * kr_renhet
    
    # Clean prices
    pris_tab$pris_base_clean <- kr_tonne+kr_tonne*pris_tab$pol_factor/100
    pris_tab$pris_bonus_clean <- (pris_tab$pris_early + pris_tab$pris_late + pris_tab$pris_TT + pris_tab$pris_vol + pris_renhet)
    pris_tab$pris_clean <- pris_tab$pris_base_clean +  pris_tab$pris_bonus_clean
    
    # Delivered prices
    pris_tab$pris_base_delivered <- pris_tab$pris_base_clean*renhet
    pris_tab$pris_bonus_delivered <- pris_tab$pris_bonus_clean*renhet
    pris_tab$pris_delivered <- pris_tab$pris_clean*renhet
    
    # Ha prices
    pris_tab$pris_base_ha <- pris_tab$pris_base_delivered*root_yield
    pris_tab$pris_bonus_ha <- pris_tab$pris_bonus_delivered*root_yield
    pris_tab$pris_ha <- pris_tab$pris_delivered*root_yield

    # Field prices
    pris_tab$pris_base_field <- pris_tab$pris_base_ha*field_size
    pris_tab$pris_bonus_field <- pris_tab$pris_bonus_ha*field_size
    pris_tab$pris_field <- pris_tab$pris_ha*field_size
    
    # Summary table data
    summary_tab_show <- c("date_full", "temp", "cum_temp", "cum_percent_loss", "cum_sug", "pol_factor","pris_base_clean","pris_bonus_clean","pris_clean")
    if("DE" %in% summary_tab_show_input) summary_tab_show <- c(summary_tab_show, "pris_base_delivered","pris_bonus_delivered","pris_delivered")
    if("HA" %in% summary_tab_show_input) summary_tab_show <- c(summary_tab_show, "pris_base_ha","pris_bonus_ha","pris_ha")
    if("FI" %in% summary_tab_show_input) summary_tab_show <- c(summary_tab_show, "pris_base_field","pris_bonus_field","pris_field")
    data_end_date <- last_day
    if(input$data_restrict) data_end_date <- delivery_date
    summary_tab <- pris_tab[, summary_tab_show]
    summary_tab <- summary_tab[which(summary_tab$date_full >= harvest_date),]
    summary_tab <- summary_tab[which(summary_tab$date_full <= data_end_date),]
        
    cum_loss_delivery = summary_tab$cum_temp[summary_tab$date_full==delivery_date]
    
    summary_graph_temp(
      ggplot(summary_tab) + 
        #geom_bar(aes(x=date, weight = temp)) +
        geom_line(aes(x=date_full, y=cum_temp, color = "Cum. temperature")) + 
        geom_vline(xintercept = as.POSIXct(delivery_date), linetype="dotted") +
        geom_hline(yintercept = cum_loss_delivery, linetype="dotted") +
        #scale_y_continuous(sec.axis = sec_axis(~., name = "Temperature (C)")) +
        scale_colour_manual("", 
                            breaks = c("Cum. temperature"),
                            values = c("Cum. temperature"="red3")) +
        ylab("Cumulative temperature (Cd)") + 
        xlab("Date") +
        labs(title = "Temperature") + 
        theme(plot.title = element_text(size=15, face="bold.italic"), legend.position="bottom")
    )

    loss_max <- max(summary_tab$cum_percent_loss)
    pol_max <- max(summary_tab$cum_sug)
    pol_min <- min(summary_tab$cum_sug)
    pol_diff <- pol_max - pol_min
    amplify_factor <- 0.5
    amplify <- loss_max/pol_diff*amplify_factor
    move <- pol_max*amplify - loss_max*0.85
    
    summary_graph_loss(
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
    )
      
    summary_graph_price(
      ggplot(summary_tab, aes(x=date_full)) + 
        geom_line(aes(y = pris_clean, colour = "Total payment")) +
        geom_line(aes(y = pris_base_clean, colour = "Base payment")) + 
        geom_line(aes(y = pris_bonus_clean, colour = "Bonus payment")) +
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
    )
    
    summary_graph_price_delivered(
      ggplot(summary_tab, aes(x=date_full)) + 
        geom_line(aes(y = pris_delivered, colour = "Total payment")) +
        geom_line(aes(y = pris_base_delivered, colour = "Base payment")) + 
        geom_line(aes(y = pris_bonus_delivered, colour = "Bonus payment")) +
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
    )
    
    price_tab <- pris_tab[which(pris_tab$date_full == delivery_date),]
    price_tab <- matrix(price_tab[c("pris_base_delivered","pris_bonus_delivered","pris_delivered",
                                    "pris_base_clean","pris_bonus_clean","pris_clean",
                                    "pris_base_ha","pris_bonus_ha","pris_ha",
                                    "pris_base_field","pris_bonus_field","pris_field")],
                           byrow=T,nrow=4)
    rownames(price_tab) <- c("Tonne - delivered", "Tonne - clean", "Hectare", "Field")
    colnames(price_tab) <- c("Base price","Bonuses","Total payment")
    
    summary_tab_names <- c("Date", "Temperature (C)", "Cum. Temp (Cd)", "Cum. % loss", "Pol", "Pol factor","Base price - clean tn","Bonus - clean tn","Payment - clean tn")
    if("DE" %in% summary_tab_show_input) summary_tab_names <- c(summary_tab_names, "Base price - delivered tn","Bonus - delivered tn","Payment - delivered tn")
    if("HA" %in% summary_tab_show_input) summary_tab_names <- c(summary_tab_names, "Base price - ha","Bonus - ha","Payment - ha")
    if("FI" %in% summary_tab_show_input) summary_tab_names <- c(summary_tab_names, "Base price - field","Bonus - field","Payment - field")
    colnames(summary_tab) <- summary_tab_names
    
    summary_tab(
      summary_tab
    )

    pris_tab_post_harvest <- pris_tab[which(pris_tab$date_full>=harvest_date),]    
    pris_tab(
      pris_tab_post_harvest
    )
          
    price_tab
    
  })
  
  para_tab = reactive({
    para_tab <- data.frame(input$pol,input$renhet, input$delivery_date, para_tab_factor(), input$root_yield*input$field_size)
    colnames(para_tab) <- c("Pol", "Cleanness", "Delivery date", "Storage loss rate (%/Cd)", "Harvest")
    para_tab
    })
  
  observeEvent(input$comp_1, {
                 comp_1_tab$data <- summary_tab()
                 comp_1_tab$para <- para_tab()
                 comp_1_tab$payment <- pris_tab()
               })
  
  observeEvent(input$comp_2, {
                comp_2_tab$data <- summary_tab()
                comp_2_tab$para <- para_tab()
                comp_2_tab$payment <- pris_tab()
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
  output$price_tab = renderTable({
    price_tab()
  },rownames = T, digits=0)
  
  # Summary outputs  
  output$summary_tab = renderTable({
    summary_tab()
  })
  
  # temp graph
  output$summary_graph_temp = renderPlot({
    summary_graph_temp()
  })

  # loss graph
  output$summary_graph_loss = renderPlot({
    summary_graph_loss()
  })
  
  # price graph
  output$summary_graph_price = renderPlot({
    summary_graph_price()
  })
  
  output$summary_graph_price_delivered = renderPlot({
    summary_graph_price_delivered()
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
        geom_line(aes(y = pris_field, colour = "Total payment")) +
        geom_line(aes(y = pris_base_field, colour = "Base payment")) + 
        geom_line(aes(y = pris_bonus_field, colour = "Bonus payment")) +
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
      geom_line(aes(y = pris_field, colour = "Total payment")) +
      geom_line(aes(y = pris_base_field, colour = "Base payment")) + 
      geom_line(aes(y = pris_bonus_field, colour = "Bonus payment")) +
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

harvest_date <- "2020-10-01"
delivery_date <- "2020-12-25"
cover_date <- "2020-11-15"
field_size <- 100
root_yield <-100
delivery_distance <- 5
delivery_cost <- 10000
delivery_loads <- 50
ref_temp <- 5
actual_temp <- c(10,8,8,8,7,9,7,8,9,8,7,6,7,6,6,6,7,8,8,7,6,6,6,7,6,5,4,4,5,5,6,5,4,3,4,5,6,5,4,3,3,3,2,1,1,2,6,5,6,7,8,5,4,5,6,7,6,5,4,4,4,5,5,5,4,4,3,2,3,3,4,4,5,6,7,8,8,8,8,7,6,7,6,5,6,5,4,3,3,2,2)
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

input <- data.frame(harvest_date, delivery_date, cover_date, root_yield, field_size, delivery_distance, delivery_cost, delivery_loads,
                    ref_temp, pol, clamp_size, moisture, factor, renhet, vol, price, data_restrict)
