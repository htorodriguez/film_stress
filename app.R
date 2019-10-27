#Author: Humberto Rodriguez-Alvarez
#Version: 0.1
#Date: 27.10.2019
#

library(shiny)
library(dplyr)
library(ggplot2)
###constants
nu_Si<-0.28#Poissn 100 Wafer, 0.06 110 Wafer 
E_Si<-130e3#MPa 100 wafer, 170 for 110 Wafer
nu_SiC<-0.14
E_SiC<-400e3
##
bow_from_radius<-function(radius, diameter){
        radius*(1-cos(diameter*1000/2/radius))
}
     
####
radius_from_bow2<-function(b_input, w_diam, r_start=10000000){
        sum_square_diff<-function(r_diff, b_diff=b_input){
                diff_sq<-b_diff-bow_from_radius(r_diff, w_diam)
                result<-diff_sq^2
                result
        }
        res<-optim(par=r_start, fn = sum_square_diff, control=c(maxit=10000), method='Brent', lower=150, upper=1e11)
        output<-res$par[1]
        output
}

radius_from_bow<-function(b_input, w_diam){
        (4*b_input^2+(w_diam*1e3)^2)/(8*b_input)
}
###

stress_from_bow<-function(bow_before, bow_after, E_Si, nu_Si, Si_wafer_th, film_th, w_diam){
        r_before<-radius_from_bow(bow_before, w_diam)
        r_after<-radius_from_bow(bow_after, w_diam)
        r_final<-1/(1/r_after-1/r_before)
        -(E_Si*(Si_wafer_th)^2)/(6*(1-nu_Si)*(film_th/1000)*r_final)
}

rfinal_from_stress<-function(stress, E, nu, wafer_th, film_th, w_diam){

        -(E*(wafer_th)^2)/(6*(1-nu)*(film_th/1000)*stress)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
        
        # Application title
        titlePanel("Thin film stress calculator"),
        h4("Calculates film stress bases on bow measurements on a <100> Si Wafer"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        sliderInput("th_wafer",
                                    "Si Wafer Thickness in micrometers",
                                    min = 100,
                                    max = 900,
                                    value = 672),
                        
                        numericInput("diam_wafer",
                                    "Si Wafer diameter in millimeters",
                                    value=150),
                        
                        numericInput("th_film",
                                    "Thin film thickness in nanometers", 
                                    value=275),
                        
                        numericInput("bow_before",
                                    "Bow of Si Wafer before deposition in micrometers",
                                    value=0.1  ),
                        
                        numericInput("bow_after",
                                    "Bow of Si Wafer after deposition in micrometers",
                                    value = 11.45)
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        h2("Stress Results"),

                        tableOutput("table_res"),
                        
                        tableOutput("table_rad"), 
                        
                        tableOutput("table_sic")

                        )
                        )
                        )

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        output$table_res<- renderTable({
                stress<-stress_from_bow(as.numeric(input$bow_before), 
                                        as.numeric(input$bow_after), 
                                        E_Si, 
                                        nu_Si, 
                                        as.numeric(input$th_wafer), 
                                        as.numeric(input$th_film),
                                        as.numeric(input$diam_wafer))
                type<-if (sign(stress)<0) {"compressive"} else {"tensile"}
                        "compressive"
                data.frame("Stress_MPa"=stress, "Type"=type)
                
        }) 
        
        output$table_rad<- renderTable({

                data.frame("Radius before"=radius_from_bow(input$bow_before, input$diam_wafer)*1e-6, 
                           "Radius after"=radius_from_bow(input$bow_after, input$diam_wafer)*1e-6)
                
        }) 

        output$table_sic<- renderTable({
                
                stress<-stress_from_bow(as.numeric(input$bow_before), 
                                        as.numeric(input$bow_after), 
                                        E_Si, 
                                        nu_Si, 
                                        as.numeric(input$th_wafer), 
                                        as.numeric(input$th_film),
                                        as.numeric(input$diam_wafer))
                
                radius_sic_150<-rfinal_from_stress(stress, 
                                            E_SiC, 
                                            nu_SiC, 
                                            150, 
                                            as.numeric(input$th_film),
                                            as.numeric(input$diam_wafer))
                
                radius_sic_180<-rfinal_from_stress(stress, 
                                           E_SiC, 
                                           nu_SiC, 
                                           180, 
                                           as.numeric(input$th_film),
                                           as.numeric(input$diam_wafer))
                
                bow_sic_150<-round(bow_from_radius(radius_sic_150, input$diam_wafer))
                bow_sic_180<-round(bow_from_radius(radius_sic_180, input$diam_wafer))
                
                data.frame("Simulated_Bow_150um_SiC"=bow_sic_150, 
                           "Simulated_Bow_180um_SiC"=bow_sic_180)
                
        }) 
        
}

# Run the application 
shinyApp(ui = ui, server = server)
