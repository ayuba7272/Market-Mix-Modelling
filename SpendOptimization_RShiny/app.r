require(shiny)
require(readxl)
require(lpSolve)
require(DT)
# setwd("C:/Users/ayuba/OneDrive/Documents/GitHub/Market-Mix-Modelling/SpendOptimization_RShiny")

server <- shinyServer(
  function(input,output)
{
  fitter <- reactive({
    
    B_channel1 = 0.02
    B_channel2 = 0.07
    B_channel3 = 0.003
    B_channel4 = 1.2
    B_channel5 = 2.5
    B_channel6 = 1.8
    B_channel7 = 1.2
    B_channel8 = .5
    B_channel9 = 0.2
    
    Channel1_spend = 200000
    Channel2_spend = 600000
    Channel3_spend = 6500000
    Channel4_spend = 5000000
    Channel5_spend = 5000000
    Channel6_spend = 5500000
    Channel7_spend = 1000000
    Channel8_spend = 1000000
    Channel9_spend = 600000
    
    original_spend = 25400000
    total_Sales = 60000000
    total_mkt_contri = 25000000
    non_mkt_contri = total_Sales - total_mkt_contri
    mkt_sales_ROI = total_mkt_contri/original_spend
    
    objective.fun  <- c(
       B_channel1
      ,B_channel2
      ,B_channel3
      ,B_channel4
      ,B_channel5
      ,B_channel6
      ,B_channel7
      ,B_channel8
      ,B_channel9
    )
    
    const <- matrix(c(
      1,1,1,1,1,1,1,1,1,
      1,0,0,0,0,0,0,0,0,
      1,0,0,0,0,0,0,0,0,
      0,1,0,0,0,0,0,0,0,
      0,1,0,0,0,0,0,0,0,
      0,0,1,0,0,0,0,0,0,
      0,0,1,0,0,0,0,0,0,
      0,0,0,1,0,0,0,0,0,
      0,0,0,1,0,0,0,0,0,
      0,0,0,0,1,0,0,0,0,
      0,0,0,0,1,0,0,0,0,
      0,0,0,0,0,1,0,0,0,
      0,0,0,0,0,1,0,0,0,
      0,0,0,0,0,0,1,0,0,
      0,0,0,0,0,0,1,0,0,
      0,0,0,0,0,0,0,1,0,
      0,0,0,0,0,0,0,1,0,
      0,0,0,0,0,0,0,0,1,
      0,0,0,0,0,0,0,0,1
    ), nrow=19, byrow=TRUE)
    
    const.dir <- c("<=",
                   ">=","<=",
                   ">=","<=",
                   ">=","<=",
                   ">=","<=",
                   ">=","<=",
                   ">=","<=",
                   ">=","<=",
                   ">=","<=",
                   ">=","<="
    )
    
    Total_Spend = original_spend * (1+input$SpendChange[1]/100)
    Channel1_min = (1+(input$Channel1_range[1]/100))*Channel1_spend
    Channel1_max = (1+(input$Channel1_range[2]/100))*Channel1_spend
    Channel2_min = (1+(input$Channel2_range[1]/100))*Channel2_spend
    Channel2_max = (1+(input$Channel2_range[2]/100))*Channel2_spend
    Channel3_min = (1+(input$Channel3_range[1]/100))*Channel3_spend
    Channel3_max = (1+(input$Channel3_range[2]/100))*Channel3_spend
    Channel4_min = (1+(input$Channel4_range[1]/100))*Channel4_spend
    Channel4_max = (1+(input$Channel4_range[2]/100))*Channel4_spend
    Channel5_min = (1+(input$Channel5_range[1]/100))*Channel5_spend
    Channel5_max = (1+(input$Channel5_range[2]/100))*Channel5_spend
    Channel6_min = (1+(input$Channel6_range[1]/100))*Channel6_spend
    Channel6_max = (1+(input$Channel6_range[2]/100))*Channel6_spend
    Channel7_min = (1+(input$Channel7_range[1]/100))*Channel7_spend
    Channel7_max = (1+(input$Channel7_range[2]/100))*Channel7_spend
    Channel8_min = (1+(input$Channel8_range[1]/100))*Channel8_spend
    Channel8_max = (1+(input$Channel8_range[2]/100))*Channel8_spend
    Channel9_min = (1+(input$Channel9_range[1]/100))*Channel9_spend
    Channel9_max = (1+(input$Channel9_range[2]/100))*Channel9_spend

    rhs <- c(Total_Spend
			,Channel1_min
			,Channel1_max
			,Channel2_min
			,Channel2_max
			,Channel3_min
			,Channel3_max
			,Channel4_min
			,Channel4_max
			,Channel5_min
			,Channel5_max
			,Channel6_min
			,Channel6_max
			,Channel7_min
            ,Channel7_max
            ,Channel8_min
            ,Channel8_max
            ,Channel9_min
            ,Channel9_max   
    )

    prod.sol<-lp("max",objective.fun,const,const.dir,rhs,compute.sens=TRUE)
    
    optimum_mkt_contri = prod.sol$objval
    optimized_sales = non_mkt_contri + optimum_mkt_contri
    optimized_sales_ROI = optimum_mkt_contri/Total_Spend
    
    # val1 = rhs[2]

    op1 <- list(
      c(Decription = 'Channel1',OptimumSpendValue = prod.sol$solution[1],OriginalSpend = Channel1_spend),
      c(Decription = 'Channel2',OptimumSpendValue = prod.sol$solution[2],OriginalSpend = Channel2_spend),
      c(Decription = 'Channel3',OptimumSpendValue = prod.sol$solution[3],OriginalSpend = Channel3_spend),
      c(Decription = 'Channel4',OptimumSpendValue = prod.sol$solution[4],OriginalSpend = Channel4_spend),
      c(Decription = 'Channel5',OptimumSpendValue = prod.sol$solution[5],OriginalSpend = Channel5_spend),
      c(Decription = 'Channel6',OptimumSpendValue = prod.sol$solution[6],OriginalSpend = Channel6_spend),
      c(Decription = 'Channel7',OptimumSpendValue = prod.sol$solution[7],OriginalSpend = Channel7_spend),
      c(Decription = 'Channel8',OptimumSpendValue = prod.sol$solution[8],OriginalSpend = Channel8_spend),
      c(Decription = 'Channel9',OptimumSpendValue = prod.sol$solution[9],OriginalSpend = Channel9_spend),
      c(Decription = 'Overall Spend',OptimumSpendValue = Total_Spend,OriginalSpend = original_spend),
      c(Decription = 'Total Sales',OptimumSpendValue = optimized_sales,OriginalSpend = total_Sales),
      c(Decription = 'Marketing Contribution',OptimumSpendValue = optimum_mkt_contri,OriginalSpend = total_mkt_contri ),
      c(Decription = 'Marketing Sales ROI per $1000 spend',OptimumSpendValue = optimized_sales_ROI*1000,OriginalSpend = mkt_sales_ROI*1000)
    )

    op1 = t(as.data.frame(op1))
    row.names(op1) <- NULL
    op1 = as.data.frame(op1)
    op1$OptimumSpendValue<-as.numeric(as.character(op1$OptimumSpendValue))
    op1$OriginalSpend<-as.numeric(as.character(op1$OriginalSpend))

    op1$PCT_Change <- (op1$OptimumSpendValue - op1$OriginalSpend) / op1$OriginalSpend
    
    op2 = op1[,c("Decription","OriginalSpend","OptimumSpendValue","PCT_Change")]
    
    op_f = datatable(op2, class = 'display',colnames = c('Description (Channel)', 'Current Scenario','Optimized Scenario', '% Change')
                     ,rownames = F
                     ,extensions = 'Buttons'
                     ,options = list(pageLength = 15, info = FALSE,lengthMenu = list(c(15, -1), c("15", "All"))
                                     ,dom= 'Bfrtip',buttons = list("copy", list(extend = "collection", buttons = list(
                                       list(extend = 'csv', filename = "Optimization Results"),
                                       list(extend = 'excel', filename = "Optimization Results"),
                                       list(extend = 'pdf', filename = "Optimization Results"))
                                                         , text = "Download Optimization Scenarios")
                                            )
                                     )
                     )%>% formatCurrency(
                      c('OriginalSpend','OptimumSpendValue'), currency = '$',interval = 3, mark = ',', before = TRUE,digits = 0) %>% formatPercentage(
                      c("PCT_Change"),c(2)) %>% formatStyle('Decription',target = 'row'
                                                            ,backgroundColor = styleEqual(c('Overall Spend','Total Sales','Marketing Contribution','Marketing Sales ROI per $1000 spend'), c('teal','#6EC6BA','teal','#6EC6BA'))
                                                            ,fontWeight = styleEqual(c('Overall Spend','Total Sales','Marketing Contribution','Marketing Sales ROI per $1000 spend'),c('bold','bold','bold','bold'))
                                                            ,color = styleEqual(c('Overall Spend','Total Sales','Marketing Contribution','Marketing Sales ROI per $1000 spend'),c('white','white','white','white'))
                                                            )


    return(op_f)
    # return(optimum_mkt_contri)
    
  })
  
  output$Evaluation <- renderDataTable({fitter()})
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Spend Optimization Scenarios', ".csv", sep = ",")
    },
    content = function(file) {
      write.csv(fitter(), file, row.names = T)
    }
  )
}
)


########################################################################################################################################


require(shiny)
require(DT)
#setwd("C:/Users/ayuba/OneDrive/Documents/GitHub/Market-Mix-Modelling/SpendOptimization_RShiny")

ui <- shinyUI(
  fluidPage(
    title="Spend Optimization Scenarios",
    #App title
    titlePanel(
      fluidRow(
        
		column(2,img(height = 100, align = "left", src = "https://w7.pngwing.com/pngs/126/675/png-transparent-digital-marketing-services-marketing-marketing-strategy-search-engine-marketing-marketing-mix-search-engine-optimization-text-logo.png")),
        column(8,h1(id="big-heading",HTML("MEDIA MIX MODELLING <br/> SPEND OPTIMIZATION SCENARIO SIMULATOR"),align = "middle")),
        tags$style(HTML("#big-heading{color: navy;font-family: Tahoma;font-size: 36px;font-weight: bold;}"))
      )
    ),
    
    sidebarLayout
    (
      #Sidebarpanel
      sidebarPanel(
        #fluidRow(
        #  column(10,img(width = "120%",align = "center", src = "Next_Dollar_ROI.png"))
        #),
        h3("Please Select Scenario Details below:"),
        sliderInput("SpendChange", "% CHANGE IN SPEND:",
                    min = -25, max = +25, value = -5,
                    step = 1
        ),
        fluidRow(
          column(4,
                 sliderInput("Channel1_range", "Channel1 Change%:",
                             min = -100, max = +100, value = c(-20, -10),
                             step = 5,width = '100%'
                 )
          ),
          column(4,
                 sliderInput("Channel2_range", "Channel2 Change%:",
                             min = -100, max = +100, value = c(-20, 0),
                             step = 5,width = '100%'
                 )
          ),
          column(4,
                        sliderInput("Channel3_range", "Channel3 Change%:",
                                    min = -100, max = +100, value = c(-20, 5),
                                    step = 5,width = '100%'
                        )
                 )
        ),
        fluidRow(
          column(4,
                 sliderInput("Channel4_range", "Channel4 Change%:",
                             min = -100, max = +100, value = c(-20, 20),
                             step = 5,width = '100%'
                 )
          ),
          column(4,
                 sliderInput("Channel5_range", "Channel5 Change%:",
                             min = -100, max = +100, value = c(-20, 20),
                             step = 5,width = '100%'
                 )
          ),
          column(4,
                 sliderInput("Channel6_range", "Channel6 Change%:",
                             min = -100, max = +100, value = c(-20, 20),
                             step = 5,width = '100%'
                 )
          )
        ),
        fluidRow(
          column(4,
                 sliderInput("Channel7_range", "Channel7 Change%:",
                             min = -100, max = +100, value = c(-20, 20),
                             step = 5,width = '100%'
                 )
          ),
          column(4,
                 sliderInput("Channel8_range", "Channel8 Change%:",
                             min = -100, max = +100, value = c(-20, 20),
                             step = 5,width = '100%'
                 )
          ),
          column(4,
                 sliderInput("Channel9_range", "Channel9 Change%:",
                             min = -100, max = +100, value = c(-20, 20),
                             step = 5,width = '100%'
                 )
          )
        ),
        width = 6
      ),
      
      #MainPanel
      mainPanel(
        
        h1("Spend Optimization Scenarios:"),
        fluidRow(
          column(12,dataTableOutput("Evaluation"))
          # column(12,tableOutput("Evaluation"))
          # column(12,textOutput("Evaluation"))
        )
        ,width = 6
      )
    )
  )
)


########################################################################################################################################

shinyApp(ui = ui, server = server)