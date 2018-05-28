# Rakendus Konjunktuuriinstituudi andmete kuvamiseks ja allalaadimiseks
# Autorid: Taavi Unt ja Andres Võrk
# TÜ CITIS

library(shiny)
library(ggplot2)
library(scales)




source("konjunktuuriinstituudi_andmete_allalaadimine.R")

KI_data <- download_KI_data()




ui <- fluidPage(
   
   titlePanel("Konjunktuuriinstituudi baromeetrid"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("tabel",
                     "Vali andmestik",
                     choices = unique(KI_data$KI_andmestik), selected = "majandus"),
         selectInput("naitaja",
                     "Vali näitaja",
                     choices = c("Vali kõik", sort(unique(KI_data$indikaator[KI_data$KI_andmestik == "majandus"])))),
         br(),
         hr(),
         downloadButton("KI_andmed", label = "Lae kogu tabel alla"),
         br(),
         hr(),
         p("Alusandmed ja metoodika ", a("Konjunktuuriinstituudi lehel", href="http://www.ki.ee/baromeetrid")), 
         p("Lähekood ", a("Githubis", href="https://github.com/TaaviU/CITIS-Taavi")), 
         width = 3
      ),
      
      
      mainPanel(
         plotOutput("distPlot", height = '960px')
      )
   )
)

server <- function(input, output, session) {
  
  valitud_andmed <- reactiveValues()
  
  observe({
    updateSelectInput(session, "naitaja",
                      "Vali näitaja",
                      choices = c("Vali kõik", sort(unique(KI_data$indikaator[KI_data$KI_andmestik == input$tabel]))))
  })
  
  observe({
    
    if(input$naitaja == "Vali kõik"){
      valitud_andmed$df <- dplyr::filter(KI_data, KI_andmestik == input$tabel)
    } else{
      valitud_andmed$df <- dplyr::filter(KI_data, KI_andmestik == input$tabel, indikaator == input$naitaja)
    }
  })
   
   output$distPlot <- renderPlot({
     
     p <- ggplot(valitud_andmed$df, aes(x = kpv, y = vaartus))
     
     line_size_adj <- 0
     point_size_adj <- 0
     
     if(input$naitaja == "Vali kõik") {
       p <- p + facet_wrap(~indikaator, scales = "free_y", ncol = 2)
       line_size_adj <- 1
       point_size_adj <- 1.7
     }
     
     p +
       geom_line(color = "steelblue3", size = 2 - line_size_adj, alpha = 0.8) +
       geom_point(color = "steelblue3", size = 3 - point_size_adj, alpha = 1) +
       scale_x_date(breaks = pretty_breaks(n = 10)) + 
       labs(x = "", y = "") +
       theme_bw(base_size = 16) +
       theme(axis.text.x = element_text(angle = 60, hjust = 1))
     
   })
   
   output$KI_andmed <- downloadHandler(
     filename = function() {
       paste('KI_andmed', '.csv', sep='')
     },
     content = function(con) {
       write.csv(KI_data, con, row.names = FALSE, fileEncoding = "latin1")
     }
   )
}


shinyApp(ui = ui, server = server)

