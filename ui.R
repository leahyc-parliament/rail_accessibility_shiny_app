ui <- function(request) {
  fluidPage(
    
  title = "Local area data: Accessibility of train stations",
    
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
    
  
  fluidRow(column(width = 12, class="pickerhighest",
                   pickerInput(
                     inputId = "conSelect",
                     label = "Select a constituency (click and type to search)", 
                     choices = constituencies,
                     width="100%",
                     options = list(
                       `live-search` = TRUE)))),
  
  
  fluidRow(column(width=12,
                  h2(textOutput("title1"))  |>
                    tagAppendAttributes(style="margin-top:0px;margin-bottom:10px",tabindex = 0)
  )),
  
  fluidRow(column(width = 3, align="center", style ="background-color:#ffffff;font-family: Lato;", span( style="font-size: 38pt; font-weight: bold; color: #006548;",  
                                                                                                        htmlOutput("total")) |>
                    tagAppendAttributes(tabindex = 0),
                  tags$p(span( style="font-weight: bold;","Total stations")) |>
                    tagAppendAttributes(tabindex = 0) 
                  ),
           column(width = 3, align="center", style ="background-color:#ffffff;font-family: Lato;", span( style="font-size: 38pt; color: #006548;", 
                                                                                                         htmlOutput("const_step_free_whole")) |>
                    tagAppendAttributes(tabindex = 0),
                  tags$p("Fully Step Free Access") |>
                    tagAppendAttributes(tabindex = 0)
           ),
           column(width = 3, align="center", style ="background-color:#ffffff;font-family: Lato;", span( style="font-size: 38pt; color: #006548;", 
                                                                                                         htmlOutput("const_step_free_partial")) |>
                    tagAppendAttributes(tabindex = 0),
                  tags$p("Partial Step Free Access") |>
                    tagAppendAttributes(tabindex = 0)
           ),
           column(width = 3, align="center", style ="background-color:#ffffff;font-family: Lato;", span( style="font-size: 38pt; color: #006548;", 
                                                                                                         htmlOutput("const_ticket_mach")) |>
                    tagAppendAttributes(tabindex = 0),
                  tags$p("Accessible Ticket Machines") |>
                    tagAppendAttributes(tabindex = 0)
           ),
           column(width = 3, align="center", style ="background-color:#ffffff;font-family: Lato;", span( style="font-size: 38pt; color: #006548;", 
                                                                                                         htmlOutput("const_ramp")) |>
                    tagAppendAttributes(tabindex = 0),
                  tags$p("Ramp for Train Access") |>
                    tagAppendAttributes(tabindex = 0)
           ),
           column(width = 3, align="center", style ="background-color:#ffffff;font-family: Lato;", span( style="font-size: 38pt; color: #006548;", 
                                                                                                         htmlOutput("const_toilet")) |>
                    tagAppendAttributes(tabindex = 0),
                  tags$p("National Key Toilet") |>
                    tagAppendAttributes(tabindex = 0)
           ),
           column(width = 3, align="center", style ="background-color:#ffffff;font-family: Lato;", span( style="font-size: 38pt; color: #006548;", 
                                                                                                         htmlOutput("const_induction")) |>
                    tagAppendAttributes(tabindex = 0),
                  tags$p("Induction Loop") |>
                    tagAppendAttributes(tabindex = 0)
           ),
           column(width = 3, align="center", style ="background-color:#ffffff;font-family: Lato;", span( style="font-size: 38pt; color: #006548;", 
                                                                                                         htmlOutput("const_setdown")) |>
                    tagAppendAttributes(tabindex = 0),
                  tags$p("Impaired Mobility Set Down") |>
                    tagAppendAttributes(tabindex = 0)
           ),
           ),
  #map 
  fluidRow(column(width=12,
                  h2(textOutput("title2"))  |>
                    tagAppendAttributes(style="margin-top:25px;margin-bottom:8px",tabindex = 0)
  )),
  fluidRow(column(width=12,
                  tags$p("You can click or tap on a dot to view details.") |>
                    tagAppendAttributes(style="font-size:10pt;margin-top:-5px;margin-bottom:10px",tabindex = 0)
  )),
  fluidRow(column(width = 12,
                  leafletOutput("map", height = "500"))),
  fluidRow(
    column(width=12, align="right",
           span(style ="font-size: 9pt;",
                tags$a(href="https://leafletjs.com/","Leaflet", target = "_blank")," | © ",
                tags$a(href="https://www.openstreetmap.org/copyright","OpenStreetMap" , target = "_blank")," contributors | © ",
                tags$a(href="https://carto.com/attribution/","CARTO" , target = "_blank")
           )
    )
  ),
  
  fluidRow(column(width=12,
                  tags$p("These figures are based on ",tags$a(href="https://www.gov.uk/government/collections/road-accidents-and-safety-statistics","data published by the Department for Transport.", target = "_blank"), "They include all collisions reported to the police that resulted in injury.") |>
                    tagAppendAttributes(style="margin-top:10px",tabindex = 0)
  )),
  #table
  fluidRow(column(width=12,
                  h2(textOutput("title3")) |>
                    tagAppendAttributes(tabindex = 0)
  )),
  DTOutput("table")

  
) #end fluid page
}

