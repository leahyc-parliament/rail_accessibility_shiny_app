server <- function(input, output, session) {
  
  # filters csv to only return stations in inputted const
  input_const <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect))
  })
  
  # filters table_data to only return stations in inputted const
  filtered_data <- reactive({
    req(input$conSelect)  
    table_data |> 
      filter(ConstituencyName == input$conSelect) |> 
      select(-ConstituencyName) 
  })
  
  # displays filtered_data in table
  output$table <- renderDT({
    datatable(filtered_data(), 
              rownames = FALSE, 
              options = list(
                pageLength = 10,
                scrollY = "300px",
                scrollCollapse = TRUE,
                paging = FALSE,
                dom = 'lrtip'
                ))
  })
  
  # counting total number of stations in inputted const
  output$total <- renderText(formatC(nrow(input_const()),big.mark = ",",digits=0,format="f"))
  
  # counting number of stations without step free access in inputted const
  const_step_free_unavail <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(StepFreeAccess %in% "noPartOfStation")
  })
  output$const_step_free_unavail <- renderText(formatC(nrow(const_step_free_unavail()),big.mark = ",",digits=0,format="f"))
  
  # counting number of stations with full step free access in inputted const
  const_step_free_whole <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(StepFreeAccess %in% "wholeStation")
  })
  output$const_step_free_whole <- renderText(formatC(nrow(const_step_free_whole()),big.mark = ",",digits=0,format="f"))
  
  # counting number of stations with partial step free access in inputted const
  const_step_free_partial <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(StepFreeAccess %in% "partialStation")
  })
  output$const_step_free_partial <- renderText(formatC(nrow(const_step_free_partial()),big.mark = ",",digits=0,format="f"))
  
  # counting number of stations with accessible ticket machines in inputted const
  const_ticket_mach <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(AccessibleTicketMachines %in% TRUE)
  })
  output$const_ticket_mach <- renderText(formatC(nrow(const_ticket_mach()),big.mark = ",",digits=0,format="f"))
  
  # counting number of stations with ramp for train access in inputted const
  const_ramp <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(RampForTrainAccess %in% TRUE)
  })
  output$const_ramp <- renderText(formatC(nrow(const_ramp()),big.mark = ",",digits=0,format="f"))
  
  # counting number of stations with national key toilets in inputted const
  const_toilet <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(NationalKeyToilet %in% TRUE)
  })
  output$const_toilet <- renderText(formatC(nrow(const_toilet()),big.mark = ",",digits=0,format="f"))
  
  # counting number of stations with induction loops in inputted const
  const_induction <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(InductionLoop %in% TRUE)
  })
  output$const_induction <- renderText(formatC(nrow(const_induction()),big.mark = ",",digits=0,format="f"))

  # counting number of stations with imparied mobility setdown in inputted const
  const_setdown <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(ImpairedMobilitySetDown %in% TRUE)
  })
  output$const_setdown <- renderText(formatC(nrow(const_setdown()),big.mark = ",",digits=0,format="f"))
  
  const_boundary <- reactive({
    d <- csv %>% filter(ConstituencyName == input$conSelect)
    st_as_sf(d)
  })

# Titles 
output$title1 <- renderText({paste("Number of stations in",as.character(input$conSelect),"with selected accessibility criteria")})
output$title2 <- renderText({paste("Map of stations in",as.character(input$conSelect),"with step free access")})
output$title3 <- renderText({paste("Accessibility features for stations in",as.character(input$conSelect))})


# Map
output$map <- renderLeaflet({
  
  b <- const_boundary()
  bounds <- b %>% st_bbox() %>% as.character()
  
  pal <- colorFactor(c("#006548","#e09f00","#682f7f"), domain = c("Full", "Partial", "Unavailable"))
  
  leaflet(csv, options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 15))  %>% 
    
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = TRUE,minZoom = 8, maxZoom = 17))  %>%
    
    addPolygons(data=b,
                color = "#121212",
                group = "cons",
                stroke = TRUE,
                weight = 3,
                opacity = 0.2,
                fillOpacity = 0) %>%
    
    addCircleMarkers(lng = const_step_free_whole()$Longitude, lat = const_step_free_whole()$Latitude,
                     radius = 6,
                     group = "Show whole step free access",
                     fillOpacity = 0.6,
                     fillColor = "#006548",
                     stroke = TRUE,
                     weight = 1.2,
                     opacity = 1,
                     popup = lapply(const_step_free_whole()$label, HTML),
                     color = "#FFFFFF") %>%
    
    addCircleMarkers(lng = const_step_free_partial()$Longitude, lat = const_step_free_partial()$Latitude,
                     radius = 6,
                     group = "Show partial step free access",
                     fillOpacity = 0.6,
                     fillColor = "#e09f00",
                     stroke = TRUE,
                     weight = 1.2,
                     opacity = 1,
                     popup = lapply(const_step_free_partial()$label, HTML),
                     color = "#FFFFFF") %>%

    addCircleMarkers(lng = const_step_free_unavail()$Longitude, lat = const_step_free_unavail()$Latitude,
                     radius = 6,
                     fillOpacity = 0.75,
                     group = "Show unavailable step free access",
                     fillColor = "#682f7f",
                     stroke = TRUE,
                     weight = 1,
                     popup = lapply(const_step_free_unavail()$label, HTML),
                     color = "#FFFFFF") %>%
    
    addLegend(pal = pal,
              title = "Step-free access",
              values = c("Unavailable", "Partial","Full"),
              opacity = 0.8
    ) %>%
    
    addLayersControl(
      overlayGroups = c("Show whole step free access", "Show partial step free access", "Show unavailable step free access"),
      position = "bottomright",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    htmlwidgets::onRender(
      "function(el, x) {
          L.control.zoom({position:'topleft'}).addTo(this);
        }")
})

}