server <- function(input, output, session) {
  
  input_const <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect))
  })
  
  # filtered_data <- reactive({
  #   table_data[table_data$Constituency == input$conSelect]
  # }) 
  # 
  filtered_data <- reactive({
    req(input$conSelect)  # Ensure input is not NULL
    table_data |> 
      filter(Constituency == input$conSelect) |> 
      select(-Constituency)  # Correct way to exclude the column
  })
  
  output$table <- renderDT({
    datatable(filtered_data(), 
              rownames = FALSE, 
              options = list(pageLength = 30))
  })
  
  input_la <- reactive ({
    csv %>% filter(LAName %in% as.character(input$laSelect))
  })

  input_reg <- reactive ({
    csv %>% filter(RegionName %in% as.character(input$regionSelect))
  })

  input_station <- reactive ({
    csv %>% filter(StationName %in% as.character(input$stationSelect))
  })

  output$total <- renderText(formatC(nrow(input_const()),big.mark = ",",digits=0,format="f"))
  
  
  const_step_free_whole <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(StepFreeAccess %in% "wholeStation")
  })

  output$const_step_free_whole <- renderText(formatC(nrow(const_step_free_whole()),big.mark = ",",digits=0,format="f"))
  
  const_step_free_partial <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(StepFreeAccess %in% "partialStation")
  })
  
  output$const_step_free_partial <- renderText(formatC(nrow(const_step_free_partial()),big.mark = ",",digits=0,format="f"))
  
  const_ticket_mach <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(AccessibleTicketMachines %in% TRUE)
  })
  
  output$const_ticket_mach <- renderText(formatC(nrow(const_ticket_mach()),big.mark = ",",digits=0,format="f"))
  
  const_ramp <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(RampForTrainAccess %in% TRUE)
  })
  
  output$const_ramp <- renderText(formatC(nrow(const_ramp()),big.mark = ",",digits=0,format="f"))
  
  const_toilet <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(NationalKeyToilet %in% TRUE)
  })
  
  output$const_toilet <- renderText(formatC(nrow(const_toilet()),big.mark = ",",digits=0,format="f"))
  
  const_induction <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(InductionLoop %in% TRUE)
  })
  
  output$const_induction <- renderText(formatC(nrow(const_induction()),big.mark = ",",digits=0,format="f"))

  const_setdown <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(ImpairedMobilitySetDown %in% TRUE)
  })
  
  output$const_setdown <- renderText(formatC(nrow(const_setdown()),big.mark = ",",digits=0,format="f"))

  # con_points_fatal <- reactive ({
  #   points %>% filter(Name == as.character(input$conSelect)) %>%
  #     filter(age_band_of_casualty %in% input$ageSelect) %>%
  #     filter(casualty_type %in% input$typeSelect ) %>%
  #     filter(accident_year %in% input$yearSelect) %>%
  #     filter(casualty_severity == "Fatal")
  # })
  # 
  # output$fatal <- renderText(formatC(nrow(con_points_fatal()),big.mark = ",",digits=0,format="f"))
  
output$title1 <- renderText({paste("Number of stations in",as.character(input$conSelect),"with selected accessibility criteria")})
output$title2 <- renderText({paste("Map of stations in",as.character(input$conSelect),"with step free access")})
output$title3 <- renderText({paste("Accessibility features for stations in",as.character(input$conSelect))})


# map
#the map
output$map <- renderLeaflet({
  
  b <- st_as_sf(input_const())
  bounds <- st_bbox(b) %>% as.character()
  
  pal <- colorFactor(c("#d50000","#e09f00","#006548"), domain = c("Whole Station", "Partial Station", "Unavailable"))
  
  leaflet(csv, options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 15))  %>% 
    
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = TRUE,minZoom = 8, maxZoom = 17))  %>%
    
    #  i don't think I want to show this because of boundary cases 
    addPolygons(data=b,
                color = "#121212",
                group = "constituencies",
                stroke = TRUE,
                weight = 3,
                opacity = 0.2,
                fillOpacity = 0) %>%
    
    addCircleMarkers(lng = const_step_free_partial()$LONG, lat = const_step_free_partial()$LAT,
                     radius = 6,
                     group = "Show partial step free access",
                     fillOpacity = 0.6,
                     fillColor = "#e09f00",
                     stroke = TRUE,
                     weight = 1.2,
                     opacity = 1,
                     popup = lapply(const_step_free_partial()$label, HTML),
                     color = "#FFFFFF") %>%
    
    # addCircleMarkers(lng = con_points_serious()$longitude, lat = con_points_serious()$latitude,
    #                  radius = 6,
    #                  group = "Show serious casualties",
    #                  fillOpacity = 0.6,
    #                  fillColor = "#c82074",
    #                  stroke = TRUE,
    #                  weight = 1.2,
    #                  opacity = 1,
    #                  popup = lapply(con_points_serious()$label, HTML),
    #                  color = "#FFFFFF") %>%
    
    addCircleMarkers(lng = const_step_free_whole()$LONG, lat = const_step_free_whole()$LAT,
                     radius = 6,
                     fillOpacity = 0.75,
                     group = "Show step free access",
                     fillColor = "#006548",
                     stroke = TRUE,
                     weight = 1,
                     popup = lapply(const_step_free_whole()$label, HTML), 
                     color = "#FFFFFF") %>%
    
    addLegend(pal = pal,
              title = "Step-free access",
              values = c("Whole","Partial","Unavailable"),
              #con_points()$casualty_severity,
              opacity = 0.8
    ) %>%
    
    addLayersControl(
      overlayGroups = c("Show step free access","Show partial step free access"),
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