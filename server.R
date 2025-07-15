server <- function(input, output, session) {
  
  input_const <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect))
  })
  
  filtered_data <- reactive({
    req(input$conSelect)  
    table_data |> 
      filter(ConstituencyName == input$conSelect) |> 
      select(-ConstituencyName) 
  })
  
  output$table <- renderDT({
    datatable(filtered_data(), 
              rownames = FALSE, 
              options = list(pageLength = 30))
  })

  output$total <- renderText(formatC(nrow(input_const()),big.mark = ",",digits=0,format="f"))
  
  const_step_free_unavail <- reactive ({
    csv %>% filter(ConstituencyName %in% as.character(input$conSelect)) %>%
      filter(StepFreeAccess %in% "noPartOfStation")
  })
  
  output$const_step_free_unavail <- renderText(formatC(nrow(const_step_free_unavail()),big.mark = ",",digits=0,format="f"))
  
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
  

# Titles 
output$title1 <- renderText({paste("Number of stations in",as.character(input$conSelect),"with selected accessibility criteria")})
output$title2 <- renderText({paste("Map of stations in",as.character(input$conSelect),"with step free access")})
output$title3 <- renderText({paste("Accessibility features for stations in",as.character(input$conSelect))})


# Map
output$map <- renderLeaflet({
  
  b <- st_as_sf(input_const(), coords = c("Longitude", "Latitude"), crs = 4326)
  bounds <- st_bbox(b) %>% as.character()
  
  pal <- colorFactor(c("#006548","#e09f00","#d50000"), domain = c("Whole Station", "Partial Station", "Unavailable"))
  
  leaflet(csv, options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 15))  %>% 
    
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = TRUE,minZoom = 8, maxZoom = 17))  %>%
    
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
                     fillColor = "#d50000",
                     stroke = TRUE,
                     weight = 1,
                     popup = lapply(const_step_free_unavail()$label, HTML),
                     color = "#FFFFFF") %>%
    
    addLegend(pal = pal,
              title = "Step-free access",
              values = c("Unavailable", "Partial Station","Whole Station"),
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