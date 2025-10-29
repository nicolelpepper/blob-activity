# ------------------------------------------------------------------------------
# Author: Nicole Pepper
# Date: 10/29/25

# ------------------------------------------------------------------------------

library(shiny)
library(bslib)

library(leaflet)
library(leaflet.extras)

library(sf)
library(smoothr)

# ------------------------------------------------------------------------------
# ---- UI ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(
    fg = "#E7E7E7",
    bg = "#0A2B36"), # END theme
  
  
  # UI for blob map
  leafletOutput("blob_map",
                height = "99vh"),
  
  # UI for color swap button
  absolutePanel( top = 10, right =20,
    actionButton("swap_color_button",
                 "Swap Colors",
                 class = "btn-info",
                 icon = icon("refresh")) # END actionButton
    
  ) # END absolutePanel
  
) # END fluidPage

# ------------------------------------------------------------------------------
# ---- Server ------------------------------------------------------------------
# ------------------------------------------------------------------------------

server <- function(input, output){
  
  # ---- Create blobs ----------------------------------------------------------
  
  # ---- Blob 1 ----
  # Define coordinates for blob
  coords1 <- matrix(c(-106.6603843, 40.6107532, -106.786727, 40.5690389, -106.7620078, 40.4729972, -106.7208091, 40.4562803, -106.7592612, 40.3914629, -106.6947165, 40.3286768, -106.593093, 40.3433321, -106.5464011, 40.434333, -106.5821067, 40.5095511, -106.6315451, 40.5325175, -106.6219321, 40.5773839, -106.6603843, 40.6107532),
                    ncol = 2,
                    byrow = TRUE)
  
  # Convert coords to polygon
  shape1 <- st_polygon(list(coords1)) |> 
    
    # project
    st_sfc(crs = 4326)
  
  # Edit properties
  blob1 <- st_sf(
    geometry = shape1,
    name = "Blob 1") |>
    smooth(method = "chaikin")
  
  # ---- Blob 2 ----
  # Define coordinates for blob
  coords2 <- matrix(c(-106.9283109, 40.5085464, -106.9241911, 40.5795093, -106.9873625, 40.5993238, -107.0532804, 40.5930672, -107.0834928, 40.5805523, -107.1178251, 40.5961956, -107.1658903, 40.5961956, -107.1837431, 40.5690783, -107.1851164, 40.527338, -107.1178251, 40.4991486, -107.0216947, 40.5033255, -106.9791227, 40.5252503, -106.9612699, 40.5095905, -106.9283109, 40.5085464),
                    ncol = 2,
                    byrow = TRUE)
  
  # Convert coords to polygon
  shape2 <- st_polygon(list(coords2)) |> 
    
    # project
    st_sfc(crs = 4326)
  
  # Edit properties
  blob2 <- st_sf(
    geometry = shape2,
    name = "Blob 2") |>
    smooth(method = "chaikin")
  
  
  # ---- Join blobs ----
  blobs <- rbind(blob1, blob2)
  
  # ---- Define reactive colors ------------------------------------------------
  
  # Define reactive value for swap color
  swap_color <- reactiveVal(TRUE)
  
  default_colors <- c("#6CAD2B","#FFA8E4")
  
  label_lookup <- c(
     "#6CAD2B" = "Green",
     "#FFA8E4" = "Pink")
  
  # Define reactive value for current color
  current_colors <- reactive({
    if (swap_color()) rev(default_colors) else default_colors
  })
  
  current_labels <- reactive({
    unname(label_lookup[current_colors()])
  })


  
  
  # ---- Create blob map -------------------------------------------------------
  output$blob_map <- renderLeaflet({
    leaflet() |>
      setMapWidgetStyle(style = list(background = "transparent")) |>
      setView(-106.8, 40.6, zoom = 10) |>
      setMaxBounds(lng1 = -107.0, lat1 = 40.4,
                   lng2 = -106.8, lat2 = 40.6) |>
      addPolygons(data = blobs,
                  color = isolate(current_colors()),
                  fillColor = isolate(current_colors()),
                  fillOpacity = 0.5,
                  label = isolate(current_labels()))
      
  }
    
  ) # END renderLeaflet
  
  
  # ---- Update blob colors in map ---------------------------------------------
  
  observeEvent(
    input$swap_color_button,{
      
      # When you click the button, swap argument from T to F or F to T
      swap_color(!swap_color())
      
      # Update blob map with new color
      leafletProxy("blob_map") |>
        clearShapes()|>
        addPolygons(
          data = blobs,
          color = current_colors(),
          fillColor = current_colors(),
          fillOpacity = 0.6,
          label = current_labels()
        )
      
    }
  )
  
  
  
}





# ---- Combine in App ----------------------------------------------------------
shinyApp(server = server, ui = ui)