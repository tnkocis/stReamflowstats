# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(ggplot2)


gauges <- read.csv("gauges.csv")
row.names(gauges)<- paste(gauges$site_no)
function(input, output, session) {
	
#	# Route select input box
#	output$routeSelect <- renderUI({
#				live_vehicles <- getMetroData("VehicleLocations/0")
#				
#				routeNums <- sort(unique(as.numeric(live_vehicles$Route)))
#				# Add names, so that we can add all=0
#				names(routeNums) <- routeNums
#				routeNums <- c(All = 0, routeNums)
#				selectInput("routeNum", "Route", choices = routeNums, selected = routeNums[2])
#			})
#	
#	# Locations of all active vehicles
#	vehicleLocations <- reactive({
#				input$refresh # Refresh if button clicked
#				
#				# Get interval (minimum 30)
#				interval <- max(as.numeric(input$interval), 30)
#				# Invalidate this reactive after the interval has passed, so that data is
#				# fetched again.
#				invalidateLater(interval * 1000, session)
#				
#				getMetroData("VehicleLocations/0")
#			})
#	
#	# Locations of vehicles for a particular route
#	routeVehicleLocations <- reactive({
#				if (is.null(input$routeNum))
#					return()
#				
#				locations <- vehicleLocations()
#				
#				if (as.numeric(input$routeNum) == 0)
#					return(locations)
#				
#				locations[locations$Route == input$routeNum, ]
#			})
#	
#	# Get time that vehicles locations were updated
#	lastUpdateTime <- reactive({
#				vehicleLocations() # Trigger this reactive when vehicles locations are updated
#				Sys.time()
#			})
#	
#	# Number of seconds since last update
#	output$timeSinceLastUpdate <- renderUI({
#				# Trigger this every 5 seconds
#				invalidateLater(5000, session)
#				p(
#						class = "text-muted",
#						"Data refreshed ",
#						round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
#						" seconds ago."
#				)
#			})
#	
#	output$numVehiclesTable <- renderUI({
#				locations <- routeVehicleLocations()
#				if (length(locations) == 0 || nrow(locations) == 0)
#					return(NULL)
#				
#				# Create a Bootstrap-styled table
#				tags$table(class = "table",
#						tags$thead(tags$tr(
#										tags$th("Color"),
#										tags$th("Direction"),
#										tags$th("Number of vehicles")
#								)),
#						tags$tbody(
#								tags$tr(
#										tags$td(span(style = sprintf(
#																"width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
#																dirColors[4]
#														))),
#										tags$td("Northbound"),
#										tags$td(nrow(locations[locations$Direction == "4",]))
#								),
#								tags$tr(
#										tags$td(span(style = sprintf(
#																"width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
#																dirColors[1]
#														))),
#										tags$td("Southbound"),
#										tags$td(nrow(locations[locations$Direction == "1",]))
#								),
#								tags$tr(
#										tags$td(span(style = sprintf(
#																"width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
#																dirColors[2]
#														))),
#										tags$td("Eastbound"),
#										tags$td(nrow(locations[locations$Direction == "2",]))
#								),
#								tags$tr(
#										tags$td(span(style = sprintf(
#																"width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
#																dirColors[3]
#														))),
#										tags$td("Westbound"),
#										tags$td(nrow(locations[locations$Direction == "3",]))
#								),
#								tags$tr(class = "active",
#										tags$td(),
#										tags$td("Total"),
#										tags$td(nrow(locations))
#								)
#						)
#				)
#			})
#	
#	# Store last zoom button value so we can detect when it's clicked
#	lastZoomButtonValue <- NULL
	
	output$busmap <- renderLeaflet({
				leaflet(gauges) %>%
						addTiles(
								urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
								attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
						) %>%
						setView(lng = -120.42, lat = 37.52, zoom = 6)%>%
#						addMarkers(~long,~lat,popup=~as.character(site_no))
						addCircleMarkers(~long,~lat,
								radius = 5,
								color = "blue",
								stroke = FALSE, fillOpacity = 0.5,
								layerId = ~site_no
								#popup = ~paste("USGS ",as.character(site_no))
						)
			})
	
	showGaugePopup <- function(event) {
#		selectedGauge <- gauges$site_no[id]
		content <- as.character(tagList(
						tags$h4(paste("USGS ",event$id))
				))
		leafletProxy("busmap") %>% addPopups(lng=event$lng, lat=event$lat, popup=content)
	}
	
#	# When map is clicked, show a popup with city info
	observe({
				leafletProxy("busmap", data=gauges) %>% clearPopups()
				event <- input$busmap_marker_click
				if (is.null(event))
					return()
				
				isolate({
##					leafletProxy("busmap") %>% addPopups(lng=event$lng,
##							lat=event$lat, popup=c("blah"))
						d <- data.frame(x=seq(1,10,1),y=rnorm(10))
						plotd <- ggplot(d,aes(x,y)) + geom_point() + ggtitle(paste(event$id))
						output$plot_d <- renderPlot({
									plotd
								})
							showGaugePopup(event)
						})
			})

}