source('dependencies.R')
# library(foreign)
# library(leaflet)
# library(leaflet.extras)
# library(tidyverse)
# library(sp)
# library(rgeos)
# library(maps)
# library(rgdal)
# library(dplyr)
#library(rmapshaper)
#library(shiny)
#library(shinyBS)
#library(ggplot2)
#library(DT)
#library(rsconnect)
#library(grid)
#library(gridExtra)
#library(hddtools) to access GRDC data
#For hatched polygons
#devtools::install_github("statnmap/HatchedPolygons", build_vignettes = TRUE)
#library(HatchedPolygons)
#vignette("leaflet_shading_polygon", package = "HatchedPolygons")

################## SEE DATA PREP AT THE END OF THIS CODE ##########################

load("Map_8.RData")

#Set up palettes
extpal <- colorBin(palette=c('#FFFFCC', "#FEB24C","#E31A1C", "#800026"), bins=c(0,0.20,0.40,0.60,0.80))
fishpal <- colorFactor(palette=c('#006d2c', "#E8E8E8","#1a1a1a"), levels= c("HighEndemism","LowEndemism","NoEWUdata"),na.color = "#ffffff")
ndcpal <- colorFactor(palette=c('#D66460', "#E8E8E8"), levels= c("HighNDC","LowNDC"),na.color = "#ffffff")
floodpal <- colorFactor(palette=c('#69ABC0', "#E8E8E8","#1a1a1a"), levels= c("HighRisk","LowRisk","NoFEMAdata"),na.color = "#ffffff")

fishndcpal <- colorFactor(palette=c('#006d2c', "#E8E8E8",
                                 "#1a1a1a","#1a1a1a",
                                 '#D66460','#000000'), 
                       levels= c("HighEndemism_LowNDC","LowEndemism_LowNDC",
                                 "NoEWUdata_LowNDC","NoEWUdata_HighNDC",
                                 "LowEndemism_HighNDC","HighEndemism_HighNDC"),na.color = "#ffffff")
fishfloodpal <- colorFactor(palette=c('#006d2c', "#E8E8E8",
                                   "#1a1a1a","#1a1a1a","#1a1a1a","#1a1a1a",
                                   '#69ABC0','#000000'), 
                         levels= c("HighEndemism_LowRisk","LowEndemism_LowRisk",
                                   "HighEndemism_NoFEMAdata","LowEndemism_NoFEMAdata","NoEWUdata_LowRisk","NoEWUdata_HighRisk",
                                   "LowEndemism_HighRisk","HighEndemism_HighRisk"),na.color = "#ffffff")
ndcfloodpal <- colorFactor(palette=c('#D66460', "#E8E8E8",
                                  "#1a1a1a","#1a1a1a",
                                  '#69ABC0','#000000'), 
                        levels= c("HighNDC_LowRisk","LowNDC_LowRisk",
                                  "HighNDC_NoFEMAdata", "LowNDC_NoFEMAdata",
                                  "LowNDC_HighRisk", "HighNDC_HighRisk"),na.color = "#ffffff")

ndcfloodpal <- colorFactor(palette=c('#D66460', "#E8E8E8",
                                     "#1a1a1a","#1a1a1a",
                                     '#69ABC0','#000000'), 
                           levels= c("HighNDC_LowRisk","LowNDC_LowRisk",
                                     "HighNDC_NoFEMAdata", "LowNDC_NoFEMAdata",
                                     "LowNDC_HighRisk", "HighNDC_HighRisk"),na.color = "#ffffff")

allpal <- colorFactor(palette=c('#000000','#49006a','#014636','#543005',
                                '#D66460','#006d2c','#69ABC0',
                                "#E8E8E8"),
                      levels=c("HighNDC_HighRisk_HighEndemism","HighNDC_HighRisk_LowEndemism","LowNDC_HighRisk_HighEndemism","HighNDC_LowRisk_HighEndemism",
                               "HighNDC_LowRisk_LowEndemism","LowNDC_LowRisk_HighEndemism","LowNDC_HighRisk_LowEndemism",
                               "LowNDC_LowRisk_LowEndemism"), na.color= "#ffffff")


###########################
#Create pop-ups for shapes
#For world gages
worldgagepop <- paste("Years of data in 2015 :", GRDCdat$t_yrs)
#For US gages
gagefc$links <- paste0('<a href="https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=',
                       gagefc$allgages_merge_manual_site_no,
                       '" target="_blank">Access data</a>')
#For US basins
polypopup <- paste0("<strong>HUC6 name: </strong>", simplified$NAME,
                    "<br><strong>ID: </strong>", simplified$HUC6,
                    "<br><strong>Decline risk: </strong>", round(100*as.numeric(simplified$Quasiext.huc6),2),"%",
                    "<br><strong>NDC: </strong>", round(as.numeric(simplified$NDC_HUC),2),
                    "<br><strong>Flood risk: </strong>", round(100*as.numeric(simplified$flood_FEMA),2),"%",
                    "<br><strong>EWU: </strong>", round(as.numeric(simplified$EWU),2))


###########################################################################################################################################################
ui <- navbarPage(windowTitle = 'Interactive Gages',
                 title=HTML('<div><a href="http://www.sciencemag.org/journal-department/policy-forum" target="_blank">Losing the pulse of the Earth’s fresh waters</a></div>'), #Link to online article
                 theme="simplex.css", # for shinyapps.io
                 #theme="http://bootswatch.com/simplex/bootstrap.css", #for local/RStudio and shiny-server
                 #shinytheme() from shinythemes package must be avoided because it conflicts with bsModal in shinyBS.
                 id="nav",
                 
                   tabPanel(title="United States",value="US",
                          div(class="outer",
                              leafletOutput("USmap", width = "100%", height = "100%"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
                                            draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            wellPanel(
                                              uiOutput("checkboxgroupUS"),
                                              p(HTML("<strong>USGS hydrometric network</strong>")),
                                              checkboxInput("gagechkact", label="Active streamgages", value=T),
                                              checkboxInput("gagechkdeact", label="Deactivated streamgages", value=F),
                                              sliderInput("gageyear", strong("Year of stream age activity"), min=1870, max=2016, value=2016,
                                                          step = 1, round=T, sep=""),
                                              bsTooltip(id="gageyear", title=paste0("Slide the knob to see how the U.S. stream gage network changed over time.",
                                                                                    "Point size reflects the length of the time series until that year"),
                                                        "left",options = list(container = "body"))
                                            )
                              ),
                              tags$div(id="cite2",
                                       HTML('<a href="http://www.sciencemag.org/journal-department/policy-forum" target="_blank">Ruhi, A., Messager, L. M., & Olden, J. D. (2017) "Losing the pulse of the Earth&#39s fresh waters"</a>')
                              )
                          )
                 ),
                 
                 tabPanel("Explore U.S. data",
                          div(class="outer",
                              tags$head(tags$script(src = "message-handler.js")), #Allow for pop-up message when user does not select any basin, see https://shiny.rstudio.com/articles/action-buttons.html
                              fluidRow(
                                checkboxGroupInput("checkcat", label = h3("Subset by high threat category"), 
                                                   choices = list("Streamgaging decline risk" = "dec",
                                                                  "Water scarcity" = "scar",
                                                                  "Flood risk" = "flood",
                                                                  "Fish diversity" = "fish"),
                                                   selected = NULL,
                                                   width = 500)
                              ),
                              fluidRow(
                                DT::dataTableOutput("table")
                              ),
                              fluidRow(column(2,
                                              downloadButton('save', 'Save basin data'),
                                              bsTooltip(id="save", 
                                                        title="Download data table for basins in the selected high threat categories", 
                                                        placement="top", trigger="hover")
                                              ),
                                       column(2,
                                              downloadButton('saveselected', 'Save gage data'),
                                              bsTooltip(id="saveselected", 
                                                        title="Download information for gages located within selected basins (click rows to select basins)", 
                                                        placement="top", trigger="hover")
                                              ),
                                       column(2,
                                              actionButton("zoom", label = "Zoom to selected basin"),
                                              bsTooltip(id="zoom", 
                                               title="Click rows in the table to select basins and click here to zoom to the selected basins. To deselect a single basin, click again.", 
                                               placement="top", trigger="hover")
                                              ),
                                       column(1,offset=-1,
                                              actionButton("clear", label= "Clear selection"))
                                       )
                              )),
                 
                 tabPanel("World", value="wrld",
                          div(class="outer",
                              tags$style(type = "text/css", 
                                         ".outer {position: fixed; top: 30px; left: 0; right: 0; bottom: 0; overflow: scroll; padding: 20px}", #Padding to make table fully visible
                                         ".leaflet .legend i{border-radius: 50%; width: 10px;height: 10px;margin-top: 4px;}", #Allow for circle legend
                                         ".navbar {font-size: 16px}", ".navbar-default .navbar-brand {font-size: 20px}", #Increase font size of navigation bar text
                                         ".tooltip-inner {text-align: left}"), #Left-align tooltips that display upon hover
                  
                              leafletOutput("Worldmap", width = "100%", height = "100%"),
                              absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
                                            draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 350, height = "auto",
                                            wellPanel(
                                              h4("GRDC hydrometric network explorer",  tipify(icon("info-circle"), 
                                                                                              title = HTML(paste0(
                                                                                                "For more information about the Global Runoff Data Center (GRDC), refer to Methodology in the <i>About</i> tab")),
                                                                                              trigger="hover")),
                                              p("Gages within map bounds"),
                                              
                                              plotOutput("BoundYears", height = 350),
                                              br(),
                                              sliderInput(inputId="gageyearworld", label="Year of streamgage reporting:", min=1806, max=2015, value=2010,
                                                            step = 1, round=T, sep=""),
                                              bsTooltip(id="gageyearworld", title=paste0("Slide the knob to see how the GRDC stream gage network changed over time.", 
                                                                                         "Point size reflects the length of the time series until that year."),
                                                        "left",options = list(container = "body")),
                                              tipify(icon("info-circle"), 
                                                     title = HTML(paste0(
                                                       "Note that a gage could be inactive for two reasons:",
                                                       "<br>- the actual discontinuation of data recording",
                                                       "<br>- the discontinuation of reporting by the entity collecting the data")),
                                                     trigger="hover")
                                            ),
                                            style = "opacity: 0.90"
                              ),
                              
                              tags$div(id="cite2",
                                       HTML('<a href="http://www.sciencemag.org/journal-department/policy-forum" target="_blank">Ruhi, A., Messager, L. M., & Olden, J. D. (2017) "Losing the pulse of the Earth&#39s fresh waters"</a>')
                              )
                          )
                 ),
                
                tabPanel("About",
                         h2("About this application"),
                         HTML('
                              <p>This application provides supplementary material to the Policy Forum article 
                              <a href="http://www.sciencemag.org/journal-department/policy-forum" target="_blank">Losing the pulse of the Earths fresh waters</a> 
                              published in December 2017. <br/>
                              <br/>
                              The <b>United States</b> tab shows the history of water information in the US. In addition, it shows flood risk, water scarcity, fish diversity, and the risk of gaging density decline at the river basin level.
                              </br>
                              The <b>Explore data</b> tab allows users to download data on US river basins shown in the "United States" tab
                              </br>
                              The <b>World</b> tab shows the history of stream gages data reporting to the
                              <a href="http://www.bafg.de/GRDC/EN/Home/homepage_node.html">Global Runoff Data Center (GRDC)</a>. A gage is considered active if its records were reported to the GRDC that year. </br>
                              Therefore, an inactive gage could either be due to the discontinuation of data recording or the lack of reporting of recorded data by the entity managing the gage
                              <br/>'),
                         br(),
                         h2("Methodology"),
                         tags$iframe(style="height:400px; width:50%; scrolling=yes",  #PDF inset
                                     src="Ruhi_et_al_streamgaging_SM_def.pdf"),
                         h2("Contact information and source code"),
                         HTML('
                              <p>Original publication: Ruhi, A., Messager, L. M., & Olden, J. D. (2017) "Losing the pulse of the Earths fresh waters" Under review.</br>
                              App developer: Mathis Messager (email: messamat@uw.edu) </br>
                              Study source code: github link </br>
                              <a href="https://github.com/messamat/Ruhietal2017_Shinyapp" target="_blank">Map source code</a></br>')
                         )
)


server <- function(input, output, session) {
  ######################################################CUSTOM FUNCTIONS ####################################################################
  #Custom function to make circle legends from https://stackoverflow.com/questions/37446283/creating-legend-with-circles-leaflet-r
  addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.75){
    colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    
    return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, position="bottomleft", title="Streamgages",layerId="gage"))
  }
  
  #Custom function to create tooltip for each line of checkboxGroupInput. 
  #Modified from https://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text/36696224#36696224 to include icons
  makeCheckboxTooltip <- function(checkboxValue, Tooltip){
    tags$script(HTML(paste0("
          $(document).ready(function() {
            var inputElements = document.getElementsByTagName('input');
            for(var i = 0; i < inputElements.length; i++){
              var input = inputElements[i];
              if(input.getAttribute('value') == '", checkboxValue, "'){
                var buttonID = 'button_' + Math.floor(Math.random()*1000);

                var button = document.createElement('button');
                button.setAttribute('id', buttonID);
                button.setAttribute('type', 'button');
                <!--Edit this to change button style -->
                button.setAttribute('class', 'btn action-button btn-inverse btn-xs');

                <!-- Edited part from stackoverflow based on https://stackoverflow.com/questions/25095401/glyphicon-on-dynamic-button -->
                var icon = document.createElement('span'); 
                icon.className ='glyphicon glyphicon-info-sign';
                button.appendChild(icon);
                <!---->

                input.parentElement.parentElement.appendChild(button);
                shinyBS.addTooltip(buttonID, \"tooltip\", {\"placement\": \"bottom\", \"trigger\": \"hover\", \"title\": \"", Tooltip, "\"}) 
              };
            }
          });
        ")))
  }
  
  #Function to get USGS TNM base maps as tiles
  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
  }
  
  ################################################### United States tab ##########################################################################
  ###########################
  # USGS gaging network map
  ###########################
  #Get US National Maps ready (from https://owi.usgs.gov/blog/basemaps/)
  #Specify line of attribution text to display in the map
  att <- paste0("<a href='https://www.usgs.gov/'>",
                "U.S. Geological Survey</a> | ",
                "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
                "Policies</a>")
  
  #Set up basic map
  output$USmap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -98.5795, lat = 39.8282, zoom = 4) %>%
      addProviderTiles(provider = "Hydda.Base", group = "Hydrography base") %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "World Physical", options = providerTileOptions(opacity=0.75)) %>%
      addWMSTiles(GetURL("USGSHydroCached"), group= "Hydrography", options = WMSTileOptions(format = "image/png", transparent = TRUE), layers="0") %>%
      addLayersControl(
        baseGroups = c("Hydrography base",
                       "OSM",
                       "World Physical",
                       "World Imagery"),
        overlayGroups = c("Hydrography"),
        position="bottomleft",
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("Hydrography")
  })
  
  #Create action buttons to select threats to display including tooltips from custom function
  #Example code from https://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text/36696224#36696224
  output$checkboxgroupUS <-   renderUI({
    list(
      checkboxGroupInput("chck",  label=tags$span(HTML("<strong>Basin-level threat category</strong>"),   
                                                  tipify(icon("info-circle"), title=HTML("For more information about threats, refer to Methodology in the <i>About</i> tab"))),
                         choices = list("High streamgaging decline risk" = "dec",
                                        "Water scarcity" = "scar",
                                        "Flood risk" = "flood",
                                        "Fish diversity" = "fish"),
                         selected = NULL),
      makeCheckboxTooltip(checkboxValue = "dec", 
                          Tooltip = "<strong>Basins with >50% risk of having their gage network density halved by 2022</strong>"),
      makeCheckboxTooltip(checkboxValue = "scar", 
                          Tooltip = HTML(paste0(
        "<strong>High water scarcity: basins with Normalized Deficit Cumulated (NDC) > 1</strong>",
        "<br>The NDC is equal to the maximum cumulative deficit between average daily water demand in 2010 and local daily renewable supply from 1949 to 2010, ",
        "divided by the average annual rainfall volume in that basin (Devineniet et al. 2015)."))),
      #"<a href='http://onlinelibrary.wiley.com/wol1/doi/10.1002/2015GL063487/full' target='_blank'>Devineni et al. 2015</a></br>",
      makeCheckboxTooltip(checkboxValue = "flood", 
                          Tooltip = "<strong>High flood risk: basins with >5% of the population living within a 100-year flood zone</strong>"),
      makeCheckboxTooltip(checkboxValue = "fish", 
                          Tooltip = HTML(paste0(
        "<strong>High fish diversity: basins with endemism weighted richness/units (EWU) > 0.28 (U.S.-wide median value)</strong>",
        "<br>EWU is the sum of the portion of each species range contained within the basin. It expresses both the richness and degree of endemism of species.")))
    )
  })
  
  #Add threats based on user checking action buttons
  #Troubleshooting for legend error:https://stackoverflow.com/questions/30654397/leaflet-map-legend-in-r-shiny-app-has-doesnt-show-colors
  # fishpal <- colorFactor(palette=c('#006d2c', "#E8E8E8","#1a1a1a"), levels= c("HighEndemism","LowEndemism","NoEWUdata"),na.color = "#ffffff")
  # ndcpal <- colorFactor(palette=c('#D66460', "#E8E8E8"), levels= c("HighNDC","LowNDC"),na.color = "#ffffff")
  # floodpal <- colorFactor(palette=c('#69ABC0', "#E8E8E8","#1a1a1a"), levels= c("HighRisk","LowRisk","NoFEMAdata"),na.color = "#ffffff")
  
  #Add threats based on user checking action buttons. 
  #Show different coloring and legends based on each possible combination of threat selection including multiple threats
  observe({
    #If nothing is selected
    if (is.null(input$chck)) {
      leafletProxy("USmap") %>%
        removeControl("basins") %>%
        clearGroup("basins")
    }
    #####################################
    # ONE CATEGORY SELECTED
    #####################################
    #If only water scarcity is selected
      if ("scar" %in% input$chck && !any(c("flood","fish") %in% input$chck)) {
        leafletProxy("USmap") %>%
          removeControl("basins") %>%
          clearGroup("basins") %>%
          addPolygons(data=simplified, weight = 1, smoothFactor = 0.5, fillColor = ~ndcpal(ndc_category), color = ~ndcpal(ndc_category),fillOpacity = 0.8, opacity = 0.5,
                      group = "basins",popup=polypopup,
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F, opacity=1)) %>%
          addLegend(position="bottomleft", colors=c('#D66460', "#E8E8E8", "#ffffff"), labels=c("High (NDC > 1)", "Low (NDC < 1)","Insufficient gage data"),
                    title="Water scarcity", opacity=1,layerId="basins")
      }
    #If only flood risk is selected
      if ("flood" %in% input$chck && !any(c("scar","fish") %in% input$chck)) {
        leafletProxy("USmap") %>%
          removeControl("basins") %>%
          clearGroup("basins") %>%
          addPolygons(data=simplified,weight = 1, smoothFactor = 0.5, fillColor = ~floodpal(flood_category), color = ~floodpal(flood_category),fillOpacity = 0.8, opacity =0.5,
                      group = "basins",popup=polypopup,
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F, opacity=1)) %>%
          addLegend(position="bottomleft", colors=c('#69ABC0', "#E8E8E8","#ffffff"), labels=c('High (> 5% population)','Low (< 5% population)','Insufficient gage data'), 
                    title="Flood risk", opacity=1,layerId="basins")
      }
    #If only fish diversity is selected
      if ("fish" %in% input$chck && !any(c("flood","scar") %in% input$chck)) {
        leafletProxy("USmap") %>%
          removeControl("basins") %>%
          clearGroup("basins") %>%
          addPolygons(data=simplified,weight = 1, smoothFactor = 0.5, fillColor = ~fishpal(fish_category), color = ~fishpal(fish_category),fillOpacity = 0.8, opacity = 0.5,
                      group = "basins",popup=polypopup,
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F, opacity=1)) %>%
          addLegend(position="bottomleft", colors=c('#006d2c', "#E8E8E8","#ffffff"), labels=c('High (> U.S.-wide median EWU)', 'Low (< U.S.-wide median EWU)', 'Insufficient gage data'),
                    title="Fish diversity", opacity=1,layerId="basins")
      }
      
    #####################################
    # TWO CATEGORIES SELECTED
    #####################################
    #If only fish diversity and water scarcity are selected
    if (("fish" %in% input$chck) && ("scar" %in% input$chck) && !("flood" %in% input$chck)) {
        leafletProxy("USmap") %>%
          removeControl("basins") %>%
          clearGroup("basins") %>%
          addPolygons(data=simplified, weight = 1, smoothFactor = 0.5, fillColor = ~fishndcpal(fishndc_category), color = ~fishndcpal(fishndc_category),fillOpacity = 0.8, opacity = 0.5,
                      group = "basins",popup=polypopup,
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F, opacity=1)) %>%
          addLegend(position="bottomleft",
                    colors=c('#000000','#006d2c', '#D66460',"#E8E8E8","#ffffff"), 
                    labels=c("High diversity/High scarcity", "High diversity/Low scarcity","Low diversity/High scarcity","Low diversity/Low scarcity", "Insufficient gage data"),
                    title="Fish diversity/Water scarcity", opacity=1,layerId="basins")
    }
    #If only fish diversity and flood risk are selected
      if (("fish" %in% input$chck) && ("flood" %in% input$chck) && !("scar" %in% input$chck)) {
        leafletProxy("USmap") %>%
          removeControl("basins") %>%
          clearGroup("basins") %>%
          addPolygons(data=simplified, weight = 1, smoothFactor = 0.5, fillColor = ~fishfloodpal(fishflood_category), color = ~fishfloodpal(fishflood_category),fillOpacity = 0.8, opacity = 0.5,
                      group = "basins",popup=polypopup,
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F, opacity=1)) %>%
          addLegend(position="bottomleft",
                    colors=c('#000000','#006d2c','#69ABC0', "#E8E8E8","#ffffff"), 
                    labels=c("High diversity/High risk", "High diversity/Low risk","Low diversity/High risk","Low diversity/Low risk", "Insufficient gage data"),
                    title="Fish diversity/Flood risk", opacity=1,layerId="basins")
      }
      #If only water scarcity and flood risk are selected
      if (("scar" %in% input$chck) && ("flood" %in% input$chck)&& !("fish" %in% input$chck)) {
        leafletProxy("USmap") %>%
          removeControl("basins") %>%
          clearGroup("basins") %>%
          addPolygons(data=simplified, weight = 1, smoothFactor = 0.5, fillColor = ~ndcfloodpal(ndcflood_category), color = ~ndcfloodpal(ndcflood_category),fillOpacity = 0.8, opacity = 0.5,
                      group = "basins",popup=polypopup,
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F, opacity=1)) %>%
          addLegend(position="bottomleft",
                    colors=c('#000000','#D66460', '#69ABC0',"#E8E8E8","#ffffff"), 
                    labels=c("High scarcity/High risk", "High scarcity/Low risk","Low scarcity/High risk","Low scarcity/Low risk", "Insufficient gage data"),
                    title="Water scarcity/Flood risk", opacity=1,layerId="basins")
      }
      
    #####################################
    # THREE CATEGORIES SELECTED
    #####################################
    if (("scar" %in% input$chck) && ("flood" %in% input$chck) && ("fish" %in% input$chck)) {
      leafletProxy("USmap") %>%
        removeControl("basins") %>%
        clearGroup("basins") %>%
        addPolygons(data=simplified, weight = 1, smoothFactor = 0.5, fillColor = ~allpal(all_category), color = ~allpal(all_category),fillOpacity = 0.8, opacity = 0.5,
                    group = "basins",popup=polypopup,
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F, opacity=1)) %>%
        addLegend(position="bottomleft",
                  colors=c('#000000','#49006a','#014636','#543005',
                           '#D66460','#006d2c','#69ABC0',
                           "#E8E8E8", "#ffffff"),
                  labels=c("High scarcity/High risk/High diversity","High scarcity/High risk/Low diversity","Low scarcity/High risk/High diversity","High scarcity/Low risk/High diversity",
                           "High scarcity/Low risk/Low diversity","Low scarcity/Low risk/High diversity","Low scarcity/High risk/Lowdiversity",
                           "Low scarcity/Low risk/Low diversity","Insufficient gage data"),
                  title="Water scarcity/Flood risk/Fish diversity", opacity=1,layerId="basins")
    }
    #####################################
    # ADD HATCHING FOR DECLINE RISK
    #####################################
    if ("dec" %in% input$chck) {
      leafletProxy("USmap") %>%
        addPolylines(data= SpatialLinesDF, weight=1, color="#000000", opacity = 0.5,group="Quasi-extinction probability",
                     #SpatialLinesDF IDs stem from the polygons ID slot from the HUC6 polygon dataframe (simplified)
                     #while the HUC6 dataframe row numbers start by 1, polygons IDs start by 0, so you need to add+1 to SpatialLinesDF IDs to match them back to the HUC6 dataframe 
                     popup=polypopup[as.numeric(as.character(SpatialLinesDF@data$ID))+1]) 

    } else {
      leafletProxy("USmap") %>%
        clearGroup("Quasi-extinction probability")
    }
  })
  

  #Create subset of gages based on date selected on toggle bar
  observe({
    #If either active or inactive gages are displayed
    if (input$gagechkact | input$gagechkdeact) {
      #Make a subset of gages based on the selected year in the toggle bar
      gagefc@data$actyr <- rowSums(gagefc@data[,paste0("X",1861:input$gageyear)])
      fcgages_subset <- gagefc[gagefc@data[,paste0("X",input$gageyear)]==1,]
      
      #If the deactived gages box is checked
      if (input$gagechkdeact) {
        fcgages_extinct <- gagefc[gagefc@data[,paste0("X",input$gageyear)]==0 & rowSums(gagefc@data[,paste0("X",1861:input$gageyear)]) > 0,]
        gageextclickpop <- paste0("<strong>Site Number: </strong>", fcgages_extinct$allgages_merge_manual_site_no,
                                  "<br>",fcgages_extinct$links)
        leafletProxy("USmap") %>%
          clearGroup(group="Stream gages extinct") %>%
          addCircles(data= fcgages_extinct,weight = 1,radius = (~actyr*200), 
                     group="Stream gages extinct", popup=gageextclickpop, color="#252525",fillOpacity = 0.4)
      } else {
        leafletProxy("USmap") %>%
          clearGroup(group="Stream gages extinct")
      }
      
      #If the 'active gages' box is checked
      if (input$gagechkact) {
        fcgages_subset@data$actyr <- rowSums(fcgages_subset@data[,paste0("X",1861:input$gageyear)])
        gagesubclickpop <- paste0("<strong>Site Number: </strong>", fcgages_subset$allgages_merge_manual_site_no,
                                  "<br>",fcgages_subset$links)
        gagepop <- paste("Years of data in",input$gageyear,":", fcgages_subset$actyr)
        leafletProxy("USmap") %>%
          clearGroup(group="Stream gages") %>%
          addCircles(data = fcgages_subset, weight = 1,radius = (~actyr*200), 
                     group="Stream gages", label=gagepop, popup=gagesubclickpop, color="#fc4e2a",fillOpacity = 0.6)
      } else {
        leafletProxy("USmap") %>%
          clearGroup(group="Stream gages")
      }
      
    #Either way, clear the previous legend if there was one and add one (clear the previous one so that legends don't keep accumulating)
      leafletProxy("USmap") %>%
        removeControl("gage") %>%
        addLegendCustom(colors = c("#fc4e2a","#252525"), labels = c("Active", "Deactivated"), sizes = c(10, 10))
    } else {
      leafletProxy("USmap") %>%
        clearGroup(group="Stream gages") %>%
        clearGroup(group="Stream gages extinct") %>%
        removeControl("gage")
    }
  })

  ################################################ EXPLORE US DATA TAB ###############################################################
  #Make data selection based on checkbox
  tabdat <- reactive ({
    data_merge_sel <- data_merge_format
    if ("dec" %in% input$checkcat) {
      data_merge_sel <- data_merge_sel[data_merge_sel$Quasiext.huc6 >= 0.5,]
    }
    if ("scar" %in% input$checkcat) {
      data_merge_sel <- data_merge_sel[data_merge_sel$NDC_HUC >= 1,]
    }
    if ("flood" %in% input$checkcat) {
      data_merge_sel <- data_merge_sel[data_merge_sel$flood_FEMA>= 0.05,]
    }
    if ("fish" %in% input$checkcat) {
      data_merge_sel <- data_merge_sel[data_merge_sel$EWU>= 0.2754,]
    }
    data_merge_sel
  })
  
  #Render table
  output$table <- DT::renderDataTable(
    DT::datatable({tabdat()},style='bootstrap', class='compact',selection = "multiple", rownames = F,
                  colnames = c('Name'=1, 'Decline risk %'=3, 'Water scarcity'=4, 'NDC'=5, 'Flood risk %' = 6, '% pop. in flood zone' = 7, 
                               'Fish diversity' = 8, 'EWU' = 9, 'Total population'=10, '% pop. in area with flood study'=11),
                  extensions = 'FixedHeader', options=list(scrollY=TRUE, fixedHeader = TRUE, columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
      formatRound(c(3,5,7,9,11), digits=2)
  )

  #Allow for user to zoom in to selected basin
  #If user has not been on US tab, then the action button only change tab and does not zoom in -- but functions the second time
  proxy = dataTableProxy('table')
  
  observeEvent(input$zoom, {
    #Send pop-up message if user doesn't select any basin to zoom to and clear the pulsing marker if one was present 
    if (is.null(input$table_rows_selected)){
      #https://shiny.rstudio.com/articles/action-buttons.html
      session$sendCustomMessage(type = 'testmessage',
                                message = 'You did not select any basin to zoom to/unselected the displayed basin')
    } else {
      #Switch tab to US
      updateNavbarPage(session, inputId="nav",selected="US")
      #Create shapefile of selected basins
      selectedHUCs <- simplified[simplified@data$HUC6 %in% tabdat()[input$table_rows_selected,"HUC6"],]
      #Determine selected polygons' bounding box
      HUCbbox<- bbox(selectedHUCs)
      #Add selected basins to US map as red polygon boundaries
      leafletProxy("USmap") %>%
        clearGroup("selectedHUC") %>% #Clear previous selection
        addPolygons(data=selectedHUCs, weight = 2, smoothFactor = 0.5, fillColor = 'red', color = 'red',fillOpacity = 0, opacity = 0.8,
                    group = "selectedHUC", 
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F, opacity=1),
                    popup = polypopup[which(simplified$HUC6 %in% selectedHUCs$HUC6)]) %>%
        fitBounds(HUCbbox[1,1]-1,HUCbbox[2,1]-1,HUCbbox[1,2]+1,HUCbbox[2,2]+1) #Pan/Zoom to polygons' bounding  box
        
      #Code to add a pulsing marker at the centroid of a single selected HUC6 basin - replaced by red polygon boudaries to avoid confusion with gage points
      # #Get the coordinates of the centroid (labpt attribute of spatial polygon) of the selected basin (first polygon if multi-polygon feature)
      # HUCcentroid<- slot(slot(simplified[simplified@data$HUC6 == tabdat()[input$table_rows_selected,"HUC6"],], 'polygons')[[1]],'labpt') 
      # leafletProxy("USmap") %>%
      #   clearGroup("pulse") %>% #Clear previous pulsing marker
      #   #Add a pulsing marker at the coordinate of the basin's centroid using leaflet.extra functionality (https://github.com/bhaskarvk/leaflet.extras)
      #   addPulseMarkers(lng=HUCcentroid[1], lat=HUCcentroid[2],icon = makePulseIcon(heartbeat = 0.5),group='pulse') %>%
      #   #Zoom to coordinates of centroid
      #   setView(HUCcentroid[1], HUCcentroid[2],zoom=7)
    }
    })
  
  observeEvent(input$clear, {
    proxy %>% selectRows(NULL)
    leafletProxy("USmap") %>%
      clearGroup("selectedHUC")
  })
  
  ########################################################### DATA DOWNLOAD #############################################################################
  #Zipping structure from https://groups.google.com/forum/#!topic/shiny-discuss/zATYJCdSTwk
  #When run locally, can be buggy if Rtools + env.path not correct. Troubleshoot here: https://stackoverflow.com/questions/29129681/create-zip-file-error-running-command-had-status-127
  #In my case adding C:\Rtools\bin was insufficient. Instead, had to add C:\RBuildTools\3.3\bin to PATH in environmental variables
  output$save  <- downloadHandler(
    filename = 'Ruhietal2017_basindata.zip',
    content = function(fname) {
      #create temp file
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      #Write three files in temp file
      write.csv(tabdat()[input[["table_rows_all"]], ], file = "basin_data.csv", row.names=F)
      write.table(readme, file="README.txt",row.names=F)
      write.csv(metadata, file="metadata.csv", row.names=F)
      #Zip up
      zip(zipfile=fname, files=c("basin_data.csv","metadata.csv", "README.txt"))
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    #Tell shiny what type of file it is
    contentType = "application/zip"
  )
  
  output$saveselected <- downloadHandler(
    filename = 'Ruhietal2017_gagedata.zip',
    content = function(fname_sel) {
      #create temp file
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      #Write three files in temp file
      write.csv(dischargecast[dischargecast$HUC6 %in% tabdat()[input$table_rows_selected, "HUC6"],], file= "selected_basin_gagedata.csv", row.names = F)
      write.table(readme_gages, file="README.txt",row.names = F)
      write.csv(metadata_gages, file="metadata.csv", row.names = F)
      #Zip up
      zip(zipfile=fname_sel, files=c("selected_basin_gagedata.csv","metadata.csv", "README.txt"))
      if(file.exists(paste0(fname_sel, ".zip"))) {file.rename(paste0(fname_sel, ".zip"), fname_sel)}
    },
    #Tell shiny what type of file it is
    contentType = "application/zip"
  )
  
  
  ###################################################### WORLD TAB ####################################################################
  ###########################
  # GRDC world gaging network map
  ###########################
  #Set up basic map
  output$Worldmap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 34, lat = 28, zoom = 2) %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
               options = tileOptions(opacity=0.5)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery", options = providerTileOptions(opacity=0.75)) %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "World Physical", options = providerTileOptions(opacity=0.75)) %>%
      addCircles(data=  GRDCdat[GRDCdat@data$t_start <= 2010 & GRDCdat@data$t_end < 2010,],weight = 1,radius = ~200*(t_yrs), 
                 group="World gages extinct", color="#252525",fillOpacity = 0.4) %>%
      addCircles(data= GRDCdat[GRDCdat@data$t_start <= 2010 & GRDCdat@data$t_end >= 2010,],weight = 1,radius = ~200*(2010-t_start), 
                 group="World gages", color='#fc4e2a',fillOpacity = 0.4) %>%
      # addLegendCustom(colors = c("#fc4e2a","#252525"), labels = c("Reporting", "Reporting discontinued"), sizes = c(10, 10)) %>%
      addLayersControl(
        baseGroups = c("OSM (default)",
                       "World Imagery",
                       "World Physical"),
        position="bottomleft",
        options = layersControlOptions(collapsed = TRUE))
  })
  
  #Display gages based on selected data in the toggle bar
  observeEvent(input$gageyearworld,{
    worldgages_subset <- GRDCdat[GRDCdat@data$t_start <= input$gageyearworld & GRDCdat@data$t_end >= input$gageyearworld,]
    worldgages_extinct <- GRDCdat[GRDCdat@data$t_start <= input$gageyearworld & GRDCdat@data$t_end < input$gageyearworld,]
    #Edit popup for gages based on number of years of data reported by that gage until selected year in toggle bar
    worldgagepop <- paste("Years of data in",input$gageyearworld,":", input$gageyearworld-worldgages_subset$t_start) 
    #Add gages to map
    leafletProxy("Worldmap") %>%
      clearGroup(group="World gages") %>%
      clearGroup(group="World gages extinct") %>%
      #The size of gages that do not report is independent of the selected year
      addCircles(data= worldgages_extinct,weight = 1,radius = ~200*(t_yrs), 
                 group="World gages extinct", color="#252525",fillOpacity = 0.4) %>%
      addCircles(data = worldgages_subset, weight = 1,radius = ~200*(input$gageyearworld-t_start), 
                 group="World gages",label=worldgagepop, color="#fc4e2a",fillOpacity = 0.8)
  })
  
  ################################################
  # Make reactive graphs based on display bounds 
  ################################################
  # A reactive expression that returns the set of gages that are in bounds right now
  gagesinbounds <- reactive({
    if (is.null(input$Worldmap_bounds))
      return(as.data.frame(GRDCdat@data))
    bounds <- input$Worldmap_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    as.data.frame(GRDCdat[GRDCdat@coords[,2] >= latRng[1] & GRDCdat@coords[,2] <= latRng[2] &
                            GRDCdat@coords[,1] >= lngRng[1] & GRDCdat@coords[,1] <= lngRng[2],]@data)
  })
  #Build plots
  output$BoundYears <- renderPlot({
    # If no gages in view, don't plot
    if (nrow(gagesinbounds()) == 0)
      return(NULL)
    gagerec <- data.frame(year=min(gagesinbounds()$t_start):max(gagesinbounds()$t_end), count=NA)
    gagerec_activity <- data.frame(GRDCdat$grdc_no)
    for (i in min(gagesinbounds()$t_start):max(gagesinbounds()$t_end)) {
      gagerec[gagerec$year == i,"count"] <- length(which(gagesinbounds()$t_start <= i & gagesinbounds()$t_end >= i))
    }
    hist <- ggplot(data=gagesinbounds()[gagesinbounds()$t_start <= 2010,], aes(x=2010-t_start)) + geom_histogram(fill="#fc4e2a", bins = 20) +
      labs(x="Number of years of record in 2010", y='Number of gages') +
      theme_classic() + 
      theme(text = element_text(size = 14))
    line <- ggplot(data=gagerec, aes(x=year,y=count)) + 
      geom_line(color="#fc4e2a", size=1.5) +
      geom_line(data=gagerec[gagerec$year >2010,], color="#969696",size=1.5)+
      labs(x="Year", y="Reporting streamgages") + 
      scale_x_continuous(limits=c(1806,2015)) +
      geom_vline(xintercept=input$gageyearworld, size=1.25) + #Create a vertical line on plot whose x position depends on the selected date in toggle bar
      theme_classic()+ 
      theme(text = element_text(size = 14))
    grid.arrange(hist, line, ncol=1)
  })
  
  #Add popover for plot
  addPopover(session, "BoundYears", title="Plots", content=HTML(paste0(
    'Both of these plots are based on the stream gages within the displayed area and change as you zoom in and pan around.
    </br></br>- The top plot shows the frequency distribution of the number of years of data the gages within the displayed area reported to the GRDC by 2016
    </br></br>- The bottom plot shows the number of reporting stream gages over time within the displayed area')), 
    placement="left", trigger="hover")
}

shinyApp(ui, server)


################################################################## DATA PREPARATION ##########################################################################
#Please contact Mathis Messager @ messamat@uw.edu for the raw data required to perform data prep. 
#The working space should be sufficient to run the app locally provided one or two changes in the code

###############################
# Prepare world gages network
###############################
#Import GRDC data
# GRDCdat <- read.csv("F:/Miscellaneous/Hydro_classes/Analysis/World_gages/20170124_GRDC_Stations 2_metadata.csv")
# coordinates(GRDCdat) <- ~long+lat
# class(GRDCdat)
# 
# ###############################
# # Prepare US basins attributes
# ###############################
# #Read in data on quasi-extinction probability (decline risk)
# quasi_ext <- read.csv("F:/Miscellaneous/Hydro_classes/Analysis/Quasi-extinction/version_5/Quasiext.huc6_v5.csv")
# quasi_ext [quasi_ext $rownames_HUC6 < 100000,'HUC6'] <- paste('0',as.character(quasi_ext [quasi_ext $rownames_HUC6 < 100000,'rownames_HUC6']),sep="")
# quasi_ext [quasi_ext $rownames_HUC6 >= 100000,'HUC6'] <- as.character(quasi_ext [quasi_ext $rownames_HUC6 >= 100000,'rownames_HUC6'])
# quasi_ext <- quasi_ext[,-c(1,2)]
# 
# #Read in data on basin characteristics
# fishdiv <- read.csv("F:/Miscellaneous/Hydro_classes/Analysis/Fish/HUC6div.csv", colClasses=c("character", rep('numeric',4)))
# ndc <- read.csv("F:/Miscellaneous/Hydro_classes/Analysis/Water_Scarcity/HUC6_NDC_pr.csv", colClasses=c("character","character", rep('numeric', 12)))
# ndc <- select(ndc, HUC6, NDC_HUC)
# flood <- read.dbf("F:/Miscellaneous/Hydro_classes/Analysis/Flood/HUC6_floodFEMA_data.dbf")
# 
# #Check data structure
# str(quasi_ext)
# str(fishdiv)
# str(ndc)
# str(flood)
# 
# #Not useful - test
# read.dbf("F:/Miscellaneous/Hydro_classes/Analysis/Fish/HUC6div_category.dbf")
# 
# #Get HUC6 names
# fgdb = "F:/Miscellaneous/Hydro_classes/Analysis/wbdhu6_a_us_march2017/wbdhu6_a_us_march2017.gdb"
# HUC6_march0217 = readOGR(dsn=fgdb,layer="WBDHU6")
# HUC6_name <- HUC6_march0217@data[,c('HUC6','NAME')]
# rm(HUC6_march0217)
# 
# 
# #Merge fishdiv, ndc, flood, quasi-ext with HUC6
# data_merge <- merge(quasi_ext, fishdiv, by.x="HUC6", by.y="HUC6_id", all.x =T) %>%
#   merge(., ndc, by="HUC6", all.x =T) %>%
#   merge(., flood, by="HUC6", all.x =T) %>%
#   merge(., HUC6_name, by="HUC6", all.x=T)
# 
# #Categorize basins in two levels for each characteristic: fish diversity, water scarcity, and flood rosk
# #Fish div (categorize based on median fish EWU)
# med_EWU = median(data_merge$EWU, na.rm=T)
# data_merge[data_merge$EWU >= med_EWU & !is.na(data_merge$EWU),"fish_category"] <- "HighEndemism"
# data_merge[data_merge$EWU < med_EWU & !is.na(data_merge$EWU),"fish_category"] <- "LowEndemism"
# data_merge[is.na(data_merge$EWU),"fish_category"] <- "NoEWUdata"
# 
# #NDC (categorize as high NDC when historical maximum cumulative deficit exceeds a full year of precipitation)
# med_NDC_HUC = median(data_merge$NDC_HUC, na.rm=T)
# data_merge[data_merge$NDC_HUC >= 1 & !is.na(data_merge$EWU),"ndc_category"] <- "HighNDC"
# data_merge[data_merge$NDC_HUC < 1 & !is.na(data_merge$EWU),"ndc_category"] <- "LowNDC"
# 
# #Flood (categorized as high flood risk when over 5% of the population inhabiting an area with a flood risk map lives wihtin a 100-yr flood zone)
# med_floodpop = median(data_merge$flood_FEMA, na.rm=T)
# data_merge[data_merge$flood_FEMA >= 0.05 & !is.na(data_merge$EWU),"flood_category"] <- "HighRisk"
# data_merge[data_merge$flood_FEMA < 0.05 & !is.na(data_merge$EWU),"flood_category"] <- "LowRisk"
# data_merge[data_merge$HUC6_pop_FEMAzone == 0 & !is.na(data_merge$EWU),"flood_category"] <- "NoFEMAdata"
# 
# data_merge$fishndc_category <- paste(data_merge$fish_category,data_merge$ndc_category,sep="_")
# data_merge$fishflood_category <- paste(data_merge$fish_category,data_merge$flood_category,sep="_")
# data_merge$ndcflood_category <- paste(data_merge$ndc_category,data_merge$flood_category,sep="_")
# data_merge$all_category <- paste(data_merge$ndc_category,data_merge$flood_category, data_merge$fish_category, sep="_")
# 
# ###################################
# # Prepare US gages geospatial data
# ###################################
# #Read the feature class of gages
# fgdb = "F:/Miscellaneous/Hydro_classes/Figures/Figure_1_map/Figure1_map.gdb"
# gagefc = readOGR(dsn=fgdb,layer="allgages_merge_manual_hist")
# #Change column names
# dischargecast<-read.csv("F:/Miscellaneous/Hydro_classes/discharge_yearly_cast_all.csv",colClasses=c(rep("character",3), rep("numeric",156)))
# colnames(gagefc@data)[9:164] <- colnames(dischargecast[,4:159])
# #Remove gages with NA values (46 gages/23k)
# gagefc <- gagefc[!is.na(gagefc@data$X1861),]
# 
# ###################################
# # Prepare US basins geospatial data
# ###################################
# 
# # Import basins shapefile
# folder = "F:/Miscellaneous/Hydro_classes/Figures/Figure_3/FloodNDC_Map"
# fc = readOGR(dsn=folder,layer="HUC6")
# # Determine the FC extent, projection, and attribute information
# class(fc)
# summary(fc)
# 
# #Simplify polygons to reduce load time
# summary(fc)
# object.size(fc)
# simplified <- rmapshaper::ms_simplify(fc)
# object.size(simplified)
# #Merge shapefile with attributes
# simplified <- merge(simplified, data_merge, by="HUC6",all.x=T)
# 
# #Prepare hatched polygons to represent basins with high gaging density decline risk
# #Twicked from https://statnmap.com/en/2017/05/how-to-fill-a-hatched-area-polygon-with-holes-in-leaflet-with-r/
# #hatched.SpatialPolygons didn't work so had to troubleshoot the function to generate hatches + didn't need to have the 'holes' functionality
# #Can change density and angle for aesthetics
# #Select basins with high decline risk
# extsimplified <- simplified[simplified@data$Quasiext.huc6 >= 0.5 & !is.na(simplified@data$Quasiext.huc6),]
# density = 5
# angle = 45
# fillOddEven = FALSE
# #Make sure that input isspatialPolygons
# if (!is(extsimplified, "SpatialPolygons"))
#   stop("Not a SpatialPolygons object")
# 
# n <- length(slot(extsimplified, "polygons"))
# polys <- slot(extsimplified, "polygons")
# pO <- slot(extsimplified, "plotOrder")
# 
# if (length(density) != n)
#   density <- rep(density, n, n)
# if (length(angle) != n)
#   angle <- rep(angle, n, n)
# 
# all.Lines <- list()
# all.Lines.ID <- numeric(0)
# 
# for (j in pO) {
#   print(j)
#   ID = polys[[j]]@ID
#   Sr <- polys[[j]]
#   if (!is(Sr, "Polygons"))
#     stop("Not a Polygons object")
# 
#   pO2 <- slot(Sr, "plotOrder")
#   polys2 <- slot(Sr, "Polygons")
#   #print(pO2)
# 
#   all.Lines2 <- list()
#   for (i in pO2) {
#     #print(i)
#     if (!slot(polys2[[i]], "hole")) {
#       # Transform polygon as parallel lines
#       lines.hatch <- polygon.fullhatch(slot(polys2[[i]], "coords"),
#                                        density = density[j], angle = angle[j], fillOddEven = fillOddEven)
#       #The code wasn't working when the polygon was so small that no line could go through it/be calculated (e.g. island of HUC6 040301)
#       # So skip very small polygons in multi-polygon records
#       if (!is.null(lines.hatch)) {
#         # Transform as SpatialLines
#         Lines.i <- SpatialLines(list(Lines(
#           apply(lines.hatch, 1,
#                 function(x) Line(cbind(c(x[1], x[3]), c(x[2], x[4])))),
#           ID = i)))
# 
#         # Clean Lines if over a "hole"
#         Lines.i.holes <- rgeos::gIntersection(Lines.i, SpatialPolygons(list(Sr)),
#                                               drop_lower_td = TRUE)
# 
#         if (!is.null(Lines.i.holes)) {
#           Lines.i.holes@lines[[1]]@ID <- paste0(ID, ".", i)
#           all.Lines2[[length(all.Lines2) + 1]] <- Lines.i.holes@lines[[1]]
#         }
#       } else {
#         print('skip')
#       }
#     }
#   }
#   all.Lines.ID <- c(all.Lines.ID, rep(polys[[j]]@ID, length(all.Lines2)))
#   all.Lines[length(all.Lines) + 1:length(all.Lines2)] <- all.Lines2
# }
# # Correct ID
# SpatialLinesDF <- SpatialLinesDataFrame(
#   SpatialLines(all.Lines),
#   data = data.frame(ID = all.Lines.ID),
#   match.ID = FALSE)
# #
# 
# ###############################
# #Prepare formatted table
# ###############################
# data_merge_format <- data_merge[,c(13,1,2,15,7,16,11,14,4,8,12)]
# data_merge_format$totalpop <- round(as.numeric(data_merge_format$totalpop))
# data_merge_format[data_merge_format$ndc_category == 'LowNDC' & !is.na(data_merge_format$ndc_category), "ndc_category"] <- "Low"
# data_merge_format[data_merge_format$ndc_category == 'HighNDC' & !is.na(data_merge_format$ndc_category), "ndc_category"] <- "High"
# data_merge_format[data_merge_format$flood_category == 'LowRisk' & !is.na(data_merge_format$flood_category), "flood_category"] <- "Low"
# data_merge_format[data_merge_format$flood_category == 'HighRisk' & !is.na(data_merge_format$flood_category), "flood_category"] <- "High"
# data_merge_format[data_merge_format$fish_category == 'LowEndemism' & !is.na(data_merge_format$fish_category), "fish_category"] <- "Low"
# data_merge_format[data_merge_format$fish_category == 'HighEndemism' & !is.na(data_merge_format$fish_category), "fish_category"] <- "High"
# 
# ################################
# #Add metadata files to workspace
# ###############################
# readme <- readtext('F:/Miscellaneous/Hydro_classes/Map/Map_8/www/README.txt')
# metadata <- read.csv('F:/Miscellaneous/Hydro_classes/Map/Map_8/www/metadata_columns.csv')
# 
# readme_gages <- readtext('F:/Miscellaneous/Hydro_classes/Map/Map_8/www/README_gages.txt')
# metadata_gages <- read.csv('F:/Miscellaneous/Hydro_classes/Map/Map_8/www/metadata_columns_gages.csv')
# 
# rm(fc) 
# rm(all.Lines)
# rm(polys)
# save.image("F:/Miscellaneous/Hydro_classes/Map/Map_8/Map_8.RData")
