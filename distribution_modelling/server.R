#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

server <- function(input, output){
      
            
            #plot the model
            output$distPlot <- renderLeaflet({
                  
                  
                  #### Perform SDM ###d
                  
                  # filter the occurrences based on user choice
                  obs <- filter(df_orig, species == input$species) %>%
                        dplyr::select(species, decimalLatitude, decimalLongitude) %>%
                        rename(latitude = decimalLatitude, longitude = decimalLongitude) %>% 
                        select(species, longitude, latitude)
                  
                  
                  # build the model
                  SDM <- modelling(algorithm = input$algorithm,
                                   Occurrences = obs,
                                   Env = predictors,
                                   Xcol = "longitude",
                                   Ycol = "latitude")
                  
                  
                  # plot the model
                  tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
                  map <- tm_shape(SDM@projection)+
                        tm_layout(title = paste("Predicted Species Distribution with",
                                                as.character(round(SDM@evaluation[1,6]*100, 2)),
                                                "% Accuracy"))+
                        tm_raster(alpha = 0.6, saturation = 1, title = "Probability")
                  
                  tmap_leaflet(map)
                  
            })
            
            
            #plot the predictor importance
            output$predPlot <- renderPlot({
                  
                  # filter the occurrences based on user choice
                  obs <- filter(df_orig, species == input$species) %>%
                        dplyr::select(species, decimalLatitude, decimalLongitude) %>%
                        rename(latitude = decimalLatitude, longitude = decimalLongitude) %>% 
                        select(species, longitude, latitude)
                  
                  # build the model
                  SDM <- modelling(algorithm = input$algorithm,
                                   Occurrences = obs,
                                   Env = predictors,
                                   Xcol = "longitude",
                                   Ycol = "latitude")
                  
                  # plot the importance of environmental variables
                  pred_importance <- gather(data = SDM@variable.importance,
                                            key = "predictor",
                                            value = "value")
                  
                  ggplot(pred_importance, aes(x = reorder(predictor, -value), y = value))+
                        geom_col()+
                        labs(title = "Important Model Predictors",
                             x = "Environmental Predictors",
                             y = "Percentage Contribution to the Model")+
                        theme(title = element_text(face = "bold", size = 16),
                              axis.title.x = element_text(face = "bold", size = "14"),
                              axis.title.y = element_text(face = "bold", size = "14"),
                              axis.text = element_text(size = 12))+
                        coord_flip()
                  
            })
            
      }
  








    
 
      









