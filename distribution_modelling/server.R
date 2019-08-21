#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

server <- function(input, output, session){

      
      obs <- reactive({
            if (is.null(input$multi)) {return()}
            
            if(length(input$multi)>=2){
                  
                  species = df_orig[which(df_orig$species%in%input$multi),]
                  dplyr::select(species, species, decimalLatitude, decimalLongitude) %>%
                        rename(latitude = decimalLatitude, longitude = decimalLongitude) %>%
                        select(species, longitude, latitude)
            } else {
                  
                  if(length(input$single) == 1){
                        filter(df_orig, species == input$single) %>%
                              dplyr::select(species, decimalLatitude, decimalLongitude) %>%
                              rename(latitude = decimalLatitude, longitude = decimalLongitude) %>%
                              select(species, longitude, latitude)
                  }
                  
            }
            
            
      })
      
      observe({
            print(class(input$multi))
      })
                        

      
            SDM <- eventReactive(input$submit, {
                  
                  if(input$model == "Multiple Species"){
                         stack_modelling(algorithms = input$algorithm,
                                         Occurrences = obs(),
                                         Env = predictors,
                                         ensemble.thresh = 0,
                                         Xcol = "longitude",
                                         Ycol = "latitude",
                                         Spcol = "species",
                                         rep = 1,
                                         method = "pSSDM",
                                         tmp = F,
                                         cores = 3)
                                                 
                  } else {
                        
                   modelling(algorithm = input$algorithm,
                             Occurrences = obs(),
                             Env = predictors,
                             Xcol = "longitude",
                             Ycol = "latitude")
                                         
                  }
                  
                  
            })
            
            
            
            #plot the model
            
            output$distPlot <- 
                           
                           
                            renderLeaflet({
                                 
                                 if(input$model =="Single Species"){
                                       
                                       # plot the SDM model
                                       tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
                                       map <- tm_shape(SDM()@projection)+
                                             tm_layout(title = paste("Predicted Species Distribution with", 
                                                                     as.character(round(SDM()@evaluation[1,6]*100, 0)),
                                                                     "% Accuracy"))+
                                             tm_raster(alpha = 0.6, saturation = 1, title = "Probability")
                                       
                                       tmap_leaflet(map)
                                       
                                 } else {
                                       # plot the SSDM model 
                                       
                                       map <- tm_shape(SDM()@diversity.map)+
                                             tm_layout(title = paste("Predicted Species Richness with", 
                                                                     as.character(round(SDM()@evaluation[1,2]*100, 2)), "% Accuracy"))+
                                             tm_raster(alpha = 0.6, saturation = 1, title = "Species Richness") 
                                       
                                       tmap_leaflet(map)
                                 }
                                 
                                 
                           })
                           
                         
                        
            
            
      
                
                
                #plot the predictor importance
                output$predPlot <- 
                      
                            
                            renderPlot({
                                  # plot the importance of environmental variables
                                  pred_importance <- gather(data = SDM()@variable.importance,
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
                            
                    
               
                      
                      
              
      
                 
            
      
      # obs <- reactive({
      # 
      #       if(input$model=="Single Species"){
      #             species = filter(df_orig, species == input$species) %>%
      #                   dplyr::select(species, decimalLatitude, decimalLongitude) %>%
      #                   rename(latitude = decimalLatitude, longitude = decimalLongitude) %>%
      #                   select(species, longitude, latitude)
      #       } else {
      #             species = df_orig[which(df_orig$species%in%input$species),]
      #             species = dplyr::select(species, decimalLatitude, decimalLongitude) %>%
      #                   rename(latitude = decimalLatitude, longitude = decimalLongitude) %>%
      #                   select(species, longitude, latitude)
      #       }
      # 
      # 
      # })
      
      
      # SDM <- reactive({
      # 
      #       if(input$model == "Multiple Species"){
      #             model <- stack_modelling(algorithms = input$algorithm,
      #                                      Occurrences = obs(),
      #                                      Env = predictors,
      #                                      ensemble.thresh = 0,
      #                                      Xcol = "longitude",
      #                                      Ycol = "latitude",
      #                                      Spcol = "species",
      #                                      rep = 1,
      #                                      method = "pSSDM",
      #                                      tmp = F,
      #                                      cores = 3)
      #       } else {
      #             model = modelling(algorithm = input$algorithm,
      #                               Occurrences = obs(),
      #                               Env = predictors,
      #                               Xcol = "longitude",
      #                               Ycol = "latitude")
      #       }
      # 
      # 
      # })
      
      # SDM <- reactive({
      #       
      #       if(input$model =="Single Species"){
      #             model = modelling(algorithm = input$algorithm,
      #                               Occurrences = obs(),
      #                               Env = predictors,
      #                               Xcol = "longitude",
      #                               Ycol = "latitude")
      #       } else {
      #             
      #             model <- stack_modelling(algorithms = input$algorithm,
      #                                          Occurrences = obs(),
      #                                          Env = predictors,
      #                                          ensemble.thresh = 0,
      #                                          Xcol = "longitude",
      #                                          Ycol = "latitude",
      #                                          Spcol = "species",
      #                                          rep = 1,
      #                                          method = "pSSDM",
      #                                          tmp = F,
      #                                          cores = 3) 
      #       }
      #       
      #       
      # })
      
            
            # #plot the model
            # output$distPlot <- renderLeaflet({
            #       
            #       if(input$model =="Single Species"){
            #             
            #             # plot the SDM model
            #             tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = 1)
            #             map <- tm_shape(SDM()@projection)+
            #                   tm_layout(title = paste("Predicted Species Distribution with",
            #                                           as.character(round(SDM()@evaluation[1,6]*100, 0)),
            #                                           "% Accuracy"))+
            #                   tm_raster(alpha = 0.6, saturation = 1, title = "Probability")
            #             
            #             tmap_leaflet(map)
            #             
            #       } else {
            #             # plot the SSDM model 
            #             
            #             map <- tm_shape(SDM()@diversity.map)+
            #                   tm_layout(title = paste("Predicted Species Richness with", 
            #                                           as.character(round(SDM()@evaluation[1,2], 2)), "% Accuracy"))+
            #                   tm_raster(alpha = 0.6, saturation = 1, title = "Species Richness") 
            #             
            #             tmap_leaflet(map)
            #       }
            # })
            
            
            # #plot the predictor importance
            # output$predPlot <- renderPlot({
            #       
            #       # plot the importance of environmental variables
            #       pred_importance <- gather(data = SDM()@variable.importance,
            #                                 key = "predictor",
            #                                 value = "value")
            #       
            #       ggplot(pred_importance, aes(x = reorder(predictor, -value), y = value))+
            #             geom_col()+
            #             labs(title = "Important Model Predictors",
            #                  x = "Environmental Predictors",
            #                  y = "Percentage Contribution to the Model")+
            #             theme(title = element_text(face = "bold", size = 16),
            #                   axis.title.x = element_text(face = "bold", size = "14"),
            #                   axis.title.y = element_text(face = "bold", size = "14"),
            #                   axis.text = element_text(size = 12))+
            #             coord_flip()
            #       
            # })
            
      }
  








    
 
      









