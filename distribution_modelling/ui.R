#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

ui <- fluidPage(

  # Application title
  titlePanel("Species Distribution Modelling"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(

    sidebarPanel(
          helpText("Depending on which species and algorithm you choose, 
                   calculating the predictive map may take up to several minutes."),

       selectInput(inputId = "model",
                   label = "Choose a Model Method",
                   choices = c("Single Species", "Multiple Species"),
                   selected = "Single Species",
                   multiple = FALSE),
       
       selectInput(inputId = "algorithm",
                   label = "Choose an Algorithm",
                   choices = c("GLM", "GAM", "MARS", "GBM", "CTA", "RF", "SVM"),
                   selected = "GLM",
                   multiple = FALSE),
       
       conditionalPanel(condition = "input.model == 'Single Species'",
                        selectInput(
                              inputId = "single",
                              label = "Choose a Species to Model",
                              choices = sort(unique(df_orig$species)),
                              selected = "Selasphorus rufus",
                              multiple = FALSE)),
       
    conditionalPanel(condition = "input.model == 'Multiple Species'",
                     selectInput(
                           inputId = "multi", 
                           label = "Choose Multiple Species to Model",
                           choices = sort(unique(df_orig$species)),
                           selected = NULL,
                           multiple = TRUE)),
    
    actionButton(inputId = "submit",
                 label = "Plot Model"),
      
       
       bsTooltip(id = "model",
                 title = "You may choose between modeling a single species or pick multiple species to find regions that would support the greatest number of your chosen organisms.",
                 placement = "top",
                 trigger = "hover"),
       
       bsTooltip(id = "species",
                 title = "Select only a single species name if modeling a single species or select multiple names if modeling different species together",
                 placement = "top",
                 trigger = "hover"),
       
       bsTooltip(id = "algorithm",
                 title = "Selecting different algorithms will slightly change the prediction map and accuracy. Certain models may predict better than others. Some models will perform better with different species.",
                 placement = "top",
                 trigger = "hover")
    ),

    # Show a plot of the generated distribution
    mainPanel(
          bsPopover(id = "distPlot", 
                    title = "Species Distribution Map", 
                    content = "The map depicts an untested hypothesis for where species are more likely to be found based on known occurrence. Species are expected to be observed in areas with a higher probability score.",
                    placement = "left"),
          
       leafletOutput("distPlot"),
       h1(""),
       h1(""),
       h1(""),
       
       bsPopover(id = "predPlot",
                 title = "Predictor Importance",
                 content = "This plot indicates the relative importance the environmental variables have in predicting the species occurrence.",
                 placement = "left"),
       plotOutput("predPlot")
    )
  )
)
















