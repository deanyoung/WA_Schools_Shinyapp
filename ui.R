shinyUI(fluidPage(theme = "bootstrap.superhero.css",
  h1(strong("KNN Model for Washington Schools"), align="center"),
  sidebarLayout(
    sidebarPanel(
      h3(strong("Options")),
      selectizeInput("school","Select School",
                     choices=unique(data$School),
                     options=list(placeholder = "Please select a school",
                                  onInitialize = I('function() { this.setValue("A G WEST BLACK HILLS"); }'))
                     
                     ),
      uiOutput("district"),
      sliderInput("k", "Select K", min = 1, 
                  max = 30, value = 5),
      checkboxGroupInput("restrict", label = h3(strong("Restrictions")), 
                         choices = list("District", "Category", "Type")),
      uiOutput("ggvis_plot_ui"),
      actionButton("go","Submit")
      
    ),
  
  mainPanel(
    h3("Selected School"),
    tableOutput("school"),
    div(textOutput("warn"), style="color:red"),
    h3(textOutput("knn_title")),
    tableOutput("knn_table"),
    textOutput("pred"),
    uiOutput("diff"),
    ggvisOutput("ggvis_plot")
    
    
  )
  )
  
  
))