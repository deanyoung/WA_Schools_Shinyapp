
shinyServer(function(input, output) {
  
  # Adjusts district options based on what school was selected
  output$district <- renderUI({
    district.options <- data %>% filter(School==input$school)
    selectInput("district","Select District", choices = district.options$District)
                   
  })
    
  # Forces app to be non-reactive
  observeEvent(input$go, {
    
  output$first_title <- renderText({
    "Selected School"
  })
  
  # attributes of selected school
  school.attr <- data %>% filter(School==isolate(input$school), District==isolate(input$district))

  # Filter according to restrictions selected by user
  restricted <- data
  if ("District" %in% input$restrict){
    restricted %<>% filter(District == school.attr$District[1])
    
  }
  
  if("Category" %in% input$restrict){
    restricted %<>% filter(SchoolCategory == school.attr$SchoolCategory[1])
    
  }
  
  if("Type" %in% input$restrict){
    restricted %<>% filter(SchoolType == school.attr$SchoolType[1])
    
  }
  
  # Filter out selected school (KNN other than itself) then compute euclidean distances of predictors
  data.euclid <- restricted %>% filter(ID != school.attr$ID) %>% 
    mutate(euclid = sqrt((norm_LowIncome-school.attr[["norm_LowIncome"]])^2 +
                           (norm_ELL-school.attr[["norm_ELL"]])^2 +
                           (norm_Enrollment-school.attr[["norm_Enrollment"]])^2)) %>%
    arrange(euclid)
  
  # Check if the selected k is greater than the number of valid neighbors
  if(dim(data.euclid)[1] >= isolate(input$k)){
    data.euclid %<>% slice(1:isolate(input$k))
    output$warn <- renderText({""})
  }else{
    output$warn <- renderText({
      paste("There exists only",dim(data.euclid)[1],"nearest-neighbors.")
      
    })
  }
  
  # Compute prediction based on neighbors
  output$pred <- renderText({
    predicted <- round(mean(data.euclid$mathLImet,na.rm=TRUE),2)
    paste("Predicted percentage of low-income students who meet standards: ",predicted,"%",sep="")
  })
  
  # Determine whether output$diff should be red or white (negative or positive)
  if(dim(data.euclid)[1]>0){
  
    diff <- school.attr$mathLImet[1] - mean(data.euclid$mathLImet,na.rm=TRUE)
    diff <- round(diff,2)
  
    neg <<- ifelse(diff<0,"Yes","No")
  }else{
    neg <<- "Invalid"
    
  }
  
  # output differential between selected school and nearest neighbors
  output$diff <- renderUI({
    if(neg=="Yes"){
      div(paste("Differential in actual and predicted percentage: ",diff,"%",sep=""),
          style="color:red")
    }else{
      if(neg=="No"){
        paste("Differential in actual and predicted percentage: +",diff,"%",sep="")
      }else{
        paste("Differential in actual and predicted percentage: N/A")
      }
    }
  })
  
  # output attributes of selected school
  output$school <- renderTable({
    school.attr %>% select(-norm_LowIncome,-norm_ELL,-norm_Enrollment,-ID) %>%
      rename(Category = SchoolCategory, Type = SchoolType, "ELL %" = ELL, 
             "L.I. Pass Rate (math)" = mathLImet, "Low Income %" = LowIncome)
    
  })
  
  output$knn_title <- renderText({
    paste(isolate(input$k),"Nearest-Neighbor Schools")
    
  })
  
  # output attributes of nearest neighbors
  output$knn_table <- renderTable({
    
    data.euclid %>% select(-norm_LowIncome,-norm_ELL,-norm_Enrollment,-euclid,-ID) %>%
      rename(Category = SchoolCategory, Type = SchoolType, "ELL %" = ELL, 
             "L.I. Pass Rate (math)" = mathLImet, "Low Income %" = LowIncome)
    
  })
  
  graph_data <- bind_rows(school.attr %>% mutate(Color="Yes"), data.euclid %>% mutate(Color="No"))
  
  # nn = nearest neighbor rank
  graph_data %<>% mutate(nn = as.numeric(rownames(graph_data)) - 1)
  
  # rank = pass rate rank
  graph_data %<>% arrange(desc(mathLImet)) %>% mutate(rank = rownames(graph_data))
  
  # order by pass rate
  graph_data$School <- factor(graph_data$School, levels=graph_data$School[order(desc(graph_data$mathLImet))])
  
  graph_data %>%
    ggvis(~School,~mathLImet, key := ~rank, fill = ~Color) %>%
    layer_bars() %>%
    hide_legend("fill") %>%
    set_options(height = 700, width = 1000) %>%
    add_axis("x", grid = FALSE, properties = axis_props(labels=list(
                                                      text = ""),
                                          title=list(
                                                      fill = "white")
                                          )) %>%
    add_axis("y", title="L.I. Pass Rate (math) %", 
             properties = axis_props(labels=list(
                                                  fill = "white"),
                                     title=list(
                                                  fill = "white")
                                     )) %>%
    add_tooltip(function(df) paste0("<b>", "<font color='black'>", "School: ", df$x, "<br>", 
                                    "Pass Rate (PR): ",round(as.numeric(df$stack_upr_),2), "<br>", 
                                    "PR Rank: ", df$rank, " out of ", dim(graph_data)[1],"<br>", 
                                    "NN Rank: ", graph_data$nn[graph_data$rank == df$rank], " out of ", dim(graph_data)[1] - 1),
                                    "hover") %>%
    bind_shiny("ggvis_plot","ggvis_plot_ui")
  
  })
  
})