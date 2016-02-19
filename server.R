
shinyServer(function(input, output) {
  
  output$district <- renderUI({
    district.options <- data %>% filter(School==input$school)
    selectInput("district","Select District", choices = district.options$District)
                   
  })
    
  
  
  observeEvent(input$go, {
  
  school.attr <- data %>% filter(School==isolate(input$school), District==isolate(input$district))
  
  output$restrictlist <- renderText({
    isolate(input$restrict)
    
  })
  
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
  
  data.euclid <- restricted %>% filter(ID != school.attr$ID) %>% 
    mutate(euclid = sqrt((norm_LowIncome-school.attr[["norm_LowIncome"]])^2 +
                           (norm_ELL-school.attr[["norm_ELL"]])^2 +
                           (norm_Enrollment-school.attr[["norm_Enrollment"]])^2)) %>%
    arrange(euclid)
  
  if(dim(data.euclid)[1] >= isolate(input$k)){
    data.euclid %<>% slice(1:isolate(input$k))
    output$warn <- renderText({""})
  }else{
    output$warn <- renderText({
      paste("There exists only",dim(data.euclid)[1],"nearest-neighbors.")
      
    })
  }
  
  
  output$pred <- renderText({
    predicted <- round(mean(data.euclid$mathLImet,na.rm=TRUE),1)
    paste("Predicted percentage of low-income students who meet standards: ",predicted,"%",sep="")
  })
  
  if(dim(data.euclid)[1]>0){
  
    diff <- school.attr$mathLImet[1] - mean(data.euclid$mathLImet,na.rm=TRUE)
    diff <- round(diff,1)
  
    neg <<- ifelse(diff<0,"Yes","No")
  }else{
    neg <<- "Invalid"
    
  }
  
  
  output$diff <- renderUI({
    if(neg=="Yes"){
      div(paste("Differential in actual and predicted percentage: ",diff,"%",sep=""),
          style="color:#D9534F")
    }else{
      if(neg=="No"){
        paste("Differential in actual and predicted percentage: +",diff,"%",sep="")
      }else{
        paste("Differential in actual and predicted percentage: N/A")
      }
    }
  })
  
  output$school <- renderTable({
    school.attr %>% select(-norm_LowIncome,-norm_ELL,-norm_Enrollment,-ID) %>%
      rename(Category = SchoolCategory, Type = SchoolType, "ELL %" = ELL, 
             "L.I. Pass Rate (math)" = mathLImet, "Low Income %" = LowIncome)
    
  })
  
  output$knn_title <- renderText({
    paste(isolate(input$k),"Nearest-Neighbor Schools")
    
  })
  
  
  output$knn_table <- renderTable({
    
    data.euclid %>% select(-norm_LowIncome,-norm_ELL,-norm_Enrollment,-euclid,-ID) %>%
      rename(Category = SchoolCategory, Type = SchoolType, "ELL %" = ELL, 
             "L.I. Pass Rate (math)" = mathLImet, "Low Income %" = LowIncome)
    
  })
  
  
  })
  
})