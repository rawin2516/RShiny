#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


library(shiny)

ui<-navbarPage("Margin Engine Testing by Awin",
               tabPanel("Data Import",
                        sidebarLayout(sidebarPanel( fileInput("file","Upload your CSV",multiple = FALSE),
                                                    tags$hr(),
                                                    h5(helpText("Select the read.table parameters below")),
                                                    checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                                    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                                    radioButtons(inputId = 'sep', label = 'Separator', 
                                                                 choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                        ),
                        mainPanel(uiOutput("tb1"))
                        ) ),
               
               tabPanel("Model_dev",
                        sidebarLayout(sidebarPanel(
                          uiOutput("model_select"),
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select"),
                          uiOutput("quantity"),
                          uiOutput("credit")
                        ),
                        mainPanel(helpText("Your Selected variables"),
                                   verbatimTextOutput("other_val_show"),verbatimTextOutput("margin"))))
)
server<-function(input,output) { data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  
})  
output$table <- renderTable({
  if(is.null(data())){return ()}
  data()
})
output$tb1 <- renderUI({
  tableOutput("table")
})
output$model_select<-renderUI({
  selectInput("modelselect","Select Algo",choices = c("Regression"="lm","SVM"="svm"))
})
output$var1_select<-renderUI({
  selectInput("ind_var_select","Select your dependent Variable", choices =as.list(names(data())),multiple = FALSE)
})
output$rest_var_select<-renderUI({
  checkboxGroupInput("other_var_select","Select your independent Variable/s",choices =as.list(names(data())))
})
output$quantity<-renderUI({
  numericInput("quantity","Enter Quantity", value = 0)
})
output$credit<-renderUI({
  numericInput("credit","Enter Credit Period", value = 0)
})

data1 <- reactive({
  
  data.frame("group_quantity"=input$quantity, "credit_days"= input$credit)
  
})

data2 <- reactive({
  
  input$other_var_select
  input$ind_var_select
  
  f<-data()
  
  
  library(caret)
  
  form <- sprintf("%s~%s","actual_margin",paste0("group_quantity",collapse="+"))
  
  logreg <-lm(as.formula(form),data=f)
  
  mean_x <- mean(f$group_quantity)
  
  n <- length(f$group_quantity)
  
  lev <- vector("numeric", n)
  
  cooks <- vector("numeric", n)
  
  ss_x <- sum( (f$group_quantity - mean(f$group_quantity) )^2 )
  
  df <- n - 2
  
  threshold<-4/(n-1-1)
  
  reg_x <- logreg
  
  pred_cook <- predict(reg_x, newdata=f)
  
  res <- -f$actual_margin + pred_cook
  
  mean_res <- mean(res)
  
  ss_res <- sum( (res - mean(res) )^2 )
  
  ss_ms <- ss_res/df
  
  for(i in 1:n){
    
    lev[i] <- (1/n) + ((f$group_quantity[i] - mean_x)^2/ss_x)
    cooks[i] <- (res[i]^2/ss_ms)*(lev[i]/(1-lev[i])^2)/2
    
  }
  
  trans_fin <- cbind(f, lev, cooks)
  
  out_class <- vector("numeric", n)
  
  
  for(i in 1:n){
    
    if(is.na(cooks[i])){
      
      out_class[i]<-0
      
    }else if(cooks[i] > threshold){
      
      out_class[i] <- 1
      
    }else{
      
      out_class[i]<-0
    }
    
  }
  
  
  trans_fin <- cbind(f, cooks, lev, out_class)
  
  trans_fin <- trans_fin[with(f, order(-out_class)),]
  
  n1 <- sum(out_class)
  
  if(n1>0){
    m <- as.matrix(expand.grid(rep(list(0:1),n1)))
    
    binary_comb <- t(m)
    
    temp_mat <- matrix(0, nrow = n-n1, ncol = ncol(binary_comb))
    
    fin_binary_comb <- rbind(binary_comb, temp_mat)
    
    trans_fin_list <- list()
    reg_list <- list()
    pred_list <- list()
    mse_list <- list()
    
    for(i in 1:(2**n1)){
      
      trans_fin_list[[i]] <- cbind(subset(trans_fin, select=-c(out_class)), out_class=fin_binary_comb[,i])
      reg_list[[i]] <- lm(actual_margin ~ group_quantity, data= subset(trans_fin_list[[i]], trans_fin_list[[i]]$out_class==0))
      mse_list[[i]] <- mean(summary(reg_list[[i]])$residuals^2)
      
    }
    
    data.frame(subset(trans_fin_list[[which.min(mse_list)]], trans_fin_list[[which.min(mse_list)]]$out_class==0))
    
  }else{
    data.frame(trans_fin)
  }
  
  
  })

output$other_val_show<-renderPrint({
  
  input$other_var_select
  input$ind_var_select
  input$quantity
  input$credit
  
  library(caret)
  
  form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  
  f2 <- data2()
  
  logreg_new <-lm(as.formula(form),data=f2)
  
  print(f2)
  
  print(logreg_new)
  
  f1<-data1()
  
 if(is.na(logreg_new$coefficients[2]) && input$quantity != mean(f2$group_quantity)){
    
    print("VERTICAL LINE")
    
  }else{
    
    print(predict(logreg_new, newdata=f1))
    
  }
})

}

shinyApp(ui=ui,server=server)