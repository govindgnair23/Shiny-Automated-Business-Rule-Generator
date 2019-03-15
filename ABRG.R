#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#Version 11_1: Attempt to align input and output objects
#Version 11_2: Fixing use of impute_missing function as it converts numeric to factor&
#              updated code to generate predictions
#version 11_3: Cleaning up appearance and introuducing a tab with notes/instructions
#version 12_1: Adding new widget to analyze interaction between variables
#version 13_1 - 13.7: Adding additional dials to widget to sele3ct only few levels of a factor variable and
#                     customizing number of cuts for numeric variable
#version 14_1: make cosmetic modifications
#version 14_2: Error message in UI needs to be fixed
#veron 14:3: Update plot tiles for interaction plots
#version 14_4 - 14_5: Cleaning up rule performance output
#version 14_6: remove redundant code for customizing interaction plots
#version 14_7: Fix isues with hover in interaction plot
#Version 15_1: Clean up presentation of rule performance metrics
#Version 15_2: Add %Fraud and %Non-Fraud to train-valid -split and change color of Fraud/Non Fraud
#Version 15_3: Add visuals to demonstrate train-test split
#Version 15_4: Add feature to recommend the most important variables
#version 15_5: Fix possible bug in cleaning data
#version 15_6: Replace table recommending variables with a chart
#version 15_7: Implement Train -  Validation Split
#version 15_8: Fix error message if random forest does not work
#version 16_2: Include automated rule generation capability
#version 16_3: Include progress bar for automat rule generation
#version 16_4: Evaluate variable types provided by user and issue warnings
#Version 16_6: Include validation results


library(shiny)
library(descr)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(plotly)
library(DT)
library(formattable)
library(randomForest)

options(shiny.maxRequestSize = 100*1024^2)
# Define UI 
ui <- fluidPage(
  tags$style("#select_LHS1 {font-size:6px;}"),
  tags$style("#select_Operator1{font-size:6px;}"),
  tags$style("#select_RHS1{font-size:6px;}"),
  
  # App title ----
  titlePanel(title =div(img(src="Key.png"),"Business Rule Generator 1.0"),
             windowTitle = "Business Rule Generator"),
  
  tabsetPanel(
    tabPanel("main",
             #Read in a CSV dataset
             fluidRow(
               
               column(4,
                      wellPanel(
                        
                        h3("Load Dataset"),
                        # Input: Select a file ----
                        fileInput( inputId = "file1", "Choose CSV File",
                                   multiple = TRUE,
                                   accept = '.csv'),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: Checkbox if file has header ----
                        checkboxInput("header", "Header", TRUE),
                        
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: Select number of rows to display ----
                        radioButtons("disp", "Display",
                                     choices = c(Head = "head",
                                                 All = "all"),
                                     selected = "head")
                      )       
               ),
               
               column(8,
                      tableOutput(outputId= "contents")
               )
             ),
             br(),
             #Assign variable types to each field in the input dataset
             fluidRow(
               
               column(4,
                      wellPanel(
                        h3("Define targets and variable types"),
                        #Define attributes 
                        uiOutput('select_vartypes')
                        
                        
                      )       
               ),
               
               
               column(8,
                      tableOutput(outputId = "variable_types")
               )
             ),
             
             br(),
             ##Summarize and Clean data
             fluidRow(
               
               column(4,
                      wellPanel(
                        h3("Summarize and Clean Data"),
                        actionButton("summarizeData", "Summarize Data"),
                        radioButtons(inputId = "cleanDataYesorNo",label = "Clean Data?",
                                     choices = c("Yes","No"),selected = "No"),
                        conditionalPanel(condition = "input.cleanDataYesorNo=='Yes'",uiOutput('SelectVarsToClean'))
                        
                      )       
               ),
               
               
               column(8,
                      verbatimTextOutput(outputId = "DataSummary")
               )
             ),
             
             br(),
             
             ## Split data into train and validation sets
             fluidRow(
               
               column(4,
                      wellPanel(
                        h3("Split into Trains and validation Sets") ,
                        numericInput("seed","Enter seed",value=0),
                        sliderInput("TrainValidSplit"," Enter train-valid split",min=0.5,max=1,value=0.8),
                        actionButton("SplitData", "Split Data")
                      )       
               ),
               
               
               column(8,
                      #verbatimTextOutput(outputId = "trainvalidsummary")
                      plotlyOutput(outputId = "trainvalidsummarychart")
               )
             ),
             
             br(),
             
             ## Get variable importance
             fluidRow(
               
               column(4,
                      wellPanel(
                        h3("Get variable importance") ,
                        actionButton("RankVars", "Rank Variables")
                      )       
               ),
               
               
               column(8,
                      plotOutput(outputId = "VariableRank")
                      #verbatimTextOutput(outputId = "VariableRank")
                      
               )
             ),
             
             br(),
             
             
             
             # Visualize single variable in the data
             fluidRow(
               
               column(4,
                      wellPanel(
                        h3("Visualize Single Variable"),
                        uiOutput('select_plotvar')
                      )       
               ),
               
               
               column(8,
                      plotlyOutput(outputId = "distributionplot")
               )
             ),
             
             br(),
             # Visualize interaction between two variables in the data
             fluidRow(
               
               column(4,
                      wellPanel(
                        h3("Visualize Interactions"),
                        uiOutput('select_vars_interaction'),
                        uiOutput('customize_vars')
                      )       
               ),
               
               
               column(8,
                      plotlyOutput(outputId = "interactionplot")
                     #tableOutput(outputId = "interaction_plot_data")
               )
             ),
             
             br(),
             ##Create Rules and Present Results
             fluidRow(
               
               column(4,
                      wellPanel(
                        h3("Create Rules"),
                        #Set style
                        
                        fixedRow(
                          column(width=6,uiOutput("select_LHS1")),
                          column(width=2,uiOutput("select_Operator1")),
                          column(width=4,uiOutput("select_RHS1"))),
                        selectInput("AddClause1","",c("AND","OR","END"),selected="END"),
                        conditionalPanel(condition = "input.AddClause1=='AND' || input.AddClause1 == 'OR'",
                                         fixedRow(
                                           column(width=6,uiOutput("select_LHS2")),
                                           column(width=2,uiOutput("select_Operator2")),
                                           column(width=4,uiOutput("select_RHS2"))),
                                         selectInput("AddClause2","",c("AND","OR","END"),selected="END"),
                                         conditionalPanel(condition = "input.AddClause2=='AND' || input.AddClause2 == 'OR'",
                                                          fixedRow(
                                                            column(width=6,uiOutput("select_LHS3")),
                                                            column(width=2,uiOutput("select_Operator3")),
                                                            column(width=4,uiOutput("select_RHS3"))),
                                                          selectInput("AddClause3","",c("AND","OR","END"),selected="END"))),
                        actionButton("TestRule","Submit"),
                        actionButton("SaveRule","Save Rule")
                      )      
               ),
               
               
               column(8,
                      uiOutput(outputId = 'conf_matrix_header'),
                      br(),
                      verbatimTextOutput(outputId = "conf_matrix_results"),
                      uiOutput(outputId = 'rule_perf_header'),
                      br(),
                      verbatimTextOutput("rule_perf_metrics")
               )
             ),
             
             br(),
             ####Automated Rule Generation
             fluidRow(
               column(4,wellPanel(
                 h3("Auto generate Rules"),
                 numericInput("max_iterations","Maximum Number of Iterations",value =10),
                 sliderInput("max_fraud_captured","Target % of Fraud to be Captured",min=0.5,max=1,value=0.99),
                 sliderInput("alert_rate","Alert Rate",min=0,max=1,value=0.3),
                 actionButton("generate_rules","Generate Rules"),
                 actionButton("validate_rules","Validate Rules")
               )),
               
               column(8,
                      dataTableOutput(outputId = "auto_rules"))
               
               
               
             )
             
             
    ),
    ##Saved Rules
    tabPanel("Saved Rules",
             column(12,
                    DT::dataTableOutput(outputId = "TrainRules")
             ),
             column(6,
                    uiOutput(outputId = 'finalizerules'))
    ),
    ##Final Set of  Rules
    tabPanel("Final Rules",
             fluidRow(column(12,
                    uiOutput(outputId = 'manual_rules_header'),
                    DT::dataTableOutput(outputId = "FinalRules")
             )),
             fluidRow(column(12,
                    uiOutput(outputId = 'auto_rules_header'),
                    #verbatimTextOutput(outputId = "FinalAutoRulePerf")
                    DT::dataTableOutput(outputId = "FinalAutoRulePerf"),
                    uiOutput(outputId = 'auto_ruleset_header'),
                    #verbatimTextOutput(outputId = "FinalAutoRuleSetPerf")
                    DT::dataTableOutput(outputId = "FinalAutoRuleSetPerf")
             ))
    ),
    ##Instructions and Notes
    tabPanel("Read Me",
             column(12,
                    br(),
                    h3('Reading data'),
                    HTML(paste(" This application currently works only with CSV files.<br/>
                                 The input file should have a flag identifying fraud (should be 1) and
                                  Non fraud ( should be 0)")),
                    br(),
                    h3('Define targets and variable types'),
                    HTML(paste("Include only variables that are relevent to business rules.<br/>
                               The variable flagging fraud/non fraud should be tagged as <b>target</b>.<br/>
                               Any variable that is not a candidate for a business rule should 
                               be tagged as <b>character </b>.<br/>
                               Any variable that is continuous or numeric such as Trxn Amount
                               should be tagged as <b> numeric </b>.<br/>
                               Any variable that has classes or categories such as Sub Product
                               Code, Hour of the day or Country should be tagged as
                               <b> categorical</b> <br/>."
                                )),
                    br(),
                    h3("Summarize and Clean Data"),
                    HTML(paste("Use the Clean Data feature to replace missing values. Missing 
                               values for a field need to be replaced only if the values are
                               expected to be available in production.")),
                    br(),
                    h3("Split into Trains and validation Sets"),
                    HTML(paste("This can be used to create train and valdiation splits of
                               the data.Since this split is created randomly,the seed ensures it is reproducible.
                               Changing the seed will give a slightly different train-validation split.<br/>
                               The train split in the data is typically analyzed to create rules while the
                               rules are then validated on the validation set")),
                    br(),
                    h3("Get variable importance"),
                    HTML(paste("This ranks the predictors according to their
                               ability to identify fraud. This can be used if there are
                               a large number of variables to analyze.<br/>")),
                    br(),
                    h3("Visualize Single Variable"),
                    HTML(paste("Visualizations can be created only for numeric and categorical 
                               variables. A density plot is created for numeric variables
                               and a bar plot is created for categorical variables.")),
                    br(),
                    h3('Visualize Interactions'),
                    HTML(paste("Explore interactions between variables.<br/>
                               1) Continuous vs Categorical - How does the continuous variable vary for
                                  different levels of the categorical variable. E.g. Does Transaction Amount
                                  show different behavior for different sub product codes or merchants <br/>
                              2) Continuous vs Continuous - Is effectivenss higher at a particular range of values </br>
                              3) Categorical vs Categorical - Is effectiveness higher at particular combinations of
                                 levels </br>")),
                    br(),
                    h3('Create Rules'),
                    HTML(paste("This widget allows you to create rules which have up to 3 
                               conditions.<br/>
                               The results presented include a confusion matrix which desribeS 
                               the performance of the rule.<br/>
                               Additionaly , the following metrics are presented: <br/>
                               1. Precision: The accuracy of the rule i.e. what fraction of
                                  trxns predicted by the rule to be fraud was actually fraud<br/>
                               2. Recall: The efficacy of the rule i.e. What fraction of 
                                  fraudulent transactions in the data were captured by the rule <br/>
                               3. F Measure: A metric that captures both the precision and
                                  recall of a rule. Varies from 0 (worst) to 1 (best)<br/>
                               4. Predicted Fraud: The number of trxns identified as fraud by the rule.<br/>
                               5. Actual Fraud: The no of trxns that were truly fraudulent among those
                                  identified as fraudulent by the rule.")),
                    h3('Auto Generate Rules'),
                    HTML(paste("This module automatically recommends an optimal set of business rules
                                subject to the following user defined constraints: <br/>
                               1.<b>Maximum Number of Iterations</b>: This is a proxy for your time budget.If this
                                 is set to a high value, then a larger number of fraud will be captured but
                                 the computation will take longer.<br/>
                               2.<b>Target % of Fraud to be Captured</b>: This indicates what % of fraud
                                    in the data you want to capture. This can initially be set to a high 
                                    value.<br/>
                               3.<b>Alert Rate</b>: This denotes what % of the transactions in the data are you 
                                    willing to create alerts on. If operational limitations allow for only a
                                    small volume of alerts, this can be set to a low value.<br/>")),
                    br(),
                    br(),
                    HTML(paste("<b> Please report any bugs to: govind_g_nair@keybank.com </b>")
                         )
                    
                    )
            )
    
  )
)



# Define server logic to read selected file ----
server <- function(input, output,session) {
  ###utility functions for automated rule generation###
  source('ABRG_utilitiesv3.R',local=TRUE)
  ######Function  to generate decision trees that collectively capture all fraud ######
  
  ###Inputs:
  #train_data: training data set
  #Valid_data: validation data set
  #weights: observation weights
  #max_iterations: maximum iterations to carry before stopping
  #max_fraud_captured# What fraction of total fraud are we aiming to capture
  
  ###Outputs:
  #trees : List of trees that capture the fraud in the dataset
  #thresholds: Vector of optimal thresholds corresponding to each tree
  #perc_fraud_captured: Cumulative perc fraud captured by each tree when applying optimal thresholds
  #No_predicted_fraud: Cumulative No of predicted fraud by each tree
  #features_used : Cumulative set of features used across all trees/rules
  generate_trees <- function(train_data,predictors,target,valid_data,max_iterations,
                             max_fraud_captured,updateProgress=NULL){
    
    train_fraud <- train_data %>% select(one_of(target))%>%pull()
    valid_fraud <- valid_data %>% select(one_of(target))%>%pull()
    final_vars <- predictors
    
    perc_fraud_captured <-0
    max_fraud <- sum(train_fraud==1)
    actual <- as.numeric(as.character(train_fraud))
    predicted_fraud_list <-list()
    thresholds <- list()
    trees <- list()
    vars_list <- list()
    features_list <- list()
    nr <- nrow(train_data) #No of observations
    nvars <- ncol(train_data) - 1 # No of variables in data excluding fraud flag
    weights <- rep(1,nr)/nr
    
    perc_captured <- list()
    No_predicted_fraud <- list()
    
    i = 1
    ##Select features based on variable importance in Random Forest #
    if(nvars > 10){
      final_vars <- get_important_vars2(train_data,predictors,target,n_vars_selected=10, weights) 
      train_data_rf <- train_data %>% select(one_of(c(final_vars,target))) 
    } else {train_data_rf <- train_data %>% select(one_of(c(final_vars,target)))  }
    
    while(perc_fraud_captured<= max_fraud_captured & i <= max_iterations){
      ###############This is unrelated to actual function###########
      # If we were passed a progress update function, call it
      if (is.function(updateProgress)) {
        text <- paste0("Doing Iteration:",i)
        updateProgress(detail = text)
      }
      ##############################################################
      
      results1 <- get_best_rules(train_data_rf,predictors =final_vars,target =target,valid_data,weights)
      
      trees[[i]] <- results1$best_trees
      thresholds[i] <- results1$thresholds
      features_list[[i]] <- attr(trees[[i]]$terms,'term.labels')
      train_pred_prob <- predict(trees[[i]],class='prob')
      train_preds <- ifelse(train_pred_prob[,'1'] >= thresholds[i],1,0)
      predicted_fraud_list[[i]] <- train_preds
      ###Predictions from each decision tree so far
      preds_df <- data.frame(predicted_fraud_list)
      ###Prediction from all trees combined
      temp <- apply(preds_df,1,sum)
      preds_all <- ifelse(temp>0,1,0)
      check <- actual*preds_all ###Identify all true positives###
      perc_fraud_captured <- sum(check)/max_fraud
      perc_captured[i] <- perc_fraud_captured
      No_fraud_predicted <- sum(preds_all)
      No_predicted_fraud[i] <- No_fraud_predicted
      
      # Update weights to focus on fraud previously not captured
      weights <- update_weights(actual,preds_all,weights)
      vars <- get_important_vars2(train_data,predictors,target,n_vars_selected=10,weights)
      train_data_rf <- train_data %>% select(vars,target)
      
      i = i+ 1
    }
    
    thresholds <- unlist(thresholds)  
    perc_captured <- unlist(perc_captured) 
    No_predicted_fraud <- unlist(No_predicted_fraud)  
    features_used <- unique(unlist(features_list))
    
    return(list('trees'=trees,'thresholds'=thresholds,'perc_fraud_captured'=perc_captured,
                'No_predicted_fraud'=No_predicted_fraud,'features_used'=features_used,'predictions'=preds_df))
  }
  
  ################Utility function#########################
  #Function to impute missing values - Replaces missing values by resampling available values
  #for fraud and non-fraud separately
  impute_missing <- function(var,target){
    ##Identify missing variables that are non fraud(0) and fraud(1)
    target_0 <-  var[target==0]
    target_1 <-  var[target==1]
    ##Identify missing observations
    miss_0 <- (is.na(var)& target==0)
    miss_1 <- (is.na(var)& target==1)
    ##Identify missing observations
    non_miss_0 <- (!is.na(var)& target==0)
    non_miss_1 <- (!is.na(var)& target==1)
    
    ##Identify no of missing observations
    Nmiss_0 <- sum(miss_0)
    Nmiss_1 <- sum(miss_1)
    ##Replace missing observation by resampling non missing observations
    var[miss_0] <- sample(var[non_miss_0],Nmiss_0,replace=TRUE)
    var[miss_1] <- sample(var[non_miss_1],Nmiss_1,replace=TRUE)
    return(var)
  }
  
  #To evaluate logical expressions passed in as strings
  parse_fn<- function(x) eval(parse(text=x))
  #To evaluate absence of elements in a list
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  
  
  ##Function to generate perfromance metrics
  precall <- function(actual,predicted){
    
    actual <- factor(actual,levels=c("0","1"))
    preds <- factor(predicted,levels=c("0","1"))
    
    tab <- as.matrix(table(actual,preds))
    
    predicted_fraud <- tab[1,2] + tab[2,2]
    precision <-  tab[2,2]/ predicted_fraud 
    recall <- tab[2,2]/(tab[2,1]+tab[2,2])
    identified_fraud <- round(tab[2,2],1)
    
    f <-  2*precision*recall/(precision+recall)
    if (is.na(f)| is.nan(f)) f.measure <- 0
    else f.measure <- f
    
    
    results <- c(formattable(precision,digits=2,format='f'),
                 formattable(recall,digits=2,format='f'),
                 formattable(f.measure,digits=2,format='f'),
                 predicted_fraud,
                 identified_fraud)
    
    names(results) <- c('precision','recall','Fmeasure','predicted_fraud',' Identified Fraud')
    
    return(results)
  }
  
  ###Function to recommend the most  important variables ###
  get_important_vars<- function(data,predictors,target){
    
     y_vec <- data %>% select(one_of(target))%>%pull()
     X_df <- data %>% select(one_of(predictors))
     tryCatch({
        rf <- randomForest(y=y_vec,x=X_df,importance =TRUE)
       #Use accuracy rank as it is more stable than gini rank
       imp_df <- data.frame(rf$importance)
       imp_df$Variable <- rownames(imp_df)
       
       p <- ggplot(imp_df,aes(x=reorder(Variable,MeanDecreaseAccuracy),y=MeanDecreaseAccuracy)) +
         geom_bar(stat='identity',fill='cornflowerblue') +coord_flip()+
         labs(x='Variables')
       
       return(p)
       
       
       
       },
                    error = function(e) {return("")})
    
    
  }
  

  
  ##################################################################
  
  ##################Reactive value to hold most recent data ########
  final_data <- reactiveValues(data=NULL)
  train_data <- reactiveValues(data=NULL)
  valid_data <- reactiveValues(data=NULL)
  ########################################################################
  
  ###read in data
  
    input_df <- reactive({
      req(input$file1$datapath)
      read.csv(input$file1$datapath,
               header = TRUE,na.strings="",
               sep = ",")
  })
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    
    if(input$disp == "head") {
      return(head(input_df()))
    }
    else {
      return(input_df())
    }
    
  })
  
  #Get all column headers to determine target variable,continuous and categorical variables
  
  column_names <- reactive({
    colnames(input_df())
  })
  
  ##Select buttons to select variable type for each variable
  output$select_vartypes <- renderUI({
    div(
    lapply(column_names(),function(var_n){
      selectInput(inputId = paste0(var_n,"_type"),label = var_n,choices=list('continuous','categorical',
                                                                             'character','target'))
    }),
    actionButton("submitValues", "Submit")
    )
  })
  

  
  ##Get information about the input dataset including desired variable types, data,target and predictor variables
  data_info <- eventReactive(input$submitValues,{
    ncols <- length(column_names())
    values <<- rep(NA,ncols)
    # get the values of each selectInput and store them in "values"
    for(i in c(1:ncols)) { 
      inputName <- paste0(column_names()[i],"_type")
      # only get a value if the  input exists
      if (!is.null(inputName))
        values[[i]] <<- input[[inputName]]
    }
    
    
    # Convert columns into appropriate formats
    numeric_index <- (values == 'continuous')
    categorical_index <- (values== 'categorical')| (values=='target')
    character_index <- (values== 'character')
    target_index <- (values== 'target')
    #Create copy of input data set
    input_df2 <- input_df()
    #Identify target variable
    target_variable <-column_names()[target_index]
    predictor_variables <- base::setdiff(column_names()[numeric_index|categorical_index],target_variable)
    
    #Convert columns to appropriate formats
    input_df2[numeric_index] <- lapply(input_df2[numeric_index],as.numeric)
    input_df2[categorical_index] <- lapply(input_df2[categorical_index],as.factor)
    input_df2[character_index] <- lapply(input_df2[character_index],as.character)
    #Update final dataset
    final_data$data <- input_df2
    list(vartypes=values,target=target_variable,predictors = predictor_variables)
  })
  
  

  
  #Output variable type of updated table
  var_types <- reactive(sapply(final_data$data,class))

  
  ##get variable types of varios fields in input data
  observeEvent(input$submitValues,
               output$variable_types <- renderTable(
                 if('target' %in% values){data.frame(variable = colnames(final_data$data),
                                                               type = var_types())}else{
                              data.frame("Error!" ='You have not selected a target variable') 
                                                               }))
  
  
  
  #Output summary of data
  observeEvent(input$summarizeData,{
    output$DataSummary <- renderPrint(print(summary(final_data$data)))
    
    
  })
  
  #Get variables to be cleaned
  output$SelectVarsToClean <- renderUI(
    div(selectizeInput(inputId="clean_var",label = 'Select Variables to Clean',choices = data_info()[['predictors']],multiple=TRUE),
        actionButton("CleanSelectedVars", "Impute Missing Values")
    )
  )
  

  ##Update final data by replacing missing values if Clean Variable Button is pressed
  
  observeEvent(input$CleanSelectedVars,{
    newdata <- final_data$data
    ##Identify variables for imputation
    target_var <- data_info()[['target']]
    
    missing_data <- reactive(newdata %>% select(one_of(input$clean_var)))
    non_missing_data <- reactive(newdata %>% select(-one_of(input$clean_var)))
    
    fraud <- newdata %>% select(target_var) %>% pull()
    
    complete_data <- reactive(data.frame(lapply(missing_data(),impute_missing,target=fraud)))
    final_data$data <- cbind(non_missing_data(),complete_data())
    
  }
  )
  

  
  # Split into train and valid sets
  
  observeEvent(input$SplitData,{
    full_data <-  final_data$data
    nr <- nrow(full_data)
    #Index for training observations
    set.seed(input$seed)
    train_index <- sample(c(1:nr),nr*input$TrainValidSplit)
    
    #training  and validation data for outgoing trxns
    train_data$data <- full_data[train_index,]
    valid_data$data <- full_data[-train_index,]
    
    target_var <- data_info()[['target']]
    
    #Get summaries
    t1 <- table(train_data$data %>% select(target_var) %>% pull())
    t2 <- table(valid_data$data %>% select(target_var) %>% pull())
    
    train_test_summary <- data.frame('N'=c(as.vector(t1),as.vector(t2)))
    train_test_summary$fraud_flag <-  as.factor(rep(c(0,1),2))
    train_test_summary$partition <- rep(c('Train','Valid'),each=2)
    train_test_summary <- train_test_summary %>%
                            group_by(partition)%>%
                               mutate(perc = paste0(round(N/sum(N)*100,1),'%'))
    output$trainvalidsummarychart <- renderPlotly({
      ggplotly(
      ggplot(train_test_summary,aes(x=partition,y=N,fill=fraud_flag)) + geom_bar(stat='identity',position = 'dodge')+
        geom_text(aes(label=perc),position=position_dodge(width=1),vjust=-0.25)+
        scale_fill_manual(name="",labels=c('Non Fraud','Fraud'),values=c("#00BFC4","#F8766D"))+
        labs(title="Train Validation Split",y="Volume",x=" ")+
        theme(plot.title=element_text(hjust=0.5))
      )
      })
    
    #output$trainvalidsummary <- renderPrint({summary(train_data$data)})
  })
  
  
  ###Rank variables by importance
  observeEvent(input$RankVars,{
    p <- get_important_vars(train_data$data,data_info()[['predictors']],
                            data_info()[['target']]) 
    
    
     output$VariableRank <- renderPlot({
       validate(
         need( p != "", "The data has missing values,NaNs or infinite values that should be replaced")
       )
      
       p
       })
   
  })
  
  
  
  
  
  
  #Get variable to be plotted
  output$select_plotvar <- renderUI(
    div(selectizeInput(inputId="plot_var",label = 'Select Variable to plot',choices = data_info()[['predictors']]),
        actionButton("GenerateChart", "Generate Chart")
    )
  )
  
  
  #Get varibles whose interaction is to be studied
  output$select_vars_interaction <- renderUI(
    
        if (is.null(data_info())|is.na(data_info())){
          
        }else{
        div(
        selectizeInput(inputId = "plot_var1", label = "Select variable 1", choices = data_info()[['predictors']]),
        selectizeInput(inputId = "plot_var2", label = "Select variable 2", choices = data_info()[['predictors']])
        )
        }
        
        
        
  )
  
  
  ##Get right choices to be displayed for the variables to be studied
  
choices <- eventReactive(c(input$plot_var1,input$plot_var2),{
    req(input$plot_var1,input$plot_var2)

    if(var_types()[input$plot_var1] == 'numeric' & var_types()[input$plot_var2] == 'factor'){
      
       final_data$data %>% select(input$plot_var2)%>%unique() %>% pull()
    
    } else if (var_types()[input$plot_var1] == 'factor' & var_types()[input$plot_var2] == 'numeric'){
      
       final_data$data %>% select(input$plot_var1)%>%unique() %>% pull()
      
    }
    
  })
  
  
  ##Add widget to allow customization of chart
  
  output$customize_vars <- renderUI(
    if (is.null(data_info())|is.na(data_info())){
    }else{
    div(
    if((var_types()[input$plot_var1] == 'numeric' & var_types()[input$plot_var2] == 'factor')|
       (var_types()[input$plot_var1] == 'factor' & var_types()[input$plot_var2] == 'numeric')){
      
      
      selectizeInput(inputId = 'categoryselector',label = 'Subset categories',choices = choices(),multiple=TRUE)
      
      
    } else if (var_types()[input$plot_var1] == 'numeric' & var_types()[input$plot_var2] == 'numeric'){
      
      
      numericInput(inputId = 'cutselector',label = 'Select Number of Cuts',value =10)
      
    },
    
    actionButton("GenerateInteractionChart","Generate Chart"))}

        
  )
  
  
  

  
  #Prepare data for plotting
  
  plot_data <- eventReactive(input$GenerateChart,{
      target_var <- data_info()[['target']]
    if(var_types()[input$plot_var] == 'factor' ){
      x <- train_data$data %>% select(input$plot_var) %>% pull()
      y <- train_data$data %>% select(target_var) %>% pull()
      flag_ct <- CrossTable(x,y,prop.t=FALSE,prop.chisq=FALSE)
      flag_df <- data.frame(flag_ct$prop.col)

    }

    else{


       flag_df <- train_data$data %>% select(input$plot_var,target_var)
       names(flag_df) <- c('x','y')
      }
      flag_df
                        
        }
                             )
  
 
  #Plot the data
  observeEvent(input$GenerateChart,{
    
    if(var_types()[input$plot_var] == 'factor' ){
      p <- ggplot(plot_data(),aes(x=x,y=Freq,fill=y))+geom_bar(position="dodge",stat="identity")+
           labs(list(title= paste0("Distribution over ",input$plot_var) , x=input$plot_var,y="Percent of Total"))+
           scale_y_continuous(labels=percent)+ 
           scale_fill_manual(values=c("#00BFC4","#F8766D"),labels=c('Non Fraud','Fraud'),name="")
      output$distributionplot <- renderPlotly({ggplotly(p)})
    } else {
      target_var <- reactive(data_info()[['target']])
      p <- ggplot(plot_data(),aes(x= x,fill= y))+geom_density(alpha=0.3)+
           ggtitle(paste0("Distribution over ",input$plot_var)) +
           scale_fill_manual(values=c("#00BFC4","#F8766D"),labels=c('Non Fraud','Fraud'),name="")+
           xlab(input$plot_var) 
      output$distributionplot <- renderPlotly({ggplotly(p)})
      
    }
    
  })
  
  #Prepare data for interactive plotting
  plot_data_interact <- eventReactive(input$GenerateInteractionChart,{
    
    shiny::validate(
      need(input$plot_var1 != input$plot_var2, "Please select two different variables")
    )
    
    target_var <- data_info()[['target']]
    var1_name <- input$plot_var1
    var2_name <- input$plot_var2
    
    if(var_types()[input$plot_var1] == 'numeric' & var_types()[input$plot_var2] == 'numeric') {
      plot_df <- train_data$data %>% 
        select(input$plot_var1,input$plot_var2,target_var)
      
      
      ##Discretize into bins
      plot_df$var1 <- cut(plot_df[,1],input$cutselector,dig.lab=6)
      plot_df$var2 <- cut(plot_df[,2],input$cutselector,dig.lab=6)
      
      #Get no of alerts
      plot_df_alerts <- plot_df %>%
        select(var1,var2)%>%
        group_by(var1,var2)%>%
        summarize(alerts=n())
      
      ####Get no of eff alerts####
      #Create condition for standard evaluation
      l <- paste(target_var,"==","'1'")
      
      plot_df_effalerts <- plot_df %>%
        select(var1,var2,target_var)%>%
        filter_(l) %>%
        group_by(var1,var2)%>%
        summarize(eff_alerts=n())
      
      plot_df2 <- left_join(plot_df_alerts,plot_df_effalerts)
      #Get effectiveness
      plot_df2$effectiveness <- plot_df2$eff_alerts/plot_df2$alerts
      #Rename dataframe columns 
      colnames(plot_df2)[1:2] <- c(input$plot_var1,input$plot_var2)
      plot_df2[is.na(plot_df2)] <- 0
      
      
    } else if(var_types()[input$plot_var1] == 'factor' & var_types()[input$plot_var2] == 'factor' )  {
      plot_df <- train_data$data %>% select(input$plot_var1,input$plot_var2,target_var)
      
      #Get no of alerts
      plot_df_alerts <- plot_df %>%
        group_by_(input$plot_var1,input$plot_var2)%>%
        summarize(alerts=n())
      ######Get No of effective alerts in each bucket######
      #Create condition for standard evaluation
      l <- paste(target_var,"==","'1'")
      plot_df_effalerts <- plot_df %>%
        filter_(l) %>%
        group_by_(input$plot_var1,input$plot_var2)%>%
        summarize(eff_alerts=n())
      
      plot_df2 <- left_join(plot_df_alerts,plot_df_effalerts)
      
      #Get effectiveness
      plot_df2$effectiveness <- plot_df2$eff_alerts/plot_df2$alerts
      plot_df2[is.na(plot_df2)] <- 0
      
    } else if(var_types()[input$plot_var1] == 'factor' & var_types()[input$plot_var2] == 'numeric' ) {
      plot_df <- train_data$data %>% 
        select(input$plot_var1,input$plot_var2,target_var)
      
      
      ##If nothing is included in the filter include full data
      if(!isTruthy(input$categoryselector)){plot_df2 <- plot_df 
      }else {
        plot_df2 <- plot_df[Reduce(`|`, Map(`==`, plot_df[input$plot_var1],input$categoryselector)),]
        plot_df2 <- na.omit(plot_df2)
        
      }
      colnames(plot_df2)[3] <- 'fraud_flag'
      
    }else{
      plot_df <- train_data$data %>% 
        select(input$plot_var1,input$plot_var2,target_var)
      
      
      
      ##If nothing is included in the filter include full data
      if(!isTruthy(input$categoryselector)){plot_df2 <- plot_df 
      } else{
        
        plot_df2 <- plot_df[Reduce(`|`, Map(`==`, plot_df[input$plot_var2],input$categoryselector)),]
        plot_df2 <- na.omit(plot_df2)
        
      }
      colnames(plot_df2)[3] <- 'fraud_flag'
    }
    
    plot_df2
  }
  )
  
  
  ##Print sample of interaction data to see if it is being created in right form
  
  # observeEvent(input$GenerateInteractionChart,
  #               {
  #                 output$interaction_plot_data <- renderTable({plot_data_interact()})
  #               })

  
  
  #Plot interaction data
  observeEvent(input$GenerateInteractionChart,{

    if(var_types()[input$plot_var1] == 'numeric' & var_types()[input$plot_var2] == 'numeric') {


      p <- ggplot(plot_data_interact(),aes_string(x = input$plot_var1,y = input$plot_var2,z='effectiveness')) +
            geom_tile(aes(fill=effectiveness))+
             labs(list(title= paste0("Distribution over ",input$plot_var1, " & ",input$plot_var2) , x=input$plot_var1,y=input$plot_var2))+
               scale_fill_gradient(low='seagreen',high='firebrick')+
                  theme(axis.text.x = element_text(angle=90,size=5),
                        axis.text.y = element_text(angle=0,size=5),
                        axis.ticks.x = element_blank(),
                        axis.ticks.y = element_blank())
      output$interactionplot <- renderPlotly({
        shiny::validate(
          need(input$plot_var1 != input$plot_var2, "Please select two different variables")
        )
        
        ggplotly(p)})

    } else if(var_types()[input$plot_var1] == 'factor' & var_types()[input$plot_var2] == 'numeric'){

      target_var <- reactive(data_info()[['target']])
      plotdata <- plot_data_interact()

      p <- ggplot(plotdata,aes_string(x = input$plot_var2,fill= 'fraud_flag'))+geom_density(alpha=0.3)+
        ggtitle(paste0("Distribution over ",input$plot_var2)) +
        scale_fill_manual(values=c("#00BFC4","#F8766D"),labels=c('Non Fraud','Fraud'),name="")+
        xlab(input$plot_var2) # + facet_wrap(input$plot_var1,ncol=1)
      output$interactionplot <- renderPlotly({ggplotly(p)})


    } else if (var_types()[input$plot_var1] == 'numeric' & var_types()[input$plot_var2] == 'factor' ){

      target_var <- reactive(data_info()[['target']])
      plotdata <- plot_data_interact()

      p <- ggplot(plotdata,aes_string(input$plot_var1 ,fill= 'fraud_flag'))+geom_density(alpha=0.3)+
        ggtitle(paste0("Distribution over ",input$plot_var1)) +
        scale_fill_manual(values=c("#00BFC4","#F8766D"),labels=c('Non Fraud','Fraud'),name="")+
        xlab(input$plot_var1) #  + facet_wrap(input$plot_var2,ncol=1)

      output$interactionplot <- renderPlotly({ggplotly(p)})


    } else{

      p <- ggplot(plot_data_interact(),aes_string(x = input$plot_var1,y = input$plot_var2 ,z='effectiveness')) +
        geom_tile(aes(fill=effectiveness))+
        labs(list(title= paste0("Distribution over ",input$plot_var1, " & ",input$plot_var2) , x=input$plot_var1,y=input$plot_var2))+
        scale_fill_gradient(low='seagreen',high='firebrick')

      output$interactionplot <- renderPlotly({
        shiny::validate(
          need(input$plot_var1 != input$plot_var2, "Please select two different variables")
        )
        ggplotly(p)})
    }



  })
  
  
  
  
  
  #Input rules
  output$select_LHS1<- renderUI({
    selectizeInput(inputId="LHS1",label = '',choices = data_info()[['predictors']])
    })
  
  output$select_LHS2<- renderUI({
    selectizeInput(inputId="LHS2",label = '',choices = data_info()[['predictors']])
  })
  
  output$select_LHS3<- renderUI({
    selectizeInput(inputId="LHS3",label = '',choices = data_info()[['predictors']])
  })
  
  
  
  #Identify operators  based on input
  operators1 <- reactive({
    req(input$LHS1)
    if((var_types()[input$LHS1] == 'factor')){c('in','not in') }
    else { c('>','<','==','>=','<=','!=')}
    
  })
  
  operators2 <- reactive({
    req(input$LHS2)
    if((var_types()[input$LHS2] == 'factor')){c('in','not in') }
    else { c('>','<','==','>=','<=','!=')}
    
  })
  
  operators3 <- reactive({
    req(input$LHS3)
    if((var_types()[input$LHS3] == 'factor')){c('in','not in') }
    else { c('>','<','==','>=','<=','!=')}
    
  })
  
  #Identify RHS based on input
  
  
  rhsvalues1 <- reactive({
    req(input$LHS1)
    if((var_types()[input$LHS1] == 'factor')){
      final_data$data %>% select(input$LHS1)%>%unique() %>% pull()
      
    }
    else { }
    
  })
  
  rhsvalues2 <- reactive({
    req(input$LHS2)
    if((var_types()[input$LHS2] == 'factor')){
      final_data$data %>% select(input$LHS2)%>%unique() %>% pull()
      
    }
    else { }
    
  })
  
  rhsvalues3 <- reactive({
    req(input$LHS3)
    if((var_types()[input$LHS3] == 'factor')){
      final_data$data %>% select(input$LHS3)%>%unique() %>% pull()
      
    }
    else { }
    
  })
  
  
  #######Select operators for each clause########
  
  output$select_Operator1<- renderUI({
    
    selectizeInput(inputId="Operator1",label = '',choices = operators1())
  })
  
  output$select_Operator2<- renderUI({
    
    selectizeInput(inputId="Operator2",label = '',choices = operators2())
  })
  
  output$select_Operator3<- renderUI({
    
    selectizeInput(inputId="Operator3",label = '',choices = operators3())
  })
  
  #######Select RHS #########
  
  output$select_RHS1 <- renderUI({
      req(input$LHS1)
    if((var_types()[input$LHS1] == 'factor')){
      selectizeInput(inputId="RHS1",label='',choices=rhsvalues1(),multiple=TRUE)
      
    }
    else {
      numericInput(inputId = "RHS1",label="",value=0)
    }
  })
  
  
  output$select_RHS2 <- renderUI({
    req(input$LHS2)
    if((var_types()[input$LHS2] == 'factor')){
      selectizeInput(inputId="RHS2",label='',choices=rhsvalues2(),multiple=TRUE)
      
    }
    else {
      numericInput(inputId = "RHS2",label="",value=0)
    }
  })
  
  output$select_RHS3 <- renderUI({
    req(input$LHS3)
    if((var_types()[input$LHS3] == 'factor')){
      selectizeInput(inputId="RHS3",label='',choices=rhsvalues3(),multiple=TRUE)
      
    }
    else {
      numericInput(inputId = "RHS3",label="",value=0)
    }
  })
  
    performance_metrics <- eventReactive(input$TestRule,{
  
    
    ##############Evaluate on train data############
    #Part 1 of the rule
    train_rule_p1 <- reactive({
      req(input$LHS1,input$Operator1,input$RHS1)
      lhs1 <- train_data$data %>% select(input$LHS1) %>% pull()        
      
      if (input$Operator1 == 'in'){
        op1 <- '%in%' 
        paste0("\"",lhs1,"\"",op1,paste0("c(","\"",paste(input$RHS1,collapse="\",\""),"\")")) 
      } else if (input$Operator1 == 'not in'){
        op1 <- '%!in%'
        paste0("\"",lhs1,"\"",op1,paste0("c(","\"",paste(input$RHS1,collapse="\",\""),"\")")) 
      }else {
        op1 <- input$Operator1
        paste0(lhs1,op1,input$RHS1)
      } 
      
    })
    
    #Part 2 of the rule
    train_rule_p2 <- reactive({
      req(input$LHS2,input$Operator2,input$RHS2)
      
      add_clause1 <- if(input$AddClause1=="AND"){'&'} else {'|'}
    
      
      lhs2 <- train_data$data %>% select(input$LHS2) %>% pull()        
      
      if (input$Operator2 == 'in'){
        op2 <- '%in%' 
        paste0(add_clause1,"\"",lhs2,"\"",op2,paste0("c(","\"",paste(input$RHS2,collapse="\",\""),"\")")) 
      } else if (input$Operator2 == 'not in'){
        op2 <- '%!in%'
        paste0(add_clause1,"\"",lhs2,"\"",op2,paste0("c(","\"",paste(input$RHS2,collapse="\",\""),"\")")) 
      }else {
        op2 <- input$Operator2
        paste0(add_clause1,lhs2,op2,input$RHS2)
      }
      
      
      
      
    })
    
    #Part 3 of the rule
    train_rule_p3 <- reactive({
      req(input$LHS3,input$Operator3,input$RHS3)
      
      add_clause2 <- if(input$AddClause2=="AND"){'&'} else {'|'}
    
      
      lhs3 <- train_data$data %>% select(input$LHS3) %>% pull()        
      
      if (input$Operator3 == 'in'){
        op3 <- '%in%' 
        paste0(add_clause2,"\"",lhs3,"\"",op3,paste0("c(","\"",paste(input$RHS3,collapse="\",\""),"\")")) 
      } else if (input$Operator3 == 'not in'){
        op3 <- '%!in%'
        paste0(add_clause2,"\"",lhs3,"\"",op3,paste0("c(","\"",paste(input$RHS3,collapse="\",\""),"\")")) 
      }else {
        op3 <- input$Operator3
        paste0(add_clause2,lhs3,op3,input$RHS3)
      }
      
      
      
    })
    
    
    ##############Evaluate on   valid data############
    #Part 1 of the rule
    valid_rule_p1 <- reactive({
      req(input$LHS1,input$Operator1,input$RHS1)
      lhs1 <- valid_data$data %>% select(input$LHS1) %>% pull()        
      
      if (input$Operator1 == 'in'){
        op1 <- '%in%' 
        paste0("\"",lhs1,"\"",op1,paste0("c(","\"",paste(input$RHS1,collapse="\",\""),"\")")) 
      } else if (input$Operator1 == 'not in'){
        op1 <- '%!in%'
        paste0("\"",lhs1,"\"",op1,paste0("c(","\"",paste(input$RHS1,collapse="\",\""),"\")")) 
      }else {
        op1 <- input$Operator1
        paste0(lhs1,op1,input$RHS1)
      } 
      
    })
    
    #Part 2 of the rule
    valid_rule_p2 <- reactive({
      req(input$LHS2,input$Operator2,input$RHS2)
      
      add_clause1 <- if(input$AddClause1=="AND"){'&'} else {'|'}
      
      
      lhs2 <- valid_data$data %>% select(input$LHS2) %>% pull()        
      
      if (input$Operator2 == 'in'){
        op2 <- '%in%' 
        paste0(add_clause1,"\"",lhs2,"\"",op2,paste0("c(","\"",paste(input$RHS2,collapse="\",\""),"\")")) 
      } else if (input$Operator2 == 'not in'){
        op2 <- '%!in%'
        paste0(add_clause1,"\"",lhs2,"\"",op2,paste0("c(","\"",paste(input$RHS2,collapse="\",\""),"\")")) 
      }else {
        op2 <- input$Operator2
        paste0(add_clause1,lhs2,op2,input$RHS2)
      }
      
      
      
      
    })
    
    #Part 3 of the rule
    valid_rule_p3 <- reactive({
      req(input$LHS3,input$Operator3,input$RHS3)
      
      add_clause2 <- if(input$AddClause2=="AND"){'&'} else {'|'}
      
      
      lhs3 <- valid_data$data %>% select(input$LHS3) %>% pull()        
      
      if (input$Operator3 == 'in'){
        op3 <- '%in%' 
        paste0(add_clause2,"\"",lhs3,"\"",op3,paste0("c(","\"",paste(input$RHS3,collapse="\",\""),"\")")) 
      } else if (input$Operator3 == 'not in'){
        op3 <- '%!in%'
        paste0(add_clause2,"\"",lhs3,"\"",op3,paste0("c(","\"",paste(input$RHS3,collapse="\",\""),"\")")) 
      }else {
        op3 <- input$Operator3
        paste0(add_clause2,lhs3,op3,input$RHS3)
      }
      
      
      
    })
    
    ###Create rule for varying number of clauses:1,2 or 3
    train_rule <- reactive({
      if(input$AddClause1 == "END"){ train_rule_p1()} 
      else if (input$AddClause2 == "END") {paste0(train_rule_p1(),train_rule_p2())}
      else {paste0(train_rule_p1(),train_rule_p2(),train_rule_p3())}
    })
    
    valid_rule <- reactive({
      if(input$AddClause1 == "END"){ valid_rule_p1()} 
      else if (input$AddClause2 == "END") {paste0(valid_rule_p1(),valid_rule_p2())}
      else {paste0(valid_rule_p1(),valid_rule_p2(),valid_rule_p3())}
    })
    
    
    #Identifying target fraud
    target_var <- data_info()[['target']]
    train_actual <- train_data$data %>% select(target_var) %>% pull()
    valid_actual <- valid_data$data %>% select(target_var) %>% pull()
    #Predictions
    train_preds <- as.numeric(sapply(train_rule(),parse_fn))
    valid_preds <- as.numeric(sapply(valid_rule(),parse_fn))
    #Performance matrix
    train_conf_matrix <- table(train_actual,train_preds)
    train_perf_metrics <- precall(train_actual,train_preds)
    #valid_conf_matrix <- table(valid_actual,valid_preds)
    valid_perf_metrics <- precall(valid_actual,valid_preds)
    list(train_conf_matrix=train_conf_matrix,train_perf_metrics=train_perf_metrics,
         valid_perf_metrics = valid_perf_metrics )
    
 }) # End of eventReactive starting in #1016
 
    
    ##Output confusion matrix for submitted rule
    output$conf_matrix_results <- renderPrint({
      print(performance_metrics()[['train_conf_matrix']])
    })
    ##output rule performance for submitted rule
    output$rule_perf_metrics <- renderPrint({
      print(performance_metrics()[['train_perf_metrics']])})
    
    ##Display headers for above two reesults
    observeEvent(input$TestRule,{
                 output$conf_matrix_header <- renderUI({HTML("<b> Confusion Matrix </b> <br/>")})
                 output$rule_perf_header <- renderUI({HTML("<b> Rule Performance Metrics </b> <br/>")})
                 })
    
    
    
 #Create a human readable rule
 readable_rule <- eventReactive(input$SaveRule,{
   if(input$AddClause1 == "END"){ paste0(input$LHS1," ",input$Operator1," ",paste(input$RHS1,collapse=","))}
   else if (input$AddClause2 == "END") {paste0(input$LHS1," ",input$Operator1," ",paste(input$RHS1,collapse=","),
                                               " ",input$AddClause1," ",
                                               input$LHS2," ",input$Operator2," ",paste(input$RHS2,collapse=","))}
   else {paste0(input$LHS1," ",input$Operator1," ",paste(input$RHS1,collapse=","),
                " ",input$AddClause1," ",
                input$LHS2," ",input$Operator2," ",paste(input$RHS2,collapse=","),
                " ",input$AddClause2," ",
                input$LHS3," ",input$Operator3," ",paste(input$RHS3,collapse=","))}
 })
 

    
   #Create a reactive dataframe to hold all saved rules
   savedrules_train <-  reactiveValues(df = data.frame('Rule' = character(0),'precision' = numeric(0),
                                                 'recall' =numeric(0),'F Measure' = numeric(0),
                                                  'No Predicted Fraud' = numeric(0),
                                                  'No Identified Fraud' = numeric(0)))
   
   
   savedrules_valid <-  reactiveValues(df = data.frame('Rule' = character(0),'precision' = numeric(0),
                                                      'recall' =numeric(0),'F Measure' = numeric(0),
                                                      'No Predicted Fraud' = numeric(0),
                                                      'No Identified Fraud' = numeric(0)))
   
 
  #update dataframe when a new rules is saved
   observeEvent(input$SaveRule,{
     
              newrule_train <- cbind(data.frame(readable_rule()),
                               data.frame(matrix(performance_metrics()[['train_perf_metrics']],nr=1)))
              
              savedrules_train$df<- rbind(savedrules_train$df,newrule_train)
             
              newrule_valid <- cbind(data.frame(readable_rule()),
                                     data.frame(matrix(performance_metrics()[['valid_perf_metrics']],nr=1)))
              
              savedrules_valid$df<- rbind(savedrules_valid$df,newrule_valid)
              
              output$TrainRules <- DT::renderDataTable({DT::datatable(data = savedrules_train$df,
                                                                colnames = c('Rule','precision','recall',
                                                                             'F Measure','No Predicted Fraud',
                                                                              'No Identified Fraud')) %>%
                                                          formatRound(2:4,digits=2)})
              
              
          
            })
   

  observeEvent(input$SaveRule,{
    
    output$finalizerules <- renderUI({actionButton('select1','Validate Rules')})
  })
   
   
   observeEvent(input$select1,{
     
     final_rules <- savedrules_valid$df[input$TrainRules_rows_selected ,]
     output$FinalRules <- DT::renderDataTable({
       DT::datatable(data=final_rules,
                     colnames = c('Rule','precision','recall',
                                  'F Measure','No Predicted Fraud',
                                  'No Identified Fraud')
                     ) %>% formatRound(2:4,digits=2)
     })
   })

   ###Auto generate Rules
   auto_rules <- eventReactive(input$generate_rules,{
     # Create a Progress object
     progress <- shiny::Progress$new()
     progress$set(message = "Generating Rules", value = 0)
     # Close the progress when this reactive exits (even if there's an error)
     on.exit(progress$close())
    
     # Create a callback function to update progress.
     # Each time this is called:
     # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
     #   distance. If non-NULL, it will set the progress to that value.
     # - It also accepts optional detail text.
     n <- input$max_iterations
     updateProgress <- function(value = NULL, detail = NULL) {
       progress$inc(amount = 1/n, detail = detail)
     }
     
     predictors <- data_info()[['predictors']]
     target <- data_info()[['target']]
     train_data2 <- train_data$data %>% select(one_of(c(predictors,target)))
     valid_data2 <- valid_data$data %>% select(one_of(c(predictors,target)))
     results <- generate_trees(train_data2,predictors,target,valid_data=valid_data2,
                               max_iterations=input$max_iterations,
                               max_fraud_captured=input$max_fraud_captured,updateProgress)
     
     rules_list <- extract_rules(results$trees,results$thresholds)
     #Evaluate performance on training data##
     rule_perf_final <- evaluate_performance(rules_list,train_data$data,target,results$trees,
                                             results$predictions)
     # Pick optimal rules
     No_of_Alerts <- nrow(train_data$data)
     optimal_rules <- pick_optimal_rules(rule_perf_final,Alert_Rate = input$alert_rate,No_of_Alerts=No_of_Alerts,
                                         rules_list)
     
     
     list(results=results,rules_list=rules_list,rule_perf_final =rule_perf_final,
          optimal_rules=optimal_rules)
     
   })
   
   
   ###Output auto generated rules
   output$auto_rules <- DT::renderDataTable({
     DT::datatable(data=auto_rules()[['optimal_rules']],rownames = FALSE)
   }) 

   ###Output Validation Performance of auto generated rules and rulesets
   observeEvent(input$validate_rules,{
     predictors <- data_info()[['predictors']]
     target <- data_info()[['target']]
     valid_data2 <- valid_data$data %>% select(one_of(c(predictors,target)))
     optimal_rules <- auto_rules()[['optimal_rules']]
     trees<- auto_rules()[['results']]$trees
     valid_perf <- evaluate_valid_perf(valid_data2,optimal_rules[,c(1,2)],trees)
     
     valid_rules_perf <- bind_rows(valid_perf$rule_performance)
     valid_rule_set_perf <- bind_rows(valid_perf$rule_set_performance)
     output$auto_rules_header <-  renderUI({HTML("<b> Performance of Auto Generated Rules </b> <br/>")})
     output$FinalAutoRulePerf <- DT::renderDataTable({DT::datatable(valid_rules_perf)%>% 
         formatRound(3:5,digits=2)})
     output$auto_ruleset_header <-  renderUI({HTML("<b> Performance of Auto Generated Rule Sets </b> <br/>")})
     output$FinalAutoRuleSetPerf <- DT::renderDataTable({DT::datatable(valid_rule_set_perf)%>% 
         formatRound(3:5,digits=2)})
   })
 
}
# Run the application 
shinyApp(ui = ui, server = server)

