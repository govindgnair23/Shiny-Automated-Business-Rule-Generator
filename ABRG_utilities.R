######################################################################
###Script containing functions to automate Business Rule Generation###
######################################################################

####Versions######
#ABRG_utlities -> Version 1
#ABRG_utlitiesv2 -> Version 2: Compatible with shiny


#################Required packages##################
##Libraries
library(dplyr)
library(randomForest)
library(mlr)
library(rpart)
library(rpart.plot)
library(utils)
library(partykit)
library(Rglpk)
library(stringr)
####################################################

##############################################################################
#########Function to get performance of a rule or predictive model############
##############################################################################

###Inputs: 
#actual: vector with actual fraud status, 0 -> non fraud 1 -> fraud
#predicted: dataframe with predictions of domensions( No of observations * No of rules)
###Output: 
#results: dataframe of dimensions( No of rules * 5) containing 5 performance measures


precall2 <- function(actual,predicted){
  
  results <- matrix(NA,ncol(predicted),5)
  actual <- factor(actual,levels=c("0","1"))
  for(i in c(1:ncol(predicted))){
    vol <- length(actual)
    preds <- factor(predicted[,i],levels=c("0","1"))
    
    tab <- as.matrix(table(actual,preds))
    
    predicted_fraud <- round(tab[1,2] + tab[2,2])
    precision <- tab[2,2]/ predicted_fraud 
    recall <- tab[2,2]/(tab[2,1]+tab[2,2])
    identified_fraud <- tab[2,2]
    
    f <-  2*precision*recall/(precision+recall)
    if (is.na(f)| is.nan(f)) f.measure <- 0
    else f.measure <- f
    
    #vol_decrease = ( vol - predicted_fraud)/ vol*100
    results[i,] <- c(precision,recall,f.measure,predicted_fraud,identified_fraud)
  }
  colnames(results) <- c('precision','recall','Fmeasure','predicted_fraud','identified_fraud')
  
  return(results)
}


###Function to return the most predictive variables in a dataset given  more than 10 variables###
##Inputs: 
#train_data:training data
#n_vars_selected:no of variables to select
#weights: observation weights
#skip: No of highest ranked variables you want to skip
##Output: vector with top N(n_vars_selected) variables

get_important_vars2<- function(train_data,predictors,target,n_vars_selected=10,weights,skip=0){
  
  N <- nrow(train_data)
  ##Resample training data according to weights - To be used when focus is to be on different observations 
  ## in successive iterations
  train_data2 <- train_data %>% sample_n(N,replace=TRUE,weight=weights)
  
  y_vec <- train_data2 %>% select(one_of(target))%>%pull()
  X_df <- train_data2 %>% select(one_of(predictors))
  
  rf2 <- randomForest(y=y_vec,x=X_df,importance =TRUE)
  
  ##Add ranks of both and arrange according to joint rank
  #Use accuracy rank as it is more stable than gini rank
  imp_df <- data.frame(rf2$importance)
  imp_df$accuracyrank <- rank(-imp_df$MeanDecreaseAccuracy)
  imp_df$ginirank <- rank(-imp_df$MeanDecreaseGini)
  imp_df$total_rank <-   imp_df$accuracyrank + imp_df$ginirank
  final_rank <- rownames(imp_df)[order(imp_df[,'total_rank'])]
  

  
  final_vars <- final_rank[(1+skip):(n_vars_selected+skip)]
  return(final_vars)
  
}



###################utility function helps with cross validation in mlr #####################
f_measure_fn <- function(task,model,pred,feats,extra.args){
  actual <- factor(getPredictionTruth(pred),levels=c("0","1"))
  preds <- factor(getPredictionResponse(pred),levels =c("0","1"))
  #makes sure table always outputs 2x2
  tb <- table(actual,preds)
  
  prec <- tb[2,2]/(tb[1,2]+tb[2,2])
  recall <- tb[2,2]/(tb[2,1]+tb[2,2])
  
  f <- 2*prec*recall/(prec+recall)
  if (is.na(f)| is.nan(f)) return(0)
  else return(f)
  
}

f_measure <- makeMeasure(
  id = "f_measure",name = "My F1 measure",
  properties = c("classif","req.pred","req.truth"),
  minimize = FALSE,
  best=1,
  worst=0,
  fun = f_measure_fn
)

###############################################################################################
###Function to return a decision tree that captures the fraud in a given dataset#####

####Inputs

#train_data: Training Data
#valid_data: Validation data
#weights: Observation weights

####Outputs
#best_trees: decision tree obtained given data
#valid_results_best: Performance in validation dataset
#thresholds: Optimal threhsold for the decision tree that maximizes the f-measure

get_best_rules <- function(train_data,predictors,target,valid_data,weights)
{
  #features <- names(train_data)[names(train_data) != "fraud_flag"]
  #feature_set <- combn(features,n_features)
  #no_rules <- ncol(feature_set)# Number of feature sets to consider

  #train_preds_list <- list()
  #valid_preds_list <- list()
  
  #train_data <- train_data %>% select(one_of(predictors))
  
  #Create train task
  trainTask1 <- makeClassifTask(data = train_data,target=target,weights=weights,positive='1')
  #Create validation task
  validTask <- makeClassifTask(data =valid_data,target=target,positive='1')
  
  #Create a learner
  lrn <- makeLearner("classif.rpart",predict.type = "prob") 
  
    
  #Cross validation
  set.seed(0)
  cv3 <- makeResampleInstance("CV", iters=3,task= trainTask1)
    
    
  pars <- list('maxdepth'=10)
  #Set hyperparameters for modelling
  tree <- setHyperPars(lrn,par.vals = pars)
    
    
  #Create a decision tree - Use c=inbuilt cross validation to select optimal cost complexity for pruning
  tryCatch(
     {
        dt <- train(tree,trainTask1) 
        opt_index <- which.min(dt$learner.model$cptable[,"rel error"])
        cp_opt <- dt$learner.model$cptable[opt_index,"CP"]
        
        dt_opt <- prune(tree=dt$learner.model,cp=cp_opt)
        dt$learner.model <- dt_opt
    }, error = function(err){
        
      }
    )
    
  pred <- predict(dt,task=trainTask1)
    
  #Find optimal probability threshold by resampling train data
  r <- resample(lrn,  trainTask1, resampling = cv3, measures = list(f_measure), show.info = FALSE)
  tune.res <- tuneThreshold(pred=r$pred,measure = f_measure)
    
  thresh_optimal <- tune.res$th
    
  #Use optimal threhsold to make predictions
  pred.th <- setThreshold(pred,thresh_optimal)
    
  tree <- dt$learner.model
  
    
  train_preds <- getPredictionResponse(pred.th)
    
  #Also get predictions of each rule on validation set
  valid_preds <- predict(dt,validTask)
  valid_preds.th <- setThreshold(valid_preds,thresh_optimal)
  valid_preds <-  getPredictionResponse(valid_preds.th )
    
  
  
  train_preds_df <- data.frame(train_preds)
  train_results <- precall2(train_data$fraud_flag,train_preds_df)
  
  ## Evaluate performance of all rules on validation set
  valid_preds_df <- data.frame(valid_preds)
  valid_results_df <- as.data.frame(precall2(valid_data$fraud_flag,valid_preds_df))
  
  
  
  return(list('best_trees'= tree,'valid_results_best'= valid_results_df,
             'thresholds'= thresh_optimal))
}

#############################################################################################
###Functions to update observations weights so as to focus on fraud prevously not captured###
#############################################################################################

###Inputs:
#actual_fraud: true fraud flag
#Predicted fraud: predicted fraud flag
#weights: observation weights

###Outputs
#New weights: Updated observation weights


#Upweight false negatives and downweight true positives by a factor of 2 and 0.5

update_weights <- function(actual_fraud,predicted_fraud,weights){
  mistakes <- actual_fraud != predicted_fraud
  fn <- (actual_fraud==1 & mistakes) #False Negatives
  fp <- (actual_fraud==0 & mistakes) #False Positives
  correct <- actual_fraud == predicted_fraud
  new_weights <- weights
  new_weights[correct] <- weights[correct] * 0.5
  new_weights[fn]<- weights[fn] * 2
  new_weights <- new_weights / sum(new_weights)
  return(new_weights)
}

#############################################################################################
#Utility functions required for the function generate_trees### 

##########Function to return rules from a decicion tree##################

###Inputs
#model: rpart decision tree object
#th: threhsold to be used to flag fraud
listrules<-function(model,th)
{
  if (!inherits(model, "rpart")) stop("Not a legitimate
                                      rpart tree")
  #
  # Get some information.
  #
  frm <- model$frame
  names <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1,]$n
  #
  # Print each leaf node as a rule.
  #
  rule_list <- list() #temporary list to hold rules
  for (i in 1:nrow(frm))
  {
    if (frm[i,1] == "<leaf>" & frm$yval2[,5][i] >= th)
    {
      # The following [,5] is hardwired - needs work!
      cat("\n")
      #cat(sprintf(" Rule number: %s ", names[i]))
      #cat(sprintf("[Prediction=%s No_Obs=%d (%.0f%%) prob=%0.2f]\n",
      #            ylevels[frm[i,]$yval], frm[i,]$n,
      #            round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
      pth <- path.rpart(model, nodes=as.numeric(names[i]),
                        print.it=FALSE)
      rule_list[[names[i]]] <- unlist(pth)[-1]
      #cat(sprintf(" %s ", unlist(pth)[-1]), sep="")
    }
  }
  
  return(rule_list)
}
  
#########################################################################################
####Function to return a list of rules from a list of decision trees########
###Input
#trees: list of trees
#thresholds: vector of optimal thresholds corresponding to trees in the list
###output
#ruleslist: list of lists. Outer list corresponds to trees. Inner lists corresponds to rules
# in each tree

extract_rules <- function(trees,thresholds){
  ruleslist <- list()
  for (i in c(1:length(trees))){
    tree_i <- trees[[i]]
    th_i <- thresholds[i]
    rulelist_i <- listrules(tree_i,th_i)
    ruleslist[[i]] <- lapply(rulelist_i,paste,collapse= " & ")
  }
  
  return(ruleslist)
}


########################################################################################
# Function to get train set performance of each rules #

###Input
#ruleslist: List of rules (output from the function extract_rules)
#train_data: training data
#trees: List of trees to be evaluated
#predictions: Dataframe with a column corresponding to predictions from each tree
###Output: dataframe with rule performance metrics.One row for each rule.
# Output lists no of true positives and ALert Volume

evaluate_performance <-function(ruleslist,train_data,target,trees,predictions){
  rule_perf_list <- list()
  for (i in c(1:length(trees))){
    selected_rules <- names(ruleslist[[i]])
    
    if(i==1){
      fraud_flag <- train_data %>% select(one_of(target)) %>% pull()
    }else{
      
      ######Subsequent rule sets should get credit only for predicting fraud not predicted
      ######by previous rule sets
      ###########Fraud predicted by previous rules#########
      prev_preds <- apply(data.frame(predictions[,c(1:(i-1))]),1,function(x) ifelse(1 %in% x,1,0))
      ##########True positives identified by previous rulesets##########
      tp_i_prev <- fraud_flag == prev_preds & fraud_flag==1
      ##Reset fraud flags for  rulesets after ruleset 1##
      fraud_flag <- train_data %>% select(one_of(target)) %>% pull()
      fraud_flag[tp_i_prev] <- 0  
      
    }
    
    tree_i <- trees[[i]]
    rule_perf_i <- as.data.frame.matrix(table(tree_i$where,fraud_flag))
    rule_perf_i$rule_no <- rownames(tree_i$frame)[tree_i$frame$var=='<leaf>']
    ###Filter for only selected rules###
    rule_perf_i <- rule_perf_i %>%
      filter(rule_no %in% selected_rules)
    
    rule_perf_i$TP <- rule_perf_i[,2]
    rule_perf_i$Alert_Volume <- rule_perf_i[,1] + rule_perf_i[,2]
    rule_perf_i$rule_no <- paste0(i,'_',rule_perf_i$rule_no)
    
    rule_perf_list[[i]] <- rule_perf_i
    
  }
  
  return(bind_rows(rule_perf_list))
  
} 
  
 
########################################################################################
############## Function to get optimal set of rules ####################################
#######################################################################################

###Input
#rule_perf_final:  Dataframe with performance of each rule (rule_no,True Positives and Alert Volume)
# Output of evaluate_performance
#Alert_Rate: Acceptable alert rate
#No_of_Alerts: No of Alerts in the training data

###Output: A dataframe with 3 columns: rule_name,rule_id and rule_definition


pick_optimal_rules <- function(rule_perf_final,Alert_Rate,No_of_Alerts,ruleslist){
  obj <-rule_perf_final$TP
  mat <- matrix(rule_perf_final$Alert_Volume,nr=1)
  dir <- c("<=")
  rhs <- c(Alert_Rate*No_of_Alerts)
  soln <- as.logical(Rglpk_solve_LP(obj, mat, dir, rhs, max = TRUE,types = 'B')$solution)
  final_selected_rules <- rule_perf_final$rule_no[soln]
  
  ####Retrieve final rules####
  final_rules <- list()
  for (rule_no in final_selected_rules){
    rule_no_split <- unlist(strsplit(rule_no,"_"))
    rule_set <- as.numeric(rule_no_split[1])
    rule_num <- rule_no_split[2]
    final_rules[rule_no] <- ruleslist[[rule_set]][rule_num]
  }
 
  final_selected_rules <- unlist(strsplit(final_selected_rules,"_"))
  rule_set <- final_selected_rules[seq(1,length(final_selected_rules),by=2)]
  rule_num <- final_selected_rules[seq(2,length(final_selected_rules),by=2)]
  rule_df <- data.frame(rule_set,rule_num)
  return(cbind(rule_df,'rule_definition'=unlist(final_rules))) 
}




########################################################################################
############## Function to get validation performance of rule & rule set ###############
########################################################################################
###Input
#valid_data:  data frame with validation data 
#rule_ids: dataframe with rule set and rule ids
# trees : List of generated decision trees

###Output: 
#rule_performance: List of n  dataframes for n rule sets containing performance of each rule
#rule_set_performance: List of n  dataframes for n rule sets containing performance of each ruleset


evaluate_valid_perf <- function(valid_data,rule_ids,trees){
  
  rule_set_perf_list <- list()
  rule_perf_list <- list()
  uniq_rule_sets <- unique(rule_ids$rule_set)
  
  for(i in  c(1:length(uniq_rule_sets))){
    rule_set <- uniq_rule_sets[i]
    if(i==1){
      valid_fraud <- valid_data%>%select(one_of(target)) %>% pull()
    }else{
      ##Identify true positives from previous rule sets
      tp <- valid_fraud==1 & valid_fraud==valid_preds_rs
      ##Set these to 0 so subsequent rule sets don't get credit for indeitifying these
      valid_fraud[tp] <- 0
    }
    
    ##Get tree corresponding to the right rule set
    tree <- trees[[as.numeric(rule_set)]]
    ##Convert to party kit tree
    tree_p <- as.party(tree)
    ##Get node preds
    valid_node_preds <- predict(tree_p,valid_data,type= "node")
    # Get terminal ndoe label
    terminal_node_preds <- rownames(tree$frame)[valid_node_preds]
    ##Get binary prediction
    rule_names <- rule_ids[rule_ids$rule_set== rule_set,]$rule_num
    valid_preds <- lapply(rule_names,function(x){return(as.numeric(terminal_node_preds==x))})
    valid_preds_df <- data.frame(valid_preds)
    colnames(valid_preds_df) <- rule_names
    ##Get validation performance of each rule
    rule_perf <- data.frame(precall2(valid_fraud,valid_preds_df))
    rule_perf$Rule_Set <- uniq_rule_sets[i]
    rule_perf$Rule_ID <- rule_names
    ##Re-order columns
    rule_perf <- rule_perf[,c(6,7,1:5)]
    
    #rownames(rule_perf) <- rule_names
    rule_perf_list[[i]] <- rule_perf
    
    #Get validation performance for the rule set as a whole
    valid_preds_rs <- apply(valid_preds_df,1,function(x){ return(as.numeric(any(x==1)))})
    rule_set_perf <- data.frame(precall2(valid_fraud,data.frame(valid_preds_rs)))
    rule_set_perf$Rule_Set <- uniq_rule_sets[i]
    rule_set_perf <- rule_set_perf[,c(6,1:5)]
    #rownames(rule_set_perf) <- uniq_rule_sets[i]
    rule_set_perf_list[[i]] <- rule_set_perf
    
  }
  return(list('rule_performance'=rule_perf_list,'rule_set_performance'=rule_set_perf_list))
  
}

#############################################################################################
####################################Functions to simplify a given rules######################
#E.g : X>3 & X>5 -> X>5#
#E.g.: X<1 & X<5 -> X<1#
#E.g. X>= 4 & X =6  & Y =10 & Y<= 50 -> X =6 & Y = 10

###Input: A list of rules
###Output: A list of simplified rules

get_final_rules <- function(rules_list){
  rule_df_list <- lapply(rules_list,order_rules)
  rules_df_list2 <- lapply(lapply(rule_df_list,lapply,clean_rules),bind_rows)
  final_rules <- lapply(rules_df_list2,format_rules)
  return(final_rules)
}
##########################################################################################
#####Functions below are auxilliary function of the get_final_rules function########

#####Function to take a vector of rules and return##### 
#####a list of dataframes for each unique variable#####
##Input: a rule as vector
##Output:List of dataframes,with each dataframe containing rule clauses for 1 variable

order_rules <- function(rule){
  split_rule <- unlist(str_split(rule,"&"))
  lhs_rhs <- str_split(split_rule,">=|<=|>|<|=") #Get lhs and rhs of expressions
  #Convert to a dataframe
  lhs_rhs_df <- data.frame('lhs' =  unlist(lapply(lhs_rhs,function(x) str_squish(x[1]))),
                           'rhs' = unlist(lapply(lhs_rhs,function(x) str_squish(x[2]))),stringsAsFactors = FALSE)
  #Add an operator column
  lhs_rhs_df$operator <- str_extract(split_rule,">=|<=|>|<|=")  
  lhs_rhs_df <- lhs_rhs_df[,c(1,3,2)] #rearrange to e in logical order: lhs operator rhs
  return(split(lhs_rhs_df,lhs_rhs_df$lhs,drop=TRUE))
  
}


######Funtion to clean numeric rules i.e. if there are numbers on RHS######
##This function is used inside the fucntion clean_rules###

clean_numeric_rules <- function(rule_df){
  ###Precedence of operators###
  op_id <- c('>=' ='g','>' ='g','<'='l','<='='l') 
  op_gt_precedence <- c ('>' = 1, '>=' = 2)
  op_lt_precedence <- c('<' = 1, '<=' = 2)
  
  
  ##Get unique operators
  uniq_operators <- unique(rule_df$operator)
  rule_df$rhs <- as.numeric(rule_df$rhs)
  
  if(all(uniq_operators %in% c('<','<='))){ ###Less than opertors
    
    
    rule_df$precedence <- op_lt_precedence[rule_df$operator]
    #Select smaller RHS if both operators are less than, in case of a tie pick opertor with higher precedence
    rule_df <- arrange(rule_df,rhs,precedence)
    return(rule_df[1,1:3])
    
  }
  else if(all(uniq_operators %in% c('>','>='))){ ###Greater than opertors
    
    rule_df$precedence <- op_gt_precedence[rule_df$operator]
    #Select larger RHS if both operators are greater than, in case of a tie pick opertor with higher precedence
    rule_df <- arrange(rule_df,desc(rhs),precedence)
    return(rule_df[1,1:3])
    
  }
  
}

#######################Function to clean rules#################
##input: A dataframe containing clauses for a single variable
##output: A dataframe with cleaned clauses
clean_rules <- function(rule_df){
  ###Function to find intersecting elements in a list
  list_intersect <- function(x) Reduce('intersect',x)
  
  #Check if rhs is number 
  if(all(str_detect(rule_df$rhs,"(^0$|^[-+]?0\\.)|^[-+]?[1-9][0-9//.]?+"))){
    ###If there is a clause with an equal sign,only that clasue is feasilble
    if ('=' %in% unique(rule_df$operator)){
      return(rule_df[rule_df$operator=='=',])
    }
    ###Else break down by greater than and less than
    op_id <- c('>=' ='g','>' ='g','<'='l','<='='l')
    rule_df$group <- op_id[rule_df$operator]
    sub_rules <- split(rule_df,rule_df$group)
    ##Simplify the rules
    sub_rules <- lapply(sub_rules,clean_numeric_rules)
    clean_rule_df <- as.data.frame(matrix(unlist(sub_rules),nc=3,byrow=TRUE),stringsAsFactors = FALSE)
    colnames(clean_rule_df) <- colnames(sub_rules[[1]])
    
    #If RHS is a string/factor
  }else{
    
    rhs <- str_split(rule_df$rhs,",")
    rhs_intersect <- list_intersect(rhs)
    clean_rule_df <- rule_df[1,]
    clean_rule_df[,'rhs'] <- paste(rhs_intersect,collapse=",")
    
  }
  
  return(clean_rule_df)
  
}


########Function to return a concise rule from a rule dataframe list#########
##Input: A list of dataframes coorresponding to variables in a single rule
##Output: A single consolidated rule

format_rules <- function(rule_list){
  final_rule_table <- bind_rows(rule_list)
  final_rule_table$rule <- paste(final_rule_table$lhs,final_rule_table$operator,final_rule_table$rhs)
  return(paste(final_rule_table$rule,collapse =" & "))
  
}
  




