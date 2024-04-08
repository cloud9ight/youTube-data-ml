## [My Shiny APP Youtube URL](https://youtu.be/eOiH2xxQIFU)
suppressMessages(library(tidyverse))
suppressMessages(library(shiny))
suppressMessages(library(knitr))
suppressMessages(library(formattable))
suppressMessages(library(crayon))
suppressMessages(library(gridExtra))
suppressMessages(library(kableExtra))
suppressMessages(library(pander))
suppressMessages(library(lime))
suppressMessages(library(caret))
suppressMessages(library(vtreat))
suppressMessages(library(caTools))
suppressMessages(library(e1071))
suppressMessages(library(randomForest))
suppressMessages(library(ggmice))
suppressMessages(library(shinythemes))
suppressMessages(library(DALEX))
suppressMessages(library(pROC))
library(rpart)
library(rpart.plot)


data_path <- "Global\ YouTube\ Statistics.csv"
df <- read.csv(data_path)
ytb <- df



choose_response <- function(data) {
  transformed_data <- data |>
    mutate(yearly_income = (highest_yearly_earnings + lowest_yearly_earnings) / 2) |>
    within({
      # income_cat
      income_cat <- NA
      income_cat[is.na(yearly_income)] <- 0
      income_cat[yearly_income <= 5e+06] <- 0
      income_cat[yearly_income > 5e+06] <- 1
      income_cat <- as.factor(income_cat)
    }) |>
    select(-lowest_monthly_earnings, -lowest_yearly_earnings, -highest_yearly_earnings, -highest_monthly_earnings, -yearly_income,-video_views_for_the_last_30_days) 
  
  return(transformed_data)
}


# create
eval_model <- function(model, data) {
  # Make predictions on the test set using the final model
  predictions <- predict(model, newdata = data)
  
  # Evaluate the final model
  conf_matrix <-confusionMatrix(predictions, data$income_cat)
  
  uc_value <- conf_matrix$byClass["ROC"]
  print(conf_matrix)
  #print(uc_value)
}





mydata <- choose_response(df)

train_index <- sample(1:nrow(mydata), 0.9 * nrow(mydata))
train <- mydata[train_index,]
test <- mydata[-train_index,]


# split the training set into a training set and a validation set
# use an 90/10 ratio for the splitting
validation_index <- sample(1:nrow(train), 0.1 * nrow(train))
validation <- train[validation_index,]



# Function to count missing values in each column
count_missing <- function(data) {
  # Apply the is.na function to each column and calculate the sum of missing values
  sapply(data, FUN = function(col) sum(is.na(col)))
}

# Function to print the counts of missing values for columns with missing data
print_nacounts <- function(data) {
  # Get the counts of missing values for each column
  nacounts <- count_missing(data)
  # Identify columns with missing values
  hasNA <- which(nacounts > 0)
  
  # Create a data frame for printing
  na_df <- data.frame(Column = names(nacounts)[hasNA], Missing_Count = nacounts[hasNA])
  
  # Print the table
  #cat("Missing Value Counts:\n")
  print(na_df, row.names = FALSE)
}


# Change the dataframe where character columns to factors and return the dataframe
change_to_factor <- function(data) {
  df <- data %>%
    mutate_if(is.character, as.factor)
  
  return(df)
}



prepration_pipeline <- function(data,feature_engineering_method=select_more) {
  prepared_data <- data |>
                  data_transformation_pipeline() |>
                  data_cleaning_pipeline() |>
                  feature_engineering_method()
  
  return(prepared_data)
}

data_transformation_pipeline <- function(data) {
  selected_data <- data |>
                  rename(education.enrollment = Gross.tertiary.education.enrollment....) |>
                  remove_columns() |>
                  merge_columns() |>
                  add_columns()
  
  return(selected_data)
}

data_cleaning_pipeline <- function(data) {
  cleaned_data <- data |>
                  # step 1: convert invalid values to NA
                  clean_invalid_data() |>
                  # step 2: dealing with missing values
                  drop_rows() |>
                  clean_categorical_data() |>
                  clean_numerical_data()
  
  return(cleaned_data)
}

#feature_engineering_method 1: select less features(only keep one country information)
select_less <- function(data) {
  selected_data <- data |>
    select(-c(Population,
education.enrollment,
Unemployment.rate,           
Urban_population,video.views,uploads))
  
  return(selected_data)
}

# selcte more features(keep all country information)
select_more <- function(data) {
  selected_data <- data |>
  select(-c(Population,
Unemployment.rate))
  return(selected_data) }

remove_columns <- function(data) {
  remove_vars <- c("rank","Title","Youtuber","channel_type_rank","video_views_rank","country_rank","Abbreviation","Latitude","Longitude","created_month","created_date")

selected_data <- data[, !names(data) %in% remove_vars]

return(selected_data)
}



merge_columns <- function(data) {
  merged_data <- data |>
    mutate(
      # Fill missing values in 'channel_type' using values from 'category'
      # create a new variable called channel_category
      channel_type = ifelse(channel_type == "nan", category, channel_type)
    )|>
    # Science & Technology -> Tech
    # Film & Animation -> film
    # Pets & Animals & ->
    # Howto & Style -> Howto
    # Shows ->  Entertainment
    # Gaming -> Games
    # People & Blogs -> People 
    mutate(
      channel_type = ifelse(channel_type == "Science & Technology", "Tech", channel_type),
      channel_type = ifelse(channel_type == "Film & Animation", "Film", channel_type),
      channel_type = ifelse(channel_type == "Pets & Animals", "Film", channel_type),
      channel_type = ifelse(channel_type == "Howto & Style", "Howto", channel_type),
      channel_type = ifelse(channel_type == "Shows", "Entertainment", channel_type),
      channel_type = ifelse(channel_type == "Gaming", "Games", channel_type),
      channel_type = ifelse(channel_type == "People & Blogs", "People", channel_type),
    ) |> select(-category) |> 
    filter(channel_type != "nan")
  
  return(merged_data)
}

add_columns <- function(data) {
  added_data <- data |>
    mutate(
      # add new numerical variables
      subscriber_increase_rate = subscribers_for_last_30_days/subscribers,
      views_per_upload = video.views/uploads
      
    )
  
  return(added_data)
}



clean_invalid_data <- function(data) {
  cleaned_data <-  mutate(data,
                          # numerical variable
                          # video views 0 -> NA
                          video.views = na_if(video.views, 0),
                          # created year 1970 -> NA
                          created_year = na_if(created_year, 1970),
                          # education.enrollment >100 -> NA
                          education.enrollment =ifelse(education.enrollment > 100, NA, education.enrollment),
                          # uploads == 0 -> NA
                          uploads = na_if(uploads, 0)
                          
  ) 
  #print_nacounts(data)
  return(cleaned_data)
}


  drop_rows <- function(data) {
    cleaned_data <- data |>
      # drop yearly income < 0 (becasue for log scale)
      #filter(yearly_income > 0) |>
      # Remove rows where 'video.views', are NA
      filter(! is.na(video.views)  ) |>
      filter(! is.na(uploads)  ) |>
      filter(! is.na(created_year)  ) 
    
    return(cleaned_data) }
    
 
  clean_categorical_data <- function(data) {
    cleaned_data <- data %>%
      mutate(Country = ifelse(Country == "nan", 'missing', Country)) %>% 
      group_by(Country) %>%
      mutate(Country = ifelse(n() < 10, 'other', Country)) %>%
      ungroup() %>%
      
      # Replace 'channel_type' values with 'other' for counts less than 40
      group_by(channel_type) %>%
      mutate(channel_type = ifelse(n() < 10, 'other', channel_type)) %>%
      ungroup() |>
      change_to_factor()
    
    return(cleaned_data)
  }
  
  

  clean_numerical_data <- function(data,cleaning_method) {
    # Calculate time duration
    current_year <- 2023
    
    cleaned_data <- data |>
      mutate( age = current_year - created_year,
              uploads_frequency = uploads / age) |>
      within({
        # uploads_freq_cat
        uploads_freq_cat <- NA
        uploads_freq_cat[is.na(uploads_frequency)] <- "missing"
        uploads_freq_cat[uploads_frequency < 10] <- "low"
        uploads_freq_cat[uploads_frequency >= 10 & uploads_frequency <= 1000] <- "middle"
        uploads_freq_cat[uploads_frequency > 1000] <- "high"
        uploads_frequency[is.na(uploads_frequency)] <- 0
        
        # subscriber_increase_rate
        subscriber_increase_rate[is.na(subscriber_increase_rate)] <- 0
        
        # Country information
        Population[is.na(Population)] <- 0
        Unemployment.rate[is.na(Unemployment.rate)] <- 0
        education.enrollment[is.na(education.enrollment)] <- 0
        Urban_population[is.na(Urban_population)] <- 0
        
        # add new categorical variables
        # time_cat
        age_cat <- NA
        age_cat[age <= 3] <- "yong"
        age_cat[age > 3  & age < 13] <- "middle"
        age_cat[age >= 13] <- "old"
        
      }) |> change_to_factor()
    
    cleaned_data <- select(cleaned_data,-created_year,-subscribers_for_last_30_days,-subscribers)
    return(cleaned_data)
  }
  
  
  
  
  
  train_data <- prepration_pipeline(train)
  validation_data <- prepration_pipeline(validation)
  test_data <- prepration_pipeline(test)
  
  train_prepared <- train_data
  test_prepared <- test_data
  
  
  
single_variable <- function(data,variable_name,target_variable) {
 
    suppressWarnings({
      
      formula <- as.formula(paste(target_variable, "~", variable_name))
      logic_regression <- train(formula, data = data, method = "glm", family = "binomial")
      
      return(logic_regression)
      
      
    })
  
}




selection_method_1 <- function(data, target_variable) {

  feature_names <- setdiff(colnames(data), target_variable)
  

  result_df <- data.frame(Feature = feature_names, Feature_Score = numeric(length(feature_names)))
  

  for (i in seq_along(feature_names)) {
    col_name <- feature_names[i]
    #print(col_name)
    
    single_variable_model <- single_variable(data, col_name, target_variable)
    summary_result <- summary(single_variable_model)

    null_deviance <- summary_result$null.deviance
    residual_deviance <- summary_result$deviance
      

      difference <- null_deviance - residual_deviance
      

      result_df$Feature_Score[i] <- round(difference, 2)
    
  }
  
  result_df <- result_df[order(result_df$Feature_Score, decreasing = TRUE), ]
  
  return(result_df)
}

result <- selection_method_1(train_data, "income_cat")

selected_features_1 <- result$Feature[result$Feature_Score > 50]


model <- randomForest(income_cat ~ . , data = train_data, ntree = 100)
importance <- importance(model)
#print(importance)


importance_df <- as.data.frame(importance) |>
  filter(MeanDecreaseGini > 10) 


selected_features_2 <- rownames(importance_df)


normalise <- function(data){
  # normalise the numerical data excepet target variable income_cat
  # Extract numeric columns (excluding 'income_cat')
  numeric_columns <- colnames(data[, sapply(data, is.numeric) & colnames(data) != 'income_cat'])
  
  scaled_data <- data |>
    mutate_at(vars(numeric_columns), ~ (.-mean(.))/sd(.))
  
  
  return(scaled_data)
}

perform_feature_engineering <- function(data, normalisation = TRUE, selection = 1) {
  
  if (normalisation == TRUE) {
    data <- normalise(data)
  }
  
  if (selection == 1) {
    
    
    data <- data[, c(selected_features_1, "income_cat")]
  }
  
  if (selection == 2) {
    
    data <- data[, c(selected_features_2, "income_cat")]
  }
  
  return(data)
}


    #### single variable
single_variable <- function(data,variable_name,target_variable) {

  suppressWarnings({
    # lm
    formula <- as.formula(paste(target_variable, "~", variable_name))
    logic_regression <- train(formula, data = data, method = "glm", family = "binomial")
    
    return(logic_regression)
  })
  
}

model_3 <- rpart(income_cat ~ ., 
                 data = perform_feature_engineering(train_data, normalisation = FALSE, selection = 0),
                 method = "class")

ctrl <- trainControl(method = "cv", number = 5)  


param_grid <- expand.grid(cp = seq(0.01, 0.15, by = 0.01))  

model_4 <- train(
  income_cat ~ .,
  data = perform_feature_engineering(train_data, normalisation = FALSE, selection = 2),
  method = "rpart",
  trControl = ctrl,
  tuneGrid = param_grid
)


best_model <- rpart(income_cat ~ .,
                    data = perform_feature_engineering(train_data, normalisation = FALSE, selection = 2),
                    method = "class",
                    control = rpart.control(cp = model_4$bestTune$cp))  
#### roc

plot_ROC <- function (model, data) {
  pred <- predict(model,newdata = data,type = "prob")
  roc_obj <- roc(data$income_cat, pred[,2])
  plot(roc_obj, col = "red", main = "ROC Curve")
  text(0.6, 0.2, labels = paste("AUC  =", round(auc(roc_obj), 3)), col = "red")
}

plot_ROC_classification <- function (roc_obj1, roc_obj2)  {
  plot(roc_obj1, col = "red", main = "ROC Curve Comparison")
  lines(roc_obj2, col = "blue")
  
  
  legend("bottomright", legend = c("Model 1", "*Model 2(improved)"), col = c("red", "blue"), lty = 1)
  
  

  text(0.6, 0.2, labels = paste("AUC Model 1 =", round(auc(roc_obj1), 3)), col = "red")
  text(0.6, 0.1, labels = paste("*AUC Model 2 =", round(auc(roc_obj2), 3)), col = "blue")
  
  
}



    #### clustering function
get_scaled_df <- function(df, vars_not_to_use) {
  vars_to_scale <- setdiff(names(df), vars_not_to_use)
  scaled_df <- scale(df[, vars_to_scale])
  scaled_df <- as.matrix(scaled_df[complete.cases(scaled_df), ])
  return(scaled_df)
}

get_scaled_center_and_scale <- function(df, vars_not_to_use) {
  vars_to_scale <- setdiff(names(df), vars_not_to_use)
  scaled_df <- scale(df[, vars_to_scale])
  center <- attr(scaled_df, "scaled:center")
  scale <- attr(scaled_df, "scaled:scale")
  return(list(center = center, scale = scale))
}
df_cluster <-data_cleaning_pipeline(data_transformation_pipeline(df))
df_cluster <- na.omit(df_cluster)
vars_not_to_use <- c("Country", "channel_type","age_cat","uploads_freq_cat","lowest_yearly_earnings","highest_yearly_earnings")
scaled_cluster <- get_scaled_df(df_cluster, vars_not_to_use)

performHierarchicalClustering <- function(scaled_cluster, df_cluster, sample_size, num_of_cluster, var) {
  set.seed(123)  
  sample_indices <- sample(1:nrow(scaled_cluster), sample_size)
  subset_scaled_cluster <- scaled_cluster[sample_indices, ]
  subset_df_cluster <- df_cluster[sample_indices, ]
  
  d <- dist(subset_scaled_cluster, method = "euclidean")
  pfit <- hclust(d, method = "ward.D2")
  plot(pfit, labels = subset_df_cluster[[var]], main = "Cluster Dendrogram")
  num_clusters <- num_of_cluster
  rect.hclust(pfit, k = num_clusters)
  
  clusters <- cutree(pfit, k = num_clusters)
  
  return(list(pfit, subset_df_cluster))
}

print_clusters <- function(df, groups, cols_to_print) {
  Ngroups <- max(groups)
  for (i in 1:Ngroups) {
    print(paste("cluster", i))
    print(df[groups == i, cols_to_print])
  }
}

# SHINY APP

ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # In the navbar, we will have 3 tabs 
                navbarPage(
                  "youTube channels",
                  
                  # Tab 1: single variable model
                  tabPanel("Single Variable",
                           sidebarPanel(
                             width = 3,
                             selectInput(inputId = "input_single_var",
                                         label = "Choose Variable:",
                                         choices = colnames(train_prepared))
                           ),
                           mainPanel(
                             fluidRow(
                               verbatimTextOutput("sv_output"),
                               br(), br(), br(), 
                               plotOutput("sv_roc")
                             )
                           )
                  ),
                  
                  # Tab 2: Classification 
                  tabPanel("Logic Regression",
                           
                           
                           mainPanel(
                             fluidRow(
                               verbatimTextOutput("lr_output"),
                               br(), br(), br(), 
                               plotOutput("lr_roc", height = "400px", width = "900px")
                             )
                           )
                  ),
                  
                  # Tab 3: Decision Tree 
                  tabPanel("Decision Tree",
                           
                           mainPanel(
                             fluidRow(
                               plotOutput("dt_output", height = "400px", width = "900px"),
                               br(), br(), br(), 
                               plotOutput("dt_roc", height = "400px", width = "900px")
                             )
                           )
                  ),
                  
                  # Tab 4: Clustering Results
                  tabPanel("Clustering",
                           sidebarPanel(
                               width = 3,
                               selectInput(inputId = "input_cluster_method", 
                                                  label = "Choose Method:",
                                                  choices = c("Hierarchical Clustering", "Sample Result","K-Means")),
                               conditionalPanel(
                                 condition = "input.input_cluster_method == 'Sample Result'",
                                 selectInput(inputId = "input_cluster_variable",
                                           label = "Choose Variable:",
                                           choices = c("Country", "channel_type", "age_cat", "uploads_freq_cat", "highest_monthly_earnings")),
                                 selectInput(inputId = "input_sample_size",
                                           label = "Choose Sample Size:",
                                           choices = c(25,50,75,100))),
                               conditionalPanel(
                                 condition = "input.input_cluster_method == 'K-Means'",
                                 selectInput(inputId = "input_variableA",
                                             label = "Choose Variable 1:",
                                             choices = c("Country", "channel_type", "age_cat", "uploads_freq_cat")),
                                 selectInput(inputId = "input_variableB",
                                             label = "Choose Variable 2:",
                                             choices = c("channel_type", "age_cat", "uploads_freq_cat")))
                                  ),
                           mainPanel(
                               width = 9, 
                               plotOutput("cluster", height = "800px", width = "800px")
                           )
                  )
))

# Define the server
server <- function(input, output, session) {
  options(warn = -1)
  output$sv_output <- renderPrint({
    
     summary(single_variable(train_prepared, input$input_single_var, "income_cat"))

  })
  
  output$sv_roc <- renderPlot({
    model <- single_variable(train_prepared, input$input_single_var, "income_cat")
    plot_ROC(model, test_prepared)

    
    
  })
 
  
  ##tab 2
  output$lr_output <- renderPrint({
      
    model_2 <- glm(income_cat ~ ., 
                   data = perform_feature_engineering(train_data, normalisation = TRUE, selection = 1),
                   family = "binomial")
    summary(model_2)
    
      
    })
    
    output$lr_roc <- renderPlot({
      
      
      model_1 <- glm(income_cat ~ ., 
                     data = perform_feature_engineering(train_data, normalisation = FALSE, selection = 0),
                     family = "binomial")
      model_2 <- glm(income_cat ~ ., 
                     data = perform_feature_engineering(train_data, normalisation = TRUE, selection = 1),
                     family = "binomial")
      predictions1 <- predict(model_1, 
                              newdata = perform_feature_engineering(validation_data, normalisation = FALSE, selection = 0),
                              type = "response")
      
      
      
      predictions2 <- predict(model_2, 
                              newdata = perform_feature_engineering(validation_data, normalisation = TRUE, selection = 1), 
                              type = "response")
      roc_obj_1 <- roc(validation_data$income_cat, predictions1)
      roc_obj_2 <- roc(validation_data$income_cat, predictions2)
      
      plot_ROC_classification(roc_obj_1, roc_obj_2)
      
      
    })
    
    
    
    
    


  
  
  ##tab 3 
  output$dt_output <- renderPlot({
  
    best_model <- rpart(income_cat ~ .,
                        data = perform_feature_engineering(train_data, normalisation = FALSE, selection = 2),
                        method = "class",
                        control = rpart.control(cp = model_4$bestTune$cp))  
    
    
    rpart.plot(best_model)
    

  })
  
  output$dt_roc <- renderPlot({
    
    
    predictions3 <- predict(model_3, 
                            newdata = perform_feature_engineering(validation_data, normalisation = FALSE, selection = 0), 
                            type = "prob")
    predictions4 <- predict(model_4, 
                            newdata = perform_feature_engineering(validation_data, normalisation = FALSE, selection = 2), 
                            type = "prob")
    roc_obj3 <- roc(validation_data$income_cat, predictions3[,2])
    roc_obj4 <- roc(validation_data$income_cat, predictions4[,2])
    plot_ROC_classification(roc_obj3, roc_obj4)
    
  })
  
  
  ##tab4 
  output$cluster <- renderPlot({
    method <- input$input_cluster_method
    
    if (method == "Hierarchical Clustering") {
      d <- dist(scaled_cluster, method = "euclidean")
      pfit <- hclust(d, method="ward.D2")
      df_cluster <- na.omit(df_cluster)
      princ <- prcomp(scaled_cluster)  # Principal components of scaled_cluster
      nComp <- 2  # First two principal components
      project2D <- as.data.frame(predict(princ, newdata = scaled_cluster)[, 1:nComp])
      hclust.project2D <- cbind(project2D, cluster = as.factor(cutree(pfit, k=5)), channel = df_cluster$channel_type)
      find_convex_hull <- function(proj2Ddf) {
        do.call(rbind,
                lapply(unique(cutree(pfit, k=5)),
                       FUN = function(c) {
                         f <- subset(proj2Ddf, cluster==c);
                         f[chull(f),]
                       }))}
      
      hclust.hull <- find_convex_hull(hclust.project2D)

      ggplot(hclust.project2D, aes(x=PC1, y=PC2)) +
        geom_point(aes(shape=cluster, color=cluster)) +
        geom_text(aes(label=channel, color=cluster), hjust=0, vjust=1, size=3) +
        geom_polygon(data=hclust.hull, aes(group=cluster, fill=as.factor(cluster)),
                     alpha=0.4, linetype=0) + theme(text=element_text(size=20))+
        labs(title = "Cluster Analysis of channel_type")+ 
        theme(text=element_text(size=20))
      
    } else if (method == "Sample Result") {

      
      hierarchical_result <- performHierarchicalClustering(scaled_cluster, df_cluster, input$input_sample_size,5,input$input_cluster_variable)
      s_groups <- cutree(hierarchical_result[[1]], k=5)
      
      print_clusters(hierarchical_result[[2]], s_groups, c("channel_type","age_cat","uploads_freq_cat"))
      
    } else if (method == "K-Means") {
      
      
      kmeans_result <- kmeans(scaled_cluster, centers = 6)
      cluster_assignments <- factor(kmeans_result$cluster)
      df_cluster$cluster <- cluster_assignments
      var_a <- input$input_variableA
      var_b <- input$input_variableB
      kmeans_data <- data.frame(a = df_cluster[[var_a]], 
                                b = df_cluster[[var_b]], 
                                Cluster = factor(df_cluster$cluster))

      ggplot(kmeans_data, aes(x = a, y = b, fill = Cluster)) +
        geom_tile() +
        scale_fill_manual(values = 1:6) +
        labs(x = input$input_variableA, y = input$input_variableB, title = "K-means Clustering Results (K = 6)")+ 
        theme(text=element_text(size=20), axis.text.x = element_text(angle = 90, hjust = 1))
    }

  })
  
 
  
}

shinyApp(ui, server)



