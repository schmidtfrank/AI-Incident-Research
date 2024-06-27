#Set Up
setwd("~/scratch/Garza")
library(dplyr)
library(caret)
library(e1071)

main <- read.csv('main_v10_clean(in) (3).csv')

main <- read.csv('ai_incidents.csv')
main = main %>% select(-X)
main2 <- read.csv('newestFewClean(in).csv')
main2 = main2 %>% select(-incident_id)

head(main)
main2 <- read.csv("Testing-Table 1.csv")

#Entire AI Datasest as Naive Bayes Classifier
training <- main[1:nrow(main),]
trainLabels <- training[,4]
trainLabels <- factor(trainLabels)
naive_bayes <- naiveBayes(training, trainLabels, laplace=1)

#Assisting Keywords Lists
deepfake_list <- c("Deepfake", "deepfak", "featur", "impersonat", "imit")
autonomous_list <- c("Autonomous", "autonm vehicl")
chatbot_list <- c("Chatbot", "chatbot", "chatgpt", "bot")
business_market_list <- c("Business/Market", "underwriting", "financi")
arts_visuals_list <- c("Visual Arts/Images", " art ", "arts", "imag", "image gener", "depict")
content_search_list <- c("Content Search", "google search", "search engine")
facial_recognition_list = c("Face Recognition", "facial")
device_list <- c("Devices", "devic")
overall_list <- c(deepfake_list, autonomous_list, chatbot_list, business_market_list, arts_visuals_list, content_search_list, facial_recognition_list, device_list)

final_choices <- cbind()
for (i in 1:nrow(main2)) {
  
  print(i)
  
  #Without Title
  new_data <- data.frame(
    date = as.Date(main2[i,1], format = "%m/%d/%Y"),
    Alleged.Deployer = main2[i,2],
    Alleged.Developer = main2[i,3],
    #cleaned_title = main2[i, 5],
    cleaned_description = main2[i,5]
  )
  
  #All Results
  all_results <- cbind()
  
  #Model's Prediction
  model_prediction = predict(naive_bayes, new_data)
  print(model_prediction)
  all_results <- append(all_results, model_prediction)
  
  intuition_count = 0
  matched_categories <- cbind()
  
  #Intuition's Prediction
  for (list in overall_list) {
    if (grepl(list, main2[i,5], fixed = TRUE)) {
      if (list %in% deepfake_list) {
        matched_categories <- append(matched_categories, "Deepfake")
        intuition_count = intuition_count + 1
      }
      if (list %in% chatbot_list) {
        matched_categories <- append(matched_categories, "Chatbot")
        intuition_count = intuition_count + 1
      }
      if (list %in% business_market_list) {
        pmatched_categories <- append(matched_categories, "Business/Market")
        intuition_count = intuition_count + 1
      }
      if (list %in% arts_visuals_list) {
        matched_categories <- append(matched_categories, "Visual Arts/Images")
        intuition_count = intuition_count + 1
      }
      if (list %in% content_search_list) {
        matched_categories <- append(matched_categories, "Content Search")
        intuition_count = intuition_count + 1
      }
      if (list %in% facial_recognition_list) {
        matched_categories <- append(matched_categories, "Face Recognition")
        intuition_count = intuition_count + 1
      }
      if (list %in% device_list) {
        matched_categories <- append(matched_categories, "Devices")
        intuition_count = intuition_count + 1
      }
      if (list %in% autonomous_list) {
        matched_categories <- append(matched_categories, "Autonomous")
        intuition_count = intuition_count + 1
      }
    }
    
  }
  
  print("Predicted:")
  print(matched_categories)
  
  if (intuition_count > 0) {
    match_count = 0
    for (category in matched_categories) {
      if (model_prediction == category) {
        print("Final Choice:")
        print(category)
        match_count = match_count + 1
        final_choice = category
      }
    }
    
    if (match_count == 0) {
      print("Final Choice:")
      print(category)
      final_choice = category
    }
    
  }
  
  else {
    
    #Without Description
    new_data <- data.frame(
      date = as.Date(main2[i,1], format = "%m/%d/%Y"),
      Alleged.Deployer = main2[i,2],
      Alleged.Developer = main2[i,3],
      cleaned_title = main2[i, 4]
      #cleaned_description = main2[i,6]
    )
    
    #Model's Prediction
    model_prediction2 = predict(naive_bayes, new_data)
    
    intuition_count = 0
    matched_categories <- cbind()
    
    review_phase <- TRUE
    
    #Intuition's Prediction
    for (list in overall_list) {
      if (grepl(list, main2[i,4], fixed = TRUE)) {
        if (list %in% deepfake_list) {
          matched_categories <- append(matched_categories, "Deepfake")
          intuition_count = intuition_count + 1
          
          print("Final Choice: Deepfake")
          final_choice = "Deepfake"
          
        }
        if (list %in% chatbot_list) {
          matched_categories <- append(matched_categories, "Chatbot")
          intuition_count = intuition_count + 1
          
          print("Final Choice: Chatbot")
          final_choice = "Chatbot"
          
        }
        if (list %in% business_market_list) {
          pmatched_categories <- append(matched_categories, "Business/Market")
          intuition_count = intuition_count + 1
          
          print("Final Choice: Business/Market")
          final_choice = "Business/Market"
          
        }
        if (list %in% arts_visuals_list) {
          matched_categories <- append(matched_categories, "Visual Arts/Images")
          intuition_count = intuition_count + 1
          
          print("Final Choice: Visual Arts/Images")
          final_choice = "Visual Arts/Images"
          
        }
        if (list %in% content_search_list) {
          matched_categories <- append(matched_categories, "Content Search")
          intuition_count = intuition_count + 1
          
          print("Final Choice: Content Search")
          final_choice = "Content Search"
          
        }
        if (list %in% facial_recognition_list) {
          matched_categories <- append(matched_categories, "Face Recognition")
          intuition_count = intuition_count + 1
          
          print("Final Choice: Face Recognition")
          final_choice = "Face Recognition"
          
        }
        if (list %in% device_list) {
          matched_categories <- append(matched_categories, "Devices")
          intuition_count = intuition_count + 1
          
          print("Final Choice: Devices")
          final_choice = "Devices"
          
        }
        if (list %in% autonomous_list) {
          matched_categories <- append(matched_categories, "Autonomous")
          intuition_count = intuition_count + 1
          
          print("Final Choice: Autonomous")
          final_choice = "Autonomous"
          
        }
        
      }
    }
    
    print("hhh")
    
    if (intuition_count > 0) {
      if (model_prediction2 %in% all_results) {
        print("No Review")
        review_phase <- FALSE
      }
      
      
      
      if (review_phase == TRUE) {
        print("REVIEW!")
        final_choice = "REVIEW"
        
      }
    }
    
    else {
      final_choice <- as.character(model_prediction)
    }
    
  }
  
  
  final_choices <- append(final_choices, final_choice)
  print("-----------")
}

data.frame(final_choices)


#Combining and Forming New CSV
Date <- c(main[1:nrow(main),1], main2[,1])
Alleged.Deployer <- c(main[1:nrow(main),2], main2[,2])
Alleged.Developer <- c(main[1:nrow(main),3], main2[,3])
Primary <- c(main[1:nrow(main),4], final_choices)
cleaned_title <- c(main[1:nrow(main),5], main2[,4])
cleaned_description <- c(main[1:nrow(main),6], main2[,5])

final_data <- data.frame(Date, Alleged.Deployer, Alleged.Developer, Primary, cleaned_title, cleaned_description)
final_data <- final_data[order(as.Date(final_data$Date, format="%m/%d/%Y")),]

View(final_data)
write.csv(final_data, "ai_incidents.csv")
