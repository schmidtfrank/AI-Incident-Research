main <- read.csv('main_v10_clean(in) (3).csv')

main <- read.csv('ai_incidents.csv')

main2 <- main[692:720,]

#Entire AI Datasest as Naive Bayes Classifier
training <- main[1:691,]
trainLabels <- training[,5]
testLabels <- main2[,5]
trainLabels <- factor(trainLabels)
testLabels = factor(testLabels)
naive_bayes <- naiveBayes(training, trainLabels, laplace=1)

m_pred3 <- predict(naive_bayes, main2)
table(m_pred3, testLabels)

final_choices <- cbind()
for (i in 1:nrow(main2)) {
  
  #Without Title
  new_data <- data.frame(
    date = as.Date(main2[i,2], format = "%m/%d/%Y"),
    Alleged.Deployer = main2[i,3],
    Alleged.Developer = main2[i,4],
    cleaned_title = main2[i, 5],
    cleaned_description = main2[i,6]
  )
  
  #Incident ID
  print(main2[i,1])
  
  #All Results
  all_results <- cbind()
  
  #Model's Prediction
  model_prediction = predict(naive_bayes, new_data)
  final_choices = append(final_choices, as.character(model_prediction))
  
  
  
}

table(final_choices, testLabels)
