set.seed(1)

library(caret)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop("USAGE: evalate_model.R [model_type] [data_set] [target]")
}

method <- "rf"
dataset <- "iris"
target <- "Species"

my_data <- get(dataset)

if(! target %in% colnames(my_data)) {
  stop(paste0("The '", dataset, "' dataset does not have a column named '", target, "'"))
}
  
inTrain <- createDataPartition(my_data[, target], list=FALSE, p=0.8)

# split training and test
training <- my_data[inTrain,]
testing <- my_data[-inTrain,]

formula <- paste0(target, " ~ .")
model <- train(as.formula(formula), data=training, method=method)
confusion_matrix <- confusionMatrix(predict(model, testing), testing[, target])

print(model)
plot(model)

cf_df <- as.data.frame(confusion_matrix$table)
ggplot(cf_df, aes(x=Prediction, y=Reference, fill=Freq)) + 
  geom_tile() + 
  ggtitle(paste("Confusion Matrix for", model$modelInfo$label))
  
save(model, file="results/model.RData")

metric <- model$metric
index_of_best_model <- which(max(model$results[, metric]) == model$results[, metric])

diagnostics = list("Train Accuracy"=model$results[index_of_best_model, "Accuracy"], 
                   "Test Accuracy"=confusion_matrix$overall[["Accuracy"]])

for (my_name in names(model$bestTune)) {
  diagnostics[[paste0("Param(",my_name, ")")]] = model$bestTune[[my_name]]
}

library(jsonlite)
fileConn<-file("dominostats.json")
writeLines(toJSON(diagnostics), fileConn)
close(fileConn)
