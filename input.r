
model_predictions <- function(input_data){
  model <- readRDS("./final_model.rds")
  model$predict(task, row_ids = input_data)
}

model_predictions()
