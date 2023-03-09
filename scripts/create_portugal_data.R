
data <- readr::read_csv(
    "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Student/data/student-merged.csv"
)
grades <- c("G1mat", "G2mat", "G3mat", "G1por", "G2por", "G3por")
predictors <- c(
    "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "traveltime", 
    "studytime", "failures", "schoolsup", "famsup", "paid", "activities", "nursery", 
    "higher", "internet", "romantic", "famrel", "freetime", "goout", "Dalc", "Walc", 
    "health", "absences"
)

data <- data[c("G3mat", predictors)]
colnames(data) <- c("math_grade", predictors)
colnames(data) <- vapply(colnames(data), function(x) tolower(x), character(1), USE.NAMES = FALSE)
readr::write_csv(data, here::here("datos", "portugal.csv"))
