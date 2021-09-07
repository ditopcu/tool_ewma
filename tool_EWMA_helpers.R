library(tidyverse)


read_template_file <- function(filename = "Results.xlsx") {
  
  
  raw_data <- readxl::read_excel(filename, sheet = 1)
  
  print(names(raw_data))
  
  if(!all(c("test", "day", "id_day", "result",  "test_name", "mean",  "sd") %in%  names(raw_data))) {
    
    print("h1")
    
    return(list(results_df = NULL, stats_df = NULL, error_text = "Wrong Excel File"))
  }
  
  results_df <- raw_data |> 
    select(test = test, day, id_day, result) %>% 
    drop_na()|> 
    distinct()
  
  if (nrow(results_df) < 3) {
    
    print("h2")
    
    return(list(results_df = NULL, stats_df = NULL, error_text = "Insufficient Results"))
    
  }
  


  stats_df <- raw_data |> 
    select(test = test_name, mean, sd) %>% 
    drop_na() |> 
    distinct()
  
  
  test_names <- results_df |> pull(test) |> unique()
  test_stats <- (stats_df |> pull(test)) 
  
  if (length(test_stats) > length(test_names) ) {
    
    return(list(results_df = NULL, stats_df = NULL, error_text = "Error in test stats"))
  }
  

  error_text <- ""
  
  
  list(results_df = results_df, stats_df = stats_df, error_text = error_text)
}



ewma_plot <- function(df, lambda, center, sd) {
  
  
  
  my_L = 2.7 
  
  
  ewma_result <- ewma(df, nsigmas = my_L,data.name = "Results",
                      center = center, std.dev = sd, 
                      lambda = lambda,
                      plot = FALSE)
  
  plot(ewma_result)
  
}
