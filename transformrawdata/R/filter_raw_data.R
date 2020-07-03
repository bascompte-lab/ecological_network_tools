filter_raw_data = function(data,covariable_to_filter_by,level_of_covariable_to_filter_by){
  #############################################
  # return a subset of data filter w.r.t. to one value of the chosen covariable
  #############################################
  if (covariable_to_filter_by == "none" | level_of_covariable_to_filter_by == "none") {
  }
  else{
    data = subset(data,data[,covariable_to_filter_by] == level_of_covariable_to_filter_by)
  }
  
  return(data)
}