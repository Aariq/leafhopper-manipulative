#attribute stripper function
rm_attr <- function(x){
  attributes(x) <- NULL
  return(x)
}

#Model chooser
#note, this ONLY works when models are in order null, line, step, hinge
bestAIC <- function(AIC){
  if(any(AIC[1] >= AIC[2:4] + 2)){
    if(any(AIC[2] >= AIC[3:4] + 2)){
      return(min(AIC[3:4]))
    } else {
      return(AIC[2])
    }
  } else {
    return(AIC[1])
  }
}


#Custom tidier for chngpt models. Turns output into dataframe.  Warning: this is not very reusable code.
tidy.chngpt <- function(chngpt.mod) {
  df <- bind_cols(
    chngpt.mod$coefficients[1:2] %>%
      t() %>%
      as_tibble(),
    summary(chngpt.mod)$chngpt %>% 
      set_names(c("chngpt", "chngpt.lower.CI", "chngpt.upper.CI")) %>%
      t() %>% 
      as_tibble()
  ) %>% 
    add_column(coef.p.value = summary(chngpt.mod)$coefficients[2,2])
  return(df)
}





# #Custom tidier for chngpt models. Turns output into dataframe.  Warning: this is not very reusable code.
# tidy.chngpt <- function(chngpt.mod) {
#   df <- bind_cols(
#     chngpt.mod$coefficients[1:2] %>% 
#       t() %>% 
#       as_tibble(),
#     summary(chngpt.mod)$chngpt %>% 
#       set_names(c("chngpt", "chngpt.lower.CI", "chngpt.upper.CI")) %>%
#       t() %>% 
#       as_tibble()
#   )
#   return(df)
# }