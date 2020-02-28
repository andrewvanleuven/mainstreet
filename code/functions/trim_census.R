trim_census <- function(x) {
  x %>% mutate_if(is.character, str_replace_all, pattern = " city", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " CDP", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " village", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " borough", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " town", replacement = "") %>% 
    mutate_if(is.character, str_trim)
  }