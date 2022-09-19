
# method 1
IGSQ<- as.data.frame(listB[["QS33"]])
IGSQ <- IGSQ %>% mutate_at(.vars = vars(13:16),
                           .funs = function(x)str_replace_all(x, "0 times in the week","1"))

# method 2
IGSQ <- IGSQ %>% 
mutate_at(.vars = vars(13:14),
          .funs =function(x)ifelse(grepl("0 times",x),1,x))

