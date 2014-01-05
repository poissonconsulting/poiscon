#' @title Knit models
#'
#' @description
#' Takes all models in r files in working directory 
#' with 'models-' in name and prints model code in markdown
#' format together with a table of a description of the parameters.
#' 
#' @param replacement an optional named character vector which specifies replacement
#' names for directories as well as the order to print the estimates.
#' @return Knits model code and parameter descriptions.
#' @export
knit_models <- function (replacement) {
  
  files <- list.files(pattern = "^models-.*[.][rR]$")
  
  titles <- substr(files, 8, 50)
  titles <- substr(titles, 1, nchar(titles) - 2)
  
  if(!is.null(replacement)) {
    order <- format(1:length(replacement), width = 4)
    names(order) <- names(replacement)
    
    sort <- titles
    
    for(i in seq_along(titles)) {
      for (j in seq_along(order)) {
        sort[i] <- str_replace_all(sort[i], names(order)[j], order[j])
      }
    }
    files <- files[order(sort)]
    titles <- titles[order(sort)]
  }
  
  for (i in seq_along(replacement))
    titles <- str_replace_all(titles, names(replacement)[i], replacement[i])
  
  titles <- gsub("(^|[[:space:]|-])([[:alpha:]])", "\\1\\U\\2", titles,
                 perl=TRUE)
  
  kdescription <- function (description) {
    desc <- description
    desc <- desc[order(names(desc))]
    mat <- matrix(NA, nrow = length(desc), ncol = 2)
    mat[,1] <- names(desc)
    mat[,2] <- desc
    colnames(mat) <- c("Variable/Parameter","Description")
    kable(mat)
  }
  
  for (i in seq_along(files)) {
    
    file <- files[i]
    title <- titles[i]
    
    cat(c("\n###"," ",title,"\n"))
    
    description <- NULL
    models <- NULL
    
    source(file)
    
    if (!is.null(description))
      kdescription(description)
    
    if (!is.null(models)) {
      
      for (i in 1:nmodels(models)) {
        cat(c("\n#### ",title," - Model",i,"\n"))
        
        cat("\n\n")
        
        cat("```\n")
        
        cat(model_code(models)) 
        
        cat("\n```\n")
      }     
    }
    
    invisible ()
  }
}
