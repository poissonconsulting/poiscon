

parallel <- function (processes = 3) {
  if (.Platform$OS.type == ) {
    doMC::registerDoMC(3)    
  } else {
    cl <- doSNOW::makeCluster(3)
    doSNOW::registerDoSNOW(cl)     
  }
  invisible(processes)
}
