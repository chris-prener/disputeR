devtools::load_all()

mydir <- function(path){

  ## check inputs with disputeR
  dis_not_missing(.f = rlang::is_missing(path))
  dis_dir(x = path, error = FALSE)

}

mydir(path = "ham/")

mydir(path = "toast/eggs/")
