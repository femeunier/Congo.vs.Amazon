write.Caret.script <- function(dir.name,
                               file.name,
                               config.file){

  file <- file.path(dir.name,file.name)

  writeLines("rm(list = ls())",con = file)
  write("",file=file,append=TRUE)
  write("Congo.vs.Amazon::load.everything()",file=file,append=TRUE)
  write("",file=file,append=TRUE)

  write(paste0("run.Caret.IFL(","\"",
               config.file,"\"",
               ")"),file=file,append=TRUE)


}
