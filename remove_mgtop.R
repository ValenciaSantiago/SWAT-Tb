##### This r- script removes all the managment operation for trees, perennials
### and annuals landcover classes

filenames<-dir(getwd(),pattern=".mgt") # listing all .mgt files
remove.mgt <- function(i) {
  mgtfile<-file(filenames[i],"r+")
  text<-readLines(mgtfile)
  
  close(mgtfile)
  ## select managment files for the specified landuse CODES
  landuse<-str_detect(text[1],c("RFTT","FRSE","RNGE","RNGB","WETL","WETF"))
  if(any(landuse)==TRUE){
    # remove mamagement operations when TRUE
    text[29] <- "              0    | NROT: number of years of rotation"  # chnage NROT to 0
    text[31] <- " "
    text[32] <- " "
    text[33] <- " "
    text[34] <- " "
    text[35] <- " "
    
    mgtfile<-file(filenames[i],"w+")
    writeLines(text,mgtfile)
    close(mgtfile)}
  
}

sapply(1:length(filenames),remove.mgt)