#######################################################################################
#Script to add trasnition months from two dry to wet seasons to *.sub files for SWAT-Tb model
#######################################################################################

#Set path to your SWAT project 
setwd("")

#Get list with all sub-files
filenames<-dir(getwd(),pattern=".sub")
sub.tot<-length(filenames) # total number of *.sub files in the project
#Add two more lines with the new parameters to each *.sub-files
add.SOS <- function(i) {
  subfile<-file(filenames[i],"r+")
  text<-readLines(subfile)
  close(subfile)
  
  text[16:(length(text)+2)] <- text[14:length(text)]
  text[14] <- "              3    | SOS1: Starting month of transition to first wet season " 
  text[15] <- "              4    | SOS2: Ending month of transition to first wet season"
  text[16] <- "              6    | SOS3: Ending of the first wet season and the starting of the second dry season"
  text[17] <- "              10   | SOS4: Starting month of transition to second wet season"
  text[18] <- "              11   | SOS5: Ending month of transition to second wet season"  
  subfile<-file(filenames[i],"w+")
  writeLines(text,subfile)
  close(subfile)
}

sapply(1:sub.tot,add.SOS)

########### SV check one mgt file to see lines that will be modified
mgtfile <- file("000070001.sub","r+")
text <- readLines(mgtfile)
text

               
               
			   
              
              	