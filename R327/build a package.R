library(devtools)
?create
create("cdx")


document("cdx")#produce the Rd file for functions  
check("cdx")#copy from http://pages.stat.wisc.edu/~jgillett/327-3/2package/jgUtilities/DESCRIPTION

build("cdx")  

install.packages("/Users/CDX/cdx_0.1.tar.gz",repos = NULL,type = "source")
library(cdx)


