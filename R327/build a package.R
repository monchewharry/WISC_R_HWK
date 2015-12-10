library(devtools)
?create
create("cdx")


document("cdx")#produce the Rd file for functions  
check("robust")#copy from http://pages.stat.wisc.edu/~jgillett/327-3/2package/jgUtilities/DESCRIPTION

build("robust")  

install.packages("/Users/CDX/robust_0.1.tar.gz",repos = NULL,type = "source")
library(robust)


