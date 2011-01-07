#
# Create a workspace with all the package functions.
# Having a workspace makes it easier to develop and debug the functions.
#
# use this to obtain gradientForest script files
#
# Empty the global environment and delete the temporary COML directory if needed
remove(list=ls())
require(gradientForest)
shell("rm -fr COML")

# Copy the gradientForest environment to the global environment
envir <- environment(gradientForest::gradientForest)
for (i in ls(env=envir))
  assign(i, get(i,env=envir))
detach("package:gradientForest")
  
# Create a package from all the files
package.skeleton("COML")

# use this to create a workspace version of package
setwd('COML/R')
files <- shell("ls *.[rR] ",intern=T)
for (f in files) source(f)
rm(f,files)
setwd("../..")
save.image("gradientForest.RData")
