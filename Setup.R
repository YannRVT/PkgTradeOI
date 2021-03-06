#Creation de base
if(!require(devtools)){install.packages("devtools")}
if(!require(usethis)){install.packages("usethis")}
if(!require(roxygen2)){install.packages("roxygen2")}

mydir <- "/Users/yannrivallant/Documents/Package/PkgTradeOI"
mypackage <- "TradeOI"
path <- file.path(mydir, mypackage)

my_description<-list("Title" = "TradeOI R package",
                     "Version" ="1.0",
                     "Authors@R"= "person('Yann', 'Rivallant', email = 'yrivallant@intracen.org', role = c('aut', 'cre'))",
                     "Description" = "Some functions to compile correctly the TradeOI app.",
                     "License" = "GPL-3"
)

create_package(path, my_description, open=FALSE)

#Ajout data
product<-read.csv("/Users/yannrivallant/Documents/ITC/TradeOI/02-données/product.csv", header=TRUE)
revision<-read.csv("/Users/yannrivallant/Documents/ITC/TradeOI/02-données/revision.csv", header=TRUE)
trade<-read.csv("/Users/yannrivallant/Documents/ITC/TradeOI/02-données/trade.csv", header=TRUE, sep = ";")
setwd("/Users/yannrivallant/Documents/Package/PkgTradeOI/TradeOI/")
usethis::use_data(product, overwrite = TRUE)
usethis::use_data(revision, overwrite = TRUE)
usethis::use_data(trade, overwrite = TRUE)

#generer la documentation
setwd(path)
document()

#Compiler et installer

setwd(path)
build(, quiet=T)
install()

library(TradeOI)

TradeOI::run_app()
