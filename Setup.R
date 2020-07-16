#Creation de base
if(!require(devtools)){install.packages("devtools")}
if(!require(usethis)){install.packages("usethis")}
if(!require(roxygen2)){install.packages("roxygen2")}

mydir <- "/Users/yannrivallant/Documents/Package/PkgTradeOI/"
mypackage <- "TradeOI"
path <- file.path(mydir, mypackage)
unlink(path, recursive=TRUE)

my_description<-list("Title" = "TradeOI R package",
                     "Version" ="1.0",
                     "Authors@R"= "person('Yann', 'Rivallant', email = 'yrivallant@intracen.org', role = c('aut', 'cre'))",
                     "Description" = "Some functions to compile correctly the TradeOI app.",
                     "License" = "GPL-3"
)

create_package(path, my_description, open=FALSE)

