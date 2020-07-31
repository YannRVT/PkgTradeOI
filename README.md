# TradeOI package

Welcome on the ITC package/application developed by the Trade Market Intelligence (TMI) section.$

This package has been create to save and protect different functions for using TradeOI shiny app.

There are several parts in this application.

* The first is the import of your data (trade data, product table and revision table).

* A second part with the parameters you will have to determine to calculate what you need.
    * Unstable HS revision
    * Delete heterogeneous series
    * Compute indices
    * Detect outliers and choose what you want to do with them (delete, keep, replace by different methods)


* Then one last one with the results of your request in the form of an exportable data table and graph.
    * Different tables: summary table, outliers table, heterogeneous table, indices table

There are many explanations on how the application works.



There are also some functions to use correctly the standalone version of TradeOI Shiny app.

## Getting Started

### Prerequisites

You need R (>= 2.10) and RStudio.

### Installing

#### Step 1: Install the devtools package

To install a TradeOI package, start by installing the devtools package. The best way to do this is from CRAN, by typing:

```
install.packages("devtools")
```

#### Step 2: Install the package TradeOI from GitHub

```
library(devtools)
install_github("YannRVT/TradeOI")
```

#### Step 3: Load the package TradeOI

```
library(TradeOI)
```

#### Step 4: Launch the app

```
TradeOI::run_app()
```

## Built With

* [shiny](https://cran.r-project.org/web/packages/shiny/index.html) - Web Application Framework for R
* [shinydashboard](https://cran.r-project.org/web/packages/shinydashboard/index.html) - Create Dashboards with 'Shiny' 
* [shinyjs](https://cran.r-project.org/web/packages/shinyjs/index.html) - Easily Improve the User Experience of Your Shiny Apps in Seconds
* [shinyalert](https://cran.r-project.org/web/packages/shinyalert/index.html) - Easily Create Pretty Popup Messages (Modals) in 'Shiny'
* [shinycssloaders](https://cran.r-project.org/web/packages/shinycssloaders/index.html) - Add Loading Animations to a 'shiny' Output While It's Recalculating
* [filehash](https://cran.r-project.org/web/packages/filehash/index.html) - Simple Key-Value Database
* [sqldf](https://cran.r-project.org/web/packages/sqldf/index.html) - Manipulate R Data Frames Using SQL
* [stringr](https://cran.r-project.org/web/packages/stringr/index.html) - Simple, Consistent Wrappers for Common String Operations
* [stringi](https://cran.r-project.org/web/packages/stringi/index.html) - Character String Processing Facilities
* [parallel](https://www.rdocumentation.org/packages/parallel/versions/3.6.2) - Support for Parallel computation in R
* [robustbase](https://cran.r-project.org/web/packages/robustbase/index.html) - Basic Robust Statistics
* [data.table](https://cran.r-project.org/web/packages/data.table/index.html) - Extension of 'data.frame'
* [doParallel](https://cran.r-project.org/web/packages/doParallel/index.html) - Foreach Parallel Adaptor for the 'parallel' Package
* [gsubfn](https://cran.r-project.org/web/packages/gsubfn/index.html) - Utilities for Strings and Function Arguments
* [proto](https://cran.r-project.org/web/packages/proto/index.html) - Prototype Object-Based Programming
* [RSQLite](https://cran.r-project.org/web/packages/RSQLite/index.html) - 'SQLite' Interface for R
* [foreach](https://cran.r-project.org/web/packages/foreach/index.html) - Provides Foreach Looping Construct
* [iterators](https://cran.r-project.org/web/packages/iterators/index.html) - Provides Iterator Construct
* [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html) - A Grammar of Data Manipulation
* [optiRum](https://cran.r-project.org/web/packages/optiRum/index.html) - Financial Functions & More
* [compiler](https://www.rdocumentation.org/packages/compiler/versions/3.6.2) - Byte code compiler for R.
* [DT](https://cran.r-project.org/web/packages/DT/index.html) - A Wrapper of the JavaScript Library 'DataTables'
* [highcharter](https://cran.r-project.org/web/packages/highcharter/index.html) - A Wrapper for the 'Highcharts' Library

## Contributing

Please read [DESCRIPTION.md](https://github.com/YannRVT/PkgTradeOI/blob/master/TradeOI/DESCRIPTION) for details on our code of conduct, and the process for submitting pull requests to us.

## Authors

* **Yann Rivallant** - *Initial work* - [Yann RVT](https://github.com/YannRVT)
* **Christophe Durand** - *Manager*

See also the list (below) of contributors who participated in this project:
* Christian Delachenal - ITC Senior Market Analyst

## License

This project is licensed under the GPL-3
