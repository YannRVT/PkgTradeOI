tabItem(tabName = "datapreparation",
        fluidPage(
          box(title = h4("Import your data", style = "color:#0999DC"), height = NULL, 

                      fileInput("myData", label = ("Input trade dataset"),accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
              radioButtons('sepUI1','Separator',c(Comma=',',Semicolon=';',Tab='\t', Space=" "), selected = ',', inline = TRUE),
              checkboxInput('headerUI1','Header',TRUE),
              hr(),
                     
                      fileInput("product_table", label = ("Product Table"),accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
              radioButtons('sepUI2','Separator',c(Comma=',',Semicolon=';',Tab='\t', Space=" "), selected = ',', inline = TRUE),
              checkboxInput('headerUI2','Header',TRUE),
              hr(),
                         
                      fileInput("revision_table", label = ("Revision Table"),accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
              radioButtons('sepUI3','Separator',c(Comma=',',Semicolon=';',Tab='\t', Space=" "), selected = ',', inline = TRUE),
              checkboxInput('headerUI3','Header',TRUE)
                    
          ),

          box(title = h4("Variable's names", style = "color:#0999DC"), height = NULL,
              
              uiOutput("responseUI1"),
              uiOutput("responseUI2"),
              uiOutput("responseUI3"),
              uiOutput("responseUI4"),
              uiOutput("responseUI5"),
              uiOutput("responseUI6"),
              uiOutput("responseUI7"),
              hr(),
              uiOutput("responseUI8"),
              uiOutput("responseUI9"),
              hr(),
              uiOutput("explanatoryUI"),
              uiOutput("selectDeselectUI")
            )
          )
        )