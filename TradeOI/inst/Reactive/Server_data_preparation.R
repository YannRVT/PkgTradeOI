#reactive object, responsible for loading the main data
####################################################
##On importe la table des produits
rawInputData = reactive({
  rawData = input$myData
  headerTag = input$headerUI1;
  sepTag = input$sepUI1;

  if(!is.null(rawData)) {
    myData = fread(rawData$datapath, sep = sepTag, stringsAsFactors=FALSE, header = headerTag, na.strings=".", dec=".", colClasses = "character")
    return(as.data.frame(myData))
  } else {
    return(NULL);
  }
});

##On importe la table des produits
rawInputData2 = reactive({
  rawData = input$product_table
  headerTag = input$headerUI2;
  sepTag = input$sepUI2;

  if(!is.null(rawData)) {
    product_table = fread(rawData$datapath, header=headerTag, sep=sepTag, na.strings=".", dec=".", stringsAsFactors=FALSE, colClasses= "character")
    return(product_table)
  } else {
    return(NULL);
  }
});

##On importe la table des revisions
rawInputData3 = reactive({
  rawData = input$revision_table
  headerTag = input$headerUI3;
  sepTag = input$sepUI3;
  
  if(!is.null(rawData)) {
    revision_table = fread(rawData$datapath, header=headerTag, sep=sepTag, na.strings=".", dec=".", stringsAsFactors=FALSE, colClasses= "character")
    return(revision_table)
  } else {
    return(NULL);
  }
})