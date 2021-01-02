app <- ShinyDriver$new("../../")
app$snapshotInit("test1")

app$uploadFile(dat = "test_datasets/kodama_prepped.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(yi_name = "yi")
app$setInputs(vi_name = "vi")
app$setInputs(alpha_select = 0.5)
app$setInputs(analyzeClick = "click")
app$setInputs(plotClick = "click")
app$snapshot()
app$snapshot()
