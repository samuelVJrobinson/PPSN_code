#Written to remove all csv, png, tif, and aux files after an update in the cleaning process

#Note: be extremely careful when running this. This removes all cleaned yield data and associated files, with no chance of undoing it (i.e. no Recycle Bin option)

#Get all csv files
csvpaths <- list.files('.',pattern='*.csv$',recursive = TRUE)

csvpaths <- csvpaths[grepl('^202',csvpaths)]
csvpaths <- csvpaths[grepl('/clean/',csvpaths)] #Choose only from /clean/ dir
csvpaths <- csvpaths[!grepl('^20220[1-5]',csvpaths)] #Exclude 202201-202205
file.remove(csvpaths)

#Get all png files
pngpaths <- list.files('.',pattern='*.png$',recursive = TRUE)
pngpaths <- pngpaths[grepl('^202',pngpaths)]
pngpaths <- pngpaths[grepl('/clean/',pngpaths)] #Choose only from /clean/ dir
pngpaths <- pngpaths[!grepl('^20220[1-5]',pngpaths)] #Exclude 202201-202205
file.remove(pngpaths)

#Get all tif files
tifpaths <- list.files('.',pattern='*.tif$',recursive = TRUE)
tifpaths <- tifpaths[grepl('^202',tifpaths)]
tifpaths <- tifpaths[grepl('/rasters/',tifpaths)] #Choose only from /rasters/ dir
file.remove(tifpaths)

#Get all tif aux files
tifpaths <- list.files('.',pattern='*.tif.aux',recursive = TRUE)
tifpaths <- tifpaths[grepl('^202',tifpaths)]
tifpaths <- tifpaths[grepl('/rasters/',tifpaths)] #Choose only from /rasters/ dir
file.remove(tifpaths)

