# CODED BY CAMILO BRANDAO-DE-RESENDE

#--------------------------------------------------------------------------------------
#-------------------------------- DATA ENTRY ------------------------------------------
#--------------------------------------------------------------------------------------
rm(list=ls());  #clean variables
options(warn=-1)


folder = paste(getwd(),'/Example/',sep='')     # Folder containing images to be analyzed
MCHC= 32        # Mean corpuscular hemoglobin concentration obtained from a complete blood count
n_cells=30      #Maximum number of cells to be analyzed within the folder


#--------------------------------------------------------------------------------------
#----------------------------------PARAMETERS -----------------------------------------
#--------------------------------------------------------------------------------------
delta_n = 0.002*MCHC
n_ob = 1.51
pixelsize= 0.098   #micrometers
k0 = 2 * pi / 600

#--------------------------------------------------------------------------------------
#------------------------------- CREATE RESULTS FOLDER --------------------------------
#--------------------------------------------------------------------------------------
dir.create(paste(folder,'results',sep=''))


#--------------------------------------------------------------------------------------
##--------------------------- LOAD FUNCTIONS ------------------------------------------
#--------------------------------------------------------------------------------------

source(paste(getwd(),'/functions_dm.R',sep=''))




#--------------------------------------------------------------------------------------
#--------------------------------- IMAGE PROCESSING -----------------------------------
#--------------------------------------------------------------------------------------

start_time <- Sys.time()


k=1
result = processimage(k, folder)
resultssummary = result$resultssummary
cellresults = result$results
summeanH = result$meanH
allpeaks = result$peaks
print(paste('cell # ',k))


for (k in 2:n_cells){
  result = processimage(k, folder)
  resultssummary = rbind(resultssummary,result$resultssummary)
  cellresults = rbind(cellresults,result$results)
  summeanH = summeanH + result$meanH
  allpeaks = rbind(allpeaks,result$peaks)
  print(paste('cell # ',k))
  
  if(!file.exists(paste(folder,'H',(k+1),' Z2.tif',sep=''))) break
}

write.table(resultssummary, paste(folder,'results/','resultssummary.txt',sep=''), sep="\t", dec = ".",row.names = F, col.names = TRUE)
write.table(cellresults, paste(folder,'results/','results.txt',sep=''), sep="\t", dec = ".",row.names = F, col.names = TRUE)

avg_results = getaverageresults(cellresults,resultssummary)
write.table(avg_results$avg_results, paste(folder,'results/','avg_results.txt',sep=''), sep="\t", dec = ".",row.names = F, col.names = TRUE)

meanH = avg_results$meanH
meanpeaks = colMeans(allpeaks)
ci_results = avg_results$ci_results
avg_results = avg_results$avg_results

plotmeancell(meanH,meanpeaks,avg_results,ci_results,folder)

end_time <- Sys.time()
end_time - start_time
