# defocusing_microscopy_R
Code to automatically analyse DM images and get morphological and mechanical parameters from erythrocytes.

How to use:

Required R v 3.6.3 and the following packages:
install.packages(c("pixmap", "tiff", "plotrix", "pracma", "fields", "stats", "graphics", "grDevices", "utils", "datasets", "methods", "base", "mrbsizeR"))

Both image_processing_dm.R and functions_dm.R should be in the same folder

Create a folder with the videos to be analysed (An example is provided in folder /Example):
- 256 x 256 pixels
- background video (background.tiff)
- 3 videos for each cell (HN ZJ.tiff, where N is the cell number and J is the defocusing diastance, e.g. H1 Z0.tiff, H1 Z2.tiff, and H1 Z4.tiff)

Execute the code in image_processing_dm.R:
- Put all images to be analysed in the same folder (put path in DATA ENTRY)
- Also set the mean corpuscular hemoglobin concentration (MCHC) obtained from a complete blood count and the maximum number of cells to be analyzed within the folder.
- Other parameters are expained in the paper and in the thesis (https://repositorio.ufmg.br/bitstream/1843/38353/1/TESE%20CAMILO%20-%20FINAL.pdf).

A folder named '/results' will be generated in the same folder as the original folder containing images.
