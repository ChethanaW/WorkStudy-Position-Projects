# WorkStudy Position Projects

York University ITEC department get the Academic Program Report in the pdf format and hard no tool to analyse its data.

I with the guidence of my supervisor created a Direct Visualization Tool to meet their needs in analysing the report.

First I created a small R script to extract the pdf(password protected) information into excel sheet. 

##########################################

library(pdftools)

library(tidyverse)

pdf_file <- "E:/Project3/secure_pdf.pdf"

txt <- pdf_text(pdf_file,upw = getPass(msg = "PASSWORD: ", noblank = FALSE, forcemask = FALSE) ) %>% readr::read_lines()

wd <- getwd()
#write the file to the working directory as a text file 

#write the file to the working directory as a text file

file_name <- paste0(wd, "/", "temp.txt")

write(txt, file = file_name, sep = "\t")

txt.grass <-my_data[-c(1:3,6:8,20:35)]

f<- as.data.frame(txt.grass)

#read from the text file the information that could be extracted and clean later

write.xlsx(txt, "E:/Project3/check.xlsx")

#############################################



Then did some data cleaning to help in the process of analysing.

I cannot share the data used for this app due to confidential issues.

But below is screen shots of the app run with some dummy data for demostration purposes.

![image](https://user-images.githubusercontent.com/34969881/127928396-1c26c8d6-57f7-4844-8835-a0e1af2d266b.png)

![Capture0](https://user-images.githubusercontent.com/34969881/127928961-2c60fc5d-8a18-478f-8e27-2654397246dc.PNG)

![Capture](https://user-images.githubusercontent.com/34969881/127929028-f1bc00fe-d866-45b8-9662-9da1f993bc40.PNG)

![Capture2](https://user-images.githubusercontent.com/34969881/127929039-3887f767-34c8-4048-8d96-f25265c4ef41.PNG)

![Capture3](https://user-images.githubusercontent.com/34969881/127929045-fc1c0a1d-5cb3-4bea-b492-610e835cde0d.PNG)

I have also created Short Manual on How to use the new App to Visualize AP Report Data
