# scrapEU

### Description 
`scRapEU` is an R function that can be used to automatically download data about procedure files available on the European Union's Legislative Observatory (OEIL). 
It downloads most of the information available on the procedure page (see below), complements it with data from the EUR-Lex website, and stores it in a dataframe that can subsequently be saved by the user in the preferred format. 


### Usage
The scraper can be accessed directly from the R workspace.
To use the function, make sure that the following R packages are installed and updated: `rvest`, `stringr`, `rlang`, `plyr`, `dplyr`, and `modelr`. 
To import the latest version of the function in your R workspace, run the following line of code:

`source("https://mscottodivettimo.github.io/scrapeu/scRapEU.R")`

A note will appear to acknowledge the successful execution of the code, and will give you more information on how to use the web scraper. 

### Additional information
Additional information such as a documentation and a minimal report on the data that could be scraped is available at https://mscottodivettimo.github.io/project/scrapeu/.

The raw code is publicly accessible and free to download, adapt, and use. However, I would be grateful if you could acknowledge the use of the original code.
