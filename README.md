# Basics-of-Microbiome-Data-Analysis

This code is a primer on basic microbiota analysis methods as well as a
primer for anyone who is new to R. We will go over basic R functions,
rarefaction of microbiota count data, alpha diversity, beta diversity,
negative binomial regression, as well as the associated plots, tables,
and statistics.

## Things you’ll need

-   Base R: <https://www.r-project.org/>
-   R Studio: <https://rstudio.com/products/rstudio/download/>
-   The ARCHBG and Wheat data

# 0) Author’s note 2023-12-11

-   Hello! Welcome to my first tutorial on the basics of microbiome data
    analysis! This is a raw upload of a workshop tutorial I wrote while
    finishing my grad program at MSU and was not built with github in
    mind. BUT! I wanted to get my code uploaded so I have a few
    repositories to practice working with as I make updates and
    improvements to these files. If you have any suggestions, please let
    me know!

# 1) Introduction

-   This code is meant to be a primer on R code that is specifically
    geared towards how to implement and understand the Comstock lab’s
    microbiota analysis methods. There are many things I will not cover
    here, R code that I don’t really understand fully and things that I
    just won’t explain very well. For all of these things (and more!),
    there is always Google.

-   The data used here was used in the ARCH/BABY gut manuscript (doi:
    10.1371/journal.pone.0213733). The data and code used for that paper
    can be found in the supplementary materials section.

    -   Briefly, this is fecal microbiota data from pregnant women
        (Trimester 3) and their children (between a few days to 3 months
        of age). The main outcomes we were looking at were the effects
        maternal pre-pregnancy BMI, breastfeeding and delivery mode had
        on the microbiota.
    -   The full analysis code can be found online, but here we will
        only work with the infant data.

-   All data used here was produced after processing raw sequence data
    using mothur. You can find more information here
    <https://www.mothur.org/wiki/MiSeq_SOP>

    -   Note that the data used here is the output produced from the
        “Phylotypes” section of the mothur SOP.

# 2) Installing R Packages

-   Let’s start by installing the packages you’ll need for this.
    -   Packages in R are meant to make your job easier. They are
        pre-written lines of code meant to do a specific job without you
        needing to code (or fully understand) how to implement your
        method of interest.
    -   To do this, we use the function install.packages() with the
        package name (in quotations) inserted between the parentheses

``` r
install.packages("vegan")
install.packages("lubridate")
install.packages("tidyr")
install.packages("MASS")
install.packages("car")
install.packages("dunn.test")
install.packages("ggplot2")
```

-   Packages are written by members of the R community (i.e., people
    like yourself), so some caution should be used when trying out
    random packages with little/no documentation.
    -   You can find the documentation for any package in R (in this
        case the vegan package) like so:

``` r
help(package=vegan)
```

# 3) Importing OTU data

-   Before we get to the data, let’s make sure R knows to use the
    packages we just installed.

``` r
require(vegan)
require(lubridate)
require(tidyr)
require(MASS)
require(car)
require(dunn.test)
require(ggplot2)
```

-   Cool. Now R is ready to use the functions in these packages. Let’s
    get to reading our data in.
    -   There are many different file types that R can read and import.
        The ones I usually use are .txt and .csv files.
        -   For .txt files (or related file types), use the function
            read.table(); for .csv files, use read.csv().
    -   R can read excel files (.xlsx, .xls, etc.), but you need to use
        a function that can be found in some package I never use.
-   Anyway, let’s read in the raw OTU data that we get after processing
    our sequences in mothur using read.table().
-   We will assign this data the variable name “Data.Raw” using the
    “\<-” operation, like so:

``` r
knitr::opts_knit$set(root.dir = "C:/Users/ksugino/Desktop/Github_projects/Basics-of-Microbiome-Data-Analysis/Data/")
```

``` r
setwd('C:/Users/ksugino/Desktop/Github_projects/Basics-of-Microbiome-Data-Analysis/Data/')
Data.Raw<-read.table('otu_table.txt',fill=TRUE,header=TRUE)
```

-   setwd() tells R where your data is. Here, I’m specifying that R
    should look in the folder “20200122_Workshop_Code” and use that
    folder as its directory.
    -   Here, I need to specify it multiple times because of how I am
        writing this workshop. You will only need to specify this
        command once at the beginning of your code.
-   The next line reads in the data table from the directory I specified
    above, so it’s looking for the file names “otu_table.txt” in the
    folder “20200122_Workshop_Code”.
    -   “fill” is making sure R inputs our matrix correctly by filling
        in any empty spaces that might be in our file.
    -   “header” tells R that our file has a header.
-   Let’s look at the first 6 rows and 10 columns of the data

``` r
Data.Raw[c(1:6),c(1:10)]
```

    ##   label Group numOtus Otu001 Otu002 Otu003 Otu004 Otu005 Otu006 Otu007
    ## 1     1  1-1w     439   3424     31    513   1899     21     30      3
    ## 2     1   1-M     439     21  18571   3599   2905      5     12   2370
    ## 3     1  11-M     439     12  31592   1410  16013   9194     33   3111
    ## 4     1 14-1w     439  13554   9736    703     32  21752  12676   9159
    ## 5     1  14-M     439    749  19053    614   2927  25096     24   1181
    ## 6     1 16-1w     439     18     45      5     12     19 100624      4

-   The first three columns contain info on the “label”, “Group” and
    “numOtus”, while the rest of the columns contain the number of reads
    (i.e. counts) for each OTU. Briefly, an OTU stands for “Operational
    Taxonomic Unit” and I will be using it to refer to the bacterial
    counts in our dataset.
    -   The label column tells you what taxonomic level you are looking
        at
        -   This dataset contains the OTU information from Kingdom to
            Genus
    -   Group tells you the sample info.
        -   In this case, our IDs are labeled as “ID#-timepoint”, where
            “1w” are infant samples and “M” are the maternal samples.
    -   numOtus is just the number of OTU columns there are in this
        dataset. It’s telling us that there are 439 uniquely identified
        OTUS
        -   You can tell which rows contain which taxonomic level by the
            column named “label”
        -   A value of 1 = genus, 2 = family, and up to 5 = phylum.
-   So, before we go on, let me explain R syntax when working with
    matrices.
    -   You may have noticed in the previous chunk of code, there was a
        portion that looks like this:

``` r
Data.Raw[c(1:6),c(1:10)]
```

-   What we’re doing here is first calling the data we imported by
    stating “Data.Raw”, but only the first 6 rows with “\[c(1:6),\]” and
    only the first 10 columns with “\[,c(1:10)\]”.
    -   Together, we ask R to find the first 6 rows and 10 columns like
        so: \[c(1:6),c(1:10)\]
-   In R, you can specify which rows/columns you are interested in by
    using brackets –\> \[\]
    -   Within the brackets, you can place a comma and specify rows by
        putting values to the left of the comma and columns by placing
        values to the right of the comma.
    -   The “c” used in “c(1:10)” is telling R that there’s a group of
        numbers that should be read together (so if you ever specify
        more than 1 number, use c() ).
        -   In this case, the colon “:” tells R we want all numbers
            between 1 and 10.
    -   You can use the same method to remove specific columns instead,
        by adding a “-” to the front (e.g., Data.Raw\[,-c(1:10)\] would
        remove the first 10 columns and keep the rest)
-   For example, if you want to only look at row 1 and columns 1, 4, 5
    and 6 in Data.Raw, you would write:

``` r
Data.Raw[1,c(1,4:6)]
```

    ##   label Otu001 Otu002 Otu003
    ## 1     1   3424     31    513

-   You can also call columns in a matrix by their name by using “$”. It
    will give you back a list of all the contents of the column, like
    so:

``` r
Data.Raw$Group
```

    ##   [1] "1-1w"    "1-M"     "11-M"    "14-1w"   "14-M"    "16-1w"   "16-M"   
    ##   [8] "17-1w"   "17-M"    "18-1w"   "18-M"    "19-1wb"  "19-M"    "2-M"    
    ##  [15] "20-1w"   "20-M"    "21-1w"   "21-M"    "22-1w"   "22-M"    "23-1w"  
    ##  [22] "23-M"    "24-1w"   "25-M"    "26-1wb"  "26-M"    "27-1w"   "27-M"   
    ##  [29] "28-1w"   "28-M"    "29-1wb"  "29-M"    "3-1w"    "3-M"     "30-1w"  
    ##  [36] "30-M"    "33-1w"   "33-M"    "34-1wb"  "34-M"    "36-1w"   "36-M"   
    ##  [43] "37-1w"   "37-M"    "38-1w"   "38-M"    "39-1w"   "39-M"    "40-1w"  
    ##  [50] "40-M"    "41-1w"   "41-M"    "42-1w"   "42-M"    "43-1w"   "43-M"   
    ##  [57] "44-M"    "44T1-1w" "44T2-1w" "45-1w"   "45-M"    "46-1w"   "46-M"   
    ##  [64] "48-1w"   "49-1w"   "49-M"    "50-1w"   "51-1w"   "51-M"    "52-1w"  
    ##  [71] "52-M"    "55-1w"   "55-M"    "56-1wb"  "56-M"    "57-1w"   "57-M"   
    ##  [78] "60-1w"   "60-M"    "62-1w"   "62-M"    "63-1w"   "63-M"    "64-1w"  
    ##  [85] "64-M"    "1-1w"    "1-M"     "11-M"    "14-1w"   "14-M"    "16-1w"  
    ##  [92] "16-M"    "17-1w"   "17-M"    "18-1w"   "18-M"    "19-1wb"  "19-M"   
    ##  [99] "2-M"     "20-1w"   "20-M"    "21-1w"   "21-M"    "22-1w"   "22-M"   
    ## [106] "23-1w"   "23-M"    "24-1w"   "25-M"    "26-1wb"  "26-M"    "27-1w"  
    ## [113] "27-M"    "28-1w"   "28-M"    "29-1wb"  "29-M"    "3-1w"    "3-M"    
    ## [120] "30-1w"   "30-M"    "33-1w"   "33-M"    "34-1wb"  "34-M"    "36-1w"  
    ## [127] "36-M"    "37-1w"   "37-M"    "38-1w"   "38-M"    "39-1w"   "39-M"   
    ## [134] "40-1w"   "40-M"    "41-1w"   "41-M"    "42-1w"   "42-M"    "43-1w"  
    ## [141] "43-M"    "44-M"    "44T1-1w" "44T2-1w" "45-1w"   "45-M"    "46-1w"  
    ## [148] "46-M"    "48-1w"   "49-1w"   "49-M"    "50-1w"   "51-1w"   "51-M"   
    ## [155] "52-1w"   "52-M"    "55-1w"   "55-M"    "56-1wb"  "56-M"    "57-1w"  
    ## [162] "57-M"    "60-1w"   "60-M"    "62-1w"   "62-M"    "63-1w"   "63-M"   
    ## [169] "64-1w"   "64-M"    "1-1w"    "1-M"     "11-M"    "14-1w"   "14-M"   
    ## [176] "16-1w"   "16-M"    "17-1w"   "17-M"    "18-1w"   "18-M"    "19-1wb" 
    ## [183] "19-M"    "2-M"     "20-1w"   "20-M"    "21-1w"   "21-M"    "22-1w"  
    ## [190] "22-M"    "23-1w"   "23-M"    "24-1w"   "25-M"    "26-1wb"  "26-M"   
    ## [197] "27-1w"   "27-M"    "28-1w"   "28-M"    "29-1wb"  "29-M"    "3-1w"   
    ## [204] "3-M"     "30-1w"   "30-M"    "33-1w"   "33-M"    "34-1wb"  "34-M"   
    ## [211] "36-1w"   "36-M"    "37-1w"   "37-M"    "38-1w"   "38-M"    "39-1w"  
    ## [218] "39-M"    "40-1w"   "40-M"    "41-1w"   "41-M"    "42-1w"   "42-M"   
    ## [225] "43-1w"   "43-M"    "44-M"    "44T1-1w" "44T2-1w" "45-1w"   "45-M"   
    ## [232] "46-1w"   "46-M"    "48-1w"   "49-1w"   "49-M"    "50-1w"   "51-1w"  
    ## [239] "51-M"    "52-1w"   "52-M"    "55-1w"   "55-M"    "56-1wb"  "56-M"   
    ## [246] "57-1w"   "57-M"    "60-1w"   "60-M"    "62-1w"   "62-M"    "63-1w"  
    ## [253] "63-M"    "64-1w"   "64-M"    "1-1w"    "1-M"     "11-M"    "14-1w"  
    ## [260] "14-M"    "16-1w"   "16-M"    "17-1w"   "17-M"    "18-1w"   "18-M"   
    ## [267] "19-1wb"  "19-M"    "2-M"     "20-1w"   "20-M"    "21-1w"   "21-M"   
    ## [274] "22-1w"   "22-M"    "23-1w"   "23-M"    "24-1w"   "25-M"    "26-1wb" 
    ## [281] "26-M"    "27-1w"   "27-M"    "28-1w"   "28-M"    "29-1wb"  "29-M"   
    ## [288] "3-1w"    "3-M"     "30-1w"   "30-M"    "33-1w"   "33-M"    "34-1wb" 
    ## [295] "34-M"    "36-1w"   "36-M"    "37-1w"   "37-M"    "38-1w"   "38-M"   
    ## [302] "39-1w"   "39-M"    "40-1w"   "40-M"    "41-1w"   "41-M"    "42-1w"  
    ## [309] "42-M"    "43-1w"   "43-M"    "44-M"    "44T1-1w" "44T2-1w" "45-1w"  
    ## [316] "45-M"    "46-1w"   "46-M"    "48-1w"   "49-1w"   "49-M"    "50-1w"  
    ## [323] "51-1w"   "51-M"    "52-1w"   "52-M"    "55-1w"   "55-M"    "56-1wb" 
    ## [330] "56-M"    "57-1w"   "57-M"    "60-1w"   "60-M"    "62-1w"   "62-M"   
    ## [337] "63-1w"   "63-M"    "64-1w"   "64-M"    "1-1w"    "1-M"     "11-M"   
    ## [344] "14-1w"   "14-M"    "16-1w"   "16-M"    "17-1w"   "17-M"    "18-1w"  
    ## [351] "18-M"    "19-1wb"  "19-M"    "2-M"     "20-1w"   "20-M"    "21-1w"  
    ## [358] "21-M"    "22-1w"   "22-M"    "23-1w"   "23-M"    "24-1w"   "25-M"   
    ## [365] "26-1wb"  "26-M"    "27-1w"   "27-M"    "28-1w"   "28-M"    "29-1wb" 
    ## [372] "29-M"    "3-1w"    "3-M"     "30-1w"   "30-M"    "33-1w"   "33-M"   
    ## [379] "34-1wb"  "34-M"    "36-1w"   "36-M"    "37-1w"   "37-M"    "38-1w"  
    ## [386] "38-M"    "39-1w"   "39-M"    "40-1w"   "40-M"    "41-1w"   "41-M"   
    ## [393] "42-1w"   "42-M"    "43-1w"   "43-M"    "44-M"    "44T1-1w" "44T2-1w"
    ## [400] "45-1w"   "45-M"    "46-1w"   "46-M"    "48-1w"   "49-1w"   "49-M"   
    ## [407] "50-1w"   "51-1w"   "51-M"    "52-1w"   "52-M"    "55-1w"   "55-M"   
    ## [414] "56-1wb"  "56-M"    "57-1w"   "57-M"    "60-1w"   "60-M"    "62-1w"  
    ## [421] "62-M"    "63-1w"   "63-M"    "64-1w"   "64-M"    "1-1w"    "1-M"    
    ## [428] "11-M"    "14-1w"   "14-M"    "16-1w"   "16-M"    "17-1w"   "17-M"   
    ## [435] "18-1w"   "18-M"    "19-1wb"  "19-M"    "2-M"     "20-1w"   "20-M"   
    ## [442] "21-1w"   "21-M"    "22-1w"   "22-M"    "23-1w"   "23-M"    "24-1w"  
    ## [449] "25-M"    "26-1wb"  "26-M"    "27-1w"   "27-M"    "28-1w"   "28-M"   
    ## [456] "29-1wb"  "29-M"    "3-1w"    "3-M"     "30-1w"   "30-M"    "33-1w"  
    ## [463] "33-M"    "34-1wb"  "34-M"    "36-1w"   "36-M"    "37-1w"   "37-M"   
    ## [470] "38-1w"   "38-M"    "39-1w"   "39-M"    "40-1w"   "40-M"    "41-1w"  
    ## [477] "41-M"    "42-1w"   "42-M"    "43-1w"   "43-M"    "44-M"    "44T1-1w"
    ## [484] "44T2-1w" "45-1w"   "45-M"    "46-1w"   "46-M"    "48-1w"   "49-1w"  
    ## [491] "49-M"    "50-1w"   "51-1w"   "51-M"    "52-1w"   "52-M"    "55-1w"  
    ## [498] "55-M"    "56-1wb"  "56-M"    "57-1w"   "57-M"    "60-1w"   "60-M"   
    ## [505] "62-1w"   "62-M"    "63-1w"   "63-M"    "64-1w"   "64-M"

-   I’m pretty bad at explaining things like this, so I’m sure you’re
    still very confused. Sorry. You’ll get the hang of it as you use R
    more, I promise.

-   Anyway, we want the genus-level info, so we’re looking for the rows
    with a “1” under the “label” column.

``` r
Data.Raw<-Data.Raw[Data.Raw$label==1,]
```

-   Here, we are overwriting our variable name “Data.Raw” with
    “Data.Raw\<-”
    -   Next, we are calling our data “Data.Raw”
    -   Then, we are telling R that within “Data.Raw”, we want to look
        at the “label” column found in “Data.Raw” “Data.Raw$label”
    -   We want R to find values in the “label” column that are equal to
        1 “Data.Raw$label==1”
    -   Finally, we tell R to find and keep rows in “Data.Raw” where the
        “label” value is 1 “Data.Raw\[Data.Raw$label==1,\]”
-   Level==1 corresponds to the genul-level OTUs.
    -   Always use “==” when you are trying to find whether one value is
        equal to another. Using only 1 equal sign will tell R you are
        trying to set the value to the left of the equal sign to be
        equal to the value on the right (e.g., 2=3 would be you trying
        to tell R that 2 is now equivalent to 3, while 2==3 is asking R
        if 2 is equal to 3).

# 4) Subsampling your OTUS

-   Let’s take a look at how many reads this raw data has per sample:

``` r
summary(rowSums(Data.Raw[,-c(1:3)]))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   31346   88025  104517  112939  129176  271634

``` r
hist(rowSums(Data.Raw[,-c(1:3)]))
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/unnamed-chunk-7-1.png)

-   Here, we use rowSums() to get the sum of each participant’s OTUS,
    then output a summary of that data with summary() as well as a
    histogram with hist().

-   We can see that the samples have reads that range from 31,346 to
    271,643 reads. A very wide range!

-   This variation in sample reads will mess with the analyses that we
    do later. We have to subsample our OTU table so that each
    participant has the same number of reads as the rest.

    -   Subsampling (also called rarefying) is a process where you
        randomly select a pre-defined number of reads from each sample
        so that the total number of reads is equivalent for all samples.

-   Here we will use the Subsample() function:

``` r
Subsample<-function(OTU,Min=min(rowSums(OTU)),Iters=1){
  results<-matrix(0,nrow=nrow(OTU),ncol=ncol(OTU))
  pb <- txtProgressBar(min=0, max=Iters, style=3)
  for (i in 1:Iters){
    temp<-rrarefy(x=OTU, sample=Min)
    results<- temp + results
    setTxtProgressBar(pb, i)
  }
  results<-round(results/Iters)
  close(pb)
  return(results)
}
```

-   Here, I will only be going over how to use the code, not what it all
    does. If you want to know the details we can go over it later.

-   This function has three different inputs.

    -   “OTU” is how you define which OTU table you want to subsample.
    -   “Min” tells the code to pick a defined number of reads per
        sample.
    -   “Iters” makes the code perform the previous subsampling step
        several times, the code then takes the average of all the
        iterations and rounds to the nearest whole number.

``` r
RawAbun<-Data.Raw[,-c(1:3)]
Data.Group<-Data.Raw[,c(1:3)]

Subsample<-Subsample(RawAbun, 15000, 10)
```

-   This iterative subsampling process is, in my opinion, important to
    get a representative sampling of reads.

    -   For example, if you have 150,000 reads in one sample and want to
        randomly select 10,000 of those, it’s possible that you would
        get a skewed sampling of reads in your selection of 10,000 reads
        since your pool of possible reads is so much larger than 10,000.

-   To avoid this uneven sampling of reads, I usually select 999
    iterations.

-   Let’s see if our data has been subsampled to 15,000 reads.

``` r
summary(rowSums(Subsample))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   14987   14994   14996   14996   14997   15003

\*Remember, we rounded the averaged subsample data, so there will be a
little bit of variation in the number of reads per sample, but nothing
large enough to worry about.

-   Now that we have the subsampled data, let’s attach the sample info
    columns and create a new file so we don’t have to run Subsample()
    again.

``` r
Data.Subsample<-cbind(Data.Group, Subsample)
write.table(Data.Subsample, 'Data.Subsample.txt', quote=FALSE, row.names=FALSE)
```

-   write.table() is the function used to output the data as a .txt. To
    output a .csv, use write.csv.
    -   In either case, you have to state the data name in R that you
        want to output (in this case it’s “Data.Subsample”). Then you
        have to state where you want to put the file and how it should
        be named (‘D:\Data.Subsample.txt’).
-   Here, the inputs “quote” and “row.names” are specifying that we
    don’t want our data to have quotation marks around each number and
    that we don’t care about the row names in our subsampled data table.
    -   You can try changing these values to TRUE and see how it affects
        the exported file. If you don’t want to do that, I’ll just say
        that it looks terrible and doesn’t import back into R very
        cleanly.
-   Before we move on, let’s check that our subsampled data is
    representative of the raw dataset. In other words, we want to check
    that selecting 15,000 reads is enough to capture the rarer OTUs in
    our data and not just the highly abundant ones.
    -   This is more of a problem if your sample contains a complex
        community of many different types of bacteria (e.g., soil) or if
        you have to use a small number of reads (e.g., 1,000 reads may
        not be enough depth in some sample types).
-   To do this, we will make a rarefaction curve.

``` r
Color<-ifelse(grepl("1w", Data.Subsample$Group),2, ifelse(grepl("-M|-m|M", Data.Subsample$Group),1,3))
rarecurve(Data.Subsample[,-c(1:3)],step=500, xlab="Sample Size", ylab="Species",label=FALSE, col=Color)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/plot-rarefaction-curve-1.png)

-   There are two parts to this chunk of code.
    -   The first part that we set to the variable name “Color” looks at
        the “Group” column that contains our sample IDs and timepoint
        info. It looks for samples that have “1w” in the name and gives
        it a 2, while samples with an “M” in the name are given a 1.
        -   In R, basic colors have numeric codes. 1 = black, 2 = red, 3
            = green, etc.
            -   In this case, we are setting Moms to show as black (1)
                and infants to show as red (2).
    -   The second part is the rarecurve() function. There are 6 inputs
        used here.
        -   The first is the “Data” input. This is where your subsampled
            OTU data would go. Here, I’ve excluded writing “Data=”
            before I specified the dataset I want read (i.e.,
            Data.Subsample\[,-c(1:3)\]). In this case, R expects the
            first input to be the “Data”, so I only had give it my
            variable of interest without having to specify what the
            variable will contain.
        -   “Step” tells the function how often to sample from the OTU
            file. (Don’t worry about this)
        -   “xlab” and “ylab” changes the label on the x and y axes,
            respectively.
        -   “label” tells R whether you want each line to be labelled
            with its ID
        -   “col” tells R what color to make the lines.
-   The curve reaches an asymptote, meaning that having more reads in
    our dataset wouldn’t increase the number of unique OTUs in our
    rarefaction curve.
    -   If your data does not reach an asymptote, it means that you
        subsampled too much and need to include more reads in the
        Subsample() step to fully capture the OTUs in your community.
-   Since our rarefaction curve looks good, let’s make two files of our
    subsampled data. One for the infant data and the other for the mom
    data.

``` r
#infant 1w
Data.Baby1w<-Data.Subsample[grepl("1w", Data.Subsample$Group),]
write.table(Data.Baby1w, 'Data.Subsample.Baby1w.txt', quote=FALSE, row.names=FALSE)
#Mom samples#
Data.Mom<-Data.Subsample[grepl("M|m", Data.Subsample$Group),]
write.table(Data.Mom, 'Data.Subsample.Mom.txt', quote=FALSE, row.names=FALSE)
```

# 5) Importing data

-   Great. Now we can work from these files directly instead of running
    all of the previous steps to get to this point.
-   Like I mentioned before, I will only be going through the analysis
    for the infant data.
    -   To do that, we need to read in our microbiota data as well as
        the other sample data sheet (which I will be calling the BC
        data–birth certificate data) that contains info on delivery
        mode, breastfeeding and other data.
-   First, we’ll read in the infant microbiota data file we just made
    and the BC data.
    -   We also import the mom data since we only want to use infant
        data if we have data from their mom as well (don’t worry about
        why we do this)

``` r
Data.Mom<-read.table('Data.Subsample.Mom.txt',header=TRUE, fill=TRUE)
Data.Baby1w<-read.table('Data.Subsample.Baby1w.txt',header=TRUE, fill=TRUE)
BCData<-read.table('BCData.txt', header=TRUE, fill=TRUE)
```

-   Let’s take a look at how our samples are identified in the infant
    microbiota dataset

``` r
Data.Baby1w$Group
```

    ##  [1] "1-1w"    "14-1w"   "16-1w"   "17-1w"   "18-1w"   "19-1wb"  "20-1w"  
    ##  [8] "21-1w"   "22-1w"   "23-1w"   "24-1w"   "26-1wb"  "27-1w"   "28-1w"  
    ## [15] "29-1wb"  "3-1w"    "30-1w"   "33-1w"   "34-1wb"  "36-1w"   "37-1w"  
    ## [22] "38-1w"   "39-1w"   "40-1w"   "41-1w"   "42-1w"   "43-1w"   "44T1-1w"
    ## [29] "44T2-1w" "45-1w"   "46-1w"   "48-1w"   "49-1w"   "50-1w"   "51-1w"  
    ## [36] "52-1w"   "55-1w"   "56-1wb"  "57-1w"   "60-1w"   "62-1w"   "63-1w"  
    ## [43] "64-1w"

-   And now in the BC data

``` r
BCData$Group
```

    ##  [1] "1"    "14"   "16"   "17"   "18"   "19"   "20"   "21"   "22"   "23"  
    ## [11] "26"   "27"   "28"   "29"   "3"    "30"   "33"   "34"   "36"   "37"  
    ## [21] "38"   "39"   "40"   "41"   "42"   "43"   "44T1" "45"   "46"   "49"  
    ## [31] "51"   "52"   "55"   "56"   "57"   "60"   "62"   "63"   "64"

-   You can see that the microbiota dataset has the timepoint appended
    to the end of the sample ID, while the BC data just has the sample
    ID.
    -   The mom dataset IDs looks like the infant dataset IDs, but with
        “M” appended to the end instead of “1w”.
-   We eventually want to match the three datasets together by sample
    ID, so we have to make the samples IDs are identical to each other.
    -   So, let’s edit the IDs in the infant and mom microbiota datasets
        to exclude everything but the ID number.
-   Let’s use the gsub() command to do this.

``` r
Data.Mom$Group<-gsub('-.*',"",Data.Mom$Group)
Data.Baby1w$Group<-gsub('-.*',"",Data.Baby1w$Group)
```

-   gsub() works by taking an expression you want it to find and then
    replacing it.
    -   Here, we’re using regular expressions (or regex) to tell R we
        want to find the hyphen and all the text afterwards, so the
        expression that tells R to do this is “-.\*“.
    -   The replacement we specify is just an empty set, so the text is
        basically just deleted and not replaced.
    -   Regex’s are just expressions used to generalize an action.
        -   The “.” means you want the character after the “-” to be
            included in the text it finds, while the “\*” tells R to
            keep using the “.” command until it can’t anymore.
        -   So, in this case, by writing “-.\*” you’re saying to R that
            you want to look for any text that starts with a hyphen and
            includes all text after that point.
    -   Regex’s are very confusing, but you can find a pretty good list
        of them here:
        <https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf>
-   Let’s check that our study ID’s match now.

``` r
Data.Mom$Group
```

    ##  [1] "1"  "11" "14" "16" "17" "18" "19" "2"  "20" "21" "22" "23" "25" "26" "27"
    ## [16] "28" "29" "3"  "30" "33" "34" "36" "37" "38" "39" "40" "41" "42" "43" "44"
    ## [31] "45" "46" "49" "51" "52" "55" "56" "57" "60" "62" "63" "64"

``` r
Data.Baby1w$Group
```

    ##  [1] "1"    "14"   "16"   "17"   "18"   "19"   "20"   "21"   "22"   "23"  
    ## [11] "24"   "26"   "27"   "28"   "29"   "3"    "30"   "33"   "34"   "36"  
    ## [21] "37"   "38"   "39"   "40"   "41"   "42"   "43"   "44T1" "44T2" "45"  
    ## [31] "46"   "48"   "49"   "50"   "51"   "52"   "55"   "56"   "57"   "60"  
    ## [41] "62"   "63"   "64"

``` r
BCData$Group
```

    ##  [1] "1"    "14"   "16"   "17"   "18"   "19"   "20"   "21"   "22"   "23"  
    ## [11] "26"   "27"   "28"   "29"   "3"    "30"   "33"   "34"   "36"   "37"  
    ## [21] "38"   "39"   "40"   "41"   "42"   "43"   "44T1" "45"   "46"   "49"  
    ## [31] "51"   "52"   "55"   "56"   "57"   "60"   "62"   "63"   "64"

-   Awesome. Now that they match (ignore the quotations, they don’t
    matter for this application), we’re going to use merge() to combine
    our datasets by sample ID. This makes it so participants that have
    data in all three datasets are kept and that their data is aligned
    across the mom, infant and BC data.
-   We will take this aligned data and split it back into the mom,
    infant and BC data.

``` r
temp<-merge(Data.Baby1w, Data.Mom,by="Group")
temp2<-merge(temp,BCData,by="Group")
#Break into Baby, Mom, and metadata
Data.Baby1w<-temp2[,c(1:(ncol(Data.Baby1w)))]
Data.Mom<-temp2[,c(1,(ncol(Data.Baby1w)+1):(ncol(Data.Baby1w)+ncol(Data.Mom)-1))]
BCData<-temp2[,c(1,(ncol(temp)+1):ncol(temp2))]
```

-   merge() takes two variables and combines them by variables in a
    column.
    -   You specify what the column name is with the “by=” command. In
        this case all the datasets have a “Group” column that contains
        the IDs.
    -   If your datasets have different column names, you can use “by.x”
        and “by.y” to specify the column names in the first and second
        variables, respectively.
-   Alright, we’re getting there. Let’s take a look at one of our
    variables of interest, maternal pre-pregnancy BMI category.

``` r
BCData$BMI_Category_Final
```

    ##  [1] "Obese"      "Obese"      "Overweight" "Normal"     "Overweight"
    ##  [6] "Normal"     "Overweight" "Obese"      "Overweight" "Normal"    
    ## [11] "Overweight" "Normal"     "Overweight" "Obese"      "Normal"    
    ## [16] "Obese"      "Obese"      "Normal"     "Normal"     "Overweight"
    ## [21] "Overweight" "Obese"      "Normal"     "Obese"      "Normal"    
    ## [26] "Obese"      "Normal"     "Normal"     "Obese"      "Obese"     
    ## [31] "Overweight" "Obese"      "Normal"     "Overweight" "Overweight"
    ## [36] "Obese"      "Obese"      "Obese"

-   See that part at the bottom that says “Levels: Normal Obese
    Overweight”? Well, those are the categories in BMI category.
    Unfortunately, the order is wrong. We want it to be Normal,
    Overweight then Obese.
    -   The level order is important for plots like a boxplot. It will
        plot your categories in the level order shown.
-   Let’s fix that.

``` r
BCData$BMI_Category_Final<-factor(BCData$BMI_Category_Final,levels=c("Normal","Overweight","Obese"))

levels(BCData$BMI_Category_Final)
```

    ## [1] "Normal"     "Overweight" "Obese"

-   So, there’s a lot to unpack here.
-   Factor() is the main function that tells R to make the output a
    factor.
    -   The next line is telling R what factor variable we want to edit
        “BCData$BMI_Category_Final”
    -   Finally, we tell R how we want to edit the factor order
        “levels(BCData$BMI_Category_Final)\[c(1,3,2)\]”
        -   Since the order of our levels was Normal, Obese, Overweight,
            we want Normal to stay where it is in the order, but move
            the order of overweight and obese like so: \[c(1,3,2)\]
-   Of course, before we can do that, we have to read in the OTU names.
    -   You may have noticed previously that the OTUs didn’t have a
        bacterial name, just a number. Let’s get the list of bacteria
        into R.

``` r
TaxName<-read.table("tax_names.txt",header=TRUE, fill=TRUE,row.names=NULL)

head(TaxName)
```

    ##      OTU    Size
    ## 1 Otu001  849734
    ## 2 Otu002 1934549
    ## 3 Otu003  180478
    ## 4 Otu004 1177170
    ## 5 Otu005  456276
    ## 6 Otu006  436955
    ##                                                                                                                       Taxonomy
    ## 1 Bacteria(100);Proteobacteria(100);Gammaproteobacteria(100);Enterobacteriales(100);Enterobacteriaceae(100);unclassified(100);
    ## 2                   Bacteria(100);Bacteroidetes(100);Bacteroidia(100);Bacteroidales(100);Bacteroidaceae(100);Bacteroides(100);
    ## 3                     Bacteria(100);Firmicutes(100);Clostridia(100);Clostridiales(100);Lachnospiraceae(100);unclassified(100);
    ## 4   Bacteria(100);Actinobacteria(100);Actinobacteria(100);Bifidobacteriales(100);Bifidobacteriaceae(100);Bifidobacterium(100);
    ## 5                 Bacteria(100);Firmicutes(100);Negativicutes(100);Selenomonadales(100);Veillonellaceae(100);Megasphaera(100);
    ## 6                 Bacteria(100);Firmicutes(100);Negativicutes(100);Selenomonadales(100);Veillonellaceae(100);Veillonella(100);

-   You can see that there are three columns in this file. The only one
    we need is the last one, named “Taxonomy”.
    -   This column contains the ID of each OTU, classified from kingdom
        to genus. We only need the genus-level info for this analysis,
        so let’s get rid of all the other text with Edit.Taxname()

``` r
Edit.Taxname<-function(taxname,level,sep=";"){
  taxname<-data.frame(taxname)
  taxname<-separate(taxname,col=1, into=c("Kingdom","Phylum","Class","Order","Family","Genus"), sep=";")
  if(grepl("Genus|1", level, ignore.case = T)){
    x<-ifelse(grepl("unclassified|uncultured",taxname$Genus), paste(taxname$Genus, taxname$Family), 
              paste(taxname$Genus))
  }
  if(grepl("Family|2", level, ignore.case = T)){
    x<-ifelse(grepl("unclassified|uncultured",taxname$Family), paste(taxname$Order, taxname$Family), 
              paste(taxname$Family))
  }
  if(grepl("Order|3", level, ignore.case = T)){
    x<-ifelse(grepl("unclassified|uncultured",taxname$Order), paste(taxname$Class, taxname$Order), 
              paste(taxname$Order))
  }
  if(grepl("Class|4", level, ignore.case = T)){
    x<-ifelse(grepl("unclassified|uncultured",taxname$Class), paste(taxname$Phylum, taxname$Class),
              paste(taxname$Class))
  }
  if(grepl("Phylum|5", level, ignore.case = T)){
    x<-paste(taxname$Phylum)
    
  }
  x<-gsub("\\([[:digit:]]+\\)","",x)
  return(x)
}
```

-   Edit.taxname() accepts two inputs
    -   “TaxName”, as you can imagine, accepts the TaxName file that we
        just read in. Make sure to specify the “Taxonomy” column.
        -   “level” tells R which taxonomic level you want to end up
            with. 1 = genus, 2 = family, etc.

``` r
TaxName<-Edit.Taxname(TaxName$Taxonomy,level=1)
```

    ## Warning: Expected 6 pieces. Additional pieces discarded in 439 rows [1, 2, 3, 4, 5, 6,
    ## 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

-   Easy, right? Let’s see what it looks like.

``` r
head(TaxName)
```

    ## [1] "unclassified Enterobacteriaceae" "Bacteroides"                    
    ## [3] "unclassified Lachnospiraceae"    "Bifidobacterium"                
    ## [5] "Megasphaera"                     "Veillonella"

-   Great, looks like some bacterial names, right? You’ll notice some
    are named “unclassified”. It just means that the OTU couldn’t be
    identified to the genus level, only to the family level.

# 6) Stacked Barplot of OTU Abundances

-   So, what’s this data look like? Let’s take a look at it in the most
    overwhelming way possible, with the StackedBarPlot() function:

``` r
StackedBarPlot<-function(OTU,Group="Samples",TaxName,N=19,Title="Stacked Bar Chart"){
  Rowsum<-as.matrix(rowSums(OTU))
  abun<-matrix(0,nrow=nrow(OTU),ncol=ncol(OTU))
  for (i in 1:nrow(OTU)){
    for (j in 1:ncol(OTU)){
      abun[i,j]=(OTU[i,j])/(Rowsum[i])*100
    }
  }
  colnames(abun)<-TaxName
  abun<-abun[,order(-colSums(abun))]
  taxa_list<-colnames(abun)[1:N]
  taxa_list<-taxa_list[!grepl("unclassified unclassified",taxa_list)]
  N<-length(taxa_list)
  new_x<-data.frame(abun[,colnames(abun) %in% taxa_list],Others=rowSums(abun[,!colnames(abun) %in% taxa_list]))
  if (ncol(new_x)>(N+1)){
    Other<-rowSums(new_x[,c((N+1):ncol(new_x))])
    new_x<-new_x[,c(1:N)]
    new_x$Other<-Other
  }
  abun_groups<-cbind(Group,new_x)
  new_x <- abun_groups
  grouping_info<-new_x$Group
  new_x2<-new_x[,-1]
  tempname<-c(taxa_list,"Other")
  colnames(new_x2)<-tempname
  df<-NULL
  for (i in 1:dim(new_x2)[2]){
    tmp<-data.frame(row.names=NULL,Sample=rownames(new_x2),Taxa=rep(colnames(new_x2)[i],dim(new_x2)[1]),Value=new_x2[,i],Type=grouping_info)
    if(i==1){df<-tmp} else {df<-rbind(df,tmp)}
  }
  colours <- c("#F0A3FF", "#0075DC", "#993F00","#4C005C","#2BCE48","#FFCC99","#808080","#94FFB5","#8F7C00","#9DCC00","#C20088","#003380","#FFA405","#FFA8BB","#426600","#FF0010","#5EF1F2","#00998F","#740AFF","#990000","#FFFF00");
  p<-ggplot(df,aes(Sample,Value,fill=Taxa))+geom_bar(stat="identity")+facet_grid(. ~ Type, drop=TRUE,scale="free",space="free_x")
  p<-p+scale_fill_manual(values=colours[1:(N+1)])
  p<-p+theme_bw(base_size = 24)+ylab("Relative Abundance")+ggtitle(Title)+xlab("Sample ID")
  p<-p+ scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="gray85"))+theme(panel.spacing = unit(0, "lines"))
  p<-p+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
  print(p)
  return(df)
}
```

-   Like with Edit.Taxname(), I won’t be going through all this code
    here. Luckily, using it is also pretty simple.
    -   “OTU” is the just OTU table
    -   “TaxName” is the list of names for each OTU in your OTU table
    -   “Group” tells R how you want your samples to be grouped in the
        barchart
    -   “N” is how many taxa you want displayed on the chart. Max is 19.
    -   “Title” is what you want the title of the chart to be.
-   Let’s see what it looks like:

``` r
a<-StackedBarPlot(OTU=Data.Baby1w[,-c(1:3)],TaxName=TaxName)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/stacked-bar-plot-1.png)

-   This chart is showing the top 19 most abundant taxa in our data for
    each infant. We can see that there’s a lot of variation between
    infants and that they are dominated by a few OTUs that are highly
    abundant.

# 7) Alpha Diversity

-   Not super useful. Let’s start doing some of the statistical stuff,
    starting with alpha diversity.
-   To calculate alpha diversity, we’ll use the function Alpha()

``` r
Alpha<-function(OTU, Groups="Sample"){
  Chao<-t(estimateR(OTU))
  Chao<-Chao[,2]
  Shannon<-diversity(OTU,index="shannon")
  Invsimpson<-diversity(OTU,index="invsimpson")
  OTU.Subsample.Alpha<-data.frame(Groups,Chao,Shannon,Invsimpson)
  return(OTU.Subsample.Alpha)
}
```

-   Alpha() only needs 1 input:
    -   “OTU” is the only important input. This allows the funciton to
        convert your OTU table to Chao1, Shannon and inverse Simpson
        diversity.
    -   The other inputs are if you want to add ID, or another variable
        to the output table created by Alpha()

``` r
Data.Baby1w.Alpha<-Alpha(Data.Baby1w[,-c(1:3)])
```

-   That’s it! Now “Data.Baby1w.Alpha” contains our alpha diversity
    measures. Let’s looks at it.

``` r
head(Data.Baby1w.Alpha)
```

    ##   Groups  Chao   Shannon Invsimpson
    ## 1 Sample 45.20 1.5323002   3.205960
    ## 2 Sample 36.00 1.9954551   6.005949
    ## 3 Sample 23.50 1.3385116   2.762900
    ## 4 Sample 33.50 0.7901536   1.744783
    ## 5 Sample 40.00 1.1434433   2.203279
    ## 6 Sample 54.75 2.1300681   6.741989

-   The first two columns can contain info like ID or other data, but
    here we didn’t specify anything in Alpha() so it contains filler
    text.

-   The next three columns contain our alpha diversity: Chao1, Shannon
    and Inverse Simpson

-   With this data, we can start to do some analyses. First, we need to
    check whether the data are normally distributed using a Shapiro-Wilk
    test.

``` r
shapiro.test(Data.Baby1w.Alpha$Chao)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  Data.Baby1w.Alpha$Chao
    ## W = 0.74101, p-value = 8.045e-07

``` r
shapiro.test(Data.Baby1w.Alpha$Shannon)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  Data.Baby1w.Alpha$Shannon
    ## W = 0.96795, p-value = 0.3396

``` r
shapiro.test(Data.Baby1w.Alpha$Invsimpson)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  Data.Baby1w.Alpha$Invsimpson
    ## W = 0.92705, p-value = 0.01622

-   A p\<0.05 tells us that Chao1 and inverse Simpson metrics are
    non-normally distributed. As such, we have to use non-parametric
    statistical methods such as the Wilcoxon rank sum (2 levels) or
    Kruskal-Wallis (3+ levels).
    -   Stats are hard. I can’t get into the everything I just said in
        enough detail to explain it here, just know that if the data are
        normal you can use an ANOVA to analyze the data instead of
        Kruskal-Wallis.
        -   I will put an example of an ANOVA later so you have some
            code for it.
-   Anyway, let’s start looking at the data.
-   First, we’ll start with maternal pre-pregnancy BMI category.
    -   To make this easy, we’ll set each alpha diversity metric to its
        own variable:

``` r
chao<-Data.Baby1w.Alpha$Chao
shan<-Data.Baby1w.Alpha$Shannon
invsimp<-Data.Baby1w.Alpha$Invsimpson
```

-   This allows us to keep reusing code while changing only a couple
    lines (you’ll see what I mean in a bit).
-   Similarly, let’s set “a” to be equivalent to our variable of
    interest

``` r
a<-BCData$BMI_Category_Final
```

-   Great. We can finally get started with boxplot()

``` r
boxplot(chao~a,main="chao1 Index of Babies at 1 Week",ylab="Chao1 Index")
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/alpha-diversity-boxplots-1.png)

-   The first argument in boxplot is the important one. This is what R
    calls a formula.

    -   Formulas in R are written with the dependent variable (y) on the
        left of the tilde and the independent variables (x) written to
        the right.

-   The other inputs are

-   “main” which allows you to change the plot title

-   “ylab” which changes the axis label on the y axis

-   Let’s see if there’s a significant difference in richness between
    these groups with kruskal.test()

``` r
kruskal.test(chao~a)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  chao by a
    ## Kruskal-Wallis chi-squared = 1.5463, df = 2, p-value = 0.4616

-   As you can see, the input formula for this function is exactly the
    same as the input for boxplot().
    -   The important part of the output is the p-value, which in this
        case is not significant (p=0.5293)
-   Let’s do the same thing for the Shannon and inverse Simpson indices:

``` r
#Shannon
boxplot(shan~a,main="Shannon Index of Babies at 1 Week",ylab="Shannon Index")
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
kruskal.test(shan~a)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  shan by a
    ## Kruskal-Wallis chi-squared = 1.3595, df = 2, p-value = 0.5067

``` r
#Inverse Simpson
boxplot(invsimp~a, main="Inverse Simpson of Babies at 1 Week",ylab="Inverse Simpson Index")
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/unnamed-chunk-23-2.png)

``` r
kruskal.test(invsimp~a)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  invsimp by a
    ## Kruskal-Wallis chi-squared = 1.3654, df = 2, p-value = 0.5053

-   Maternal pre-pregnancy BMI is not significantly associated with any
    of the infant gut microbiota alpha diversity measures!

-   In cases where you are only comparing two factors, you would use a
    Wilcoxon rank sum test instead of a Kruskal-wallis.

    -   Let’s see how to do that with our breastfeeding data, which is
        split into exclusive (1) and non-exclusive (0) breastfeeding:

``` r
a<-BCData$Exclusive_BF

boxplot(chao~a,main="Chao1 Index of Babies at 1 Week",ylab="Chao1 Index")
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/alpha-diversity-by-BF-1.png)

``` r
wilcox.test(chao~a)
```

    ## Warning in wilcox.test.default(x = c(45.2, 36, 40, 44, 36.5, 31.5, 93.5, :
    ## cannot compute exact p-value with ties

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  chao by a
    ## W = 157, p-value = 0.7506
    ## alternative hypothesis: true location shift is not equal to 0

``` r
boxplot(shan~a,main="Shannon Index of Babies at 1 Week",ylab="Shannon Index")
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/alpha-diversity-by-BF-2.png)

``` r
wilcox.test(shan~a)
```

    ## 
    ##  Wilcoxon rank sum exact test
    ## 
    ## data:  shan by a
    ## W = 191, p-value = 0.5008
    ## alternative hypothesis: true location shift is not equal to 0

``` r
boxplot(invsimp~a,main="Inverse Simpson of Babies at 1 Week",ylab="Inverse Simpson Index")
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/alpha-diversity-by-BF-3.png)

``` r
wilcox.test(invsimp~a)
```

    ## 
    ##  Wilcoxon rank sum exact test
    ## 
    ## data:  invsimp by a
    ## W = 192, p-value = 0.4819
    ## alternative hypothesis: true location shift is not equal to 0

-   Using wilcox.test() uses the same inputs as using kruskal.test()!
-   I said I’d show you how to do an ANOVA in R, so let’s do that with
    the inverse Simpson data and compare the maternal BMI categories

``` r
a<-BCData$BMI_Category_Final
summary(aov(invsimp~a))
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## a            2   3.89   1.947   1.257  0.297
    ## Residuals   35  54.21   1.549

-   We can see from the p-value that inverse Simpson diversity is not
    significantly associated with maternal pre-pregnancy BMI – we saw
    the same association when we used kruskal.test()
    -   In general, if the data are normally distributed (if the
        shapiro.test() p\>0.05), you would use an ANOVA and if they
        aren’t, use the non-parametric kruskal.test() or wilcox.test()
-   Let’s say you want to make a table out of this with the means and
    standard deviation.
    -   A reviewer asked us to remove a single participant from out
        analysis, \*so I had to redo all of the tables manuall\*\*.
    -   So, I wrote some code to make these sorts of tables for me
        because that kinda sucked.

``` r
Alpha.Table<-function(alpha,group,...){
  a<-alpha$Chao
  b<-alpha$Shannon
  c<-alpha$Invsimpson
  d<-group
  c.<-c(paste(round(mean(a),1),"\u00b1",round(sd(a),1)),paste(round(aggregate(a,list(d),mean)[,2],1),"\u00b1",round(aggregate(a,list(d),sd)[,2],1)))
  s.<-c(paste(round(mean(b),1),"\u00b1",round(sd(b),1)),paste(round(aggregate(b,list(d),mean)[,2],1),"\u00b1",round(aggregate(b,list(d),sd)[,2],1)))
  i.<-c(paste(round(mean(c),1),"\u00b1",round(sd(c),1)),paste(round(aggregate(c,list(d),mean)[,2],1),"\u00b1",round(aggregate(c,list(d),sd)[,2],1)))
  
  alpha.table<-rbind(c.,s.,i.)
  colnames(alpha.table)<-c("Overall",levels(factor(group)))
  return(alpha.table)
}
```

-   Alpha.table() takes two inputs.
    -   “alpha” is just the output from the Alpha() function we did
        previously
    -   “group” is the variable that you want R to use to make the
        table.
-   Let’s try it with the BMI category variable

``` r
a<-BCData$BMI_Category_Final
Alpha.Table(Data.Baby1w.Alpha,a)
```

    ##    Overall       Normal        Overweight    Obese        
    ## c. "47.4 ± 22.9" "47.6 ± 20.4" "42.9 ± 19.1" "50.6 ± 27.8"
    ## s. "1.4 ± 0.4"   "1.5 ± 0.4"   "1.4 ± 0.3"   "1.4 ± 0.4"  
    ## i. "3.3 ± 1.3"   "3.7 ± 1.5"   "3 ± 0.9"     "3.2 ± 1.2"

-   Each row is a different alpha diversity metric. The orer from top to
    bottom is Chao1, Shannon and inverse Simpson (labelled as “c.”, “s.”
    and “i.”).
    -   The numbers are mean +/- standard deviation.
    -   The one thing you’ll have to do to complete this table is fill
        in any numbers that don’t end in a decimal, such as the
        overweight category’s inverse Simpson value.
        -   This happens when the number after a decimal is 0. This
            number should be 3.0, but R only outputs a 3
-   We’ve seen what to do with categorical variables, but what about
    continuous ones?
    -   It’s pretty much the same procedure as before, but we will be
        using plot() and cor.test()
-   Let’s take a look at alpha diversity and infant age

``` r
plot(Data.Baby1w.Alpha$Chao~BCData$Baby.Age,main="Chao1 Index of Babies at 1 Week",ylab="Chao1 Index")
abline(lm(Data.Baby1w.Alpha$Chao~BCData$Baby.Age))
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/scatter-plot-alpha-diversity-1.png)

-   plot() and boxplot() work the exact same way, so I won’t describe
    the inputs here.
    -   The next line abline() tells R it wants the line of best fit
        drawn on the plot. To get the equation for the line of best fit,
        you need to create a linear model with lm().
        -   lm() has the same formula input as plot().
-   cor.test() has a few different methods of finding correlations. The
    two main ones we use are the pearson (parametric) and spearman
    (non-parametric) correlations.
    -   Just like before, which one you use depends on the normality of
        the data. Here we use the non-parametric test.

``` r
cor.test(Data.Baby1w.Alpha$Chao,BCData$Baby.Age,method="spearman")
```

    ## Warning in cor.test.default(Data.Baby1w.Alpha$Chao, BCData$Baby.Age, method =
    ## "spearman"): Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  Data.Baby1w.Alpha$Chao and BCData$Baby.Age
    ## S = 9464.8, p-value = 0.4721
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.1219545

-   We see there is no significant correlation between Chao1 and baby
    age.

Let’s take a look at the other alpha diversity metrics and baby age.

``` r
plot(Data.Baby1w.Alpha$Shannon~BCData$Baby.Age,main="Shannon Index of Babies at 1 Week",ylab="Shannon Index")
abline(lm(Data.Baby1w.Alpha$Shannon~BCData$Baby.Age))
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
cor.test(Data.Baby1w.Alpha$Shannon,BCData$Baby.Age,method="spearman")
```

    ## Warning in cor.test.default(Data.Baby1w.Alpha$Shannon, BCData$Baby.Age, :
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  Data.Baby1w.Alpha$Shannon and BCData$Baby.Age
    ## S = 8785.9, p-value = 0.8074
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##         rho 
    ## -0.04147616

``` r
plot(Data.Baby1w.Alpha$Invsimpson~BCData$Baby.Age,main="Inverse Simpson of Babies at 1 Week",ylab="Inverse Simpson Index")
abline(lm(Data.Baby1w.Alpha$Invsimpson~BCData$Baby.Age))
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/unnamed-chunk-25-2.png)

``` r
cor.test(Data.Baby1w.Alpha$Invsimpson,BCData$Baby.Age,method="spearman")
```

    ## Warning in cor.test.default(Data.Baby1w.Alpha$Invsimpson, BCData$Baby.Age, :
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  Data.Baby1w.Alpha$Invsimpson and BCData$Baby.Age
    ## S = 8737.8, p-value = 0.8335
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.0357717

-   If you want to change the test to pearson, you would just change
    “method=‘spearman’” to “method=‘pearson’” in cor.test().

-   If you want to run an ANOVA instead of a correlation, the code will
    be pretty much the same for both continuous and categorical
    variables, like so:

``` r
summary(aov(Data.Baby1w.Alpha$Invsimpson~BCData$Baby.Age))
```

    ##                 Df Sum Sq Mean Sq F value Pr(>F)
    ## BCData$Baby.Age  1   1.74   1.742   1.249  0.271
    ## Residuals       35  48.79   1.394               
    ## 1 observation deleted due to missingness

-   Great. We’re, like, halfway there.

# 8) Beta Diversity

-   Let’s do beta diversity now. There’s one function for both Sorensen
    (presence/absence) and Bray-Curtis (abundance) dissimilarities and
    to produce the PCoA plot.
    -   Let’s see what Sor.bray.pcoa() does:

``` r
Sor.bray.pcoa<-function(OTUS,Dim=2,Color=1,binary,Title="PCoA",...){
  Data.df<-vegdist(OTUS,method="bray", binary)
  Data.df.PCoA<-cmdscale(Data.df, k = Dim, eig = FALSE)
  Data.df.PCoA.eig<-cmdscale(Data.df, k = Dim, eig = TRUE)
  eig.Data.df.PCoA<-Data.df.PCoA.eig$eig
  eig.Data.df.PCoA.sum<-sum(eig.Data.df.PCoA)
  a<-(eig.Data.df.PCoA/eig.Data.df.PCoA.sum)*100
  xlab<-paste("PC1","(",round(a[1],1),"%",")",sep="")
  ylab<-paste("PC2","(",round(a[2],1),"%",")",sep="")
  if(binary==TRUE){
    main<-"Sorensen PCoA"
  }else(main<-"Bray-Curtis PCoA")
  plot(Data.df.PCoA, col=Color,
       main=Title,xlab=xlab,ylab=ylab,...)
  return(Data.df.PCoA)
}
```

-   Sor.bray.pcoa() takes 6 arguments:
    -   “OTUS” is your OTU table, just like when we calculated alpha
        diversity.
    -   “Dim” is the number of dimensions you want calculated. Usually
        we just do 2 since the first 2 dimensions are often enough to
        explain most of the variance in the data, but there’s an option
        to find the PCoA dimentions for more than that here.
    -   “Color” works the same as the “col” variable we saw in the
        rarefaction plot. Usually you’ll use the groups as the “Color”
        input so you can visually represent the spatial differentiation
        between groups.
    -   “binary” is how you tell the code whether you want to use
        Sorensen or Bray-Curtis. Setting this to “TRUE” will give you a
        Sorensen plot, while “FALSE” returns a Bray-Curtis PCoA.
    -   “pch” is how you tell R what shape you want your points to be. A
        value of 16 is a solid circle, but there are other shapes such
        as triangles or open circles.
    -   You can also use this option to plot your groups with different
        shapes the same way you would plot your groups with different
        colors; we’ll get more into how to customize your shapes/colors
        in a bit.
    -   “Title” allows you to name your graph.
-   Let’s try plotting the maternal BMI data on a Sorensen plot:

``` r
df.Baby1w.Sor<-Sor.bray.pcoa(Data.Baby1w[,-c(1:3)],Dim=2,Color=BCData$BMI_Category_Final,binary=TRUE)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/sorensen-plot-1.png)

-   Cool. Before we do any stats, let’s make this graph prettier,
    starting with the point colors.
-   Instead of using the R standard output colors of black, red and
    green, we’re going to customize what colors we want:

``` r
Color.Baby<-ifelse(grepl("Normal", BCData$BMI_Category_Final),"#000000", ifelse(grepl("Overweight", BCData$BMI_Category_Final),"#E79F00","#0072B2"))
```

-   This is called an ifelse statement. An ifelse statement works by
    asking “if condition x is true, then do y. If x is not true, then do
    z”.
-   The code above looks long and complex, but it’s really three ifelse
    statements stacked together.
    -   The first statement is this:

``` r
ifelse(grepl("Normal", BCData$BMI_Category_Final),"#000000"
```

-   Here, we’re asking it to find the BMI values labelled as “normal”
    using grepl() and set those to “#000000”.
    -   “#000000” is called a hex code–it’s a way to specify a
        particular color that may not have a numeric code in R (numeric
        as in 1, 2, 3, etc.).
    -   I used these colors in particular because people with color
        blindness are able to differentiate between them. Additionally,
        they are still distinct in black and white if anyone decides to
        print the paper to read it.
-   The next two parts of the code look the same, but they just ask R to
    find BMI labels that are “overweight” and then label them as
    “E79F00”.
    -   The values that don’t have either “normal” or “overweight” as
        values (i.e., “obese”) recieve the value “#0072B2” like so:

``` r
 ifelse(grepl("Overweight", BCData$BMI_Category_Final),"#E79F00","#0072B2"))
```

-   Put it all together and it looks like this:

``` r
Color.Baby<-ifelse(grepl("Normal", BCData$BMI_Category_Final),"#000000", ifelse(grepl("Overweight", BCData$BMI_Category_Final),"#E79F00","#0072B2"))
```

-   Now, we’re going to use plot() to plot the Sorensen PCoA with our
    custom colors

``` r
plot(df.Baby1w.Sor,cex.axis=1.5,cex.lab=1.5,cex.main=1,cex=2,col=1,
     pch=21,xlim=c(-.3,.55),ylim=c(-.35,.35),xlab="PC1 (16.8%)",ylab="PC2 (13.9%)",bg=Color.Baby)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/unnamed-chunk-32-1.png)

-   Neat. Here’s how to use plot()
    -   The x and y coordinates of the points are in the R variable
        “df.Baby1w.Sor” that was generated from Sor.bray.pcoa().
    -   “cex.axis”, “cex.label”, “cex.lab” and “cex.main” are telling R
        to change the size of various graphical components on the plot.
        -   A higher number will result in a larger output of that
            graphical parameter.
    -   “col” is telling R what colors we want for the points on our
        graph.
    -   “pch” of 21 specifices we want open circles for our point type.
        -   I’m using open circles so that our points will have a black
            outline for easier readability.
    -   “xlim” and “ylim” change the min and max limits on the graph for
        the x and y axes.
    -   “xlab” and “ylab” change the axis labels.
        -   Make sure to change these to match the axes labels of the
            previous Sorensen plot, otherwise your graph won’t have PC1
            and PC2.
    -   Finally, “bg” allows us to fill in the center of our open
        circles with a color of our choosing.
-   Our plot could still use a few things, like ellipses around our
    different groups.
    -   We can do this using ordiellipse()

``` r
plot(df.Baby1w.Sor,cex.axis=1.5,cex.lab=1.5,cex.main=1,col=1,
     pch=21,cex=2,xlim=c(-.3,.55),ylim=c(-.35,.35),xlab="PC1 (16.8%)",ylab="PC2 (13.9%)",bg=Color.Baby)
ordiellipse(df.Baby1w.Sor,BCData$BMI_Category_Final,col=c("#000000","#E79F00","#0072B2"),lwd=2)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/sorensen-plot-ellipses-1.png)

-   ordiellipse() needs a few inputs.
    -   The PCoA data from Sor.bray.pcoa() “df.Baby1w.Sor”
    -   How the data is grouped “BCData$BMI_Category_Final”
    -   “col” for the color of the ellipses. Here, you need to tell it
        which colors to use for each of the your groups ellipses. The
        order is important, so make sure the ellipse on your graph
        matches the point colors!
        -   If it doesn’t, you make have to switch the color order until
            it looks right.
    -   “lwd” is used to change the width of the ellipses.
-   Finally, let’s add a legend.

``` r
plot(df.Baby1w.Sor,cex.axis=1.5,cex.lab=1.5,cex.main=1,col=1,
     pch=21,cex=2,xlim=c(-.3,.55),ylim=c(-.35,.35),xlab="PC1 (16.8%)",ylab="PC2 (13.9%)",bg=Color.Baby)
ordiellipse(df.Baby1w.Sor,BCData$BMI_Category_Final,col=c("#000000","#E79F00","#0072B2"),lwd=2)
legend(.25,-.09,c("Normal","Overweight","Obese"), 
       pch=21,col=1,pt.bg=c("#000000","#E79F00","#0072B2"))
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/sorensen-plot-ellipses-legend-1.png)

-   There’s a lot of inputs for legend, most of which you will have to
    adjust as you go to make it look right.
    -   The first two inputs of legend() tell R where to put the box.
        .25,-.09 means I want the top right edge to be place at x
        coordinate 0.25 and y coordinate -0.09.
    -   As far as I know, this is all trial-and-error. You have to guess
        and adjust your legend placement until it looks right and
        doesn’t overlap with any points
    -   If it won’t fit on your plot, consider making the plot wider by
        changing the “xlim” and “ylim” parameters in your plot()
        function.
    -   The third input tells R what you want as your legend labels.
    -   The next three inputs (pch, col and pt.bg), tell R what kind of
        shape you want (21=open circle), what color you want it to be
        (1=black) and what you want the point fill color to be (our
        custom colors)
-   Let’s run some stats with PERMANOVA() and PERMDISP()
    -   PERMANOVA() compares the centroids of each group, where a
        centroid is the center point of the ellipse.
    -   PERMDISP() compares the dispertions of each group (i.e., whether
        the variances are different).
-   Generally, we use PERMANOVA() to test for differences between groups
    and PERMDISP() to see whether the differences in our PERMANOVA() may
    be due to differences in the variance

``` r
PERMANOVA<-function(OTUS,Group,binary,iters=9999){
  Data.Dist<-vegdist(OTUS,method="bray", binary=binary)
  adonis(Data.Dist~Group,permutations=iters)
}

PERMDISP<-function(OTUS,Group,binary,iters=9999){
  Data.Dist<-vegdist(OTUS,method="bray", binary=binary)
  Data.betadisper<-betadisper(Data.Dist, group=Group)
  permutest(Data.betadisper, group=Group, permutations=iters)
}
```

-   Fortunately, the inputs for PERMANOVA() and PERMDISP() are the same.
    -   “OTUS” is the OTU table
    -   “Group” is the variable you want to use
    -   “binary” is the same as the input used in in Sor.bray.pcoa(),
        where TRUE=Sorensen, FALSE=Bray-Curtis.
    -   “iters” is the number of times you want the test to be run.
        -   PERMANOVA works by randomly drawing from the data and
            comparing the result to what the actual data looks like.
            This is essentially a test to see whether your groups may be
            different from each other due to chance or if there is
            actually a difference between the groups.
        -   Don’t worry too much about what this is doing, just know
            that you need to have a sufficiently large number of
            iterations to get a stable p-value, otherwise the p-value
            can increase and decrease somewhat randomly. 9999 iters is
            pretty much always good enough.

``` r
p<-PERMANOVA(Data.Baby1w[,-c(1:3)],BCData$BMI_Category_Final,TRUE,10)
```

    ## 'adonis' will be deprecated: use 'adonis2' instead

``` r
p$aov.tab
```

    ## Permutation: free
    ## Number of permutations: 10
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ##           Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)
    ## Group      2    0.2982 0.14909   1.251 0.06671 0.2727
    ## Residuals 35    4.1712 0.11918         0.93329       
    ## Total     37    4.4694                 1.00000

``` r
p<-PERMDISP(Data.Baby1w[,-c(1:3)],BCData$BMI_Category_Final,TRUE,10)
p$aov.tab
```

    ## NULL

-   Looks like there’s no difference between maternal BMI categories in
    the infant gut by Sorensen.
-   How about by breastfeeding?

``` r
df.Baby1w.Sor<-Sor.bray.pcoa(Data.Baby1w[,-c(1:3)],Dim=2,Color=BCData$Exclusive_BF+1,binary=TRUE)
ordiellipse(df.Baby1w.Sor,groups=BCData$Exclusive_BF,col=c(1,2))
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/unnamed-chunk-34-1.png)

``` r
p<-PERMANOVA(Data.Baby1w[,-c(1:3)],BCData$Exclusive_BF,TRUE,10)
```

    ## 'adonis' will be deprecated: use 'adonis2' instead

``` r
p$aov.tab
```

    ## Permutation: free
    ## Number of permutations: 10
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ##           Df SumsOfSqs MeanSqs F.Model      R2  Pr(>F)  
    ## Group      1    0.2205 0.22049  1.8681 0.04933 0.09091 .
    ## Residuals 36    4.2489 0.11803         0.95067          
    ## Total     37    4.4694                 1.00000          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
p<-PERMDISP(Data.Baby1w[,-c(1:3)],BCData$Exclusive_BF,TRUE,10)
p
```

    ## 
    ## Permutation test for homogeneity of multivariate dispersions
    ## Permutation: free
    ## Number of permutations: 10
    ## 
    ## Response: Distances
    ##           Df   Sum Sq   Mean Sq      F N.Perm Pr(>F)
    ## Groups     1 0.005215 0.0052153 0.6332     10 0.4545
    ## Residuals 36 0.296499 0.0082361

-   Looks like infant breastfeeding status is associated with infant
    Sorensen dissimilarity.
-   Let’s output the BMI figure as a png with png()

``` r
png("Sor.Baby1w.BMI.png", res=300, height=7, width=7.5, units="in")
plot(df.Baby1w.Sor,cex.axis=1.5,cex.lab=1.5,cex.main=1,col=1,
     pch=21,cex=2,xlim=c(-.3,.55),ylim=c(-.35,.35),xlab="PC1 (16.8%)",ylab="PC2 (13.9%)",bg=Color.Baby)
ordiellipse(df.Baby1w.Sor,BCData$BMI_Category_Final,col=c("#000000","#E79F00","#0072B2"),lwd=2)
legend(.25,-.09,c("Normal","Overweight","Obese"), 
       pch=21,col=1,pt.bg=c("#000000","#E79F00","#0072B2"))
dev.off()
```

    ## png 
    ##   2

-   There are many other types of output besides png you can use for
    figures such as tiff(), jpg() and others. The inputs are all
    basically the same, you just have to know what file type you are
    looking for.

-   To export a png (or tiff, jpg, pdf, etc.), you need to have a line
    describing:

    -   The output file name. In this case it is “Sor.Baby1w.BMI.png
    -   The image resolution
    -   The dimensions of the image (height and width)
    -   And the units of your dimension specifications
    -   “compression” compresses the file so that the file size is tiny

-   This is followed by your graphing code.

-   And ended with the test ‘dev.off()’

    -   Don’t forget this line, otherwise your file will not be created.

-   Often times, your figure will look different in R than it does after
    you export is.

    -   You will have to keep adjusting the figure and checking the
        output until it looks correct.

-   Let’s move on to Bray-Curtis.

    -   The only difference between the Sorensen code and the
        Bray-Curtis code is binary is set to “FALSE” instead of “TRUE”.
    -   In other words, you can totally copy/paste the previous set of
        code and just replace the one variable, like so for maternal
        BMI:

``` r
df.Baby1w.Bray<-Sor.bray.pcoa(Data.Baby1w[,-c(1:3)],Dim=2,Color=BCData$BMI_Category_Final,binary=FALSE)
ordiellipse(df.Baby1w.Bray,groups=BCData$BMI_Category_Final,col=c(1,2,3))
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/bray-curtis-plots-bmi-1.png)

``` r
p<-PERMANOVA(Data.Baby1w[,-c(1:3)],BCData$BMI_Category_Final,FALSE,10)
```

    ## 'adonis' will be deprecated: use 'adonis2' instead

``` r
p$aov.tab
```

    ## Permutation: free
    ## Number of permutations: 10
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ##           Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)
    ## Group      2    0.5357 0.26783 0.89217 0.04851 0.5455
    ## Residuals 35   10.5071 0.30020         0.95149       
    ## Total     37   11.0428                 1.00000

``` r
p<-PERMDISP(Data.Baby1w[,-c(1:3)],BCData$BMI_Category_Final,FALSE,10)
p
```

    ## 
    ## Permutation test for homogeneity of multivariate dispersions
    ## Permutation: free
    ## Number of permutations: 10
    ## 
    ## Response: Distances
    ##           Df  Sum Sq  Mean Sq      F N.Perm Pr(>F)
    ## Groups     2 0.03072 0.015359 1.3758     10 0.3636
    ## Residuals 35 0.39072 0.011163

-   And for breastfeeding:

``` r
df.Baby1w.Bray<-Sor.bray.pcoa(Data.Baby1w[,-c(1:3)],Dim=2,Color=BCData$Exclusive_BF+1,binary=FALSE)
ordiellipse(df.Baby1w.Bray,groups=BCData$Exclusive_BF,col=c(1,2))
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/bray-curtis-plots-bf-1.png)

``` r
p<-PERMANOVA(Data.Baby1w[,-c(1:3)],BCData$Exclusive_BF,FALSE,10)
```

    ## 'adonis' will be deprecated: use 'adonis2' instead

``` r
p$aov.tab
```

    ## Permutation: free
    ## Number of permutations: 10
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ##           Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)
    ## Group      1    0.2786 0.27859 0.93173 0.02523 0.6364
    ## Residuals 36   10.7642 0.29901         0.97477       
    ## Total     37   11.0428                 1.00000

``` r
p<-PERMDISP(Data.Baby1w[,-c(1:3)],BCData$Exclusive_BF,FALSE,10)
p
```

    ## 
    ## Permutation test for homogeneity of multivariate dispersions
    ## Permutation: free
    ## Number of permutations: 10
    ## 
    ## Response: Distances
    ##           Df  Sum Sq   Mean Sq      F N.Perm Pr(>F)
    ## Groups     1 0.00049 0.0004867 0.0402     10      1
    ## Residuals 36 0.43574 0.0121039

-   Let’s say you want to make a multi-panel figure, where there are 4
    graphs plotted on one figure.
-   To do that, we have to tell R we want to split the plotting area
    into 4 parts.
    -   In this case, we’ll specify we want out graphs in a square using
        par()

``` r
par(mfrow = c(2,2),mar=c(4,4.3,1,1))
```

-   In par, we are giving it two arguments.
    -   “mfrow” tells R how many rows and columns we want, with the
        number of rows to the left of the comma and columns to the
        right.
        -   Here we are saying we want 2 rows and 2 columns (i.e., make
            a square).
    -   “mar” adjusts the margine width around the plot.
        -   You can alter these numbers to alter the space on the
            bottom, left, top and right sides of the plots. This allows
            you to fit larger graphs or give axis labels more room if
            the names are long.
-   Just like when you were making a png, you have to follow this
    argument with the code for each of your plots:

``` r
par(mfrow = c(2,2),mar=c(4,4.3,1,1))
plot(df.Baby1w.Bray,cex.axis=1.5,cex.lab=1.5,cex.main=2,col=1,
     pch=21,cex=2,xlim=c(-.45,.65),ylim=c(-.6,.45),xlab="PC1 (26.5%)",ylab="PC2 (21.3%)",bg=Color.Baby)
ordiellipse(df.Baby1w.Bray,BCData$BMI_Category_Final,col=c("#000000","#E79F00","#0072B2"),lwd=2,bg=2)
#legend(.25,-.33,c("Normal","Overweight","Obese"), 
#       pch=21,col=1,pt.bg=c("#000000","#E79F00","#0072B2"))
text(x = -0.43, y = .43, labels = "A", xpd = NA,cex=2.5)

plot(df.Baby1w.Bray,cex.axis=1.5,cex.lab=1.5,cex.main=2,col=1,
     pch=21,cex=2,xlim=c(-.45,.65),ylim=c(-.6,.45),xlab="PC1 (26.5%)",ylab="PC2 (21.3%)",bg=c(as.numeric(BCData$ROUT)+5))
ordiellipse(df.Baby1w.Bray,BCData$ROUT,col=c(6,9),lwd=2,bg=2)
#legend(.25,-.33,c("Vaginal","C-Section","Missing"), 
#       pch=21,col=1,pt.bg=c(6,9,0))
text(x = -0.43, y = .43, labels = "B", xpd = NA,cex=2.5)

plot(df.Baby1w.Bray,cex.axis=1.5,cex.lab=1.5,cex.main=2,col=1,
     pch=21,cex=2,xlim=c(-.45,.65),ylim=c(-.6,.45),xlab="PC1 (26.5%)",ylab="PC2 (21.3%)",bg=c(as.numeric(BCData$Exclusive_BF)+8))
ordiellipse(df.Baby1w.Bray,BCData$Exclusive_BF,col=c(8,9),lwd=2,bg=2)
#legend(.05,-.4,c("Breast Milk \u2265 50%","Breast Milk < 50%"),
#       pch=21,col=1,pt.bg=c(9,8))
text(x = -0.43, y = .43, labels = "C", xpd = NA,cex=2.5)

plot.new()
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/multi-panel-plot-example-1.png)

-   We’re missing a plot in our square, but you get the point.

-   So, what if you’re interested in seeing what bacteria are associated
    with your Bray-Curtis PCoA?

-   We can use this code, Plot.Taxa()

``` r
Plot.Taxa<-function(OTUS,Data.PCOA,TaxName,CutOff=1,pval=0.05,...){
  colnames(OTUS)<-TaxName
  row<-rowSums(OTUS)
  row<-sum(row)
  col<-colSums(OTUS)
  ratio<-as.matrix(col/row*100)
  ratio<-cbind(TaxName,ratio)
  subset<-data.frame(ratio[ratio[,2]>=CutOff,])
  subset<-data.frame(subset[!subset[,1]=="unclassified unclassified",])
  newOTUS<-data.frame(OTUS[,colnames(OTUS) %in% subset[,1]])
  colname<-colnames(newOTUS)
  colnames(newOTUS)<-gsub("\\."," ",colname)
  fit<-envfit(Data.PCOA, newOTUS)
  plot(fit, p.max=pval,...)
  fit$vectors$r[fit$vectors$pvals<0.05]
}
```

-   This command takes several variables:
    -   The OTU table
    -   The PCoA output from Sor.bray.pcoa()
    -   The TaxName file containing the bacterial names
    -   A Cutoff value determining what abundance the bacteria have to
        have in order to be evaluated
        -   We usually use a cutoff of 1%, which is a 1 in this
            function.
    -   The p-value cutoff to use for plotting significant taxa.
-   This code only works for the Bray-Curtis PCoA since it plots the
    correlation between bacterial abundance and the plot coordinates.
-   Let’s see what this looks like:

``` r
df.Baby1w.Bray<-Sor.bray.pcoa(Data.Baby1w[,-c(1:3)],Dim=2,Color=BCData$BMI_Category_Final,binary=FALSE)
ordiellipse(df.Baby1w.Bray,groups=BCData$BMI_Category_Final,col=c(1,2,3))
Plot.Taxa(Data.Baby1w[,-c(1:3)],df.Baby1w.Bray,TaxName,1,0.05)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/plot-envfit-1.png)

    ## unclassified Enterobacteriaceae                     Bacteroides 
    ##                       0.5469686                       0.4122712 
    ##                 Bifidobacterium                 Parabacteroides 
    ##                       0.4585372                       0.1494442 
    ##     Clostridium_sensu_stricto_1            Escherichia Shigella 
    ##                       0.5506705                       0.9294811 
    ##                    Enterococcus 
    ##                       0.2384338

-   What this is showing is the directionality of the bacterial
    abundances; the arrows point towards samples that have a higher
    abundance of that bacteria

    -   For example, the samples at the top of the plot are much more
        abundant in Escherichia than samples at the bottom

-   The output under the plot is telling you the R-squared value of that
    bacteria (i.e., how well the bacterial correlation fits your PCoA
    plot)

-   Let’s test for differences in the abundances of each bacteria by
    group. To do this, we will use a negative binomial model.

    -   A negative binomial model is used for count data that has a high
        variance. In our case, the variance is often times equal to or
        much greater than the mean abundance of any given bacteria

-   Before we get to the negative binomial model, we’re going to remove
    our low-abundance OTUS.

    -   There are many bacteria that are in our samples at a very low
        abundance. We aren’t excluding them because they aren’t
        important per se, but because their effect on the gut and their
        interaction with the host will likely be less biologically
        relevant than that of more abundant bacteria.
    -   Here we use Subset.Taxa()

``` r
Subset.Taxa<-function(OTUS,TaxName,CutOff=1){
  colnames(OTUS)<-TaxName
  row<-rowSums(OTUS)
  row<-sum(row)
  col<-colSums(OTUS)
  ratio<-as.matrix(col/row*100)
  ratio<-cbind(TaxName,ratio)
  subset<-data.frame(ratio[ratio[,2]>=CutOff,])
  subset<-data.frame(subset[!subset[,1]=="unclassified unclassified",])
  subset<-data.frame(subset[!grepl("uncultured_ge",subset[,1]),])
  newOTUS<-data.frame(OTUS[,colnames(OTUS) %in% subset[,1]])
  colname<-colnames(newOTUS)
  colnames(newOTUS)<-gsub("\\."," ",colname)
  return(newOTUS)
}
```

-   Subset.Taxa() takes
    -   You OTU table
    -   The TaxName file
    -   And a Cutoff for the minimum average abundance a bacteria has to
        have to be included in this analysis
        -   Average abundance for an OTU is calculated by summing
            together all counts for that OTU and dividing by the total
            number of counts across all samples then multiply by 100 to
            get percent abundance.
        -   The Cutoff value you give as an input here is a percentage.
-   Here’s how you use the command:

``` r
newOTUS<-Subset.Taxa(Data.Baby1w[,-c(1:3)],TaxName=TaxName,CutOff=1)
```

-   Here, we are specifying we want to keep bacteria that have an
    average abundance of greater than or equal to 1% relative abundace.

-   Now, let’s run a neagative binomial model of all the bacteria that
    have a realtive abundance \>=1% and see if any of them are different
    by maternal BMI:

``` r
NB.overall<-function(newOTUS,Group){
m<-as.matrix(NA)
n<-as.matrix(NA)
o<-as.matrix(NA)
for (i in 1:ncol(newOTUS)){
  l<-glm.nb(newOTUS[,i]~Group)
  m<-anova(l)
  n[i]<-data.frame(m[2,5])
  o[i]<-colnames(newOTUS[i])
}
n<-p.adjust(n, method="BH")
p<-cbind(o,n)
return(p)
p[,1]<-as.character(p[,1])
p[,2]<-as.numeric(as.character(p[,2]))
par(mar=c(10,4,1,1))
plot(p[,2],xaxt = "n",ylim=c(0,1),xlab="",pch=16,ylab="p-value",main="Overall p-values")
axis(1, at=1:nrow(p), labels=FALSE)
text(x=c(1:nrow(p)), y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
     labels=p[,1], srt=45, adj=1, xpd=TRUE)
abline(h=0.05)
}
```

-   NB.overall() will calculate the overall p-value for your comparison.
    -   “newOTUS” is the Subset.Taxa() output
    -   “Group” is the group you want to compare.

``` r
Groups<-BCData
Group<-Groups$BMI_Category_Final

p<-NB.overall(newOTUS,Group)
```

-   So. You’ll probably get an error message that says something like
    this:

### Warning message:

### In theta.ml(Y, mu, sum(w), w, limit = control*m**a**x**i**t*, *t**r**a**c**e* = *c**o**n**t**r**o**l*trace \> : iteration limit reached

-   What this basically means is your taxa distribution is treated as
    Poisson (another type of distribution that is used to interpret
    count data).

    -   It’s really a problem when this occurs, it’s just telling you
        your data distribution is not what it expected.

-   Other things to note are the p.adjust() function, which is false
    discovery rate adjusting the p-values we generated.

    -   Whenever you perform multiple comparisons in a dataset (i.e.,
        generate multiple statistical tests in a dataset), you need to
        adjust for the possibility of a false positive.
    -   Why? Well, a p\<0.05 is telling you that there is less than a 5%
        chance that your results are due to random chance.
    -   However, if you were to perform 100 tests there is a 5% chance
        that each test will be a false positive. Meaning, in 100 tests,
        you’d expect there to be 5 false positives just due to random
        chance.

-   So, when we ran our negative binomial regression on several
    bacterial taxa, the chance we might get a false positive due to the
    number of tests we’re doing is increased. Therefore we have to
    adjust our results to account for the possibility of a false
    discovery.

-   Let’s look at our p-value plot.

-   This plot shows the p-value on the y axis and the bacteria being
    tested on the x axis. Any dot that is below the horizontal line has
    a p value less than 0.05, so it is a significant bacterial
    association.

-   It looks like we have 5 taxa that are significantly associated with
    maternal BMI. However, this data doesn’t tell use which BMI category
    is different from the others. To figure that out, we need to do
    pariwise significance testing

    -   In other words, we need to compare Normal-Overweight,
        Normal-Obese and Overweight-Obese.

-   So, I can’t guarantee that this code will work in all cases, but
    it’s so much nicer and easier to use than what I had before. Maybe
    someone who knows what they’re doing will make this code more
    sensible in the future.

``` r
NB.pairwise<-function(newOTUS,Group){
  Group<-as.factor(Group)
  grp<-length(levels(Group))
  otu.name<-colnames(newOTUS)
  p.vals<-data.frame()
  comp<-c()
  for(i in 1:grp){
    if(levels(Group)[1]!=levels(Group)[i]){
      comp<-c(comp,paste(levels(Group)[1],"vs",levels(Group)[i]))
    }
  }
  for (i in 1:ncol(newOTUS)){
    l<-glm.nb(newOTUS[,i]~Group)
    m<-data.frame(coef(summary(l))[,4][2:length(levels(Group))])
    j<-1
    while(j!=grp){
      p.vals[i,j]<-m[j,]
      j<-j+1
    }
  }
  for(i in 1:(grp-1)){
    p.vals[,i]<-p.adjust(p.vals[,i], method="BH")
  }
  overall<-cbind(otu.name,p.vals)
  colnames(overall)<-c("Taxa",comp)
  return(overall)
}
```

-   This code runs the same negative binomial regression as before, but
    will tell you some of the pariwise relationships between your
    groups.
-   “Some?” you may ask. Well, you’ll see in a little bit.
-   For now, let’s take a look at the companion code to the pairwise
    negative binomial code:

``` r
p.plot<-function(NB.pair,title="Plotted p-values"){
  taxa<-NB.pair[,1]
  p<-NB.pair[,-1]
  par(mar=c(10,4,1,1))
  plot(p[,1],xaxt = "n",ylim=c(0,1),xlab="",pch=16,ylab="p-value",main=paste(title))
  text(x=c(1:length(taxa)), y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
       labels=taxa, srt=45, adj=1, xpd=TRUE)
  legend(1,.95,legend=paste(colnames(p)), pch=16,col=seq(1,ncol(p)))
  axis(1, at=1:nrow(p), labels=FALSE)
  abline(h=0.05)
  for(i in 2:ncol(p)-1){
    par(new=TRUE)
    plot(jitter(1:nrow(p)),p[,i+1],ylim=c(0,1),xaxt = "n",pch=16,xlab="",yaxt = "n",ylab="",col=c(i+1))
  }
}
```

-   This code takes the output from NB.pairwise() and plots the p-values
    like we did previously. Put it all together and it looks like this:

``` r
Group<-BCData$BMI_Category_Final
temp<-NB.pairwise(newOTUS,Group)
p.plot(temp)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/post-hoc-test-and-plotting-pvalues-1.png)

-   Wow. Amazing. This graph is telling you what bacterial taxa are
    significantly different between “Normal” and the other two BMI
    categories.
    -   Sometimes you’ll see that a pairwise comparison is significant
        while the overall p-value (which we calculated previously) is
        not. If this is the case, ignore the pairwise significance.
-   This plot doesn’t tell us the relationship between “Overweight” and
    “Obese”.
    -   Remember way back at the beginning when we reordered the BMI
        Category levels so it went from “Normal, Overweight, Obese”
        instead of “Normal, Obese, Overweight”?
    -   R always uses the first factor in the list as the reference that
        it compares the others to. In this case, that’s why we get a
        comparison of only “Normal-Overweight” and “Normal-Obese”
    -   To get the “Overweight-Obese” comparison, we need to change the
        order of our levels again so that “Overweight” is first, like
        so:

``` r
Group<-factor(Group, levels(Group)[c(2,1,3)])
levels(Group)
```

    ## [1] "Overweight" "Normal"     "Obese"

-   Then, we run the exact same code as before:

``` r
temp<-NB.pairwise(newOTUS,Group)
p.plot(temp)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/unnamed-chunk-42-1.png)

-   Finally, let’s put all our bacterial abundances in a table for easy
    presentation with NB.table().

``` r
NB.table<-function(OTUS,newOTUS,Group){
  Group<-factor(Group)
  total<-rowSums(OTUS)
  rel.otu<-newOTUS/total*100
  overall<-paste(round(colMeans(rel.otu),1),"\u00b1",round(apply(rel.otu,2,sd),1))
  taxa.mean<-as.matrix(round(aggregate(rel.otu,list(Group),mean)[,-1],1))
  taxa.sd<-as.matrix(round(aggregate(rel.otu,list(Group),sd)[,-1],1))
  taxa1<-t(matrix(nrow=length(unique(Group)),paste(taxa.mean,"\u00b1",taxa.sd)))
  colnames(taxa1)<-levels(Group)
  tables<-cbind(matrix(colnames(taxa.mean)),overall,taxa1)
  return(tables)
}
```

-   NB.table takes:
    -   “OTUS”: your OTU table.
    -   “newOTUS”: the edited OTU table that only includes taxa above
        your desired abundance threshold.
    -   “Group”: the group variable you want for comparison

``` r
test<-NB.table(Data.Baby1w[,-c(1:3)],newOTUS,Group)
test
```

    ##                                         overall       Overweight   
    ##  [1,] "unclassified Enterobacteriaceae" "13.1 ± 19.2" "15.6 ± 25.2"
    ##  [2,] "Bacteroides"                     "8.7 ± 13.1"  "8 ± 14.1"   
    ##  [3,] "Bifidobacterium"                 "11.3 ± 15.6" "15.3 ± 19.6"
    ##  [4,] "Megasphaera"                     "5.8 ± 17.3"  "6 ± 19"     
    ##  [5,] "Veillonella"                     "3.9 ± 9.6"   "5.8 ± 15.8" 
    ##  [6,] "Parabacteroides"                 "3.4 ± 9.1"   "1.4 ± 2.6"  
    ##  [7,] "Streptococcus"                   "3.6 ± 6.2"   "0.9 ± 1.2"  
    ##  [8,] "Clostridium_sensu_stricto_1"     "9.2 ± 14.6"  "12.6 ± 18.6"
    ##  [9,] "Escherichia Shigella"            "21.9 ± 24.5" "19.9 ± 20.1"
    ## [10,] "Akkermansia"                     "1.1 ± 6.3"   "0 ± 0"      
    ## [11,] "Staphylococcus"                  "2.6 ± 5.7"   "2.1 ± 2.5"  
    ## [12,] "Enterococcus"                    "1.1 ± 2.3"   "0.8 ± 1.7"  
    ## [13,] "Klebsiella"                      "6.8 ± 14.7"  "4.3 ± 9.5"  
    ##       Normal        Obese        
    ##  [1,] "6.4 ± 10.7"  "16.5 ± 19.4"
    ##  [2,] "13.3 ± 14"   "5.6 ± 11.4" 
    ##  [3,] "12.5 ± 17.2" "7.5 ± 10.5" 
    ##  [4,] "0.1 ± 0.2"   "10.1 ± 22.1"
    ##  [5,] "4.8 ± 7"     "1.8 ± 4.1"  
    ##  [6,] "3.5 ± 6.7"   "4.7 ± 13.3" 
    ##  [7,] "7.1 ± 9.3"   "2.8 ± 4.2"  
    ##  [8,] "6.6 ± 9.5"   "8.9 ± 15.1" 
    ##  [9,] "28.1 ± 28.3" "18.4 ± 24.7"
    ## [10,] "0 ± 0"       "2.7 ± 10"   
    ## [11,] "5.3 ± 9.4"   "0.7 ± 1.4"  
    ## [12,] "1.2 ± 2.7"   "1.4 ± 2.5"  
    ## [13,] "5.3 ± 13"    "9.8 ± 18.9"

-   I added a couple more things below that you may or may not need to
    use. It mostly makes neat figures.
    -   Let’s start with making a step plot from our PCoA data

``` r
stepplot.pc1<-function(sor.bray.df,sample_ids,col=1,pc="PC1",...){
  m<-factor(sample_ids)
  temp<-data.frame(cbind(sor.bray.df,m,col))
  temp[,1]<-as.numeric(as.character(temp[,c(1)]))
  temp[,2]<-as.numeric(as.character(temp[,c(2)]))
  
  temp2<-temp[with(temp, order(ave(temp[,1], temp[,3], FUN =min), temp[,1])), ]
  
  temp2$m<-factor(temp2$m, levels=unique(temp2$m))
  plot(temp2$V1,temp2$m,col=factor(temp2[,4]),xlab=pc,ylab="Participant ID",yaxt="n",...)
  axis(side = 2, at = 1:length(unique(temp2$m)),labels = unique(temp2$m),las=1)
}
```

-   Just so you know, I have no idea what this sort of plot should be
    called, but my mental shorthand for it has been “step plot”. So, if
    no one but people from our lab have any idea what you’re talking
    about, you’ll know why.

-   This takes a Sor.bray.pcoa() output, sample ids, any colors/groups
    you want, and the PC1 label (so you can add the % variance to the
    plot)

    -   NOTE: This is most useful if you have timeseries data since you
        can track changes over PC1 for every individual relatively
        easily. You can definitely use it for single timepoint data
        though, as we are about to do.

``` r
df.Baby1w.Bray<-Sor.bray.pcoa(Data.Baby1w[,-c(1:3)],Dim=2,Color=BCData$BMI_Category_Final,binary=FALSE)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/sorensen-pc1-step-plot-1.png)

``` r
stepplot.pc1(df.Baby1w.Bray,BCData$Group,BCData$BMI_Category_Final)
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/sorensen-pc1-step-plot-2.png)

-   This creates a plot showing the PC1 position for each participant.

-   The other plot we’re going to make is a plot showing the change in
    microbiota abundance between two timepoint for each individual. For
    this, we’re going to switch to the SPICE dataset.

    -   The SPICE data collected microbiota data from 3 timepoints per
        participant, allowing us to look for changes in abundance over
        time

``` r
Data<-read.table("Wheat_Data.Subsample.Genus.shared",header=TRUE)
Data<-Data[-c(109:110,29),]
TaxName<-read.table("Wheat_tax_names.taxonomy",header=TRUE)
TaxName<-Edit.Taxname(TaxName$Taxonomy,1)
```

    ## Warning: Expected 6 pieces. Additional pieces discarded in 1230 rows [1, 2, 3, 4, 5, 6,
    ## 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
TaxName<-gsub(" ","",TaxName)
id<-gsub("MIWH|A|B|C|D","",Data$Group)
time<-ifelse(grepl("A",Data$Group),"A",ifelse(grepl("B",Data$Group),"B",ifelse(grepl("C",Data$Group),"C",ifelse(grepl("D",Data$Group),"D",NA))))
meta<-read.csv("Wheat_sample_form.csv",header=TRUE)
meta$SubjectID.MIWH...<-paste0(meta$SubjectID.MIWH...,meta$Week.number)

temp<-merge(meta,Data,by.x="SubjectID.MIWH...",by.y="Group")
temp$SubjectID.MIWH...<-gsub("A|B|C|D","",temp$SubjectID.MIWH...)
```

-   So, here’s our data.

``` r
bubble.plot<-function(OTUs_group1,OTUs_group2,ids_group1,ids_group2){
  rel_1<-OTUs_group1/rowSums((OTUs_group1))*100
  colnames(rel_1)<-TaxName
  rel_2<-OTUs_group2/rowSums((OTUs_group2))*100
  colnames(rel_2)<-TaxName
  
  rel_1<-rel_1[ids_group1%in%ids_group2,]
  rel_2<-rel_2[ids_group2%in%ids_group1,]
  
  ids<-ids_group1[ids_group1%in%ids_group2]
  ids<-ids[ids%in%ids_group1]
  
  rel.12<-rel_1[,-c(1:3)]-rel_2[,-c(1:3)]
  rel.12.col<-colSums(rel.12)
  
  temp<-NULL
  for (i in 1:ncol(rel.12)){
    if(abs(min(rel.12[,i]))<=1){
      temp<-c(temp,i)
    }
  }
  rel.12.edit<-rel.12[,-c(temp)]
  rel.12.edit<-rel.12.edit[,ncol(rel.12.edit):1]
  rel.12.edit<-data.frame(ids,rel.12.edit)
  
  
  df <- reshape2::melt(rel.12.edit)
  circles<-ifelse(df$value>0,20,ifelse(df$value<0,21,NA))
  size<-ifelse(abs(df$value)>=1&abs(df$value)<5,1,ifelse(abs(df$value)>=5&abs(df$value)<10,2,ifelse(abs(df$value)>=10&abs(df$value)<20,3,ifelse(abs(df$value)>=20,4,0))))
  
  par(mar=c(1,15,5,3), xpd=T)
  plot(y=as.integer(df$variable), x=as.integer(factor(df$ids)), pch=circles, cex=size, bty="n", axes = F, xlab="", ylab="")
  axis(2, at = unique(as.integer(df$variable)), labels = levels(df$variable), line = 0.5, las=2)
  axis(3, at = unique(as.integer(factor(df$ids))), labels = levels(factor(df$ids)), line = 0.5,las=2)
  legend(y=max(as.integer(df$variable)/1.5), x=max(as.integer(df$ids))+0.25, legend = c("+20%","+10%","+5%","+1%","~0%","-1%","-5%","-10%","-20%"), pch = c(20,20,20,20,0,21,21,21,21), bty="n", pt.cex=c(4,3,2,1,0,1,2,3,4))
}
```

-   And here’s our function. Honestly, the way I wrote this is kinda
    garbage, but I tried to make it transparent enough that you may be
    able to run each line of the function independently, see what the
    output is, and then fix any issues that may arise.
-   But let’s see what happens when it works.

``` r
OTUs_group1<-temp[temp$Week.number=="A",-c(1:25)]
OTUs_group2<-temp[temp$Week.number=="B",-c(1:25)]
ids_group1<-temp[temp$Week.number=="A",1]
ids_group2<-temp[temp$Week.number=="B",1]

bubble.plot(OTUs_group1,OTUs_group2,ids_group1,ids_group2)
```

    ## Using ids as id variables

    ## Warning in legend(y = max(as.integer(df$variable)/1.5), x =
    ## max(as.integer(df$ids)) + : NAs introduced by coercion

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/bubble-plot-of-wheat-data-1.png)

-   You need 4 inputs for this code.
    -   The first two are OTU tables for your comparison
        groups/timepoints.
    -   The last two are the IDs associated with the two OTU files, so
        we can match each participant to themselves. The results show
        how the taxa abundances changed between the two point using the
        size and fill of the circles.
-   Next is code to draw arrows between participant samples on a PCoA
    plot

``` r
paired.arrows<-function(pcoa.df,id,group,term1,term2,lwd=1){
  test<-cbind(paste0(as.character(group),"_-_",id),pcoa.df)
  a<-data.frame(test)
  a[,1]<-as.character(a[,1])
  a[,2]<-as.numeric(as.character(a[,2]))
  a[,3]<-as.numeric(as.character(a[,3]))
  test<-a
  for(i in (1:nrow(test))){
    for(j in (1:nrow(test))){
      if(gsub('.*_-_',"",test[i,1])==gsub('.*_-_',"",test[j,1])&gsub('_-_.*',"",test[i,1])==paste(term1)&gsub('_-_.*',"",test[j,1])==paste(term2)){
        arrows(x0=test[i,2],y0=test[i,3],x1=test[j,2],y1=test[j,3],lty=1,length=0.1,lwd=lwd)
      }
    }
  }
}
```

-   This function takes
    -   pcoa.df: the pcoa coordinates created by Sor.bray.pcoa()
    -   id: the id numbers (just the numbers) of your participants
    -   group: the timepoint of each sample
    -   term1: origin point of arrow (e.g., A-\>)
    -   term2: ending point of arrow (e.g., -\>B)
-   To make this plot, we’re going to use the SPICE data again

``` r
Data<-read.table("Wheat_Data.Subsample.Genus.shared",header=TRUE)
Data<-Data[-c(109:110,29),]
TaxName<-read.table("Wheat_tax_names.taxonomy",header=TRUE)
TaxName<-Edit.Taxname(TaxName$Taxonomy,1)
```

    ## Warning: Expected 6 pieces. Additional pieces discarded in 1230 rows [1, 2, 3, 4, 5, 6,
    ## 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
TaxName<-gsub(" ","",TaxName)
id<-gsub("MIWH|A|B|C|D","",Data$Group)
time<-ifelse(grepl("A",Data$Group),"A",ifelse(grepl("B",Data$Group),"B",ifelse(grepl("C",Data$Group),"C",ifelse(grepl("D",Data$Group),"D",NA))))
meta<-read.csv("Wheat_sample_form.csv",header=TRUE)
meta$SubjectID.MIWH...<-paste0(meta$SubjectID.MIWH...,meta$Week.number)

temp<-merge(meta,Data,by.x="SubjectID.MIWH...",by.y="Group")
temp$SubjectID.MIWH...<-gsub("A|B|C|D","",temp$SubjectID.MIWH...)

bray.df<-Sor.bray.pcoa(Data[,-c(1:25)],binary=FALSE)
paired.arrows(bray.df, id=id, group=time, term1="A", term2="B")
paired.arrows(bray.df, id=id, group=time, term1="B", term2="C")
```

![](20200327_V1.3_Workshop_Code_ARCHBG_Markdown_files/figure-markdown_github/plot-pcoa-of-paired-samples-1.png)
