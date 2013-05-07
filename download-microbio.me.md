
<link href="http://joey711.github.com/phyloseq/markdown.css" rel="stylesheet"></link>


Download microbio.me/qiime datasets
========================================================
[Originally hosted here](http://joey711.github.io/phyloseq/download-microbio.me.html)

Here is an example accessing microbiome datasets from a public repository, using entirely R code. I haven't yet figured out how to list the available studies at `microbio.me/qiime` from within R, but I have provided illustrate instructions for finding details about studies you might want to download and explore, and some example FTP addresses that I used after doing just that. Note that you likely need to create an account at `microbio.me/qiime` in order to explore the studies they have available for download.

## Explore microbio.me/qiime

There are datasets posted on [microbio.me/qiime](http://www.microbio.me/qiime/index.psp), and these usually have the "raw" OTU clustered data hosted at an FTP address. I'm not sure of a "nice" way to explor the details of the different studies using the FTP address directly, but I can post a few hand-picked datasets and import them all.

First, [create an account and login](http://www.microbio.me/qiime/index.psp). Here is what the frontpage should look like:

<img src="microbio-me-login.png" width="750px" />

Once you have logged-in, you will see a different screen. Click on "Get Study Summary and Raw Data", which will be a link near the top of the page (highlighted at the bottom of the following clipped page):

<img src="microbio-me-download-data.png" width="400px" />

And once you're at this "raw data" page, you will see a box below "Available Studies". The box is a large scrollable list, and the page below it will start off blank:

<img src="microbio-me-data-databox.png" width="250px" />

But once you click on something, details and additional links for that particular study will pop up. For example, I clicked on a version of the "Global Patterns" dataset in the box, which exposed a page below with further details about the study and links for data. If available, there is an FTP link called "Sequences, Mapping and OTU Table", which can be accessed directly from within R using the code provided farther down this page. The link itself is highlighted on this screenshot:

<img src="microbio-me-gpdata.png" width="250px" />


---

# Download, parse datasets

Load phyloseq package... and ggplot2 for plotting at the end...


```r
library("phyloseq")
packageVersion("phyloseq")
```

```
## [1] '1.5.8'
```

```r
library("ggplot2")
packageVersion("ggplot2")
```

```
## [1] '0.9.3.1'
```

```r
theme_set(theme_bw())
```


Here I define a function to download and parse data from the FTP server for `microbio.me`. This uses some useful standard R code for downloading, unzipping, creating temporary files and directories, manipulating filenames and so forth. It also includes a few lines for importing and parsing the microbiome data using [the phyloseq package](http://joey711.github.io/phyloseq/).


```r
microbio.me.ftp = function(zipftp) {
    require("phyloseq")
    zipfile = tempfile()
    download.file(zipftp, zipfile)
    import_dir = tempdir()
    unzip(zipfile, exdir = import_dir)
    studyname = gsub("_split_library_seqs_and_mapping.zip", "", basename(zipftp), 
        fixed = TRUE)
    biomfile = paste0(import_dir, "/", studyname, "_closed_reference_otu_table.biom")
    biom = import_biom(biomfile, parseFunction = parse_taxonomy_greengenes)
    sdfile = paste0(import_dir, "/", studyname, "_mapping_file.txt")
    sample_metadata = import_qiime_sample_data(sdfile)
    return(merge_phyloseq(biom, sample_metadata))
}
```


Here are several hand-picked FTP addresses of datasets I wanted to include in subsequent analysis.


```r
# Restroom Surfaces - Flores_restroom_surface_biogeography
restroomftp = "ftp://thebeast.colorado.edu/pub/QIIME_DB_Public_Studies/study_1335_split_library_seqs_and_mapping.zip"
restroom = microbio.me.ftp(restroomftp)
# # Global Patterns - CaporasoIlluminaPNAS2011_3prime gpftp =
# 'ftp://thebeast.colorado.edu/pub/QIIME_DB_Public_Studies/study_721_split_library_seqs_and_mapping.tgz'
# gp = microbio.me.ftp(gpftp) smokers - Charlson_cigarette_smokers
smokeftp = "ftp://thebeast.colorado.edu/pub/QIIME_DB_Public_Studies/study_524_split_library_seqs_and_mapping.zip"
smokers = microbio.me.ftp(smokeftp)
# rs - resistant starches - Martinez_Resistant_starches
rsftp = "ftp://thebeast.colorado.edu/pub/QIIME_DB_Public_Studies/study_495_split_library_seqs_and_mapping.zip"
rs = microbio.me.ftp(rsftp)
# abt - antibiotic timecourse - Relman_antibiotic_timeseries
abtftp = "ftp://thebeast.colorado.edu/pub/QIIME_DB_Public_Studies/study_494_split_library_seqs_and_mapping.zip"
abt = microbio.me.ftp(abtftp)
# vagina - vaginal microbiome - Ravel_reproductive_women_vagina
vaginaftp = "ftp://thebeast.colorado.edu/pub/QIIME_DB_Public_Studies/study_509_split_library_seqs_and_mapping.zip"
vagina = microbio.me.ftp(vaginaftp)
# palms - Fierer_undergraduate_palms
palmsftp = "ftp://thebeast.colorado.edu/pub/QIIME_DB_Public_Studies/study_317_split_library_seqs_and_mapping.zip"
palms = microbio.me.ftp(palmsftp)
```


## Plots
And some plots, because humans like images and it also helps demonstrate that the data is now directly available for analysis.

```r
dlist = list(restroom, smokers, rs, abt, vagina, palms)
# Remove 'empty' OTUs and samples from each dataset.  Note that I've
# chosen to keep only samples that have more than 100 total reads
dlist = lapply(dlist, function(physeq) {
    physeq = prune_taxa(taxa_sums(physeq) > 0, physeq)
    physeq = prune_samples(sample_sums(physeq) > 100, physeq)
})
# lapply(dlist, plot_richness)
plot_richness(smokers, x = "AGE", color = "SEX", shape = "BODY_PRODUCT")
```

![plot of chunk list-datasets](figure/list-datasets1.png) 

```r
plot_richness(vagina, x = "NUGENT_SCORE", color = "ETHNICITY") + stat_smooth(method = lm)
```

![plot of chunk list-datasets](figure/list-datasets2.png) 

```r
plot_richness(vagina, x = "PH", color = "ETHNICITY") + stat_smooth(method = lm)
```

![plot of chunk list-datasets](figure/list-datasets3.png) 



