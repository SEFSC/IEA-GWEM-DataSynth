# Fish-MIP training for NEMoW
This repository contain a `R` markdown notebook showing how to access climate model outputs from the [Inter-Sectoral Impact Model Intercomparison Project (ISIMIP) Repository](https://data.isimip.org/). This notebook uses the [`isimip-client`](https://github.com/ISI-MIP/isimip-client/tree/db2f3bd8aaa81b9777e3b3eae7f4376471b44b00) library for `Python` via `reticulate` to access the ISIMIP repository.  
  
## Setting up your machine
To run this notebook, you will need to have `R` and `Python` install in your local machine. We also recommend that you install RStudio to interact with `R` easily.  
  
### `R` installation
You can find and download the `R` installation file for your operating system (Windows, Linux, macOS) on the [CRAN](https://cran.r-project.org/) website. Open the installation file in your machine and follow the prompts.  
  
This notebook was developed in `R` version 4.3.1 for Windows. You must install this version of `R` or higher for this notebook to run without issues. We are including the full session information below:

```R
R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8    LC_MONETARY=English_Australia.utf8
[4] LC_NUMERIC=C                       LC_TIME=English_Australia.utf8    

time zone: Australia/Hobart
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
 [1] compiler_4.3.1  fastmap_1.1.1   cli_3.6.1       htmltools_0.5.5 tools_4.3.1     rstudioapi_0.14 yaml_2.3.7     
 [8] rmarkdown_2.21  knitr_1.42      xfun_0.38       digest_0.6.31   rlang_1.1.1     evaluate_0.20  
```
  
**Note for Windows users only**  
It is recommended that Windows users install [RTools](https://cran.r-project.org/bin/windows/Rtools/) because some packages may need it during installation.  
  
### RStudio installation
RStudio is the most popular integrated development environment (IDE) use with `R`, and it is also the IDE that we will use for this training. You can download RStudio for free from [posit](https://posit.co/download/rstudio-desktop/). Make sure you choose the installation file that matches your operating system. Open the installation file in your machine and follow the prompts.  
  
### `Python` installation
We recommend that you install `Python` via **Miniconda**, which also includes the package manager **conda** and a smaller number of `Python` libraries used in data science. Find the installer for your operating system [here](https://docs.conda.io/en/latest/miniconda.html). Once download it, open the installer in your local machine and we recommend you follow the instructions for a *Regular Installation* given in the [conda website](https://conda.io/projects/conda/en/stable/user-guide/install/index.html).  

### Installing `R` libraries
We are including a `R` script that automatically checks that you have installed all libraries used in this notebook. If there are any libraries that you have not installed yet, the script will install them for you. To run this script, open RStudio an type the following lines in the console:
  
```R
  source("scripts/Installing_R_libraries.R")  
  checking_libraries()
```
  
**Note that you need to have installed `R` and RStudio prior to running this script.**
  
### Installing `Python` libraries
We have included an environment file in this repository, which will allow you to install all `Python` libraries needed to run this notebook with relative ease. You will need to follow these steps:  
  
1. Get the full path of the folder where you have cloned or downloaded this repository. For example, if this repository is located in your *Documents* folder, your full path should look something similar to `C:/Users/user_name/Documents/FishMIP_NOAA_workshop`.  
  
2. Open a *Terminal* window if you use macOS or Linux. If you use Windows, search for *Anaconda Prompt* in the Start menu and open it.  
  
3. Install `conda-lock` with the following line: `conda install -c conda-forge conda-lock`.  
  
4. Check the current location of your *Terminal* or *Anaconda Prompt*. This should be the only line in this window, which shows the location of your `home` directory, and it should look something like this: `C:/Users/user_name/`. If the current location of your window is different to the path from step 1, then navigate to the repository folder. You can do this with the `cd` (changing directory) command. For example: `cd C:/Users/user_name/Documents/FishMIP_NOAA_workshop`.   
  
5. Once you are in the repository folder, you will install all `Python` libraries by typing the following command: `conda-lock install --name fishmip conda-lock.yml` and press `Enter`. Installation will start and this make take a few minutes.  
  
5. Finally, you can check that you have installed everything correctly by typing the following: `conda activate fishmip`. You should not get any errors or messages if everything has been successful. You can now deactivate this environment by typing `conda deactivate`.  
  
## Linking `Python` to `R`
As mentioned above, we will use the `reticulate` package to call `Python` during an `R` session. To make it easy for `R` to find `Python`, we will provide the full path where `Python` and the libraries we need have been installed (also known as an environment). To do get this path, we will follow these steps:  
  
1. Open a *Terminal* window if you use macOS or Linux. If you use Windows, search for *Anaconda Prompt* in the Start menu and open it.  
  
2. Type `conda env list`, which will list all environments installed in your local machine and the full paths where they are stored. The output should look like this:  
  
```bash
# conda environments:
#
base                  *  C:\Users\user_name\AppData\Local\miniconda3
fishmip                  C:\Users\user_name\AppData\Local\miniconda3\envs\fishmip
```
  
3. Copy the name of the path next to `fishmip`, which is the name of the environment we previously installed.  
  
4. In RStudio go to the *Files* tab (by default is located on the bottom right), find the `.Rprofile` file and click to open it.  
  
5. Replace `paste_your_path_here` with the full file path you copied in step 3. Make sure you keep the quotation marks. The contents of the `.Rprofile` should look like this: `RETICULATE_PYTHON="C:\Users\user_name\AppData\Local\miniconda3\envs\fishmip"`.  
  

