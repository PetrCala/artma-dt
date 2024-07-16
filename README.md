﻿# A simple repository for my Diploma Thesis

- **Topic** - Ability bias in the returns to schooling: How large it is and why it matters
- **Author** - Bc. Petr Čala
- **Year of defense** - 2024
- **Supervisor** - doc. PhDr. Zuzana Havránková Ph.D.

## About

### How To Use

Clone this repository onto your computer using `git clone https://github.com/PetrCala/Diploma-Thesis.git`, or download it directly using the `<> Code` button above.' Refer to the `Prerequisites` and `How To Run` sections for more detail.

### Project structure

Here is a list of some crucial files you should get to know before running the project.

- `src/` -> Folder with the main file stack.
  - `data/` -> Folder for storing data. The folder is further split into two sub-folders:
    - `source/` -> Put your `.xlsx` or `.xlsm` source data file here. In the distributed folder, there is a placeholder file called `data_set_master_thesis_cala.xlsm`.
    - `temp/` - > This folder will automatically get created upon script run. Here will be the `.csv` files created from the sheets of your data set. This allows reproducibility and consistency within the script.
  - `pckg/` -> Folder with external packages that are not available online anymore, such as `LowRankQP`.
  - `resources/` -> Folder with various resources.
    - `user_parameters_model.yaml` -> Customizable parameters. This file should be copied into the project roote, renamed to `user_parameters.yaml`, and modified to fit the user preferences. Any modifications can be done by opening the file using any text editor, such as `Notepad`. Alternatively, you may want to edit this file using fancier text editors, such as [Sublime Text](https://www.sublimetext.com/docs/vintage.html), or [VIM](https://www.vim.org/).
  - `results/` -> Folder with all results. A `.zip` file with all results will be automatically created here.
    - `graphic/` -> All graphic results will be automatically stored here.
    - `numeric/` -> All numeric results will be automatically stored here as `.csv` files.
    - `text/` -> All TeX table code will be automatically stored here. This code is directly pasteable into .tex files for compilation (may require packages such as _longtable_,...).
    - `main_results.txt` -> An R console log file where all numeric/tabular results are stored in a presentable form.
  - `main_master_thesis_cala.R` -> Main script. Call the desired methods with the specified user parameters. Automatically handle package installation, working directory handling, temporary file creation.
  - `source_master_thesis_cala.R` -> Source script with all the functions. This script is not meant to be ran. Virtaully any function called from the main script is located here. Every function (hopefully) has a docstring explaining its _functionality_ (pun intended). Navigate the script using function names.
  - `script_runner_master_thesis_cala.R` -> Script for running the code in an aesthetic way. Calls the main script using the `source` command, which omits redundant code. This script is most useful when working with RStudio. When working with a terminal, calling this script is exactly the same as calling the main script.
- `README.md` -> This README file.
- `.lintr` -> Configuration file for linting.

Running the main script (directly or using the script runner) will also create these temporary folders/files:

- `_cache/` -> Temporary cache files will be stored here.
- `user_parameters.yaml` -> File with customizable parameters. See step 5 of the [Prerequisites section](#prerequisites) for explanation.

Furthermore, the existence of all folders will be verified. Note that some do not appear in the repository, as there is nothing to distribute within these folders. All results (along with the folders) will be created and updated automatically.

## Prerequisites

1. Install the newest version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/). This is important to enable external package handling.
2. Install [JAGS 4.3.1](https://mcmc-jags.sourceforge.io/). This software is required by the [RoBMA package](https://fbartos.github.io/RoBMA/). As of the current version, I am looking for a workaround to avoid having to install this application, but for now, there is no way around.
3. Clone the repository:

   ```bash
   git clone https://github.com/PetrCala/Diploma-Thesis
   ```

4. Change into the directory:

   ```bash
   cd Diploma-Thesis
   ```

5. Run

   ```bash
   chmod +x ./run.sh
   ./run.sh setup
   ```

This will attempt to install all required R packages. In case some the installation for some of these packages fails, you may have to install them manually. To see the list of required packages, go to `src/base/const.R`. 6. In case you wish to only test the functionality of the script using placeholder data within the data folder, skip to the **How to run** section below. If you wish, on the other hand, to run your own analysis, make sure to follow the next steps as well. 7. Parameter modification is handled through the `user_parameters.yaml` file. Upon cloning the repository, this file does not exist. It will automatically be created upon running any of the main scripts, where the scripts create this file in the project root with the contents of the model parameter file `resources/user_parameters_model.yaml`. Alternatively, you can manually copy this file to the project root yourself. If you do so, make sure to name it `user_parameters.yaml`. After the file is present in the project root (and named `user_parameters.yaml`, it can be modified to your liking. However, **when modifying the user parameters file, make sure to follow instructions from step 2 of the [How to run](#how-to-run) section.** 8. Place your data file into the `data/source/` folder, right next to the placeholder data file. In case you modified the path to this folder, make sure to place the data file into that folder instead. You may delete the placeholder file in case you do not need it. 9. The file with data must contain two sheets - `data_set` and `var_list` (these are modifiable within the user parameter file). The former should contain all your data that satisfies the conditions described in step 9, while the latter should contain information about variables of the dataset, as described in step 10. 10. The data frame must contain several specific columns, named **Required columns**, and there are also several columns that are optional - **Optional Columns**. If your dataset does not contain the required columns, make sure to add them. If it does contain them, but they are named differently in your data, simply change their names in the `required_cols` section of the `user_parameters.yaml` file. This is the list of the required and optional columns:

- Required columns:
  - **obs_id** - Unique ID of the observation.
  - **study_name** - Name of the study, such as _Einstein et al. (1935)_.
  - **study_id** - ID of the study. Should be numeric and unique for each study.
  - **effect** - The main effect/estimate values. If you employ any effect transformations, such as partial correlation coefficient, set this to the transformed effect.
  - **se** - Standard error of the effect (of the transformed effect).
  - **n_obs** - Number of observations associated with this estimate.
- Optional columns:
  - **t_stat** - t-statistic of the main effect. If set to `NA`, calculated automatically as a ratio of the effect
    and its standard error.
  - **precision** - Precision of the effect. If set to `NA`, calculated automatically if omitted using the `precision_type` parameter within the `adjustable_parameters` list of the `user_parameters.yaml` file. Defaults to _1/Standard_Error_.
  - **study_size** - Number of estimates reported per study. If set to `NA`, calculated automatically if omitted.
  - **reg_df** - Number of degrees of freedom associated with the regression. If set to `NA`, the number of observations associated with the estimate will be used instead.

10. The sheet labeled as `var_list` (referred to as _variable information_) contains the information about the nature and usage of all columns/variables that appear in the main data frame. You must create this sheet and fill it with information about your dataset in accordance to the following specifications. In doing so, you may use the placeholder dataset for inspiration. If the sheet is not provided, the script will not run. Note that the columns of the variable information sheet must be listed exactly in the order in which they appear in the main data frame. There may be no special characters, and only the explicitly stated values are permitted.
    The sheet should contain the following columns:

- **var_name** - Name of the variable exactly as it appears in the data frame columns. Must not include
  spaces and various special characters. Underscores are allowed, but the name should never begin with an underscore. Example: _n_obs_.
- **var_name_verbose** - A verbose name for the variable. Can be any subset of characters, even special ones. Example: _Number of Observations_.
- **var_name_description** - A lengthy description of the variable. Example: _Number of observations used in the study._
- **data_type** - Type of the data this variable holds. Can be only one type. Can be one of:
  - _int_ - Integer. Any integer.
  - _category_ - Categorical variable. Any string.
  - _float_ - Float. Any number.
  - _dummy_ - Dummy. Either 0 or 1.
  - _perc_ - Percentage. Any value between 0 and 1, inclusive.
- **group_category** - Group of the variable. Group similar together, otherwise make a new group.
  Examples - dummies, gender, urban vs. rural, short-run vs. long-run
- **na_handling** - Specify how missing values should be handled for the variable. Can be one of:
  - _stop_ - Do not allow missing values. Throw an error in case there is a missing value.
  - _mean_ - Interpolate with the mean of the existing data.
  - _median_ - Interpolate with the median of the existing data.
  - _foo_ - Interpolate with random values. These columns should not be used for further analysis (BMA,...), as their values will be random, and thus incorrect.
- **variable_summary** - Boolean. If `TRUE`, this variable will appear in the summary statistics table.
- **effect_sum_stats** - Boolean. If `TRUE`, this variable will appear in the effect summary statistics table.
- **equal** - Float. If set to any value, the effect summary statistics table will print out the statistics
  for the main effect of the data when subsetted to this variable equal to the specified value.
  If set to any value, can not set the `gtlt` column value.
- **gtlt** - One of "_median_", "_mean_", float. Similar to "equal", but if set to _median_/_mean_, will print out the statistics
  for the effect of the data when subsetted to values above/below the median value of this variable.
  If set to float (meaning any number), the subsetting breakpoint will be that value instead.
- **bma** - Boolean. If `TRUE`, this variable will be used in the Bayesian model averaging. Do NOT set all
  values of one variable group to `TRUE`. This would create a dummy trap.
- **bma_reference_var** - Boolean. If `TRUE`, this variable is the reference variable for the **dummy**/**perc** group. Exactly one variable must be a reference variable
  for each **dummy**/**perc** group.
- **to_log_for_bma** - Boolean. If `TRUE`, this variable will be converted to logarithm during the
  Bayesian model averaging.
- **bpe** - If set to any value, this value will be used when evaluating the best practice estimate. Can also be one of the following: `mean`, `median`, `max`, `min`. If you do not wish to use this variable in the best practice estimate, set its value to `stop`. Be careful **not to set the value to** `FALSE`, as that will raise an error.
- **bpe_sum_stats** - Boolean. If `TRUE`, this variable will appear in the BPE summary stats table.
- **bpe_equal** - Similar idea to **equal**, only for the BPE summary stats table.
- **bpe_gtlt** - Similar idea to **gtlt**, only for the BPE summary stats table.

## How to Run

To run the code, follow these steps:

1. There are two options of running the script:

- **Use a script runner** - You can run the code in an aesthetic way using the R script `script_runner_master_thesis_cala.R`.
  First, modify the parameters within the `user_parameters.yaml` file as you see fit, but in line with the guidelines from the next step. Then, simply run the code.
- **Use the main script** - You can also run the main code by directly calling the `main_master_thesis_cala.R` file. The disadvantage of this approach (when working with RStudio) is that unnecessary code will be printed into the console. If you are calling this file from the terminal, things will work as intended.
  As with the script runner, make sure to modify the within the parameters `user_parameters.yaml` file as you see fit, and in doing so follow the guidelines from step 2.

2. Guidelines for parameter modification:

- Do not change the names of any of the parameters, unless told explicitly. Change only the values.
- Parameters used for calling external functions are marked with the `param_` prefix within their name (see parameters for BMA, non-linear tests,...). Make sure to keep this convention when adding new such parameters.
- Make sure to keep the object types, unless told explicitly. For example, if a value of a parameter is a vector, make sure it is still a vector after the modifications.
- `NA` values should be denoted as `.na`. Boolean values should be denoted using `true` or `false`. Null values should be denoted as `null`. None of these should have quotes around them. Use quotes only for characters.

3. If you want to run the code using your own data and not the placeholder data provided within the distributed files, put your `.xlsx` data file into the `data/source/` folder. You do not need to delete the distributed placeholder file, but you **must change the expected source file name in the user parameters** for the script to recognize these new files (parameters under the category `source_file_params`).
   Furthermore, modify any other source file parameters so that they fit your data. These include the file name, sheet names, file suffix, and the suffix you wish to attach to the created `.csv` files. If you do not modify these, your data may be read incorrectly.
4. After modifying any paramters as you see fit, run either the script runner or the main script. Note that running the script runner executes virtually the same job as running the main script, only less redundant code will be printed into the console, so I personally recommend this approach.
5. You may encounter errors caused by mismatching file names, package incompatibility, etc. The script will automatically attempt to install all the necessary packages (if they are not installed on your local machine), so in case there are any conflicts, make sure to check that you have fulfilled all prerequisites from the prerequisites section. If you, however, wish to run the code line by line, working with the main script may prove more suitable.
6. If all goes well, you should see the output in the console, and in the results folder. In the folder `results/numeric/`, you will find for numerical and text-based output, while the folder and `results/graphics/` holds graphical output. In the folder `results/tex`, you will (in the future) find .tex type code representing all result tables generated during the script run. If you wish to modify the form of these tables, see the file `resources/table_templates.yaml`. Furthermore, a file called `main_results.txt`, containing the console log with numerous clean and formatted results, will be created in the `results/` folder. Any existing files within these folders will likely be overwritten upon running the script, so make sure to save any desired files outside these folders after they are generated.
7. If you wish to look under the hood of the code, see the file `source_master_thesis_cala.R`, which contains all the technical functions, preprocessing, and validation, that is hidden in the main file.

## List of available methods

Here is a list and explanation of the available methods:

- **Variable summary statistics** - Generate a table with various summary statistics for variables of your choice. You should specify these variables in the _Variable information_ data sheet.
- **Effect summary statistics** - Generate a table with effect summary statistics across various subsets of data. As with the previous method, you should specify these subsets in the _Variable information_ data sheet.
- **Prima Facie graphs** - Plot smart graphs for various subsets of data for variables of your choice. You may specify which variables to create these graphs for from within the user parameter file by providing their group id.
- **Box plot** - Create a box plot for factors of your choice. The number of boxes is adjusted automatically to avoid overcrowded plots.
- **Funnel plot** - Construct the funnel plot using your main data frame ([source](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-6419.2009.00593.x)). Funnel plot with study medians only is also generated automatically. It is possible to convert precision to logarithm, and to modify other graph specifications.
- **T-statistic histogram** - Construct a histogram using t-statistics within your data. You may choose to highlight various statistics, such as critical t-values, mean, etc. It is also possible to add a density line to the graph.
- **Linear tests** - Calculate 6 linear tests for publication bias. These are:
  - _OLS_
  - _Between Effects_
  - _Random Effects_
  - _Study-weighted OLS_
  - _Precision-weighted OLS_
- **Non-linear tests** - Construct 6 non-linear models for publication bias. These are:
  - _Weighted Average of Adequately Powered_ - [Ioannidis et al., 2017](http://pinguet.free.fr/ioannidis17.pdf)
  - _Top10_ - [Stanley et al., 2010](https://www.tandfonline.com/doi/abs/10.1198/tast.2009.08205)
  - _Stem-based method_ - [Furukawa, 2019](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3362053)
  - _Hierarchial Bayes_ - [Allenby & Rossi (2006)](https://books.google.com/books?hl=en&lr=&id=RymGgxN3zD4C&oi=fnd&pg=PA418&dq=hierarchical+bayes&ots=a4t-KvUdua&sig=K7V5JgPF4_gJll2d7reATSQ8I-I)
  - _Selection model_ - [Andrews & Kasy, 2019](https://www.aeaweb.org/doi/10.1257/aer.20180310)
  - _Endogenous Kink model_ - [Bom & Rachinger, 2020](https://onlinelibrary.wiley.com/doi/abs/10.1002/jrsm.1352)
- **Tests relaxing exogeneity** - Construct 2 models relaxing the exogeneity assumption. These are:
  - _Instrumental Variable regression_
  - \*p-uniform\*\* - [van Aert & van Assen (2018)](https://cloud.r-project.org/web/packages/puniform/puniform.pdf)
- **P-hacking tests** - Construct 3 models that detect p-hacking. These are:
  - _Caliper tests_ - [Gerber & Malhotra, 2008](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=43eb85dd4af8c64cf6bacba73c39b1027606bcdf)
  - _Elliott tests_ - [Elliot et al. (2022)](https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA18583)
  - _MAIVE estimator_ -[Irsova et al., 2023](https://www.econstor.eu/handle/10419/268683)
- **Bayesian Model Averaging** - Automatically construct a valid formula and run Bayesian model averaging, extracting the coefficients into tables and creating graphs also. Is highly customizable within the user parameters script.
- **Frequentist Model Averaging** - Construct a Frequentist model averaging model ([source](https://www.sciencedirect.com/science/article/pii/S0304407615000342))
- **Model Averaging variables description table** - Combine the results of both model averaging approaches into a single, clean table.
- **Best-practice estimate** - For studies of your choice, generate best-practice estimates. Using one of these estimates, also calculate economic significance of variables with high PIP in the BMA model. This method requires a BMA model as input.
- **Best-practice estimate: Graphs** - Generate smart graphs that display the distribution of the generated best-practice estimates. For this, you must specify which variables you wish to use as factors - the method will evaluate various subsets of each such specified variable. These subsets are determined automatically using either the most common value of each study, or median value as a cutoff point, among other logic.
- **Best-practice estimate: Summary statistics** **_(experimental)_** - Generate a table with summary statistics of best-practice estimates across different subsets of data. Similarly to the BPE graphs, you must also provide factors (variables) which you wish to use for the smart subsetting.
- **Robust Bayesian Model Averaging** - Robust Bayesian Model Averaging ([source](https://github.com/FBartos/RoBMA)).

## Miscellaneous

- I can not guarantee the code will run perfectly in case you attempt a replication using a different data set. In case you use the data sets provided, the only caveat might be package installation, otherwise the code should run smoothly. In case of using a custom data set, various combinations of data might break some methods. As such, use the project with caution, and I hope it will be useful in any way possible.
- Try to eliminate as many missing values in your data frame as you can.
  The script will automatically use interpolation for missing data, so that model averaging
  can run, but in case of many missing values, the results may be unstable.
