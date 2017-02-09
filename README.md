# Solar Correlation Map Plot in R language
Port of python library to provide correlation visualization between explanatory and dependent variables as well as between explanatory variables pairs. The original python library source code can be found in the following repository [GitHub][1]

## Overview
With help of provided tool its easy to find visually correlations between dependent and explanatory (input) variables. Also tool visualise correlation between explanatory (input) variables allowing to reduce deature space by replacing set of inter-correlated variables with one.

The level of correlation between dependent and explanatory variables visualized as set of orbits arround "sun" (dependent variable). The closer to the "sun" variables has higher [Pearson correlation coefficient][2].

The inter-correlated explanatory variables visualized as "moons" arround specific planet. We have defined inter-correlated variables as explanatory variables with [Pearson correlation coefficient][2] score higher then `0.8`. Usually, a strong correlation is anything above a Pearson coefficient of `0.5`.

The positive and negative correlation between explanatory and dependent variables expressed as color of variables names. The green color is for positive correlation (the more, the better) and red color is for negative correlation (the less is better).

## Usage
### Command line from project root directory:
```
Rscript src/sol_corr_map.R -d CSV_FILE_PATH -v DV
```
where: 

* CSV_FILE_PATH: the path to the CSV file with data (should have header)
* DV: the name of dependent variable in the provided data set (column name)

The resulting plot will be saved as `Rplots.pdf` in working directory.

### From R script file
Just make sure to source [src/sol_corr_map.R](https://github.com/yaricom/psistats/blob/master/src/svd_varimax.R) into your script and invoke `plotSolarCorrelation` function. See source file for input parameters.

## Example
Download [Boston Housing prices](https://archive.ics.uci.edu/ml/datasets/Housing) data set and create CSV file from provided `housing.data` and `housing.names` by combining columns names from last file with data corpus from former one.

Save it into `data` directory and issue following command:
```
Rscript src/sol_corr_map.R -d data/housing.csv -v MEDV
```
The resulting plot:

![alt text][housing_prices_sol_plot]

## References
[Original python library](https://github.com/Zapf-Consulting/solar-correlation-map)

[A new visualization to beautifully explore correlations](https://www.oreilly.com/learning/a-new-visualization-to-beautifully-explore-correlations)

[1]: https://github.com/Zapf-Consulting/solar-correlation-map
[2]: https://en.wikipedia.org/wiki/Pearson_correlation_coefficient

[housing_prices_sol_plot]: https://github.com/yaricom/solar-correlation-map-R/blob/master/contents/boston_housing_sol_plot.png "The solar correlation plot for Boston Housing Prices data corpus"
