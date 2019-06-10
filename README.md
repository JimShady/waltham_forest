# Waltham Forest Air Quality modelling

## [Exposure](https://github.com/JimShady/waltham_forest/blob/master/markdown_scripts/1-make_exposure.Rmd)

Using the journeys described in [this CSV file](../csv_inputs/exposure_journeys.csv), this script calculates the walking and cycling exposure on a number of popular journeys in Waltham Forest for in 2013, and 2020. It outputs the results to the [RMarkdown page here](https://jimshady.github.io/waltham_forest/exposure.html), and to [a GeoJSON file here](../geojson_outputs/exposure_routes.geojson).

## [Maps](https://github.com/JimShady/waltham_forest/blob/master/markdown_scripts/2-waltham_forest_maps.Rmd)

This script create a number of air quality maps of baseline models, predictions and scenarios based on the air quality modelling by King's. The code and maps can be found on the [RMarkdown page here](https://jimshady.github.io/waltham_forest/maps.html). The maps are also output to [this folder of map outputs](https://github.com/JimShady/waltham_forest/tree/master/map_outputs).

## [Activity trends](https://github.com/JimShady/waltham_forest/blob/master/markdown_scripts/3-activity_trends.Rmd)

This script uses data from the London Hybrid Exposure Model to estimate changes in physical activity in Waltham Forest. The rendered output and code can by viewed on [this RMarkdown page](https://jimshady.github.io/waltham_forest/index.html), and the output [CSV is available here](https://raw.githubusercontent.com/JimShady/waltham_forest/master/csv_outputs/walking_cycling_data.csv).

## [Ward concentrations](https://github.com/JimShady/waltham_forest/blob/master/markdown_scripts/4-ward_concentrations.Rmd)

This script calculates the mean PM2.5 and NO2 concentrations, per Ward in Waltham Forest, for the years 2013 and 2020. The code and outputs can be found on this [RMarkdown page](https://jimshady.github.io/waltham_forest/ward_concentrations.html) and the results are also [output to this CSV](https://raw.githubusercontent.com/JimShady/waltham_forest/master/csv_outputs/wf_ward_concs.csv).

