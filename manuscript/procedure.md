So rather than clipping part of transect to get a specific length, I took the problem the other way and started concatenating section to build a sampling unit of 200m (150-250) . These units can then be sampled within a grid to reach a standard sampling effort, build an accumulation curve and so on.  Anyways, I have written this in a R function where we can define the length we are after (e.g. 400m or 750m).  For the distribution, I also thought about  using a convex-hull, but this method is very sensitive to few point dispersed, you only need 4 points at each corner to full cover a grid cell, even though all other points are clumped together. An alternative is to compute the cumulative area covered by the sampling unit expanded with a certain buffer around them. This way, highly clumped sampling units will have a lot of overlap and cover relatively little area while regularly distributed will cover a maximum area with a minimum overlap.


# Abundance index computation

Using the data table of visits, counts and sites, we can compute the flight curve for each species and year per region and then used the flight curve to impute expected count for each day that have not been recorded. From these counts, imputed and the observed, we then compute the abundance of counted butterfly over the monitoring season per year. This index is the total butterfly day observed.

# To do list

- verify with Andros that flight curve are computed within sensible region, the UK should be treated alone, not with the continental data. Similarly, data collected in the Pyrenees should not be mixed with the data in Finland to compute a single flight curve.
