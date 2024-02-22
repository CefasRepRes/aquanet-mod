# Example data files

These are example data files to run the AquaNet-Mod.

## Parameter files

Three parameter files are included here:

1.  **params.yml** - template parameter file.
2.  **params_baseline.yaml** - parameters for baseline disease scenario (Scenario 3).
3.  **params_no_catchment_controls.yaml** - parameters for no catchment controls disease scenario (Scenario 4).

If you use either the baseline or no catchment controls parameter values in your research, please cite the accompanying paper:

> Guilder, J., Ryder, D., Taylor, N. G. H., Alewijnse, S. R., Millard, R. S., Thrush, M. A., Peeler, E. J. & Tidbury, H. J. The aquaculture disease network model (AquaNet-Mod): a simulation model to evaluate disease spread and controls for the salmonid industry in England and Wales. In press.

## Catchment shapefile

You will need to select a catchment shapefile. We currently use the Centre for Ecology and Hydrology (CEH) Integrated Hydrological Units (IHU) sections.

> Kral, F.; Fry, M.; Dixon, H. (2015). Integrated Hydrological Units of the United Kingdom: Sections. NERC Environmental Information Data Centre. (Dataset). <https://doi.org/10.5285/a6e37e39-9e10-4647-a110-12d902403095>

## Farm and fishery data

**NB:** these data are randomly generated, and therefore entirely synthetic. Any resemblance to actual aquaculture sites are entirely coincidental.

1.  **lfm_data.csv** - live fish movements between sites.
2.  **production_data.csv** - data on production type (used to assign table producers).
3.  **river_network.csv** - river network via associated site types.
4.  **tidal_sites.csv** - sites within range of a tidal river flow.
