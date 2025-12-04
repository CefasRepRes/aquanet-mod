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

You will need to select a catchment shapefile. We currently use Catchment boundaries for England and Wales based on existing Fish Health Inspectorate (FHI) catchments 2023 in British National Grid (BNG)

> Heal, R. (2024). Catchment boundaries for England and Wales based on existing Fish Health Inspectorate (FHI) catchments 2023 - British National Grid (BNG). <https://mdr.cefas.co.uk/view/21966>

## Farm and fishery data

**NB:** these data are randomly generated, and therefore entirely synthetic. Any resemblance to actual aquaculture sites are entirely coincidental.

1.  **lfm_data.csv** - live fish movements between sites.
2.  **production_data.csv** - data on production type (used to assign table producers).
3.  **river_network.csv** - river network via associated site types.
4.  **tidal_sites.csv** - sites within range of a tidal river flow.
