# aquanet-mod
R scripts for AquaNet-Mod (The Aquaculture Disease Network Model).

## Citation
If using these scripts, please cite the scripts, package and accompanying paper.

To cite the scripts:
> Armitage, S., Harper, J., Alewijnse, S., Millard, R., Ryder, D. and Guilder, J. R scripts for AquaNet-Mod (The Aquaculture Disease Network Model) github.com/CefasRepRes/aquanet-mod

To cite the package:
> Armitage, S., Harper, J., Millard, R., Alewijnse, S., Ryder, D. and Guilder, J. Functions to Execute AquaNet-Mod (The Aquaculture Disease Network Model) github.com/CefasRepRes/aquanet

To cite the paper:
> Guilder, J., Ryder, D., Taylor, N. G. H., Alewijnse, S. R., Millard, R. S., Thrush, M. A., Peeler, E. J. & Tidbury, H. J. The aquaculture disease network model (AquaNet-Mod): a simulation model to evaluate disease spread and controls for the salmonid industry in England and Wales. In press.

## Getting started

To get started you will need the following packages available from CRAN:

* igraph
* sf
* here
* Matrix
* doParallel
* doRNG
* data.table
* dplyr

You will also need to download the aquanet package, which is available at https://github.com/CefasRepRes/aquanet

# Session info for reproducibility

R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8    LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                            LC_TIME=English_United Kingdom.utf8    

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] aquanet_2.0.0     dplyr_1.1.2       data.table_1.14.8 doRNG_1.8.6       rngtools_1.5.2    doParallel_1.0.17 iterators_1.0.14  foreach_1.5.2     yaml_2.3.7        Matrix_1.5-3      here_1.0.1        sf_1.0-12         igraph_1.4.2      beepr_1.3         remotes_2.4.2    

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.0   purrr_1.0.1        lattice_0.20-45    vctrs_0.6.3        generics_0.1.3     utf8_1.2.3         rlang_1.1.1        pkgbuild_1.4.0     e1071_1.7-13       pillar_1.9.0       glue_1.6.2         withr_2.5.0        DBI_1.1.3          bit64_4.0.5        plyr_1.8.8         audio_0.1-10       lifecycle_1.0.3    stringr_1.5.0      codetools_0.2-18   arrow_12.0.1       tzdb_0.4.0         callr_3.7.3        ps_1.7.2           curl_5.0.0         class_7.3-20       fansi_1.0.4        Rcpp_1.0.10        KernSmooth_2.23-20 classInt_0.4-9     desc_1.4.2         bit_4.0.5          digest_0.6.31      stringi_1.7.12     processx_3.8.0     grid_4.2.2         rprojroot_2.0.3    cli_3.6.1          tools_4.2.2        magrittr_2.0.3     proxy_0.4-27       tibble_3.2.1       crayon_1.5.2       pkgconfig_2.0.3    prettyunits_1.1.1  assertthat_0.2.1   rstudioapi_0.14    R6_2.5.1           units_0.8-1        compiler_4.2.2
