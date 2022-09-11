* `R_create_samples`: files copied from debruin, original (seeded) sample realizations are created. Without messing with that, the coordinates are extracted and saved in additional files (xxx_coords.Rdata) as sf objects (they are given the CRS from the `AGBstack`, which is ETRS89 LAEA)
  * `sample_from_samples`: code to sample randomly from the original realizations by randomly selecting n rows
* `/samples/original`: original sample realizations as created by the scripts in `R_create_samples`
* `/samples/sampled_700`: sample size 700
* `/samples/sampled_1000`: sample size 1000
* `/samples/sampled_3000`: sample size 3000

