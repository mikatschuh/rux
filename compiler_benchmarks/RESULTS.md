# Short benchmark run

Measured on 2026-10-20, macOS arm64, Rust 1.98.0, optimized Cargo bench profile.
Criterion 0.8.2; 10 samples, 100 ms warmup, 200 ms target measurement per case.
This is a smoke measurement with limited statistical precision. Use the default run for performance decisions.
Values below are mean estimates with 95% confidence intervals, in microseconds.
Other build/test checks overlapped part of this run; repeat on an idle machine for comparisons.

| Workload | tokenize | parse_only | build_graph | canonicalize | peephole | frontend_end_to_end | end_to_end |
| --- | --- | --- | --- | --- | --- | --- | --- |
| fibonacci | 1.782 [1.768, 1.790] | 1.096 [1.062, 1.135] | 1.695 [1.680, 1.712] | — | — | 4.500 [4.478, 4.520] | — |
| sum_loop | 1.299 [1.295, 1.302] | 0.822 [0.816, 0.829] | 1.328 [1.315, 1.340] | — | — | 3.410 [3.360, 3.448] | — |
| redundant_arithmetic | 2.378 [2.359, 2.394] | 1.200 [1.190, 1.211] | 1.858 [1.834, 1.883] | — | — | 5.415 [5.383, 5.436] | — |
| boolean_merges | 1.617 [1.600, 1.639] | 0.975 [0.953, 1.000] | 1.443 [1.420, 1.467] | 2.068 [2.028, 2.124] | 0.567 [0.559, 0.577] | 3.856 [3.841, 3.869] | 6.707 [6.652, 6.741] |
| boolean_loop | 1.217 [1.212, 1.221] | 0.713 [0.704, 0.724] | 1.079 [1.056, 1.113] | 1.402 [1.389, 1.417] | 0.212 [0.208, 0.217] | 3.035 [2.977, 3.109] | 4.737 [4.691, 4.783] |
| boolean_chain_16 | 5.304 [5.202, 5.367] | 2.243 [2.218, 2.266] | 2.679 [2.663, 2.694] | 5.644 [5.617, 5.670] | 4.271 [4.243, 4.302] | 9.649 [9.605, 9.686] | 19.842 [19.790, 19.882] |
| boolean_chain_64 | 18.871 [18.392, 19.355] | 15.933 [15.383, 16.519] | 8.723 [8.692, 8.746] | 21.536 [21.484, 21.579] | 17.734 [17.625, 17.843] | 41.482 [41.214, 41.767] | 72.704 [72.426, 72.951] |
| boolean_chain_256 | 71.029 [70.810, 71.286] | 27.035 [26.137, 28.070] | 35.114 [34.114, 36.848] | 80.220 [79.950, 80.503] | 78.275 [78.182, 78.374] | 132.726 [130.759, 134.526] | 317.322 [314.626, 319.680] |
| arithmetic_chain_16 | 8.536 [8.333, 8.680] | 3.413 [3.382, 3.447] | 3.704 [3.678, 3.728] | — | — | 14.972 [14.871, 15.062] | — |
| arithmetic_chain_64 | 30.718 [30.099, 31.569] | 19.957 [19.728, 20.147] | 14.845 [14.383, 15.261] | — | — | 55.764 [55.621, 55.911] | — |
| arithmetic_chain_256 | 119.163 [118.938, 119.364] | 44.109 [43.891, 44.305] | 52.437 [50.942, 53.736] | — | — | 216.153 [213.292, 219.546] | — |

Numeric source canonicalization and optimization are unavailable (—).

| Integer graph | Size | Peephole time (µs, mean [95% CI]) |
| --- | --- | --- |
| foldable | 16 | 2.509 [2.496, 2.524] |
| foldable | 64 | 10.001 [9.934, 10.051] |
| foldable | 256 | 42.714 [42.571, 42.832] |
| unfoldable | 16 | 0.604 [0.599, 0.612] |
| unfoldable | 64 | 2.179 [2.158, 2.202] |
| unfoldable | 256 | 9.284 [9.244, 9.338] |

The boolean chain of size 256 reduces root data dependencies from 514 to 1.
The foldable integer chain of size 256 reduces them from 257 to 1; the
unsupported-negation variant remains at 258. The boolean loop stays at 4.
These counts exclude control edges and do not measure stored graph size.

Reproduce with the fast smoke command in [README.md](README.md).
