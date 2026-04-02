# Teacher Transfer Optimization in Sri Lanka

This repository contains the R implementation for a lexicographic optimization model for teacher transfer allocation.

## Main features
- Synthetic data generation
- Feasible teacher-school assignment construction
- Five-stage lexicographic integer optimization
  - Stage 1A: Minimize total shortage
  - Stage 1B: Minimize weighted shortage
  - Stage 2: Minimize number of transfers
  - Stage 3: Maximize teacher welfare
  - Stage 4: Minimize transfer burden
- Rule-based administrative benchmark
- Performance evaluation
- Publication-quality plots
- Four weight scenarios (S1-S4)

## Structure
- `code/`: source code files
- `results/`: generated figures and tables
- `paper_support/`: optional helper scripts for paper reproduction

## Requirements
R packages used:
- lpSolve
- ggplot2

## Run
From the repository root, run:

```r
source("code/main_run.R")
```

This will execute the example scenario runner and generate outputs in `teacher_transfer_all_scenarios/` and `teacher_transfer_weight_scenarios/`.

## Reproducibility
The example automatic run uses:
- seed = `20260328`
- `n_underserved = 3`
- scenarios `S1`–`S4`

## Citation
If you archive this repository on Zenodo, update `CITATION.cff` and cite the DOI in your paper.
