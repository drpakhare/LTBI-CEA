# LTBI Screening Cost-Effectiveness Analysis — Shiny Application

## Decision Analytic Model for Latent TB Screening Before Biologic Therapy in Indian Rheumatology Patients

### Quick Start

1. **Install R** (version 4.3+) and RStudio
2. Open terminal in this folder and run:
   ```r
   Rscript install_packages.R
   ```
3. Open `app.R` in RStudio and click **Run App**, or from R console:
   ```r
   shiny::runApp(".")
   ```

### Application Structure

| Tab | Description |
|-----|-------------|
| Overview | Model diagrams, PICO framework, study design |
| Parameters | Editable parameter tables loaded from Excel; slider controls |
| Screening Results | Decision tree outcomes: TP/FP/FN/TN per strategy |
| Long-Term Outcomes | Markov trace, TB incidence, cumulative costs/QALYs |
| Cost-Effectiveness | ICER table, CE plane, NMB, interpretation |
| Sensitivity (DSA) | Tornado diagram, two-way heatmap, threshold analysis |
| Probabilistic SA | Monte Carlo scatter, CEAC, EVPI |

### Strategies Compared

| Strategy | Test | Rule |
|----------|------|------|
| S1: TST Alone | TST (1 TU) | Treat if positive |
| S2: IGRA Alone | QuantiFERON-TB Gold Plus | Treat if positive |
| S3: Cy-Tb Alone | Cy-Tb skin test | Treat if positive |
| S4: TST then IGRA | Sequential TST -> IGRA | Treat if both positive |
| S5: Treat All | None | Treat everyone |

### File Structure

```
LTBI_CEA_App/
  app.R                    # Main Shiny app
  global.R                 # Package loading, Excel import
  R/
    model_functions.R      # Decision tree + Markov engine
    cea_functions.R        # ICER, dominance, NMB
    sa_functions.R         # DSA + PSA engines
    diagram_functions.R    # DiagrammeR diagrams
    helpers.R              # Utility functions
    mod_overview.R         # Module 1
    mod_parameters.R       # Module 2
    mod_decision_tree.R    # Module 3
    mod_markov.R           # Module 4
    mod_cea.R              # Module 5
    mod_dsa.R              # Module 6
    mod_psa.R              # Module 7
  data/
    LTBI_Model_Parameters.xlsx
  www/
    custom.css
  Dockerfile
  install_packages.R
```

### Docker Deployment

```bash
docker build -t ltbi-cea-app .
docker run -p 3838:3838 ltbi-cea-app
# Access at http://localhost:3838/LTBI_CEA_App
```

### Parameters

All 120+ model parameters are stored in `data/LTBI_Model_Parameters.xlsx` with 8 sheets covering prevalence, test accuracy, TB reactivation rates, prophylaxis efficacy, costs (INR), utility weights, mortality, and data gaps.

### Citation

If using this model, please cite the parameter sources listed in the Excel workbook and acknowledge the model development team.
