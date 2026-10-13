# Data

The analysis expects these files in this folder.

| File | Contents | Source |
|---|---|---|
| `who_ecoli.csv` | *E. coli* bloodstream infections resistant to 3rd-generation cephalosporins (%), indicator 745F475 | WHO data portal, data.who.int |
| `who_mrsa.csv` | Bloodstream infections due to MRSA (%), indicator 5DD9606 | WHO data portal, data.who.int |
| `world_bank_indicators.csv` | GDP per person, health spending, UHC index | Created automatically on the first run |
| `world_bank_extra.csv` | Out-of-pocket spending, basic sanitation | Created automatically on the first run |
| `antibiotic_use.csv` | *Optional.* Antibiotic consumption (DDD per 1,000 people per day), with a 3-letter country code column, a year column and one consumption column | Downloaded separately |

## Getting the WHO files

1. Go to [data.who.int](https://data.who.int) and search for the indicator codes above, or for "SDG 3.d.2".
2. Download each indicator as a CSV file.
3. Rename the files to `who_ecoli.csv` and `who_mrsa.csv`, then save them here.

The script checks that these columns are present: `DIM_TIME`, `DIM_GEO_CODE_M49`, `DIM_GEO_CODE_TYPE`, `GEO_NAME_SHORT` and `RATE_PER_100_N`.

WHO and World Bank data are licensed under CC BY 4.0. If you redistribute them, keep the attribution shown above.
