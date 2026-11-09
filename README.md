# Replication: Landowner Preferences for Incentive Programs in North Carolina

## 1. Paper Replicated
Deviney, Courtney B., Rajan Parajuli, Frederick W. Cubbage, Erin O. Sills, and Robert E. Bardon. 2026. “Understanding Landowner Preferences for Traditional and Nature-Based Solutions Incentive Programs in North Carolina, USA.” *PLOS ONE* 21 (4): e0347042. [https://doi.org/10.1371/journal.pone.0347042](https://doi.org/10.1371/journal.pone.0347042).

## 2. Data Source
The primary survey dataset (`decoded_data.xls`) was obtained from the authors' replication package repository hosted on OSF/Dataverse as referenced in the published article's data availability statement, downloaded on September 28, 2026.

## 3. How to Run
To run the full replication pipeline on a clean machine:

```bash
git clone [https://github.com/kstackle/econ5560-replication-stackle.git](https://github.com/kstackle/econ5560-replication-stackle.git)
cd econ5560-replication-stackle
Rscript run_all.R > logs/run_log.txt 2>&1