# Public Replication Data

Place the published Data S1 file here:

```text
data/public/prebunk_public_replication.csv
```

That respondent-level CSV is intentionally ignored by Git. To recreate it from
the restricted raw YouGov exports, place the raw `.sav` files in `data/raw/` and
run:

```bash
Rscript R/create_public_replication_data.R
```

The public analysis pipeline can then be run with:

```bash
Rscript R/main_analysis_public.R
```

`prebunk_public_replication_codebook.csv` lists the variables and factor levels
used in the public replication file.
