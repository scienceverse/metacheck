# Repository survey, 2026-11-12 (#635, #636, #637)

**Temporary: delete this folder once #636 and #637 are built.** The tables that the package needs should then live in the package's own code or data; this folder only keeps the raw survey until then.

## What it is

The survey asked which repository software each DataCite repository runs, and which software the landing pages of Crossref's dataset DOIs run.

1. **All DataCite repositories:** `api.datacite.org/repositories`, 4,587 of them, each with its DOI prefixes and DataCite's own `software` field.
2. **One DOI per repository:** `api.datacite.org/dois?sample-size=1&sample-group=client`, 3,846 DOIs. Each one's registered address was followed to its landing page.
3. **A probe of each landing host:**
   - the page's generator tag;
   - `/server/api` (DSpace 7 and later);
   - `/rest/status` (legacy DSpace);
   - `/api/info/version` (Dataverse);
   - `/api/records?size=1` (Invenio);
   - `/api/3/action/status_show` (CKAN).

   Certificate checks stayed on, and each host received at most 6 requests.
4. **The same probe for Crossref:** one `dataset` or `database` DOI per Crossref publisher, 1,627 publishers holding 3.46 million DOIs.

## Files

| File | Contents |
|---|---|
| `datacite_repositories.csv` | All 4,587 DataCite repositories: id, name, `software` field, address, type, re3data id, DOI prefixes |
| `datacite_probe.csv` | One row per sampled DOI (3,846): the repository, the landing host, every probe answer, and `found`, the software the probe concluded |
| `dspace_prefixes.txt` | The 365 DOI prefixes of every repository that DataCite or the probe calls DSpace |
| `crossref_dataset_publishers.csv` | Crossref publishers with the number of dataset/database DOIs each holds |
| `crossref_probe.csv` | One sampled DOI per Crossref publisher, with the probe's answers |
| `dspace_dc.R` | First comparison of DataCite's DSpace list with `.dspace7_hosts()` |
| `crossref_sample.R` | Takes one Crossref dataset DOI per publisher |
| `repo_probe.R` | The probe; `Rscript repo_probe.R` for DataCite, `Rscript repo_probe.R crossref` for Crossref |
| `repo_probe_sum.R`, `repo_probe_cr_sum.R` | Summaries per software, and the DSpace prefix list |
| `repo_lists.R` | The markdown tables posted in #636 and #637 |

## Running the scripts again

The scripts were written for a one-off study. They read and write their working files (`dc_repos.rds`, `dc_sample.json`, `cr_sample.rds`, and the `probe/` and `probe_cr/` folders) in `data-raw/repo_survey/`. Those working files are not kept here, because the CSV files above hold their results. Running the scripts again makes new requests to DataCite, Crossref and each host. The order is:
1. `dspace_dc.R`, which saves `dc_repos.rds`;
2. `dc_sample.json`, fetched by hand from the sample address above;
3. `repo_probe.R`;
4. `repo_probe_sum.R`.

Known gap (#636): with one DOI per repository, the probe sees only one host for repositories whose DOIs lead to several hosts. Sampling 10 DOIs per repository would close it.
