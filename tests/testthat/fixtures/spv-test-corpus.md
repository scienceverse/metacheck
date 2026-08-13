# .spv test corpus (from .metacheck_repo_cache)

Real `.spv` files found in the local repo cache, useful for testing
`spv_decode_light_table()` / `spv_read_structure()` / the legacy decoder
against real-world variety (different SPSS versions, encodings, analyses).
Paths are relative to the metacheck repo root; the cache itself is not
committed, so these files are only available locally until/unless a small
subset is copied into a proper test fixture.

- .metacheck_repo_cache/osf.io_538bc/Reliability/160803ICCData.spv
- .metacheck_repo_cache/osf.io_538bc/2. Data gathering/CodedData/160803ICCData.spv
- .metacheck_repo_cache/osf.io_538bc/2. Data gathering/CodedData/CodedPilotData/151202ICCPilotData.spv
- .metacheck_repo_cache/doi.org_10.17605_OSF.IO_J7D2Q/Data and analyses/CleanData_Output.spv
- .metacheck_repo_cache/doi.org_10.17605_OSF.IO_J7D2Q/Data and analyses/Analyses/Analysis_Output.spv
- .metacheck_repo_cache/osf.io_mkxjy/Study 2/Outputs/Agency S2 freq analyses restricted sample.spv
- .metacheck_repo_cache/osf.io_mkxjy/Study 2/Outputs/Omega reliability S2.spv
- .metacheck_repo_cache/osf.io_mkxjy/Study 2/Outputs/Agency S2 freq analyses.spv
- .metacheck_repo_cache/osf.io_mkxjy/Study 1/Outputs/Omega reliability S1.spv
- .metacheck_repo_cache/osf.io_mkxjy/Study 1/Outputs/Agency S1 freq analyses.spv
- .metacheck_repo_cache/osf.io_7rvsq/0_Submission 2 Raw Data.zip.contents/0_Submission 2 Raw Data/5 UnAdults/PCA Output.spv
- .metacheck_repo_cache/osf.io_q6fvh/DyadFacetsOutput.spv
- .metacheck_repo_cache/osf.io_m7bu5_view_only_01c02750f1264276825a3011217c0b45/spss输出0504.spv — **the only LEGACY-format file found in this corpus** (has a `<vtb:path>` XML member alongside `<vtb:dataPath>` in its tableStructure — the dispatch signal documented in R/spv-structure.R). Confirmed by scanning every file in this list for that tag; every other file here is modern light-binary only.
- .metacheck_repo_cache/osf.io_xhu7z/Experiment 4/data + analysis/Output NHE4 cueing effect.spv
- .metacheck_repo_cache/osf.io_xhu7z/Experiment 4/data + analysis/Output NHE4 RT.spv
- .metacheck_repo_cache/osf.io_xhu7z/Experiment 3/data + analysis/NHE3 output.spv
- .metacheck_repo_cache/osf.io_xhu7z/Experiment 1/data + analysis/Output NHE1 accuracy, RT, efficiency.spv
- .metacheck_repo_cache/osf.io_xhu7z/Experiment 2/data + analysis/Output NHE2 RT.spv
- .metacheck_repo_cache/osf.io_whzcx/Narcissism and testosterone/Code SPSS Descriptive Statisics and Pearson Correlations.spv — the file used to build/validate spv-light-decoder.R and spv-structure.R (modern light-binary, SPSS 29)
- .metacheck_repo_cache/osf.io_nd2fr_view_only_b39f634340c444dd9506dbc6d8359565/adult IT analysis.spv
- .metacheck_repo_cache/osf.io_nd2fr_view_only_b39f634340c444dd9506dbc6d8359565/ChildChoiceAnalysis.spv
- .metacheck_repo_cache/osf.io_nd2fr_view_only_b39f634340c444dd9506dbc6d8359565/adult data choice.spv
- .metacheck_repo_cache/osf.io_nd2fr_view_only_b39f634340c444dd9506dbc6d8359565/adult curvature analysys.spv
- .metacheck_repo_cache/osf.io_nd2fr_view_only_b39f634340c444dd9506dbc6d8359565/Child Initiation Latency Analysis.spv
- .metacheck_repo_cache/osf.io_nd2fr_view_only_b39f634340c444dd9506dbc6d8359565/Child Curvature Deviation Analysis.spv
- .metacheck_repo_cache/osf.io_zpsyf/Data and Analytical Scripts/Study 3/Study 3 output.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 111/Obaidi_SJG/Study7/Output_Study7.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 111/Obaidi_SJG/ANOVAs/Output_ANOVAs.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 109/Woolley_SJG/Study2_output.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 109/Woolley_SJG/Study1_output.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 109/Woolley_SJG/Study3_output.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 111/Obaidi_SJG/Study4/Output_Study4.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 111/Obaidi_SJG/Study5/Output_Study5.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 111/Obaidi_SJG/Study6/Output_Study6.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 111/Obaidi_SJG/Study1/Output_Study1.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 111/Obaidi_SJG/Study2/Output_Study2.spv
- .metacheck_repo_cache/osf.io_xzke7/Phase 1 Reports/Article 111/Obaidi_SJG/Study3/Output_Study3.spv
- .metacheck_repo_cache/doi.org_10.5281_zenodo.18492041/Freuqency output.spv
