library(metacheck)

# remotes::install_github("scienceverse/metacheck", ref = "dev_report_project")

llm_use(TRUE)

paper <- demopaper()

report_harsh(paper,
  modules = c(
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/stat_effect_size_harsh.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/marginal_harsh.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/power_harsh.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/code_check_harsh.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/open_practices_harsh.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/stat_p_exact_harsh.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/stat_p_nonsig_harsh.R"
  ),
  output_format = "qmd"
)

report_kind(paper,
  modules = c(
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/stat_effect_size_kind.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/marginal_kind.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/power_kind.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/code_check_kind.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/open_practices_kind.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/stat_p_exact_kind.R",
    "C:/Users/dlakens/OneDrive - TU Eindhoven/git_repos/metacheck/inst/modules/stat_p_nonsig_kind.R"
  ),
  output_format = "qmd"
)
