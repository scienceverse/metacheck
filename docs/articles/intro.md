# Introducing metacheck

## The Problem

Metascientific research reveals substantial opportunities to improve how
scientists design studies, report results, and implement open science
practices. For example, researchers often use invalid and unreliable
measures (Goos et al. 2024), misinterpret non-significant results (Aczel
et al. 2018), write insufficiently specific preregistrations (Akker et
al. 2024), make mistakes in power analyses (Thibault et al. 2024), and
misuse Bayes factors (Tendeiro et al. 2024).

Despite the growing availability of best practices, their adoption
remains slow (Sharpe 2013). While scientists are responsible for staying
informed, time constraints and the rapid pace of new developments pose
significant challenges. Peer reviewers face similarly have a lack of
time, and may overlook the absence of best practices. Checklists have
been proposed to promote adherence, but in practice they have limited
impact (Dexter and Shafer 2017), as researchers might be unaware of
them, and even when used, evaluating adherence still requires
considerable expertise.

## A Solution

Human factors research suggests that automation, where researchers stay
in the loop, can offer a partial solution to automatically check if best
practices are followed. For example, algorithms could detect passages
that describe an a priori power analysis, and whether researchers check
whether it is fully reported and specifies the alpha level, desired
power, the effect size metric, and a justification for the effect size
(Lakens 2022).

Algorithms are also useful for automating straightforward but time
consuming tasks. For example, a reviewer could manually check if all
p-values in a manuscript are reported exactly (e.g., p = 0.007 instead
of p \< .05), and check whether authors unknowingly cite retracted
papers . However, these tasks can be easily automated (as is for example
done by the reference manager
[Zotero](https://www.zotero.org/blog/retracted-item-notifications/)).

Recent progress in machine learning and artificial intelligence has made
it increasingly viable to implement automated checks with sufficient
accuracy, supporting broader adoption and adherence to best practices in
scientific manuscripts. For example, the GROBID machine learning library
(*GROBID* 2008--2025) can turn scientific PDFs into structured text
files from which specific content can be extracted. While many automated
checks for best practices will require only simple text matching based
on regular expression, more complex text extraction is possible by using
large language models (LLMs).

As a team, we have a specific philosophy on how to create useful
automated checks. First, automated checks should make users aware of a
potential points of improvement, and can point to sources to learn how
to implement improvements, but the user decides whether and how to
incorporate suggestions. Second, algorithms should be validated against
community-annotated ground truth where possible, and the error rates
should be transparently communicated. We use open software and tools,
and as much as possible, open data. For each research practice to detect
or check, domain experts should establish what best practices are, and
automated checks should continually be revised and updated based on
criticism. Any use of AI must be minimal, optional, and based on open
source tools that guarantee data privacy. As a matter of principle, we
start the development modules without the use of LLMs. Finally, the
creation of modules should be open to the entire scientific community,
such that any domain expert who wants to contribute an automated check
can easily do so.

## metacheck

![metacheck Logo, a green hexagon with METACHECK in art deco
font](https://scienceverse.github.io/metacheck/logo.png)

metacheck Logo, a green hexagon with METACHECK in art deco font

In this post, we introduce
[metacheck](https://scienceverse.github.io/metacheck/), a new tool that
leverages text search, code, and large language models to extract and
supplement information from scientific documents (including manuscripts,
submitted or published articles, or preregistration documents) and
provides automated suggestions for improvement.

``` r

library(metacheck)
```

Inspired by practices in software development, where automated checks
(e.g., CRAN checks for R packages) are used to identify issues before
release, metacheck aims to screen scientific manuscripts to identify
potential issues or areas for improvement and guide researchers in
adopting best practices.

metacheck is modular, with each module focused on a specific practice
that the scientific community wants to improve, and for which automation
is viable. This modularity allows the tool to be extended and customized
by the community, enabling the development of specialized modules
tailored to the standards and best practices of different scientific
fields. It can also offer an overarching platform to integrate existing
tools (e.g., checks for retracted articles, Statcheck, etc).

## An example of the “stat_p_exact” module

``` r

paper <- demopaper()
module_run(paper, "stat_p_exact")
```

Exact P-Values: We found 1 imprecise *p* value out of 3 detected *p*
values.

In addition to the initial modules already introduced on the [metacheck
website](https://scienceverse.github.io/metacheck/), we will demonstrate
metacheck’s capabilities through four practical scenarios in an upcoming
series of blog posts:

1.  Identifying adherence to the American Psychological Association’s
    Journal Article Reporting Standards (JARS), such as reporting exact
    *p*-values and including effect sizes alongside their corresponding
    statistical test results;

2.  Reviewing the content of data repositories, such listing if data
    and/or code files are shared, and if the repository includes a
    README file, code book, or data dictionary;

3.  Extracting sample sizes from AsPredicted preregistrations and
    checking the preregistered sample size for consistency with the
    final study sample size reported in the manuscript;

4.  Detecting the presence of a priori power analyses and checking if
    they are fully reported to make them reproducible.

These examples are intended to demonstrate the breadth of practices that
can be automatically checked. The automated checks can be performed as
part of an individual or metascientific workflow. In the individual
workflow, an author or editor can run selected modules on a single paper
and receive a report highlighting potential areas for improvement and
explanations or links to further resources. In the metascientific
workflow, a single module can be run on a batch of hundreds of papers,
producing tabular data that can be used to address metascientific
questions, such as how prevalent a practice is in a specific field. For
the individual workflow our philosophy is that error rates can be higher
than in the meta-scientific workflow, as the user will check whether to
implement each recommendation or not. For a tool to be used in a
metascientific workflow, the module needs to be shown to have low error
rates when run on manually coded ground truth files.

``` r

# run the module on 250 open access papers from Psychological Science
mo <- module_run(psychsci, "stat_effect_size")
head(mo$summary_table)
```

| paper_id | ttests_with_es | ttests_without_es | Ftests_with_es | Ftests_without_es |
|:---|---:|---:|---:|---:|
| 0956797613520608 | 0 | 0 | 6 | 0 |
| 0956797614522816 | 6 | 2 | 28 | 0 |
| 0956797614527830 | 0 | 0 | 0 | 0 |
| 0956797614557697 | 0 | 2 | 5 | 0 |
| 0956797614560771 | 4 | 0 | 0 | 0 |
| 0956797614566469 | 0 | 0 | 0 | 0 |

## Future Plans

There are more possible modules than our team can create, and we welcome
anyone who wants to collaborate with us on building and validating new
modules. In addition to the modules demonstrated in the four blog posts,
future modules could be developed to check if open data and materials
are Findable, Accessible, Interoperable, and Reusable (FAIR) (Wilkinson
et al. 2016), to perform checks on specific types of articles (e.g.,
meta-analyses), check journal-specific reporting guidelines, or perform
checks on the references. We see great potential for new modules; the
only limitation is your own creativity and your engineering skills.

We are excited to continue developing metacheck in close collaboration
with the broader scientific community and welcome feedback, suggestions,
and contributions. You can explore metacheck on GitHub:
<https://scienceverse.github.io/metacheck/>. If you want to contribute
to new metacheck modules, validate existing modules in your own
scientific discipline, or explore the use of metacheck for
metascientific projects, reach out to [Lisa
DeBruine](mailto:lisa.debruine@glasgow.ac.uk?subject=metacheck) or
[Daniel Lakens](mailto:d.lakens@tue.nl?subject=metacheck).

## Metacheck Team

Metacheck is developed by a collaborative team of researchers,
consisting of (from left to right in the picture below) [Lisa
DeBruine](https://debruine.github.io) (developer and maintainer) and
[Daniël Lakens](https://sites.google.com/site/lakens2/Home) (developer),
[René Bekkers](https://research.vu.nl/en/persons/rene-bekkers)
(collaborator and PI of Transparency Check), [Cristian
Mesquida](https://ssreplicationcentre.com/author/cristian-mesquida/)
(postdoctoral researcher), and Max Littel and Jakub Werner (research
assistants).

![The faces of the metacheck team](team.png)

The faces of the metacheck team

Metacheck is partly financed by the Dutch Research Council (NWO) via the
Thematic Digital Competence Centre Social Sciences & Humanities
(TDCC-SSH) as part of the project Research Transparency Check with file
number ICT.001.TDCC.018 under the grant
<https://doi.org/10.61686/ENWVD61320>.

metacheck is a component of a broader research project, Research
Transparency Check, led by René Bekkers, and funded by a [TDCC-SSH
grant](https://tdcc.nl/tdcc-ssh-challenge-projects/research-transparency-check/)
from the Dutch Research Council (NWO). Our funded project priorities are
described in more detail in our grant proposal, available here:
<https://osf.io/cpv4d/>.

Aczel, Balazs, Bence Palfi, Aba Szollosi, et al. 2018. “Quantifying
Support for the Null Hypothesis in Psychology: An Empirical
Investigation.” *Advances in Methods and Practices in Psychological
Science* 1 (3): 357–66.

Akker, Olmo R van den, Marjan Bakker, Marcel ALM van Assen, et al. 2024.
“The Potential of Preregistration in Psychology: Assessing
Preregistration Producibility and Preregistration-Study Consistency.”
*Psychological Methods*.

Dexter, Franklin, and Steven L Shafer. 2017. “Narrative Review of
Statistical Reporting Checklists, Mandatory Statistical Editing, and
Rectifying Common Problems in the Reporting of Scientific Articles.”
*Anesthesia & Analgesia* 124 (3): 943–47.

Goos, Cas, Marjan Bakker, Jelte M Wicherts, and Michèle B Nuijten. 2024.
*Assessing Reliable and Valid Measurement as a Prerequisite for
Informative Replications in Psychology*.

*GROBID*. 2008--2025.
[Https://github.com/kermitt2/grobid](https://github.com/kermitt2/grobid);
GitHub.

Lakens, Daniël. 2022. “Sample Size Justification.” *Collabra:
Psychology* 8 (1): 33267. <https://doi.org/10.1525/collabra.33267>.

Sharpe, Donald. 2013. “Why the Resistance to Statistical Innovations?
Bridging the Communication Gap.” *Psychological Methods* 18 (4): 572–82.
<https://doi.org/10.1037/a0034177>.

Tendeiro, Jorge N, Henk AL Kiers, Rink Hoekstra, Tsz Keung Wong, and
Richard D Morey. 2024. “Diagnosing the Misuse of the Bayes Factor in
Applied Research.” *Advances in Methods and Practices in Psychological
Science* 7 (1): 25152459231213371.

Thibault, Robert T, Emmanuel A Zavalis, Mario Malički, and Hugo Pedder.
2024. “An Evaluation of Reproducibility and Errors in Published Sample
Size Calculations Performed Using g\* Power.” *medRxiv*, 2024–07.

Wilkinson, Mark D., Michel Dumontier, IJsbrand Jan Aalbersberg, et al.
2016. “The FAIR Guiding Principles for Scientific Data Management and
Stewardship.” *Scientific Data* 3 (1): 1–9.
<https://doi.org/10.1038/sdata.2016.18>.
