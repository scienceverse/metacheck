[MetaCheck](http://www.scienceverse.org/metacheck) version 0.0.0.9067  
Report Created: 2026-01-31  

[Metacheck](https://www.scienceverse.org/metacheck/) is a tool that
screens scientific manuscripts and aims to identify potential issues for
improvement, thereby guiding researchers towards best practices.
Metacheck is developed to help researchers correctly and completely
report statistical results, automatically retrieve possible relevant
information about citations, and improve how researchers share data,
code, and preregistrations.

TipLearn More

Metacheck combines existing and new checks in a module-based tool. It
mainly relies on text search, retrieving information from external
sources through API’s or web-scraping, but it also incorporates tool
that use machine learning classifiers or large language models. The use
of LLM’s is always optional. The development of Metacheck is guided by
our [values
statement](https://docs.google.com/document/d/1bbIkgUaiz3fpTXAeTF3h-gsfhqWEBN_n6OBcbFLfXjw/edit?tab=t.0).

Metacheck often needs to balance false positives against false
negatives, and prioritizes preventing false negatives. Like a spelling
checker, which often highlights words that are not spelled incorrectly,
Metacheck modules will contain false positives. We hope their rate is
acceptable, given opportunities for improvement that Metacheck
identifies. Modules are currently primarily validated on the
psychological literature.

Metacheck is under continuous development. Issues can be submitted [on
Github](https://github.com/scienceverse/metacheck/issues), and
suggestions for improvement or feedback can be sent to D.Lakens@tue.nl.

## Summary

- ⚠️ [COI Check](#coi-check): No conflict of interest statement was
  detected.  
- ⚠️ [Funding Check](#funding-check): No funding statement was
  detected.  
- ✅️ [Open Practices Check](#open-practices-check): Shared data and code
  detected.  
- ⚠️ [Power Analysis Check](#power-analysis-check): We detected 1
  potential power analysis.  
- ✅️ [Randomization and Causal
  Claims](#randomization-and-causal-claims):
  - We identified 1 sentence describing randomization.
  - No causal claims were observed in the title.
  - No causal claims were observed in the abstract.  
- ℹ️ [Preregistration Check](#preregistration-check): We found 2
  preregistrations.  
- ⚠️ [Exact P-Values](#exact-p-values): We found 1 imprecise *p* value
  out of 3 detected.  
- ⚠️ [Marginal Significance](#marginal-significance): You described 2
  effects with terms related to ‘marginally significant’.  
- ⚠️ [Effect Sizes in t-tests and
  F-tests](#effect-sizes-in-t-tests-and-f-tests): We found 1 t-test
  and/or F-test where effect sizes are not reported.  
- ⚠️ [StatCheck](#statcheck): 1 possible error in t-tests or F-tests  
- 🔍 [Non-Significant P Value Check](#non-significant-p-value-check): We
  found 2 non-significant p values that should be checked for
  appropriate interpretation.  
- 🔍 [Repository Check](#repository-check):
  - We found 13 files in 3 repositories.
  - We found 0 README files and 3 repositories without READMEs.
  - We found 1 archive file.  
- 🔍 [Code Check](#code-check):
  - We found 4 R, SAS, SPSS, or Stata code files.
  - 1 code file had no comments.
  - 1 file loaded in the code was missing in the repository.
  - Absolute file paths were found.
  - Libraries/imports were loaded in multiple places.  
- 🔍 [DOI Check](#doi-check): We checked 1 reference in CrossRef and
  found 1 missing DOI.  
- 🔍 [Reference Accuracy](#reference-accuracy): We checked 4 references
  with DOIs in CrossRef and found matches for 3.  
- ℹ️ [Replication Check](#replication-check): You cited 1 article in the
  FReD replication database.  
- ℹ️ [RetractionWatch](#retractionwatch): You cited 1 article in the
  RetractionWatch database.  
- ℹ️ [Check PubPeer Comments](#check-pubpeer-comments): You cited 1
  reference with comments in PubPeer.  
- ℹ️ [Summarise References](#summarise-references): Summary information
  provided for 4 references

## General Modules

### ⚠️ COI Check

No conflict of interest statement was detected. Consider adding one.

NoteHow It Works

Identify and extract Conflicts of Interest (COI) statements.

The COI Check module uses regular expressions to check sentences for
words related to conflict of interest statements. It will return the
sentences in which the conflict of interest statement was found.

The function incorporates code from
[rtransparent](https://github.com/serghiou/rtransparent), which is no
longer maintained. For their validation, see [the
paper](https://doi.org/10.1371/journal.pbio.3001107).

This module was developed by Daniel Lakens

### ⚠️ Funding Check

No funding statement was detected. Consider adding one.

NoteHow It Works

Identify and extract funding statements.

The Funding Check module uses regular expressions to check sentences for
words related to funding statements. It will return the sentences in
which the conflict of interest statement was found.

The function incorporates code from
[rtransparent](https://github.com/serghiou/rtransparent), which is no
longer maintained. For their validation, see [the
paper](https://doi.org/10.1371/journal.pbio.3001107).

This module was developed by Daniel Lakens

### ✅️ Open Practices Check

Shared data and code detected.

View detailed feedback

Data was openly shared for this article, based on the following text:

> Data is also available from <https://osf.io/5tbm9> and code is also
> available from <https://osf.io/629bx>.

Code was openly shared for this article, based on the following text:

> Data and analysis code is available on GitHub from
> <https://github.com/Lakens/to_err_is_human> and from
> <https://researchbox.org/4377>.

NoteHow It Works

This module incorporates ODDPub into metacheck. ODDPub is a text mining
algorithm that detects which publications disseminated Open Data or Open
Code together with the publication.

The Open Practices Check runs Open Data Detection in Publications
(ODDPub). ODDPub searches for text expressions that indicate that an
article shared Open Data or Open Code together with the publication.
More information on the package can be found at
<https://github.com/quest-bih/oddpub>. The module only returns whether
open data and code is found (the original package offers more
fine-grained results). The tool was validated in the biomedical
literature, see <https://osf.io/yv5rx/>.

ODDPub was developed by Nico Riedel, Vladislav Nachev, Miriam Kip, and
Evgeny Bobrov at the QUEST Center for Transforming Biomedical Research,
Berlin Institute of Health. <https://doi.org/10.5334/dsj-2020-042>

It might miss open data and code declarations when the words used in the
manuscript are not in the pattern that ODDPub searches for, or when the
repositories are not in the ODDpub code (e.g., ResearchBox).

This module was developed by Daniel Lakens

## Method Modules

### ⚠️ Power Analysis Check

We detected 1 potential power analysis.

View detailed feedback

Some essential information could not be detected: alpha_level

TipLearn More

Power analyses need to contain the following information to be
interpretable: the type of power analysis, the statistical test, the
software used, sample size, critical alpha criterion, power level,
effect size, and an effect size metric. In addition, it is recommended
to make sure the power analysis is reproducible (by sharing the code, or
a screenshot, of the power analysis), and to provide good arguments for
why the study was designed to detect an effect of this size.

For an a-priori power analysis, where the sample size is determined,
reporting all information would look like:

> An a priori power analysis for an independent samples t-test,
> conducted using the pwr.t.test function from pwr (Champely, 2020),
> indicated that for a Cohen’s d = 0.5, an alpha level of 0.05, and a
> desired power level of 80% required at least 64 participants in each
> group.

For a sensitivity power analysis, this sentence would look like:

> A sensitivity power analysis for an independent samples t-test,
> conducted using the pwr.t.test function from pwr (Champely, 2020),
> indicated that with 64 participants in each group, and an alpha level
> of 0.05, a desired power level of 80% was reached for an effect size
> of d = 0.5.

NoteHow It Works

This module uses a large language module (LLM) to extract information
reported in power analyses, including the statistical test, sample size,
alpha level, desired level of power,and magnitude and type of effect
size.

If you have not set llm_use(TRUE) and supplied a groq API, the module
will return paragraphs that potentially contain power analyses, based on
a regular expression search.

The Power Analysis Check module uses regular expressions to identify
sentences that contain a statistical power analysis. Without the use of
an LMM, the module uses regular expressions to classify the power
analysis as a-priori, sensitivity or post-hoc. With the use of an LMM,
it checks if the power analysis is reported with all required
information.

The regular expressions can miss power analyses, or fail to classify
them correctly. The type of power analysis is often difficult to
classify, which can easily be solved by explicitly specifying the type
of power analysis as ‘a-priori’, ‘sensitivity’, or ‘post-hoc’. Note that
‘post-hoc’ or ‘observed’ power is rarely useful. The LMM can fail to
identify information in the paper, and will not have access to
information in paragraphs in the paper other than those that contain the
word ‘power’. This package was validated by the Metacheck team on
articles in Psychological Science.

This module was developed by Lisa DeBruine, Daniel Lakens and Cristian
Mesquida

### ✅️ Randomization and Causal Claims

- We identified 1 sentence describing randomization.
- No causal claims were observed in the title.
- No causal claims were observed in the abstract.

View detailed feedback

Journal Article Reporting Standards require details about randomization
procedures, or how possible bias due to non-randomization is mitigated.
This information is often not reported. Furthermore, researchers
sometimes make causal claims that are not warranted, for example because
there was no random assignment to conditions. This module checks how
(non)randomization is reported, and checks for causal claims in the
title and abstract. Researchers are asked to double check whether this
information is reported completely and correctly.

#### Randomization

We identified 1 sentence describing randomization.

If this was a study that contained random assignment to conditions, the
journal article reporting standards (JARS) ask that you describe the
following:

1.  Random assignment method: Procedure used to generate the random
    assignment sequence, including details of any restriction (e.g.,
    blocking, stratification)

2.  Random assignment concealment: Whether sequence was concealed until
    interventions were assigned

3.  Random assignment implementation: Who generated the assignment
    sequence, who enrolled participants, who assigned participants to
    groups

#### Causal Claims

No causal claims were observed in the title.

No causal claims were observed in the abstract.

TipLearn More

For advice on how to make causal claims, and when not to, see:

Antonakis J, Bendahan S, Jacquart P, Lalive R (2010). “On making causal
claims: A review and recommendations.” *The Leadership Quarterly*,
**21**(6), 1086–1120.
[doi:10.1016/j.leaqua.2010.10.010](https://doi.org/10.1016/j.leaqua.2010.10.010).

Grosz M, Rohrer J, Thoemmes F (2020). “The Taboo Against Explicit Causal
Inference in Nonexperimental Psychology.” *Perspectives on Psychological
Science*, **15**(5), 1243–1255.
[doi:10.1177/1745691620921521](https://doi.org/10.1177/1745691620921521).

For the APA journal articles reporting standards, see
<https://apastyle.apa.org/jars>

NoteHow It Works

Aims to identify the presence of random assignment, and lists sentences
that make causal claims in title or abstract.

The Randomization and Causal Claims Check first uses regular expressions
to check whether the manuscript contains a statement about randomization
to conditions. Subsequently, it sends the title and abstract to a
[machine learning classifier developed by Rasoul
Norouzi](https://github.com/rasoulnorouzi/causal_relation_miner) that
runs on
[HuggingFace](https://huggingface.co/spaces/lakens/causal_sentences).
Causal statements are identified. Researchers are recommended to double
check if causal statements are warranted, especially if no sentences
describing randomization were detected.

The regular expressions can miss statements about randomization, or
incorrectly assume there is a sentence describing randomization. The
module can’t evaluate if the causal statements that are identified are
warranted or not, and it only reminds users to double-check.

If you want to improve the detection of sentences describing
randomization, or otherwise improve the module, reach out to the
Metacheck development team.

This module was developed by Daniel Lakens

### ℹ️ Preregistration Check

We found 2 preregistrations.

View detailed feedback

We found 2 preregistrations.

Meta-scientific research has shown that deviations from preregistrations
are often not reported or checked, and that the most common deviations
concern the sample size. We recommend manually checking the full
preregistration at the links below, and have provided the preregistered
sample size.

TipFull Preregistration

TipLearn More

For metascientific articles demonstrating the rate of deviations from
preregistrations, see:

van den Akker O, Bakker M, van Assen M, Pennington C, Verweij L,
Elsherif M, Claesen A, Gaillard S, Yeung S, Frankenberger J, Krautter K,
Cockcroft J, Kreuer K, Evans T, Heppel F, Schoch S, Korbmacher M, Yamada
Y, Albayrak-Aydemir N, Wicherts J (2024). “The potential of
preregistration in psychology: Assessing preregistration producibility
and preregistration-study consistency.” *Psychological Methods*.
[doi:10.1037/met0000687](https://doi.org/10.1037/met0000687).

For educational material on how to report deviations from
preregistrations, see:

Lakens, Daniël (2024). “When and How to Deviate From a Preregistration.”
*Collabra: Psychology*, **10**(1), 117094.
[doi:10.1525/collabra.117094](https://doi.org/10.1525/collabra.117094).

NoteHow It Works

Retrieve information from preregistrations in a standardised way, and
make them easier to check.

The Preregistration Check module identifies preregistrations on the OSF
and AsPredicted based on links in the manuscript, retrieves the
preregistration text, and organizes the information into a template. The
module then uses regular expressions to identify text from AsPredicted,
and the API to retrieve text from the OSF. The information in the
preregistration is returned.

The module can’t extract information from non-structured preregistration
templates (i.e., where the preregistration is uploaded in a single text
field) and it can’t retrieve information in preregistrations that are
stored as text documents on the OSF.

If you want to extend the package to be able to download information
from other preregistration sites, reach out to the Metacheck development
team.

This module was developed by Daniel Lakens and Lisa DeBruine

## Results Modules

### ⚠️ Exact P-Values

We found 1 imprecise *p* value out of 3 detected.

View detailed feedback

Reporting *p* values imprecisely (e.g., *p* \< .05) reduces
transparency, reproducibility, and re-use (e.g., in *p* value
meta-analyses). Best practice is to report exact p-values with three
decimal places (e.g., *p* = .032) unless *p* values are smaller than
0.001, in which case you can use *p* \< .001.

TipLearn More

The APA manual states: Report exact *p* values (e.g., *p* = .031) to two
or three decimal places. However, report *p* values less than .001 as
*p* \< .001. However, 2 decimals is too imprecise for many use-cases
(e.g., a *p* value meta-analysis), so report *p* values with three
digits.

American Psychological Association (2020). *Publication manual of the
American Psychological Association*, 7 edition. American Psychological
Association.

NoteHow It Works

List any p-values reported with insufficient precision (e.g., p \< .05
or p = n.s.)

This module uses regular expressions to identify p-values. It will flag
any values reported as p \> ? or p \< numbers greater than .001.

We try to exclude figure and table notes like “\* p \< .05”, but may not
succeed at excluding all false positives.

This module was developed by Lisa DeBruine

### ⚠️ Marginal Significance

You described 2 effects with terms related to ‘marginally significant’.

View detailed feedback

You described effects with terms related to ‘marginally significant’. If
*p* values above 0.05 are interpreted as an effect, you inflate the
alpha level, and increase the Type 1 error rate. If a *p* value is
higher than the prespecified alpha level, it should be interpreted as a
non-significant result.

TipLearn More

For metascientific articles demonstrating the rate at which
non-significant p-values are interpreted as marginally significant, see:

Olsson-Collentine, A., van Assen, M. MAL, Hartgerink &, J. CH (2019).
“The Prevalence of Marginally Significant Results in Psychology Over
Time.” *Psychological Science*, **30**, 576–586.
[doi:10.1177/0956797619830326](https://doi.org/10.1177/0956797619830326).

For the list of terms used to identifify marginally significant results,
see this [blog post by Matthew
Hankins](https://web.archive.org/web/20251001114321/https://mchankins.wordpress.com/2013/04/21/still-not-significant-2/).

NoteHow It Works

List all sentences that describe an effect as ‘marginally significant’.

The marginal module searches for regular expressions that match a
predefined pattern. The list of terms is a subset of those listed in a
[blog post by Matthew
Hankins](https://web.archive.org/web/20251001114321/https://mchankins.wordpress.com/2013/04/21/still-not-significant-2/).
The module returns all sentences that match terms describing ‘marginally
significant’ results.

Some of the terms identified might not be problematic in some contexts,
and there are ways to describe ‘marginal significance’ that are not
detected by the module.

This module was developed by Daniel Lakens

### ⚠️ Effect Sizes in t-tests and F-tests

We found 1 t-test and/or F-test where effect sizes are not reported.

View detailed feedback

We recommend checking the sentences below, and add any missing effect
sizes.

TipLearn More

For metascientific articles demonstrating that effect sizes are often
not reported:

- Peng, C.-Y. J., Chen, L.-T., Chiang, H.-M., & Chiang, Y.-C. (2013).
  The Impact of APA and AERA Guidelines on Effect Size Reporting.
  Educational Psychology Review, 25(2), 157–209.
  doi:[10.1007/s10648-013-9218-2](https://doi.org/10.1007/s10648-013-9218-2).

For educational material on reporting effect sizes:

- [Guide to Effect Sizes and Confidence
  Intervals](https://matthewbjane.quarto.pub/guide-to-effect-sizes-and-confidence-intervals/)

TipAll detected and assessed stats

NoteHow It Works

The Effect Size module checks for effect sizes in t-tests and F-tests.

The Effect Size check searches for regular expressions that match a
predefined pattern. The module was validated on APA reported statistical
tests, and might miss effect sizes that were reported in other reporting
styles. It was validated by the Metacheck team on papers published in
Psychological Science.

If you want to extend the package to detect effect sizes for additional
tests, reach out to the Metacheck development team.

This module was developed by Daniel Lakens and Lisa DeBruine

### ⚠️ StatCheck

1 possible error in t-tests or F-tests

View detailed feedback

We detected possible errors in test statistics. Note that as the
accuracy of statcheck has only been validated for *t*-tests and
*F*-tests. As Metacheck only uses validated modules, we only provide
statcheck results for *t* tests and *F*-tests.

TipLearn More

For metascientific research on the validity of statcheck, and it’s
usefulness to prevent statistical reporting errors, see:

Nuijten M, van Assen M, Hartgerink C, Epskamp S, Wicherts J (2017). “The
validity of the tool "statcheck" in discovering statistical reporting
inconsistencies.”
[doi:10.31234/osf.io/tcxaja](https://doi.org/10.31234/osf.io/tcxaja),
Preprint.

Nuijten M, Wicherts J (2023). “The effectiveness of implementing
statcheck in the peer review process to avoid statistical reporting
errors.”
[doi:10.31234/osf.io/bxau9](https://doi.org/10.31234/osf.io/bxau9),
Preprint.

NoteHow It Works

Check consistency of p-values and test statistics

The Statcheck module runs Statcheck. Statcheck searches for regular
expressions that match a predefined pattern, and identifies APA reported
statistical tests. More information on the package can be found at
<https://github.com/cran/statcheck>. The module only returns Statcheck
results for t-tests and F-tests, as these are the only tests which have
been validated, see <https://osf.io/preprints/psyarxiv/tcxaj_v1/>.

Statcheck was developed by Michèle Nuijten and Sascha Epskamp.

Statcheck considers p = 0.000 an error, as you should report p \< 0.001.
Furthermore, p \< 0.03 is an error if the p-value was 0.031, and one
should simply report exact p-values (p = 0.031). Statcheck might miss
one-sided tests, and falsely assume the p-value is incorrect. For more
information, see [StatCheck](https://statcheck.io/).

This module was developed by Daniel Lakens and Lisa DeBruine

### 🔍 Non-Significant P Value Check

We found 2 non-significant p values that should be checked for
appropriate interpretation.

View detailed feedback

Meta-scientific research has shown nonsignificant p values are commonly
misinterpreted. It is incorrect to infer that there is ‘no effect’, ‘no
difference’, or that groups are ‘the same’ after p \> 0.05.

It is possible that there is a true non-zero effect, but that the study
did not detect it. Make sure your inference acknowledges that it is
possible that there is a non-zero effect. It is correct to include the
effect is ‘not significantly’ different, although this just restates
that p \> 0.05.

Metacheck does not yet analyze automatically whether sentences which
include non-significant p-values are correct, but we recommend manually
checking the sentences below for possible misinterpreted non-significant
p values.

TipLearn More

For metascientific articles demonstrating the rate of misinterpretations
of non-significant results is high, see:

Aczel B, Palfi B, Szollosi A, Kovacs M, Szaszi B, Szecsi P, Zrubka M,
Gronau Q, van den Bergh D, Wagenmakers E (2018). “Quantifying Support
for the Null Hypothesis in Psychology: An Empirical Investigation.”
*Advances in Methods and Practices in Psychological Science*, **1**(3),
357–366.
[doi:10.1177/2515245918773742](https://doi.org/10.1177/2515245918773742).

Murphy S, Merz R, Reimann L, Fernández A (2025). “Nonsignificance
misinterpreted as an effect’s absence in psychology: Prevalence and
temporal analyses.” *Royal Society Open Science*, **12**(3), 242167.
[doi:10.1098/rsos.242167](https://doi.org/10.1098/rsos.242167).

For educational material on preventing the misinterpretation of p
values, see [Improving Your Statistical
Inferences](https://lakens.github.io/statistical_inferences/01-pvalue.html#sec-misconception1).

NoteHow It Works

This module checks for imprecisely reported p values. If p \> .05 is
detected, it warns for misinterpretations.

The nonsignificant p-value check searches for regular expressions that
match a predefined pattern. The module identifies all p-values in a
manuscript and selects those that are not reported to be smaller than or
equal to 0.05. It returns all sentences containing non-significant
p-values.

In the future, the Metacheck team aims to incorporate a machine learning
classifier to only return sentences likely to contain
misinterpretations. If you want to help to improve the module, reach out
to the Metacheck development team.

This module was developed by Daniel Lakens

### 🔍 Repository Check

- We found 13 files in 3 repositories.
- We found 0 README files and 3 repositories without READMEs.
- We found 1 archive file.

View detailed feedback

README files are a way to document the contents and structure of a
folder, helping users locate the information they need. You can use a
README to document changes to a repository, and explain how files are
named. Please consider adding a README to each repository.

The following files are archives: Archive.zip. We did not examine their
content. Consider uploading these individually to improve
discoverability and re-use.

NoteHow It Works

This module retrieves information from repositories.

The Repository Check module lists files on the OSF, GitHub, and
ResearchBox based on links in the manuscript.

If you want to extend the package to be able to download files from
additional data repositories reach out to the Metacheck development
team.

This module was developed by Daniel Lakens and Lisa DeBruine

### 🔍 Code Check

- We found 4 R, SAS, SPSS, or Stata code files.
- 1 code file had no comments.
- 1 file loaded in the code was missing in the repository.
- Absolute file paths were found.
- Libraries/imports were loaded in multiple places.

View detailed feedback

Below, we describe some best coding practices and give the results of
automatic evaluation of these practices in the code files below. This
check may miss things or produce false positives if your scripts are
less typical.

#### Code Comments

Best programming practice is to add comments to code, to explain what
the code does (to yourself in the future, or peers who want to re-use
your code).

#### Missing Files

The scripts load files, but 1 script loaded 1 file that could not be
automatically identified in the repository. Check if the following files
are made available, so that others can reproduce your code, or that the
files are missing:

#### Absolute Paths

Best programming practice is to use relative file paths instead of
absolute file paths (e.g., C://Lakens/files) as these folder names do
not exist on other computers. The following absolute file paths were
found in 4 code files:

#### Libraries / Imports

Best programming practice is to load all required libraries/imports in
one block near the top of the code. In 3 code files, libraries/imports
were at multiple places (i.e., with more than 3 non-comment lines in
between).

NoteHow It Works

This module retrieves information from repositories checked by
repo_check about code files (R, SAS, SPSS, Stata).

The Code Check module checks R, Rmd, Qmd, SAS, SPSS, and Stata files,
using regular expressions to check the code. The regular expression
search will detect the number of comments, the lines at which
libraries/imports are loaded, attempts to detect absolute paths to
files, and lists files that are loaded, and checks if these files are in
the repository. The module will return suggestions to improve the code
if there are no comments, if libraries/imports are loaded in lines
further than 4 lines apart, if files that are loaded are not in the
repository, and if absolute file paths are found.

The regular expressions can miss information in code files, or falsely
detect parts of the code as a fixed file path. Libraries/imports might
be loaded in one block, even if there are more than 3 intermittent
lines. The package was validated internally on papers published in
Psychological Science. There might be valid reasons why some loaded
files can’t be shared, but the module can’t evaluate these reasons, and
always gives a warning.

If you want to extend the package to perform additional checks on code
files, or make the checks work on other types of code files, reach out
to the Metacheck development team.

This module was developed by Daniel Lakens

## Reference Modules

### 🔍 DOI Check

We checked 1 reference in CrossRef and found 1 missing DOI.

View detailed feedback

Double check any references listed in the tables below. The match score
gives an indication of how good the match was. Many books do not have a
DOI or are not listed in CrossRef. Garbled references are usually a
result of poor parsing of the paper by grobid; we are working on more
accurate alternatives.

NoteHow It Works

This module checks references for missing DOIs or DOIs with an invalid
format.

This module works by identifying references that do not have a DOI or
have a DOI that does not have a valid format. It then looks up these
references by title, author and journal or book title in CrossRef. A DOI
match is returned for the reference with the highest match above the
`crossref_min_score` (default of 50).

Carefully check the returned results, as problems with reference import
can lead to false positives.

This module was developed by Daniel Lakens and Lisa DeBruine

### 🔍 Reference Accuracy

We checked 4 references with DOIs in CrossRef and found matches for 3.

View detailed feedback

Double check any references listed in the tables below. This tool has a
high false positive rate.

NoteHow It Works

This module checks references for mismatches with CrossRef.

It looks up the DOIs originally present in your paper (not those found
by ref_doi_check) and returns the bibliographic information.

We then check that the title from your reference section is the same as
the retrieved title (ignoring differences in capitalisation) and that
all author last names in your reference section are also in the
retrieved author list (we do not check first names or order yet). This
check is done for all references with crossref entries, including those
found by ref_doi_check, if it was previously run.

Mismatches may be because of problems with our parsing of references
from your PDF (we’re working on improving this), incorrect formatting in
CrossRef, or minor differences in punctuation.

This module was developed by Daniel Lakens and Lisa DeBruine

### ℹ️ Replication Check

You cited 1 article in the FReD replication database.

View detailed feedback

We checked 4 references with DOIs. You cited 1 article in the FReD
replication database.

Check if you are aware of the replication studies, and cite them where
appropriate.

NoteHow It Works

This module checks references and warns for citations of original
studies for which replication studies exist in the Replication Database.

The Replication Check module compares the reference list against studies
in the FORRT replication database based on the DOI. If a study in the
database is found, a reminder is provided that a replication of the
original study exists, and should be cited (currently, a warning is
provided regardless of whether the replication study is already cited).

The module requires that the reference has a DOI. If you run the
ref_doi_check module in a pipeline before this, it will use the enhanced
DOI list from that module, otherwise it will only run on references with
existing DOIs.

It is possible the original study was cited for other reasons than the
empirical claim tested, or that the replication in the FORRT replication
database is for only one of the studies in the paper, and not the study
the authors discuss.

The database can be manually updated with the \`FReD_update()\`\`
function. For more information, see <https://forrt.org/FReD/>.

This module was developed by Daniel Lakens and Lisa DeBruine

### ℹ️ RetractionWatch

You cited 1 article in the RetractionWatch database.

View detailed feedback

We checked 4 references with DOIs. You cited 1 article in the
RetractionWatch database.

Check if you are aware of the replication studies, and cite them where
appropriate.

NoteHow It Works

This module checks references and warns for citations in the
RetractionWatch Database.

The RetractionWatch Check module compares the reference list against
studies in the RetractionWatch database based on the DOI. If a study in
the database is found, a reminder is provided that the study was
retracted, has an expression of concern, or a correction.

The module requires that the reference has a DOI. If you run the
ref_doi_check module in a pipeline before this, it will use the enhanced
DOI list from that module, otherwise it will only run on references with
existing DOIs.

It is possible the authors are already aware that a study was retracted,
but the module can’t evaluate this.

The database can be manually updated with the rw_update function. For
more information, see https://gitlab.com/crossref/retraction-watch-data.

This module was developed by Daniel Lakens and Lisa DeBruine

### ℹ️ Check PubPeer Comments

You cited 1 reference with comments in PubPeer.

View detailed feedback

We checked 4 references with DOIs. You cited 1 reference with comments
in PubPeer.

Pubpeer is a platform for post-publication peer review. We have filtered
out Pubpeer comments by ‘Statcheck’. You can check out the comments by
visiting the URLs below:

NoteHow It Works

This module checks references and warns for citations that have comments
on pubpeer (excluding Statcheck comments).

The PubPeer module uses the PubPeer API to check for each reference that
has a DOI whether there are comments on the post-publication peer review
platform. If comments are found, a link to the comments is provided.
Comments by ‘Statcheck’ on PubPeer are ignored, see
https://retractionwatch.com/2016/09/02/heres-why-more-than-50000-psychology-studies-are-about-to-have-pubpeer-entries/.

The module requires that the reference has a DOI. If you run the
doi_check module in a pipeline before this, it will use the enhanced DOI
list from that module, otherwise it will only run on references with
existing DOIs.

For more information, see
[PubPeer](https://www.pubpeer.com/static/about).

This module was developed by Daniel Lakens and Lisa DeBruine

### ℹ️ Summarise References

Summary information provided for 4 references

View detailed feedback

NoteHow It Works

Summarise information about each reference in a paper.

This module summarises previously-run reference section modules:
ref_doi_check, ref_accuracy, ref_pubpeer, ref_replication, and
ref_retractiuon.

This module was developed by Lisa DeBruine
