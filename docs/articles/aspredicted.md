# Retrieving Planned Sample Sizes from AsPredicted Preregistrations

It is increasingly common for researchers to preregister their studies
(Spitzer and Mueller 2023; Imai et al. 2025). As it is a new practice,
and not all researchers are trained in how to preregister, it is not
surprising that the quality of preregistration is not always sufficient
to allow others to identify opportunistic flexibility in the data
analysis (Akker et al. 2024). Reviewers could check whether researchers
adhere to the preregistration, but this requires some effort. Automation
can provide a partial solution be making information more easily
available, and perhaps even performing automated checks.

Here we demonstrate how
[metacheck](https://scienceverse.github.io/metacheck/), our software to
perform automated checks on scientific manuscripts, can also retrieve
the content of a preregistration. The preregistration can then be
presented alongside the relevant information in the manuscript. This
makes it easier for peer reviewers to compare the information.

We will illustrate code that automatically retrieves and presents
preregistrations in this blog post. We focus on AsPredicted
preregistrations. Their structured format makes it especially easy to
retrieve information, but the same can be achieved for OSF
preregistrations, especially those that use a template. We can easily
search for AsPredicted links in all 250 open access papers from
psychological science. The metacheck package conveniently includes these
in xml format in the `psychsci` object.

## Find AsPredicted Links

Extracting links is a bit trickier than just searching for
“aspredicted.org”, so we provide a convenient function for extracting
them from papers:
[`aspredicted_links()`](https://scienceverse.github.io/metacheck/reference/aspredicted_links.md).
It cleans up the links when they are incorrectly formatted, such as
splitting “<https://aspredicted.org/blind.php?x=nq4xa3>” across two
sentences at the question mark.

This function returns a table with a row for each link found that
indicates the location of the link in each paper. Many papers include
the same link in multiple places, so we will just show you the unique
links here, returned in the “text” column.

``` r
links <- aspredicted_links(psychsci)

unique(links$text)
#>  [1] "https://aspredicted.org/ve2qn.pdf"         
#>  [2] "https://aspredicted.org/mq97g.pdf"         
#>  [3] "https://aspredicted.org/4gf64.pdf"         
#>  [4] "https://aspredicted.org/8a6ta.pdf"         
#>  [5] "https://aspredicted.org/vp4rg.pdf"         
#>  [6] "https://aspredicted.org/3kq9y.pdf"         
#>  [7] "https://aspredicted.org/rz98j.pdf"         
#>  [8] "https://aspredicted.org/z97us.pdf"         
#>  [9] "https://aspredicted.org/h9xm3.pdf"         
#> [10] "https://aspredicted.org/bj9er.pdf"         
#> [11] "https://aspredicted.org/my5jk.pdf"         
#> [12] "https://aspredicted.org/5xe8i.pdf"         
#> [13] "https://aspredicted.org/ak97v.pdf"         
#> [14] "https://aspredicted.org/p4ci6.pdf"         
#> [15] "https://aspredicted.org/iv9tb.pdf"         
#> [16] "https://aspredicted.org/dp8r5.pdf"         
#> [17] "https://aspredicted.org/Y2F_6B7"           
#> [18] "https://aspredicted.org/2YK_D6R"           
#> [19] "https://aspredicted.org/MPG_T3C"           
#> [20] "https://aspredicted.org/G68_GBZ"           
#> [21] "https://aspredicted.org/9SR_7BC"           
#> [22] "https://aspredicted.org/6D7_FVX"           
#> [23] "https://aspredicted.org/KD5_7LF"           
#> [24] "https://aspredicted.org/6tv5v.pdf"         
#> [25] "https://aspredicted.org/qs7zz.pdf"         
#> [26] "https://aspredicted.org/4mk6i.pdf"         
#> [27] "https://aspredicted.org/z5k26.pdf"         
#> [28] "https://aspredicted.org/wd3pm.pdf"         
#> [29] "https://aspredicted.org/LVH_7KX"           
#> [30] "https://aspredicted.org/LZQ_DXY"           
#> [31] "https://aspredicted.org/6X6_XZW"           
#> [32] "https://aspredicted.org/blind.php?x=nq4xa3"
#> [33] "https://aspredicted.org/blind.php?x=772w3a"
#> [34] "https://aspredicted.org/blind.php?x=55km72"
#> [35] "https://aspredicted.org/blind.php?x=yv9c2a"
#> [36] "https://aspredicted.org/blind.php?x=4xe5ih"
#> [37] "https://aspredicted.org/blind.php?x=pk8ff3"
#> [38] "https://aspredicted.org/sn9xs.pdf"         
#> [39] "https://aspredicted.org/vh8kg.pdf"         
#> [40] "https://aspredicted.org/ay3yk.pdf"         
#> [41] "https://aspredicted.org/jz2nc.pdf"         
#> [42] "https://aspredicted.org/a2wc9.pdf"         
#> [43] "https://aspredicted.org/PD5_KKS"           
#> [44] "https://aspredicted.org/9PG_LTT"           
#> [45] "https://aspredicted.org/M3P_X3P"           
#> [46] "https://aspredicted.org/H53_M3P"           
#> [47] "https://aspredicted.org/CQW_DTT"           
#> [48] "https://aspredicted.org/PW5_5VT"           
#> [49] "https://aspredicted.org/sq22k.pdf"         
#> [50] "https://aspredicted.org/u53e3.pdf"
```

## Retrieve Link Info

We can then use the function
[`aspredicted_retrieve()`](https://scienceverse.github.io/metacheck/reference/aspredicted_retrieve.md)
to get structured information from AsPredicted. You can try to run the
function on all 50 links above, but AsPredicted will eventually make you
complete a captcha before each access (we’re working on a way to log
into the site to prevent this). But for demonstration purposes, let’s
just look at one paper.

Below we provide an example where we analyze the paper “[The Evolution
of Cognitive Control in
Lemurs](https://journals.sagepub.com/doi/10.1177/09567976221082938)”,
which contains a single preregistration on AsPredicted.

``` r
paper <- psychsci$`09567976221082938`

links <- aspredicted_links(paper)

prereg <- aspredicted_retrieve(links)
#> Starting AsPredicted retrieval for 1 file...
#> * Retrieving info from https://aspredicted.org/iv9tb.pdf...
#> ...AsPredicted retrieval complete!
```

``` r
# get just the AsPredicted columns
cols <- names(prereg)
ap_cols <- cols[grepl("^AP_", cols)]
# transpose for easier reading
prereg[1, ap_cols] |> t()
#>                  [,1]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> AP_title         "Lemur executive function"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> AP_authors       "Francesca De Petrillo (Institute for Advanced Study of Toulouse, France) - francesca.de-petrillo@iast.fr\nAlexandra Rosati (University of Michigan, USA) - rosati@umich.edu"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> AP_created       "2019/06/06 11:19 (PT)"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> AP_data          "No, no data have been collected for this study yet."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#> AP_hypotheses    "Do differences in species’ socio-ecology predict variation across different aspects of executive function? How are different components of executive function related across individuals in these species?"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> AP_key_dv        "For each individual, we will take measures of their cognitive ability using a cognitive battery comprising 7 experimental tasks.\n1. Novel object task. Lemurs will be presented with a series of novel items (baseline, person, stationary object, moving object). For each item, we will measure lemurs’ latency to approach and their interest in each item.\n2. Quantity discrimination task. Lemurs will make choices between smaller and larger pieces of food. We will measure choices for the larger piece.\n3. Persistence task. Lemurs will be presented with a piece of food in a box that is impossible to access. We will measure how many times and for how long subjects interact with the box.\n4. Temporal discounting task. Lemurs will be presented with a series of choices between a smaller piece of food immediately available and a larger piece of the same food available after a delay. We will measure their choices for the larger delayed option.\n5. A-not-B error task. Lemurs will be first familiarized with finding food at a container in one location (A), and then in the test trial visibly see the food moved from A to a new container (B). We will measure lemurs’ choices for the correct location (B).\n6. Working memory task. Lemurs will see the experimenter hide food under one of 3 identical containers; after a 5s delay with occlusion, lemurs can choose one of the cups. We will measure their choices for the correct location.\n7. Reversal learning task. Lemurs will be presented with two containers (different colors and locations). They will first learn that one container provides a food reward (whereas the other is always empty). Once they learn this, the reward contingencies will be switched in the test trials. We will measure responses for the correct option in the learning and test trials."
#> AP_conditions    "This study does not have conditions: all individuals will complete the same tasks in the same order to assess species-level and individual-level variation in these cognitive abilities."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> AP_analyses      "First, we will examine whether differences in species’ socio-ecology predict variation in subjects’ performance in a battery of cognitive tests that tap into several component executive functions. To do this, we will use generalized linear mixed models to analyze species’ performance in each separate task. For each task, the dependent variable will be correct choices in that task, and the test predictor will be species. We will include subject ID as a random factor, and control for age, sex and trial number in models as relevant. Across analyses, we will compare the fit of different models using likelihood-ratio tests. In addition, we will also examine each individual’s performance across the cognitive tasks, to examine whether performance on a given cognitive task predicts performance in other tasks. To do so we will first use pairwise bivariate correlations across all individuals as well as partitioning by species. If there were significant age-related variation in cognitive performance in tasks in the first phase of analyses, we will also use linear regressions accounting for age. Second, we will use a factor analysis to detect whether performance in different task co-vary across individuals (overall, and within each species)."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> AP_outliers      "For each task, individuals will be excluded from relevant analyses if they fail to complete the task for three consecutive attempts. Any individual who fails to complete 1 or more cognitive task, will be excluded from the relevant analyses requiring that data, and we will check if including them affects other results."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> AP_sample_size   "The study will compare four lemur species: ruffed lemur, Coquerel’s sifakas, ring-tailed lemur and mongoose lemur at the Duke Lemur Center. We will test a minimum of 10 and a maximum of 15 individuals for each species based on availability and individual’s willingness to participate at the time of testing."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#> AP_anything_else "Nothing else to pre-register."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> AP_version       "2.00"
```

## Sample Size

Recent studies into how preregistrations are performed (Akker et al.
2024) have shown that one of the most common deviations from a
preregistration is a difference in the planned sample size, and the
achieved sample size. This is not necessarily problematic, as a
difference might be only a single datapoint. Nevertheless, researchers
should discuss the deviation, and evaluate whether the deviation impacts
the severity of the test (Lakens 2024).

Checking the sample size of a study against the preregistration is some
effort, as the preregistration document needs to be opened, the correct
entry located, and the corresponding text in the manuscript needs to be
identified. Recently, a fully automatic comparison tool,
([RegCheck](https://regcheck.app/)) has been created by Jamie Cummins
from the University of Bern, that relies on large language models and AI
where users upload the manuscript, the preregistration, and receive an
automated comparison. We take a slightly different approach. We retrieve
the preregistration from AsPredicted automatically, and present users
with the information about the preregistered sample size (which is
straightforward given the structured approach of the AsPredicted
template). We then recommend users to compare this information against
the method section in the manuscript.

### Preregistration Sample Size Plan

You can access the sample size plan from the results of
[`aspredicted_retrieve()`](https://scienceverse.github.io/metacheck/reference/aspredicted_retrieve.md)
under the column name `AP_sample_size`.

``` r
# get the sample size section from AsPredicted
prereg_sample_size <- unique(prereg$AP_sample_size)

# use cat("> ", x = _) with #| results: 'asis' in the code chunk
# to print out results with markdown quotes
prereg_sample_size |> cat("> ", x = _)
```

> The study will compare four lemur species: ruffed lemur, Coquerel’s
> sifakas, ring-tailed lemur and mongoose lemur at the Duke Lemur
> Center. We will test a minimum of 10 and a maximum of 15 individuals
> for each species based on availability and individual’s willingness to
> participate at the time of testing.

### Paper Sample size

Now we need to check what the achieved sample size in the paper is.

To facilitate this comparison, we can retrieve all paragraphs that
contain words such as ‘sample’ or ‘participants’ from the manuscript, in
the hope that this contains the relevant text. A more advanced version
of this tool could attempt to identify the relevant information in the
manuscript with a search for specific words used in the preregistration,
but we also show how AI can be used to identify the related text in the
manuscript.

A metacheck module could automatically print the information from the
preregistration, and possible relevant text from the manuscript. Our
main goal is to increase the ease with which important information from
the preregistration is available to the authors and peer reviewers, and
remind users that they need to discuss deviations from their
preregistration.

It might in addition be useful to retrieve the corresponding section in
the article. A simple attempt to achieve this would be to search for
words related to ‘sample’ or ‘participants’, and retrieve that
paragraph. We can use metacheck’s inbuilt
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md)
function. For this paper, we see this simple approach works well.

``` r
# match "sample" or "# particip..."
regex_sample <- "\\bsample\\b|\\d+\\s+particip\\w+"

# get full paragraphs only from the method section
sample <- search_text(paper, regex_sample, 
                      section = "method", 
                      return= "paragraph")

sample$text |> cat("> ", x = _)
```

> We tested 39 lemurs living at the Duke Lemur Center (for subject
> information, see Table S1 in the Supplemental Material available
> online). We assessed four taxonomic groups: ruffed lemurs (Varecia
> species, n = 10), Coquerel’s sifakas (Propithecus coquereli, n = 10),
> ringtailed lemurs (Lemur catta, n = 10), and mongoose lemurs (Eulemur
> mongoz, n = 9). Ruffed lemurs consisted of both red-ruffed and
> black-and-white-ruffed lemurs, but we collapsed analyses across both
> groups given their socioecological similarity and classification as
> subspecies until recently (Mittermeier et al., 2008). Our sample
> included all the individuals available for testing who completed the
> battery; two additional subjects (one sifaka and one mongoose lemur)
> initiated the battery but failed to reach the predetermined criterion
> for inclusion in several tasks or stopped participating over several
> days. All tests were voluntary: Lemurs were never deprived of food,
> had ad libitum access to water, and could stop participating at any
> time. The lemurs had little or no prior experience in relevant
> cognitive tasks such as those used here (see Table S1). All behavioral
> tests were approved by Duke University’s Institutional Animal Care and
> Use Committee .

We see that the the authors planned to test 10 mongoose lemurs, but one
didn’t feel like participating. This can happen, and it does not really
impact the severity of the test, but the statistical power is slightly
lower than desired, and it is a deviation form the original plan - both
deserve to be discussed.

![Mongoose
Lemurs](https://lemur.duke.edu/wordpress/wp-content/uploads/2025/01/12-Mongoose-Lemurs_w.jpg)

Mongoose Lemurs

### LLM Comparison

Alternatively, we can use an LLM to compare the preregistration with the
text in the manuscript. This is more costly (both financially and
ecologically) but it is possible and might work better, given the
flexible ways people discuss sample sizes in manuscripts.

We can use an overinclusive search for any paragraphs that contain the
strings “sample”, “subject” or “particip” from the method and results
sections. We can further narrow this down by including only paragraphs
that contain a number (including the word forms for small numbers).

``` r
regex_sample <- "subject|sample|particip"
regex_numbers <- "[0-9]|one|two|three|four|five|six|seven|eight|nine|ten"
methres <- search_text(paper, regex_sample,
                       section = c("method", "results"), 
                       return = "paragraph") |>
  search_text(regex_numbers, return = "paragraph")
```

This gives us 15 paragraphs to deal with. We can then send them to an
LLM, and ask which paragraph is most closely related to the text in the
preregistration.

Metacheck has a custom function to send text and a system prompt to an
LLM.

``` r
system_prompt_template <- "The following text is part of a scientific article. It describes a performed study. Part of this text should correspond to what researchers planned to do. Before data collection, the researchers stated they would:

%s

Your task is to retrieve the sentence(s) in the article that correspond to this plan, and evaluate based on the text in the manuscript whether researchers followed their plan with respect to the sample size. Start your answer with a 'The authors deviated from their preregistration' if there is any deviation."

# insert prereg text into template
system_prompt <- sprintf(system_prompt_template, prereg_sample_size)

# combine all relevant paragraphs
text <- paste(methres$text, collapse = "\n\n")

# run system_prompt
llm_response <- llm(text, system_prompt)
#> Using model = "llama-3.1-8b-instant".

llm_response$answer |> cat("> ", x = _)
```

> The authors deviated from their preregistration. In the original plan,
> they stated they would “test a minimum of 10 and a maximum of 15
> individuals for each species based on availability and individual’s
> willingness to participate at the time of testing.” However, in the
> study, the total number of lemurs included was 39, with the specific
> numbers being: ruffed lemurs (n = 10), Coquerel’s sifakas (n = 10),
> ring-tailed lemurs (n = 10), and mongoose lemurs (n = 9). The sample
> size for each species significantly exceeded the initially planned
> maximum of 15 individuals.

As we see, the LLM does a very good job evaluating whether the authors
adhered to their preregistration in terms of the sample size. The
long-run performance of this automated evaluation needs to be validated
in future research - this is just a proof of principle - but it has
potential for editors who want to automatically check if authors
followed their preregistration, and for meta-scientists who want to
examine preregistration adherence across a large number of papers. **For
such meta-scientific use-cases, however, the code needs to be
extensively validated and error rates should be acceptably low (i.e.,
comparable to human coders).**

## AI Results Always Need Checking!

The use of AI to interpret deviations is convenient, but **can’t replace
human judgment**. The following article, [Exploring the Facets of
Emotional Episodic Memory: Remembering “What,” “When,” and
“Which”](https://journals.sagepub.com/doi/10.1177/0956797621991548) is
categorized by the large language model as a deviation from the
preregistration. However, it misses that the authors explicitly say that
Cohort B was not preregistered, and therefore, falling short of the
planned sample size of 60 in that cohort should not be seen as a
deviation from the preregistration. Manually checking the sentences in
the method section that contain the words ‘sample’ or ‘particip…’ reveal
the misclassification by the LLM.

``` r
paper <- psychsci$`0956797621991548`
links <- aspredicted_links(paper)
prereg <- aspredicted_retrieve(links)
#> Starting AsPredicted retrieval for 1 file...
#> * Retrieving info from https://aspredicted.org/p4ci6.pdf...
#> ...AsPredicted retrieval complete!

#sample size
prereg_sample_size <- unique(prereg$AP_sample_size)
prereg_sample_size |> cat("> ", x = _)
```

> N = 60. Participants will be recruited from the undergraduate student
> population of the University of British Columbia, and will be
> compensated with course credit through the Human Subject Pool Sona
> system. All participants aged 18-35 will be eligible for
> participation, and must be fluent in English (to ensure instruction
> comprehension).

``` r
# LLM workflow - send potentially relevant paragraphs
regex_sample <- "subject|sample|particip"
regex_numbers <- "[0-9]|one|two|three|four|five|six|seven|eight|nine|ten"
methres <- search_text(paper, regex_sample,
                       section = c("method", "results"), 
                       return = "paragraph") |>
  search_text(regex_numbers, return = "paragraph")

text <- paste(methres$text, collapse = "\n\n")
system_prompt <- sprintf(system_prompt_template, prereg_sample_size)
llm_response <- llm(text, system_prompt)
#> Using model = "llama-3.1-8b-instant".

llm_response$answer |> cat("> ", x = _)
```

> The authors deviated from their preregistration.

The text states that “Here, we sought to collect data from 60
participants in each cohort.” However, it also states that in cohort B,
“Cohort B (n = 56; 44 female), our replication sample, the mean age was
20.64 years (SD = 1.96). Cohort B was originally intended to be another
60 participants, but data collection was interrupted because of the
COVID-19 pandemic; we shut down all laboratory testing on March 13,
2020.” This indicates that the researchers aimed to collect data from 60
participants in each cohort, as stated in the abstract, but in cohort B,
they only collected data from 56 participants due to the COVID-19
pandemic.

Therefore, the authors deviated from their preregistered sample size of
60 participants in cohort B.

``` r
# manual check - no LLM
regex_sample <- "\\bsample\\b|\\d+\\s+particip\\w+"

sample <- search_text(paper, regex_sample, 
                      section = "method", 
                      return= "paragraph")

sample$text |> cat("> ", x = _)
```

> Through memory, humans are able to relive the past in rich detail. Our
> memories are crucial to our sense of self and guide thoughts and
> actions. As one important factor, emotion plays a crucial role in the
> fidelity of memory, searing in our minds the best and worst of times.
> Still, experimental data show that not all the details from emotional
> events are preserved in memory. Here, we probed three aspects of
> memory for negative and neutral events: what, when, and which. We used
> a laboratory analogue of real-world remembering, in which participants
> watched videos designed to mimic aspects of an unfolding reallife
> experience. Emotion enhanced memory for “what” but reduced memory for
> “which.” Critically, emotion also altered memory for “when”;
> participants estimated that neutral images occurred later, but
> negative images were not associated with such bias. Thus, our results
> highlight the myriad effects of emotion on memory. conducted a power
> analysis based on Madan et al.’s (2017) second experiment. The effect
> sizes for the impact of emotion on “what” (Cohen’s d = 0.44) and
> “which” (d = 0.52) memory indicated that a sample size of 56 and 41,
> respectively, was adequate for detecting an effect on these aspects of
> memory (α = .05, 1 -β = .90). Here, we sought to collect data from 60
> participants in each cohort. In cohort A (n = 60; 42 female), the mean
> age was 20.89 years (SD = 2.94). One participant’s age was excluded
> from the mean because they entered an age of “2.” We preregistered the
> study involving cohort A We ran a replication study (exact
> replication), which involved cohort B, that was not preregistered. In
> cohort B (n = 56; 44 female), our replication sample, the mean age was
> 20.64 years (SD = 1.96). Cohort B was originally intended to be
> another 60 participants, but data collection was interrupted because
> of the COVID-19 pandemic; we shut down all laboratory testing on March
> 13, 2020.

## Putting it All Together

metacheck does not yet contain this module, as it is still under
development, but you could [create you own local
module](https://scienceverse.github.io/metacheck/articles/creating_modules.html)
by using (or improving) the following code:

``` r
# save to a file aspredicted_sample.R
# and use like module_run(paper, "aspredicted_sample.R")

aspredicted_sample <- function(paper, use_llm = FALSE) {
  # check paper for AsPredicted links
  links <- aspredicted_links(paper)
  if (nrow(links) == 0) return(list())
  
  # get prereg data from AsPredicted
  prereg <- aspredicted_retrieve(links)
  if (nrow(prereg) == 0 | is.null(prereg$AP_sample_size)) {
    return(list())
  }
  
  prereg_sample_size <- unique(prereg$AP_sample_size)
  
  # check paper for possible sample paragraphs
  regex_sample <- "\\bsample\\b|\\d+\\s+particip\\w+"
  sample <- search_text(paper, regex_sample, 
                        section = "method", 
                        return= "paragraph")
  
  # check LLM only if requested
  llm_answer <- "LLM not run"
  if (use_llm) {
    system_prompt_template <- "The following text is part of a scientific article. It describes a performed study. Part of this text should correspond to what researchers planned to do. Before data collection, the researchers stated they would:
  
  %s
  
  Your task is to retrieve the sentence(s) in the article that correspond to this plan, and evaluate based on the text in the manuscript whether researchers followed their plan with respect to the sample size. Start your answer with a 'The authors deviated from their preregistration' if there is any deviation."
    
    regex_sample <- "subject|sample|particip"
    regex_numbers <- "[0-9]|one|two|three|four|five|six|seven|eight|nine|ten"
    methres <- search_text(paper, regex_sample,
                           section = c("method", "results"), 
                           return = "paragraph") |>
      search_text(regex_numbers, return = "paragraph")
    
    text <- paste(methres$text, collapse = "\n\n")
    system_prompt <- sprintf(system_prompt_template, prereg_sample_size)
    
    # fails gracefully if is API key not available
    llm_answer <- tryCatch({
      llm_response <- llm(text, system_prompt)
      llm_response$answer
    }, error = \(e) { return(NULL) })
  }
    
  # print out messages?
  cat("Sample Size Preregistration:\n\n")
  cat(prereg_sample_size)
  cat("\n\nSample Size Text in Paper:\n\n")
  cat(sample$text)
  cat("\n\nLLM Assessment:\n\n")
  cat(llm_answer)
  
  # return structured info
  list(
    table = prereg,
    prereg_sample_size = prereg_sample_size,
    paper_sample_size = sample,
    llm_answer = llm_answer
  )
}
```

Test on the paper from above.

``` r
paper <- psychsci$`0956797621991548`
ap <- aspredicted_sample(paper)
#> Starting AsPredicted retrieval for 1 file...
#> * Retrieving info from https://aspredicted.org/p4ci6.pdf...
#> ...AsPredicted retrieval complete!
```

Sample Size Preregistration:

N = 60. Participants will be recruited from the undergraduate student
population of the University of British Columbia, and will be
compensated with course credit through the Human Subject Pool Sona
system. All participants aged 18-35 will be eligible for participation,
and must be fluent in English (to ensure instruction comprehension).

Sample Size Text in Paper:

Through memory, humans are able to relive the past in rich detail. Our
memories are crucial to our sense of self and guide thoughts and
actions. As one important factor, emotion plays a crucial role in the
fidelity of memory, searing in our minds the best and worst of times.
Still, experimental data show that not all the details from emotional
events are preserved in memory. Here, we probed three aspects of memory
for negative and neutral events: what, when, and which. We used a
laboratory analogue of real-world remembering, in which participants
watched videos designed to mimic aspects of an unfolding reallife
experience. Emotion enhanced memory for “what” but reduced memory for
“which.” Critically, emotion also altered memory for “when”;
participants estimated that neutral images occurred later, but negative
images were not associated with such bias. Thus, our results highlight
the myriad effects of emotion on memory. conducted a power analysis
based on Madan et al.’s (2017) second experiment. The effect sizes for
the impact of emotion on “what” (Cohen’s d = 0.44) and “which” (d =
0.52) memory indicated that a sample size of 56 and 41, respectively,
was adequate for detecting an effect on these aspects of memory (α =
.05, 1 -β = .90). Here, we sought to collect data from 60 participants
in each cohort. In cohort A (n = 60; 42 female), the mean age was 20.89
years (SD = 2.94). One participant’s age was excluded from the mean
because they entered an age of “2.” We preregistered the study involving
cohort A We ran a replication study (exact replication), which involved
cohort B, that was not preregistered. In cohort B (n = 56; 44 female),
our replication sample, the mean age was 20.64 years (SD = 1.96). Cohort
B was originally intended to be another 60 participants, but data
collection was interrupted because of the COVID-19 pandemic; we shut
down all laboratory testing on March 13, 2020.

LLM Assessment:

LLM not run

``` r
paper <- psychsci$`0956797621991548`
ap_with_llm <- aspredicted_sample(paper, use_llm = TRUE)
#> Starting AsPredicted retrieval for 1 file...
#> * Retrieving info from https://aspredicted.org/p4ci6.pdf...
#> ...AsPredicted retrieval complete!
#> Using model = "llama-3.1-8b-instant".
```

Sample Size Preregistration:

N = 60. Participants will be recruited from the undergraduate student
population of the University of British Columbia, and will be
compensated with course credit through the Human Subject Pool Sona
system. All participants aged 18-35 will be eligible for participation,
and must be fluent in English (to ensure instruction comprehension).

Sample Size Text in Paper:

Through memory, humans are able to relive the past in rich detail. Our
memories are crucial to our sense of self and guide thoughts and
actions. As one important factor, emotion plays a crucial role in the
fidelity of memory, searing in our minds the best and worst of times.
Still, experimental data show that not all the details from emotional
events are preserved in memory. Here, we probed three aspects of memory
for negative and neutral events: what, when, and which. We used a
laboratory analogue of real-world remembering, in which participants
watched videos designed to mimic aspects of an unfolding reallife
experience. Emotion enhanced memory for “what” but reduced memory for
“which.” Critically, emotion also altered memory for “when”;
participants estimated that neutral images occurred later, but negative
images were not associated with such bias. Thus, our results highlight
the myriad effects of emotion on memory. conducted a power analysis
based on Madan et al.’s (2017) second experiment. The effect sizes for
the impact of emotion on “what” (Cohen’s d = 0.44) and “which” (d =
0.52) memory indicated that a sample size of 56 and 41, respectively,
was adequate for detecting an effect on these aspects of memory (α =
.05, 1 -β = .90). Here, we sought to collect data from 60 participants
in each cohort. In cohort A (n = 60; 42 female), the mean age was 20.89
years (SD = 2.94). One participant’s age was excluded from the mean
because they entered an age of “2.” We preregistered the study involving
cohort A We ran a replication study (exact replication), which involved
cohort B, that was not preregistered. In cohort B (n = 56; 44 female),
our replication sample, the mean age was 20.64 years (SD = 1.96). Cohort
B was originally intended to be another 60 participants, but data
collection was interrupted because of the COVID-19 pandemic; we shut
down all laboratory testing on March 13, 2020.

LLM Assessment:

The authors deviated from their preregistration.

According to the initial plan, the researchers stated they would collect
data from 60 participants in each cohort. However, for cohort B, the
study was interrupted due to the COVID-19 pandemic, and only 56
participants were collected instead of the planned 60.

It fails gracefully if there are no links.

``` r
paper <- psychsci[[1]]
ap_no_links <- aspredicted_sample(paper)
```

The extent to which making information from the preregistration more
available will lead to more peers checking for deviations from the
preregistration remains to be seen, but we believe it has potential to
reduce the workload of peer reviewers, and it might remind authors to
discuss deviations from the preregistration.

This code can be substantially improved before we include it as a
built-in metacheck module, and also needs to be further developed to
work well with multi-study papers that contain multiple
preregistrations. If you are interested in developing this metacheck
module further, or performing such a validation study, do reach out to
us.

Akker, Olmo R. van den, Marjan Bakker, Marcel A. L. M. van Assen,
Charlotte R. Pennington, Leone Verweij, Mahmoud M. Elsherif, Aline
Claesen, et al. 2024. “The Potential of Preregistration in Psychology:
Assessing Preregistration Producibility and Preregistration-Study
Consistency.” *Psychological Methods*, October.
<https://doi.org/10.1037/met0000687>.

Imai, Taisuke, Séverine Toussaert, Aurélien Baillon, Anna Dreber, Seda
Ertaç, Magnus Johannesson, Levent Neyse, and Marie Claire Villeval.
2025. *Pre-Registration and Pre-Analysis Plans in Experimental
Economics*. 220. I4R Discussion Paper Series.
<https://www.econstor.eu/handle/10419/315047>.

Lakens, Daniël. 2024. “When and How to Deviate from a Preregistration.”
*Collabra: Psychology* 10 (1): 117094.
<https://doi.org/10.1525/collabra.117094>.

Spitzer, Lisa, and Stefanie Mueller. 2023. “Registered Report: Survey on
Attitudes and Experiences Regarding Preregistration in Psychological
Research.” *PLOS ONE* 18 (3): e0281086.
<https://doi.org/10.1371/journal.pone.0281086>.
