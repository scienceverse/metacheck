# Metacheck <img src="man/figures/logo.png" align="right" height="120" alt="" />

Metacheck is an open source tool that screens scientific manuscripts and data repositories to identify potential issues or areas for improvement. It is designed to guide researchers in adopting best practices. Inspired by practices in software development, where automated checks (e.g., CRAN checks for R packages) are used to identify issues before release, Metacheck screens scientific manuscripts to identify potential improvements. It can also process large numbers of papers, which can be useful for metascientific research, or to track the adoption of for example open science practices across all papers published at an institution. For an example of a dashboard summarizing the results across many papers, see our [summary of Metacheck reports of PsyArXiv preprints](https://lakens.github.io/psyarxiv_metacheck/totals/). 

These checks are meant to assist, but not replace, human expertise. Automated checks can be incorrect. Like a spellchecker, the goal of Metacheck is to flag potential issues, and to point out a sufficient number of practices that can be improved to make the tool useful, despite the presence of some false positives. 

Metacheck can check power analyses, preregistrations, statements about ethics approvals, conflicts of interest, data and code sharing, and funding, statistical reporting practices for p values, effect sizes, it runs Statcheck, and it checks references for PubPeer comments, retractions, and replications. It also checks repositories for FAIR data and code sharing practices, and performs automated computational reproducibility checks. See the [Metacheck Manual](https://www.scienceverse.org/metacheck_book/) for worked examples and an overview of what you
can do. 

## Our Tools

You can find the [Metacheck R package here](https://github.com/scienceverse/metacheck). The R package offers you full access to all the functionality of Metacheck. 

An [online version of Metacheck](https://metacheckserve.metacheck.src.surf-hosted.nl) hosted by SURF allows you to directly check a paper or repository without having to install any software.

Our team has also developed [bibr](https://github.com/scienceverse/bibr), a tool to convert scientific manuscripts to structured .JSON files. It is an alternative to GROBID, and returns higher quality output. 

To validate and improve Metacheck we have created [a repository of more than 15000 open access papers](https://github.com/scienceverse/papers), converted to GROBID xml, that can be loaded through metacheck. 

As a demonstration of the functionality of Metacheck, [we automatically check new preprints uploaded to the PsyArXiv preprint server](https://lakens.github.io/psyarxiv_metacheck/). 

## Our Values

The values below guide our priorities and decision-making for all Scienceverse projects.

1.  **Quality Control**: The knowledge, data, code and software we
    create will be verified for accuracy, using publicly available
    methods and measures, such that users have enough context to
    interpret the results.

2.  **Transparency**: We are committed to open source software and open
    access datasets for validation.

3.  **AI Optional**: The use of large language models (LLMs) will be
    restricted to classification of existing text, not evaluation of the
    quality of practice. The use of LLMs will always be opt-in and
    transparently declared. We will prioritise non-LLM functions where
    possible, and limit use to cases where it provides substantial
    benefits that cannot be realised with other methods, such as regular
    expressions.

4.  **No Automated Evaluation**: While our software is designed to
    assist the evaluation of research by highlighting aspects where
    practice may be improved, we will never support automated quality
    assessments, rankings or scoring.

5.  **Data Privacy**: We will be fully transparent about what data we
    have access to and how those data will be used, as well as what will
    be shared with external services. Wherever possible, we will
    prioritise functions that share minimal data and services that have
    robust data protection. We will meet all EU data privacy
    regulations.

6.  **Accessibility**: We will provide our knowledge, data, code and
    software to all potential users with minimal barriers to access. We
    will prioritise access that is not unduly limited by wealth,
    disability or technical knowledge.

7.  **Community Engagement**: Our work will reflect and respect the
    needs of a diverse range of research communities, and benefit their
    research practices. We are open to critique from the research
    community.

8.  **Sustainability**: We are committed to the long-term viability of
    our work. We will actively protect and future-proof our work through
    technical maintenance, governance structures and strategic planning
    to make it a durable and relevant public good.

9.  **Fair Recognition and Reward**: Contributions to knowledge, data,
    code, and software will be uniquely and persistently identified, and
    benefit a diverse range of contributors.

## Funding

Work on Metacheck is partly funded through the following grants: 

NWO VICI 2024 grant "Interpretable Effect Sizes for Psychological Science". https://doi.org/10.61686/UOJCN75090 (€1.499.516) PI Daniel Lakens. 

Thematic Digital Competence Centre Social Sciences & Humanities (TDCC-SSH) project "Research Transparency Check" https://doi.org/10.61686/ENWVD61320 (€248.273) PI René Bekkers.

NWO Research on Open Science 2025 grant “Assessing and Improving the Effectiveness of Data and Code Sharing Policies in Dutch Research Performing Organizations” https://doi.org/10.61686/WQEKR44289 (€257.210) PI Krist Vaesen.

NWO Research on Open Science 2025 grant "Shaping Open Science Culture: Investigating and influencing its impact and change"	https://doi.org/10.61686/BTKCI65580. (€258.498) PI Joeri Tijdink

NWO FAIR Research Outputs "FAIRifying Environmental Sensor Data for Exposure Assessment" (€50.000) PI Cristian Mesquida. 

## The Team

Metacheck is developed by a collaborative team of researchers,
consisting of [Lisa DeBruine](https://debruine.github.io) (developer and maintainer, UofG), [Daniël Lakens](https://sites.google.com/site/lakens2/Home) (developer, TU/e), [Cristian Mesquida](https://ssreplicationcentre.com/author/cristian-mesquida/)
(postdoctoral researcher, TU/e), [Jakub Werner](https://nl.linkedin.com/in/jakubwerner) (developer, TU/e), [René Bekkers](https://research.vu.nl/en/persons/rene-bekkers) (collaborator
and PI of [Transparency Check](https://tdcc.nl/tdcc-ssh-challenge-projects/research-transparency-check),
VUE), and [Max Littel](https://nl.linkedin.com/in/max-littel-a0620a233/nl) (RA, VUE).

Metacheck was initially developed by Lisa and Daniël in 2024 during
Lisa’s visiting professorship at the Eindhoven Artificial Intelligence
Systems Institute
([EAISI](https://www.tue.nl/en/research/institutes/eindhoven-artificial-intelligence-systems-institute)).

<div class="people people-3">

<figure>
<img src="https://scienceverse.github.io/images/people/debruine.jpg"
alt="Lisa DeBruine" />
<figcaption aria-hidden="true">Lisa DeBruine</figcaption>
</figure>

<figure>
<img src="https://scienceverse.github.io/images/people/lakens.jpg"
alt="Daniël Lakens" />
<figcaption aria-hidden="true">Daniël Lakens</figcaption>
</figure>

<figure>
<img src="https://scienceverse.github.io/images/people/mesquida.jpg"
alt="Cristian Mesquida" />
<figcaption aria-hidden="true">Cristian Mesquida</figcaption>
</figure>

<figure>
<img src="https://scienceverse.github.io/images/people/werner.jpg"
alt="Jakub Werner" />
<figcaption aria-hidden="true">Jakub Werner</figcaption>
</figure>

<figure>
<img src="https://scienceverse.github.io/images/people/bekkers.jpg"
alt="René Bekkers" />
<figcaption aria-hidden="true">René Bekkers</figcaption>
</figure>

<figure>
<img src="https://scienceverse.github.io/images/people/littel.jpg"
alt="Max Littel" />
<figcaption aria-hidden="true">Max Littel</figcaption>
</figure>

</div>

The following people contributed to Metacheck in research projects or internships. 

[Lavinia
Ion](https://nl.linkedin.com/in/lavinia-i-05275636b) (RA, TU/e), 
[Hadeel
Khawatmy](https://nl.linkedin.com/in/hadeel-khawatmy-a720021a2) (RA,
TU/e), 
[Levi Baruch](https://www.linkedin.com/in/levi-baruch/) (RA, TU/e), 
[Mink Veltman](https://www.linkedin.com/in/mink-veltman-74048b20a/) (RA, TU/e) 

<div class="people">

<figure>
<img src="https://scienceverse.github.io/images/people/ion.jpg"
alt="Lavinia Ion" />
<figcaption aria-hidden="true">Lavinia Ion</figcaption>
</figure>

<figure>
<img src="https://scienceverse.github.io/images/people/khawatmy.jpg"
alt="Hadeel Khawatmy" />
<figcaption aria-hidden="true">Hadeel Khawatmy</figcaption>
</figure>

<figure>
<img src="https://scienceverse.github.io/images/people/baruch.jpg"
alt="Levi Baruch" />
<figcaption aria-hidden="true">Levi Baruch</figcaption>
</figure>

<figure>
<img src="images/people/mink.jpg"
alt="Mink Veltman" />
<figcaption aria-hidden="true">Mink Veltman</figcaption>
</figure>

</div>

