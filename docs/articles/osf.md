# Checking OSF Data Repositories

*In this blog post we will explain how metacheck can automatically check
the content of data repositories that are linked to in a scientific
manuscript, using some metacheck functions for exploring OSF
repositories to create a custom module.*

There is an increasing awareness of the importance of open science
practices, and widespread support among scientists for open science
practices, such as data and code sharing (Ferguson et al. 2023). As data
and code sharing is a relatively new practice, and many scientists lack
training in open science, it is common to see badly documented data
repositories. Best practices exist, such as the [TIER
protocol](https://www.projecttier.org/tier-protocol/protocol-4-0/), but
not all researchers might be aware of best practices.

At a minimum a data repository should contain a README file with
instructions for how to reproduce the results. If data is shared, it
should be stored in a ‘data’ folder, or at least have the word ‘data’ in
the filename. Code or scripts should similarly be shared in a folder
with that name, or at least with the word in the filename. Finally, if
data is shared, there should be a codebook or data dictionary that
explains which variables are in the dataset in order to allow others to
re-use the data. Although it is easy to forget to organize a data
repository well, it is also easy to automatically check. Here we
demonstrate how [metacheck](https://scienceverse.github.io/metacheck/)
can check if a README is present, whether data and/or code are shared,
and if there is a codebook.

Ideally peer reviewers or editors would check the contents of a data
repository. In practice, time constraints mean that no one actually
checks what is in a data repository. Automation can perform some of the
checks that peers might otherwise perform manually. We provide an
illustration of some checks that could be performed. Specifically 1) is
any data that is shared clearly labeled as such, 2) is code that is
shared clearly labeled as such, 3) is there a README file that explains
to potential users which files are shared, where they can be found in
the repository, and how the can be used to reproduce any reported
results, and 4) is there a codebook or data dictionary?

## Checking an OSF repository with metacheck

We will illustrate the process of checking a data repository by focusing
on projects on the [Open Science
Framework](https://scienceverse.github.io/metacheck/articles/www.osf.io).
For this illustration we use an open access paper published in
Psychological Science that has already been converted to a metacheck
object using GROBID. There are 250 open access papers in the metacheck
object `psychsci`; we will choose one for this example.

``` r
# paper to use in this example
paper <- psychsci[[250]]
```

### Set up OSF functions

You can only make 100 API requests per hour, unless you authorise your
requests, when you can make 10K requests per day. The OSF functions in
metacheck often make several requests per URL to get all of the info, so
it’s worthwhile setting your PAT. You can authorise them by creating an
OSF token at <https://osf.io/settings/tokens> and including the
following line in your .Renviron file (which you can open using
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)):

    OSF_PAT="replace-with-your-token-string"

The OSF API server is down a lot, so it’s often good to check it before
you run a bunch of OSF functions, we provide the function
[`osf_api_check()`](https://scienceverse.github.io/metacheck/reference/osf_api_check.md)
for this. When the server is down, it can take several seconds to return
an error, so scripts where you are checking many URLs can take a long
time before you realise they aren’t working.

``` r
osf_api_check()
#> [1] "ok"
```

### Find OSF Links

We start by searching for OSF URLs using the
[`search_text()`](https://scienceverse.github.io/metacheck/reference/search_text.md)
function. OSF links can be tricky to find in PDFs, since they can insert
spaces in odd places, and view-only links that contain a ? are often
interpreted as being split across sentences. This function is our best
attempt at catching and fixing them all.

``` r
links <- osf_links(paper)
```

| text         | section |
|:-------------|:--------|
| osf.io/hv29w | method  |
| osf.io/2es6n | method  |
| osf.io/jpm5a | method  |
| osf.io/aux7s | method  |
| osf.io/nw3mc | method  |
| osf.io/ks639 | method  |
| osf.io/y75nu | method  |
| OSF.IO/4TYM7 | funding |
| OSF.IO/X4T9A | funding |

### Retrieve Link Info

If valid, the link is processed, and the OSF Application Programming
Interface (API) is used to retrieve whether the link points to a file,
project, or registration. This is achieved through the
[`osf_retrieve()`](https://scienceverse.github.io/metacheck/reference/osf_retrieve.md)
function.

This function can take a vector of OSF IDs or URLs, or a table that
contains them. If the IDs aren’t in the first column, you will need to
specify the name of the column. The function will return your table with
added information. (You can quiet the output messages with
`verbose(FALSE)`.)

The function
[`osf_retrieve()`](https://scienceverse.github.io/metacheck/reference/osf_retrieve.md)
will also retrieve all child components, files and folders if you set
the argument `recursive = TRUE`. If there are duplicate IDs, it will
only get the contents for each item once. If you set the argument
`find_project = TRUE`, it will also look up the parent project of any
links (but this requires more API calls).

``` r
info <- osf_retrieve(links, recursive = TRUE, find_project = TRUE)
#> Starting OSF retrieval for 9 URLs...
#> * Retrieving info from hv29w...
#> * Retrieving info from 2es6n...
#> * Retrieving info from jpm5a...
#> * Retrieving info from aux7s...
#> * Retrieving info from nw3mc...
#> * Retrieving info from ks639...
#> * Retrieving info from y75nu...
#> * Retrieving info from 4tym7...
#> * Retrieving info from x4t9a...
#> ...Main retrieval complete
#> Starting retrieval of children...
#> * Retrieving children for x4t9a...
#> * Retrieving files for x4t9a...
#> * Retrieving files for 6621454e716cb7048fa45a2a...
#> * Retrieving files for 6293d1cab59d5f1df8720db5...
#> * Retrieving files for 6293d1bfbbdcde278f4269ed...
#> * Retrieving files for 6293d1c5bbdcde278f4269f7...
#> * Retrieving files for 6339c10031d65306e12de5a2...
#> * Retrieving files for 6293d2e3b59d5f1df0720c6b...
#> * Retrieving files for 64f0ac666d1e8905f21516b2...
#> * Retrieving files for 64f0ab59f3dcd105d7ddd40b...
#> ...OSF retrieval complete!
```

| osf_id | name                                   | osf_type      | project |
|:-------|:---------------------------------------|:--------------|:--------|
| hv29w  | Kim,Doeller_Prereg_OSF.pdf             | files         | NA      |
| 2es6n  | suppleVideo1_learnSph.mp4              | files         | NA      |
| jpm5a  | suppleVideo2_learnPlane.mp4            | files         | NA      |
| aux7s  | suppleVideo3_objlocSph.mp4             | files         | NA      |
| nw3mc  | suppleVideo4_objlocPlane.mp4           | files         | NA      |
| ks639  | suppleVideo5_triangleSph.mp4           | files         | NA      |
| y75nu  | suppleVideo6_trianglePlane.mp4         | files         | NA      |
| 4tym7  | Cognitive maps for a spherical surface | registrations | 4tym7   |
| x4t9a  | Cognitive maps for a spherical surface | nodes         | x4t9a   |

There are multiple OSF links in this paper, but they are all part of the
same overarching OSF project, with the project ID *NA*.

### Summarize Contents

The OSF allows you to categorize components by category, and we can also
determine file types using extensions.

| osf_id                   | name                                  | filetype |
|:-------------------------|:--------------------------------------|:---------|
| hv29w                    | Kim,Doeller_Prereg_OSF.pdf            | text     |
| 2es6n                    | suppleVideo1_learnSph.mp4             | video    |
| jpm5a                    | suppleVideo2_learnPlane.mp4           | video    |
| aux7s                    | suppleVideo3_objlocSph.mp4            | video    |
| nw3mc                    | suppleVideo4_objlocPlane.mp4          | video    |
| ks639                    | suppleVideo5_triangleSph.mp4          | video    |
| y75nu                    | suppleVideo6_trianglePlane.mp4        | video    |
| f8kbn                    | virtualizerStudy1-main.zip            | archive  |
| huf2p                    | ReadMe.txt                            | text     |
| aygpc                    | main_analyseTriangleComple_20230423.m | code     |
| fkh7w                    | main_simulate_objlocTraj.m            | code     |
| rz2dy                    | supple_learningTrajectory.m           | code     |
| 8y2rs                    | poweranalysis_sph.R                   | code     |
| zd7j6                    | supple_sphWithVariousRadius_clean.m   | code     |
| xyh32                    | main_analyseObjLocTest.m              | code     |
| jpm5a                    | suppleVideo2_learnPlane.mp4           | video    |
| aux7s                    | suppleVideo3_objlocSph.mp4            | video    |
| y75nu                    | suppleVideo6_trianglePlane.mp4        | video    |
| 2es6n                    | suppleVideo1_learnSph.mp4             | video    |
| nw3mc                    | suppleVideo4_objlocPlane.mp4          | video    |
| ks639                    | suppleVideo5_triangleSph.mp4          | video    |
| avwq5                    | suppleMovie_legend.txt                | text     |
| 6k4ma                    | sumDemograph.csv                      | data     |
| p6y4b                    | rawdata_plane_triangle.csv            | data     |
| uw23q                    | rawdata_sph_objlocTest.csv            | data     |
| hwmkb                    | rawdata_sph_triangle.csv              | data     |
| xhdju                    | cleanData_combine.mat                 | code     |
| z374c                    | pilotData_triangle_combine_clean.csv  | data     |
| ye56f                    | rawdata_plane_objlocIdentity.csv      | data     |
| ej3dc                    | rawdata_sph_objlocIdentity.csv        | data     |
| vj8c2                    | rawdata_plane_objlocTest.csv          | data     |
| hv29w                    | Kim,Doeller_Prereg_OSF.pdf            | text     |
| br82f                    | findShortcut.m                        | code     |
| vbfwm                    | sph2cartFn.m                          | code     |
| x62av                    | drawGeodesic.m                        | code     |
| mq6g2                    | sph2cartMKunity.m                     | code     |
| g8qd9                    | translateOnSphere.m                   | code     |
| fmcr3                    | northVecFn.m                          | code     |
| zqdms                    | ttestplotMK2.m                        | code     |
| 8c3as                    | cart2sphFn.m                          | code     |
| fwkg6                    | rotAroundU.m                          | code     |
| wr9z7                    | psub11_objLearn_Sph_traj.tsv          | data     |
| zd273                    | psub16_objLearn_Plane_traj.tsv        | data     |
| y3hcd                    | psub06_objLearn_Sph_traj.tsv          | data     |
| wcp42                    | psub25_objLearn_Plane_traj.tsv        | data     |
| myb2c                    | psub04_objLearn_Plane_traj.tsv        | data     |
| 5mn3g                    | psub17_objLearn_Sph_traj.tsv          | data     |
| fyeck                    | psub10_objLearn_Plane_traj.tsv        | data     |
| a2z7f                    | psub08_objLearn_Plane_traj.tsv        | data     |
| symgt                    | psub14_objLearn_Sph_traj.tsv          | data     |
| 6qk9u                    | psub09_objLearn_Sph_traj.tsv          | data     |
| 64f0ace16c0f5a0650d059c8 | psub27_objLearn_Plane_traj.tsv        | data     |
| 64f0acf2d9f2c905a0d048d2 | psub32_objLearn_Sph_traj.tsv          | data     |
| 64f0acef6d1e8905ee1515e2 | psub32_objLearn_Plane_traj.tsv        | data     |
| 64f0accd6c0f5a064fd058f0 | psub21_objLearn_Sph_traj.tsv          | data     |
| 64f0acb36c0f5a0650d059ab | psub12_objLearn_Sph_traj.tsv          | data     |
| 64f0ad06989de605badd1471 | psub41_objLearn_Plane_traj.tsv        | data     |
| 64f0acd6f3dcd105d3ddd39b | psub23_objLearn_Plane_traj.tsv        | data     |
| w7v9f                    | psub44_objLearn_Sph_traj.tsv          | data     |
| 64f0acc3989de605c3dd1622 | psub17_objLearn_Plane_traj.tsv        | data     |
| 6qk9u                    | psub09_objLearn_Sph_traj.tsv          | data     |
| k75rx                    | psub06_objLearn_Plane_traj.tsv        | data     |
| f538v                    | psub37_objLearn_Plane_traj.tsv        | data     |
| ns9d3                    | psub30_objLearn_Sph_traj.tsv          | data     |
| 36nrk                    | psub23_objLearn_Sph_traj.tsv          | data     |
| 9fcnv                    | psub03_objLearn_Plane_traj.tsv        | data     |
| 27fxk                    | psub42_objLearn_Sph_traj.tsv          | data     |
| ef3hq                    | psub40_objLearn_Sph_traj.tsv          | data     |
| qkrzx                    | psub05_objLearn_Sph_traj.tsv          | data     |
| m3g6x                    | psub40_objLearn_Plane_traj.tsv        | data     |
| 6qk9u                    | psub09_objLearn_Sph_traj.tsv          | data     |
| 5s4hq                    | psub22_objLearn_Sph_traj.tsv          | data     |
| dpxwh                    | psub47_objLearn_Plane_traj.tsv        | data     |
| 64f0acc9f3dcd105dbddd4a6 | psub20_objLearn_Plane_traj.tsv        | data     |
| gdqnv                    | psub46_objLearn_Sph_traj.tsv          | data     |
| gtmjp                    | psub47_objLearn_Sph_traj.tsv          | data     |
| 64f0ad016d1e8905f3151778 | psub38_objLearn_Sph_traj.tsv          | data     |
| 64f0acac6c0f5a0650d059a9 | psub10_objLearn_Sph_traj.tsv          | data     |
| 64f0acb8f3dcd105dbddd49e | psub14_objLearn_Plane_traj.tsv        | data     |
| 64f0acc6d9f2c905a4d049b3 | psub19_objLearn_Sph_traj.tsv          | data     |
| kbghd                    | psub34_objLearn_Sph_traj.tsv          | data     |
| 64f0acfe989de605c3dd164d | psub37_objLearn_Sph_traj.tsv          | data     |
| 64f0acf36d1e8905f315176e | psub33_objLearn_Plane_traj.tsv        | data     |
| 64f0ace3f3dcd105dbddd4b8 | psub27_objLearn_Sph_traj.tsv          | data     |
| 64f0acbc6d1e8905ee1515d8 | psub15_objLearn_Plane_traj.tsv        | data     |
| 64f0acc9989de605bfdd160a | psub20_objLearn_Sph_traj.tsv          | data     |
| 64f0aca8d9f2c905a0d048b8 | psub09_objLearn_Plane_traj.tsv        | data     |
| 64f0ad00d9f2c905a4d049d6 | psub38_objLearn_Plane_traj.tsv        | data     |
| 64f0acb1f3dcd105d6ddd37c | psub12_objLearn_Plane_traj.tsv        | data     |
| 64f0ac98d9f2c905a4d04981 | psub03_objLearn_Sph_traj.tsv          | data     |
| 64f0acfd6d1e8905f3151776 | psub35_objLearn_Sph_traj.tsv          | data     |
| 3jr6q                    | psub46_objLearn_Plane_traj.tsv        | data     |
| 64f0ace8989de605c3dd163f | psub30_objLearn_Plane_traj.tsv        | data     |
| 64f0acd36c0f5a0650d059c3 | psub22_objLearn_Plane_traj.tsv        | data     |
| 64f0acb7f3dcd105d7ddd44e | psub13_objLearn_Sph_traj.tsv          | data     |
| 64f0acc0989de605bfdd1603 | psub16_objLearn_Sph_traj.tsv          | data     |
| 64f0ac9bd9f2c905a5d04a2c | psub04_objLearn_Sph_traj.tsv          | data     |
| 64f0ad0ad9f2c9059dd0482b | psub42_objLearn_Plane_traj.tsv        | data     |
| 64f0acdc6d1e8905f2151737 | psub25_objLearn_Sph_traj.tsv          | data     |
| 64f0ac9fd9f2c905a4d04983 | psub05_objLearn_Plane_traj.tsv        | data     |
| 9vzb4                    | psub44_objLearn_Plane_traj.tsv        | data     |
| 64f0acded9f2c905a4d049c8 | psub26_objLearn_Plane_traj.tsv        | data     |
| 64f0acf6d9f2c905a5d04a58 | psub34_objLearn_Plane_traj.tsv        | data     |
| 64f0acb4989de605c2dd1588 | psub13_objLearn_Plane_traj.tsv        | data     |
| 64f0ad08989de605c3dd1650 | psub41_objLearn_Sph_traj.tsv          | data     |
| 64f0aceb989de605c3dd1642 | psub31_objLearn_Plane_traj.tsv        | data     |
| 64f0acd9d9f2c905a4d049c3 | psub24_objLearn_Sph_traj.tsv          | data     |
| 64f0acbd6c0f5a064cd05891 | psub15_objLearn_Sph_traj.tsv          | data     |
| 64f0acaed9f2c905a4d049aa | psub11_objLearn_Plane_traj.tsv        | data     |
| 64f0ace5989de605c3dd163d | psub28_objLearn_Plane_traj.tsv        | data     |
| 64f0ace7d9f2c905a4d049cc | psub28_objLearn_Sph_traj.tsv          | data     |
| 4bze7                    | psub21_objLearn_Plane_traj.tsv        | data     |
| c7ryq                    | psub08_objLearn_Sph_traj.tsv          | data     |
| j2hm8                    | psub19_objLearn_Plane_traj.tsv        | data     |
| m28av                    | psub35_objLearn_Plane_traj.tsv        | data     |
| pr4km                    | psub33_objLearn_Sph_traj.tsv          | data     |
| d934m                    | psub24_objLearn_Plane_traj.tsv        | data     |
| 64f0ab74989de605c2dd14eb | LICENSE                               | NA       |
| 64f0ab746d1e8905ef1515e8 | mapface2edge.m                        | code     |
| 64f0ab7c989de605c2dd14f0 | sortrowstol.m                         | code     |
| 64f0ab7c6c0f5a0650d05905 | spheretri.m                           | code     |
| 64f0ab7ff3dcd105dbddd3db | SphereTriTestCase.m                   | code     |
| 64f0ab7f6d1e8905ee15157d | spheretribydepth.m                    | code     |
| 64f0ab72989de605c3dd155d | istriequal.m                          | code     |
| xuvp9                    | README.md                             | text     |
| 64f0ab78f3dcd105d6ddd35b | shrinkfacetri.m                       | code     |
| 64f0ab6e989de605bedd146d | combvec.m                             | code     |
| 64f0ab71f3dcd105d7ddd40c | isface.m                              | code     |
| 64f0ab6fd9f2c905a5d04929 | icosahedron.m                         | code     |

We can then use this information to determine if, for each file, the
information about the files contains text that makes it easy to
determine what is being shared. A simple regular expression text search
for ‘README’, ‘codebook’, ‘script’, and ‘data’ (in a number of possible
ways that these words can be written) is used to automatically detect
what is shared.

``` r
osf_files_summary <- summarize_contents(info)
```

| name                                  | filetype | file_category |
|:--------------------------------------|:---------|:--------------|
| Kim,Doeller_Prereg_OSF.pdf            | text     | NA            |
| suppleVideo1_learnSph.mp4             | video    | NA            |
| suppleVideo2_learnPlane.mp4           | video    | NA            |
| suppleVideo3_objlocSph.mp4            | video    | NA            |
| suppleVideo4_objlocPlane.mp4          | video    | NA            |
| suppleVideo5_triangleSph.mp4          | video    | NA            |
| suppleVideo6_trianglePlane.mp4        | video    | NA            |
| virtualizerStudy1-main.zip            | archive  | NA            |
| ReadMe.txt                            | text     | readme        |
| main_analyseTriangleComple_20230423.m | code     | code          |
| main_simulate_objlocTraj.m            | code     | code          |
| supple_learningTrajectory.m           | code     | code          |
| poweranalysis_sph.R                   | code     | code          |
| supple_sphWithVariousRadius_clean.m   | code     | code          |
| main_analyseObjLocTest.m              | code     | code          |
| suppleVideo2_learnPlane.mp4           | video    | NA            |
| suppleVideo3_objlocSph.mp4            | video    | NA            |
| suppleVideo6_trianglePlane.mp4        | video    | NA            |
| suppleVideo1_learnSph.mp4             | video    | NA            |
| suppleVideo4_objlocPlane.mp4          | video    | NA            |
| suppleVideo5_triangleSph.mp4          | video    | NA            |
| suppleMovie_legend.txt                | text     | NA            |
| sumDemograph.csv                      | data     | data          |
| rawdata_plane_triangle.csv            | data     | data          |
| rawdata_sph_objlocTest.csv            | data     | data          |
| rawdata_sph_triangle.csv              | data     | data          |
| cleanData_combine.mat                 | code     | code          |
| pilotData_triangle_combine_clean.csv  | data     | data          |
| rawdata_plane_objlocIdentity.csv      | data     | data          |
| rawdata_sph_objlocIdentity.csv        | data     | data          |
| rawdata_plane_objlocTest.csv          | data     | data          |
| Kim,Doeller_Prereg_OSF.pdf            | text     | NA            |
| findShortcut.m                        | code     | code          |
| sph2cartFn.m                          | code     | code          |
| drawGeodesic.m                        | code     | code          |
| sph2cartMKunity.m                     | code     | code          |
| translateOnSphere.m                   | code     | code          |
| northVecFn.m                          | code     | code          |
| ttestplotMK2.m                        | code     | code          |
| cart2sphFn.m                          | code     | code          |
| rotAroundU.m                          | code     | code          |
| psub11_objLearn_Sph_traj.tsv          | data     | data          |
| psub16_objLearn_Plane_traj.tsv        | data     | data          |
| psub06_objLearn_Sph_traj.tsv          | data     | data          |
| psub25_objLearn_Plane_traj.tsv        | data     | data          |
| psub04_objLearn_Plane_traj.tsv        | data     | data          |
| psub17_objLearn_Sph_traj.tsv          | data     | data          |
| psub10_objLearn_Plane_traj.tsv        | data     | data          |
| psub08_objLearn_Plane_traj.tsv        | data     | data          |
| psub14_objLearn_Sph_traj.tsv          | data     | data          |
| psub09_objLearn_Sph_traj.tsv          | data     | data          |
| psub27_objLearn_Plane_traj.tsv        | data     | data          |
| psub32_objLearn_Sph_traj.tsv          | data     | data          |
| psub32_objLearn_Plane_traj.tsv        | data     | data          |
| psub21_objLearn_Sph_traj.tsv          | data     | data          |
| psub12_objLearn_Sph_traj.tsv          | data     | data          |
| psub41_objLearn_Plane_traj.tsv        | data     | data          |
| psub23_objLearn_Plane_traj.tsv        | data     | data          |
| psub44_objLearn_Sph_traj.tsv          | data     | data          |
| psub17_objLearn_Plane_traj.tsv        | data     | data          |
| psub09_objLearn_Sph_traj.tsv          | data     | data          |
| psub06_objLearn_Plane_traj.tsv        | data     | data          |
| psub37_objLearn_Plane_traj.tsv        | data     | data          |
| psub30_objLearn_Sph_traj.tsv          | data     | data          |
| psub23_objLearn_Sph_traj.tsv          | data     | data          |
| psub03_objLearn_Plane_traj.tsv        | data     | data          |
| psub42_objLearn_Sph_traj.tsv          | data     | data          |
| psub40_objLearn_Sph_traj.tsv          | data     | data          |
| psub05_objLearn_Sph_traj.tsv          | data     | data          |
| psub40_objLearn_Plane_traj.tsv        | data     | data          |
| psub09_objLearn_Sph_traj.tsv          | data     | data          |
| psub22_objLearn_Sph_traj.tsv          | data     | data          |
| psub47_objLearn_Plane_traj.tsv        | data     | data          |
| psub20_objLearn_Plane_traj.tsv        | data     | data          |
| psub46_objLearn_Sph_traj.tsv          | data     | data          |
| psub47_objLearn_Sph_traj.tsv          | data     | data          |
| psub38_objLearn_Sph_traj.tsv          | data     | data          |
| psub10_objLearn_Sph_traj.tsv          | data     | data          |
| psub14_objLearn_Plane_traj.tsv        | data     | data          |
| psub19_objLearn_Sph_traj.tsv          | data     | data          |
| psub34_objLearn_Sph_traj.tsv          | data     | data          |
| psub37_objLearn_Sph_traj.tsv          | data     | data          |
| psub33_objLearn_Plane_traj.tsv        | data     | data          |
| psub27_objLearn_Sph_traj.tsv          | data     | data          |
| psub15_objLearn_Plane_traj.tsv        | data     | data          |
| psub20_objLearn_Sph_traj.tsv          | data     | data          |
| psub09_objLearn_Plane_traj.tsv        | data     | data          |
| psub38_objLearn_Plane_traj.tsv        | data     | data          |
| psub12_objLearn_Plane_traj.tsv        | data     | data          |
| psub03_objLearn_Sph_traj.tsv          | data     | data          |
| psub35_objLearn_Sph_traj.tsv          | data     | data          |
| psub46_objLearn_Plane_traj.tsv        | data     | data          |
| psub30_objLearn_Plane_traj.tsv        | data     | data          |
| psub22_objLearn_Plane_traj.tsv        | data     | data          |
| psub13_objLearn_Sph_traj.tsv          | data     | data          |
| psub16_objLearn_Sph_traj.tsv          | data     | data          |
| psub04_objLearn_Sph_traj.tsv          | data     | data          |
| psub42_objLearn_Plane_traj.tsv        | data     | data          |
| psub25_objLearn_Sph_traj.tsv          | data     | data          |
| psub05_objLearn_Plane_traj.tsv        | data     | data          |
| psub44_objLearn_Plane_traj.tsv        | data     | data          |
| psub26_objLearn_Plane_traj.tsv        | data     | data          |
| psub34_objLearn_Plane_traj.tsv        | data     | data          |
| psub13_objLearn_Plane_traj.tsv        | data     | data          |
| psub41_objLearn_Sph_traj.tsv          | data     | data          |
| psub31_objLearn_Plane_traj.tsv        | data     | data          |
| psub24_objLearn_Sph_traj.tsv          | data     | data          |
| psub15_objLearn_Sph_traj.tsv          | data     | data          |
| psub11_objLearn_Plane_traj.tsv        | data     | data          |
| psub28_objLearn_Plane_traj.tsv        | data     | data          |
| psub28_objLearn_Sph_traj.tsv          | data     | data          |
| psub21_objLearn_Plane_traj.tsv        | data     | data          |
| psub08_objLearn_Sph_traj.tsv          | data     | data          |
| psub19_objLearn_Plane_traj.tsv        | data     | data          |
| psub35_objLearn_Plane_traj.tsv        | data     | data          |
| psub33_objLearn_Sph_traj.tsv          | data     | data          |
| psub24_objLearn_Plane_traj.tsv        | data     | data          |
| LICENSE                               | NA       | NA            |
| mapface2edge.m                        | code     | code          |
| sortrowstol.m                         | code     | code          |
| spheretri.m                           | code     | code          |
| SphereTriTestCase.m                   | code     | code          |
| spheretribydepth.m                    | code     | code          |
| istriequal.m                          | code     | code          |
| README.md                             | text     | readme        |
| shrinkfacetri.m                       | code     | code          |
| combvec.m                             | code     | code          |
| isface.m                              | code     | code          |
| icosahedron.m                         | code     | code          |

### Report Text

Finally, we print a report that communicates to the user - for example,
a researcher preparing their manuscript for submission - whether there
are suggestions to improve their data repository. We provide feedback
about whether any of the four categories could be automatically
detected, and if not, provide additional information about what would
have made the automated tool recognize the files of interest. The output
gives a detailed overview of the information it could not find,
alongside a suggestion for how to learn more about best practices in
this domain. If researchers use this metacheck module before submission,
they can improve the quality of their data repository in case any
information is missing. metacheck might miss data and code that is
shared, but not clearly named, but by indicating this, users might
realize that the data repository can be improved by more clearly naming
folders and files.

``` r
osf_report <- function(summary) {
  files <- dplyr::filter(summary, osf_type == "files")
  data <- dplyr::filter(files, file_category == "data") |> nrow()
  code <- dplyr::filter(files, file_category == "code") |> nrow()
  codebook <- dplyr::filter(files, file_category == "codebook") |> nrow()
  readme <- dplyr::filter(files, file_category == "readme") |> nrow()
  
  traffic_light <- dplyr::case_when(
    data == 0 & code == 0 & readme == 0 ~ "red",
    data == 0 | code == 0 | readme == 0 ~ "yellow",
    data > 0 & code > 0 & readme > 0 ~ "green"
  )
  
  data_report <- dplyr::case_when(
    data == 0 ~ "\u26A0\uFE0F There was no data detected. Are you sure you cannot share any of the underlying data? If you did share the data, consider naming the file(s) or file folder with 'data'.",
    data > 0 ~ "\u2705 Data file(s) were detected. Great job making your research more transparent and reproducible!"
  )
  
  codebook_report <- dplyr::case_when(
    codebook == 0 ~ "\u26A0\uFE0F️ No codebooks or data dictionaries were found. Consider adding one to make it easier for others to know which variables you have collected, and how to re-use them. The codebook package in R can automate a substantial part of the generation of a codebook: https://rubenarslan.github.io/codebook/",
    codebook > 0 ~ "\u2705 Codebook(s) were detected. Well done!"
  )
  
  code_report <- dplyr::case_when(
    code == 0 ~ "\u26A0\uFE0F️ No code files were found. Are you sure there is no code related to this manuscript? If you shared code, consider naming the file or file folder with 'code' or 'script'.",
    code > 0 ~ "\u2705 Code file(s) were detected. Great job making it easier to  reproduce your results!"
  )
  
  readme_report <- dplyr::case_when(
    readme == 0 ~ "\u26A0\uFE0F No README files were identified. A read me is best practice to facilitate re-use. If you have a README, please name it explicitly (e.g., README.txt or _readme.pdf).",
    readme > 0 ~ "\u2705 README detected. Great job making it easier to understand how to re-use files in your repository!"
  )
  
  report_message <- paste(
    readme_report,
    data_report, 
    codebook_report,
    code_report,
    "Learn more about reproducible data practices: https://www.projecttier.org/tier-protocol/",
    sep = "\n\n"
  )

  return(list(
    traffic_light = traffic_light,
    report = report_message
  ))
}
```

``` r
report <- osf_report(osf_files_summary) 

# print the report into a file
module_report(report) |> cat()
```

✅ README detected. Great job making it easier to understand how to
re-use files in your repository!

✅ Data file(s) were detected. Great job making your research more
transparent and reproducible!

⚠️️ No codebooks or data dictionaries were found. Consider adding one to
make it easier for others to know which variables you have collected,
and how to re-use them. The codebook package in R can automate a
substantial part of the generation of a codebook:
<https://rubenarslan.github.io/codebook/>

✅ Code file(s) were detected. Great job making it easier to reproduce
your results!

Learn more about reproducible data practices:
<https://www.projecttier.org/tier-protocol/>

### Checking the Contents of files

So far we have used metacheck to automatically check whether certain
types of files exist. But it is also possible to automatically download
files, examine their contents, and provide feedback to users. This can
be useful to examine datasets (e.g., do files contain IP addresses or
other personal information), or code files. We will illustrate the
latter by automatically checking the content of R scripts stored on the
OSF, in repositories that are linked to in a scientific manuscript.

We can check R files for good coding practices that improve
reproducibility. We have created a check that examines 1) whether all
libraries are loaded in one block, instead of throughout the R script,
2) whether relative paths are used that will also work when someone runs
the code on a different computer (e.g.,
`data <- read.csv(file='../data/data_study_1.csv')` ) instead of fixed
paths (e.g., `data <- read.csv(file='C:/data/data_study_1.csv')` ), and
3) whether information is provided about the software used (i.e., the R
version), the version of packages that were used, and properties of the
computer that the analyses were performed on. In R, this can be achieved
by:

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: aarch64-apple-darwin20
#> Running under: macOS Sequoia 15.5
#> 
#> Matrix products: default
#> BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> time zone: Europe/London
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] lubridate_1.9.4      forcats_1.0.1        stringr_1.6.0       
#>  [4] dplyr_1.1.4          purrr_1.2.1          readr_2.1.6         
#>  [7] tidyr_1.3.2          tibble_3.3.1         ggplot2_4.0.1       
#> [10] tidyverse_2.0.0      metacheck_0.0.0.9068
#> 
#> loaded via a namespace (and not attached):
#>  [1] sass_0.4.10        generics_0.1.4     stringi_1.8.7      hms_1.1.4         
#>  [5] digest_0.6.39      magrittr_2.0.4     timechange_0.3.0   evaluate_1.0.5    
#>  [9] grid_4.5.2         RColorBrewer_1.1-3 fastmap_1.2.0      jsonlite_2.0.0    
#> [13] httr_1.4.7         scales_1.4.0       textshaping_1.0.4  jquerylib_0.1.4   
#> [17] cli_3.6.5          rlang_1.1.7        oddpub_7.2.3       withr_3.0.2       
#> [21] cachem_1.1.0       yaml_2.3.12        otel_0.2.0         tools_4.5.2       
#> [25] tzdb_0.5.0         DT_0.34.0          curl_7.0.0         vctrs_0.7.0       
#> [29] R6_2.6.1           lifecycle_1.0.5    fs_1.6.6           htmlwidgets_1.6.4 
#> [33] ragg_1.5.0         pkgconfig_2.0.3    desc_1.4.3         pkgdown_2.2.0     
#> [37] bslib_0.9.0        pillar_1.11.1      gtable_0.3.6       glue_1.8.0        
#> [41] systemfonts_1.3.1  xfun_0.56          tidyselect_1.2.1   rstudioapi_0.18.0 
#> [45] knitr_1.51         farver_2.1.2       htmltools_0.5.9    rmarkdown_2.30    
#> [49] compiler_4.5.2     S7_0.2.1
```

As most scientists have not been taught how to code explicitly, it is
common to see scripts that do not adhere to best coding practices. We
are no exception ourselves (e.g., you will not find a sessioninfo.txt
file in our repositories). Although code might be reproducible even if
it takes time to figure out which versions of an R package was used,
which R version was used, and by changing fixed paths, reproducibility
is facilitated if best practices are used. The whole point of automated
checks is to have algorithms that capture expertise make recommendations
that improve how we currently work.

``` r
check_r_files <- function(summary) {
  r_files <- summary |>
    dplyr::filter(osf_type == "files",
                  grepl("\\.R(md)?", name, ignore.case = TRUE)) |>
    dplyr::mutate(abs_report = NA, 
                  pkg_report = NA,
                  session_report = NA)
  
  report <- lapply(r_files$osf_id, \(id) {
    report <- dplyr::filter(r_files, osf_id == !!id)
    # Try downloading the R file
    file_url <- paste0("https://osf.io/download/", id)
    r_code <- tryCatch(
      readLines(url(file_url), warn = FALSE),
      error = function(e) return(NULL)
    )
    
    if (is.null(r_code)) return("")
    
    # absolute paths
    abs_path <- grep("[\"\']([A-Z]:|\\/|~)", r_code)
    report$abs <- dplyr::case_when(
      length(abs_path) == 0 ~ "\u2705 No absolute paths were detected",
      length(abs_path) > 0 ~ paste("\u274C Absolute paths found at lines: ",
                                   paste(abs_path, collapse = ", "))
    )
    
    # package loading
    pkg <- grep("\\b(library|require)\\(", r_code)
    report$pkg<- dplyr::case_when(
      length(pkg) == 0 ~ "\u26A0\uFE0F️ No packages are specified in this script.",
      length(pkg) == 1 ~ "\u2705 Packages are loaded in a single block.",
      all(diff(pkg) < 5) ~ "\u2705 Packages are loaded in a single block.",
      .default = paste(
        "\u274C Packages are loaded in multiple places: lines " ,
        paste(pkg, collapse = ", ")
      )
    )
    
    # session info 
    session <- grep("\\bsession_?[Ii]nfo\\(", r_code)
    report$session <- dplyr::case_when(
      length(session) == 0 ~ "\u274C️ No session info was found in this script.",
      length(session) > 0 ~ paste(
        "\u2705 Session info was found on line", 
        paste(session, collapse = ", "))
    )
    
    return(report)
  }) |>
    do.call(dplyr::bind_rows, args = _)
  
  return(report)
}
```

``` r
r_file_results <- check_r_files(osf_files_summary)
```

| name                | report  | feedback                                     |
|:--------------------|:--------|:---------------------------------------------|
| poweranalysis_sph.R | abs     | ✅ No absolute paths were detected           |
| poweranalysis_sph.R | pkg     | ✅ Packages are loaded in a single block.    |
| poweranalysis_sph.R | session | ❌️ No session info was found in this script. |

## Put it All Together

Let’s put everything together in one block of code, and perform all
automated checks for another open access paper in Psychological Science.

``` r
# Add this and the custom functions to a file called osf_file_check.R

osf_file_check <- function(paper) {
  links <- osf_links(paper)
  info <- osf_retrieve(links, recursive = TRUE)
  osf_files_summary <- summarize_contents(info)
  report <- osf_report(osf_files_summary)
  r_file_results <- check_r_files(osf_files_summary)  
  
  list(
    traffic_light = report$traffic_light,
    table = r_file_results,
    report = report$report,
    summary = osf_files_summary
  )
}
```

``` r
module_results <- module_run(psychsci$`0956797620955209`, "osf_file_check.R")
#> Starting OSF retrieval for 1 URL...
#> * Retrieving info from k2dbf...
#> ...Main retrieval complete
#> Starting retrieval of children...
#> * Retrieving children for k2dbf...
#> * Retrieving files for k2dbf...
#> * Retrieving files for 5e344fb4f6631d013e5a48c9...
#> * Retrieving files for 5b88067b7b17570016f95389...
#> ...OSF retrieval complete!
```

``` r
module_report(module_results, header = 4) |> cat()
```

#### 🔍 Module Title

⚠️ No README files were identified. A read me is best practice to
facilitate re-use. If you have a README, please name it explicitly
(e.g., README.txt or \_readme.pdf).

✅ Data file(s) were detected. Great job making your research more
transparent and reproducible!

⚠️️ No codebooks or data dictionaries were found. Consider adding one to
make it easier for others to know which variables you have collected,
and how to re-use them. The codebook package in R can automate a
substantial part of the generation of a codebook:
<https://rubenarslan.github.io/codebook/>

✅ Code file(s) were detected. Great job making it easier to reproduce
your results!

Learn more about reproducible data practices:
<https://www.projecttier.org/tier-protocol/>

A short description of the module

This module was developed by Author Name

## Future Developments

We have demonstrated a rather basic workflow that can automatically
check files stored on the Open Science Framework, and all the checks
demonstrated here can be made more accurate or complete. At the same
time, even the current simple automatic checks might already facilitate
re-use by including information (e.g., a README) and improving how files
are named. There are many obvious ways to expand these automated checks.
First, the example can be expanded to other commonly used data
repositories, such as GitHub, Dataverse, etc. Second, the checks can be
expanded beyond the properties that are automatically checked now. If
you are an expert on code reproducibility or data re-use and would like
to add checks, do reach out to us. Third, we can check for other types
of files. For example, we are collaborating with Attila Simko who is
interested in identifying the files required to [reproduce deep learning
models in the medical imaging
literature](https://arxiv.org/abs/2210.11146). We believe there will be
many such field-dependent checks that can be automated, as the ability
to automatically examine and/or retrieve files that are linked to in a
paper should be useful for a large range of use-cases.

**These examples were created using papercheck version 0.0.0.9068.**

## References

Ferguson, Joel, Rebecca Littman, Garret Christensen, Elizabeth Levy
Paluck, Nicholas Swanson, Zenan Wang, Edward Miguel, David Birke, and
John-Henry Pezzuto. 2023. “Survey of Open Science Practices and
Attitudes in the Social Sciences.” *Nature Communications* 14 (11):
5401. <https://doi.org/10.1038/s41467-023-41111-1>.
