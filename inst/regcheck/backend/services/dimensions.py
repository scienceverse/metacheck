from __future__ import annotations

from typing import Literal

DimensionSet = Literal["clinical_trials", "general_preregistration"]
Dimension = dict[str, str]


GENERAL_PREREGISTRATION_DEFAULT_DIMENSIONS: list[Dimension] = [
    {
        "dimension": "Sample size",
        "definition": (
            "The planned number of participants to be included in the study. This could include a precise "
            "number of units or observations that the researchers intend to sample or a range, minimum, or "
            "maximum. It could also include a stopping rule (that is, how the decision to terminate data "
            "collection will be made) if one is specified."
        ),
    },
    {
        "dimension": "Data source",
        "definition": (
            "Where the data will be / was obtained, and the processes therein. For example, a student sample, "
            "online (e.g., m-turk, prolific), the community, etc."
        ),
    },
    {
        "dimension": "Inclusion criteria",
        "definition": "How subjects (or units of analysis otherwise specified) will be selected for eligibility in the study.",
    },
    {
        "dimension": "Exclusion criteria",
        "definition": (
            "How data exclusions will be determined (e.g., how will outliers be determined? Will awareness "
            "checks be implemented?)."
        ),
    },
    {
        "dimension": "Incomplete and missing data",
        "definition": "A description of how incomplete and/or missing data will be handled.",
    },
    {
        "dimension": "Hypotheses",
        "definition": "A prediction or set of predictions about the result(s) of a study.",
    },
    {
        "dimension": "Manipulated variables",
        "definition": (
            "Refers to variables that are experimentally manipulated, that is, the experimenter controlled "
            "the level / treatment that the subject/ unit of analysis received."
        ),
    },
    {
        "dimension": "Measured variables",
        "definition": (
            "Variables that are observed/ recorded in a study. This will include outcome measures, as well as "
            "any measured predictors or covariates. Also related is how the variable(s) will be measured "
            "(e.g., \"The primary outcome variable will be the perceived tastiness of the single brownie each "
            "participant will eat. We will measure this by asking participants 'How much did you enjoy eating "
            "the brownie' (on a scale of 1-7, 1 being 'not at all', 7 being 'a great deal').\"). If authors "
            "mention that measures will be combined into an index/ composite (or a mean), this is also relevant."
        ),
    },
    {
        "dimension": "Transformations",
        "definition": (
            "Steps, decisions, or approaches that relate to processing data from one form, structure, or scale "
            "to another to prepare it for analysis (e.g., log transforming, centering, or recoding the data)."
        ),
    },
    {
        "dimension": "Statistical models",
        "definition": (
            "The statistical model(s) used to test the hypotheses. This includes the type of model (e.g. ANOVA, "
            "RMANOVA, MANOVA, multiple regression, SEM, etc) and the specification of the model. It also "
            "includes each variable that will be included, all interactions, subgroup analyses, pairwise or "
            "complex contrasts, and any follow-up tests from omnibus tests. If transformations are planned or "
            "reported (e.g., log transforming, centering, or recoding the data) this information should also "
            "be extracted. If any inference criteria are specified (e.g., alpha thresholds, bayes factors, "
            "specific model fit indices or other cut-off criteria), that should also be extracted. This "
            "includes details about using one-tailed vs. two-tailed tests."
        ),
    },
]


CLINICAL_TRIALS_DEFAULT_DIMENSIONS: list[Dimension] = [
    {
        "dimension": "Eligibility: inclusion criteria",
        "definition": (
            "All explicitly stated conditions, characteristics, or thresholds that must be satisfied for a "
            "participant or study unit (e.g., individual, cluster, site) to be enrolled in the study. This "
            "includes required demographic characteristics (e.g., age range, sex/gender, pregnancy/menopausal "
            "status), diagnosis or target condition (including staging, severity, symptom duration, biomarker "
            "status), clinical status (e.g., performance status, treatment-naive status), prior or current "
            "treatments that are required or permitted, contextual factors (e.g., recruitment setting, "
            "healthcare setting, geography), and other positive requirements (e.g., language proficiency, "
            "ability to comply with study procedures, capacity to consent, access to required technology or "
            "devices), typically expressed using terms such as \"must have,\" \"must meet,\" \"only participants "
            "with,\" or \"eligible if.\" Commonly appears as bullet points or prose under headings such as "
            "\"Inclusion criteria,\" \"Eligibility criteria,\" or \"Participants\" in trial registries, protocols, "
            "and manuscripts."
        ),
    },
    {
        "dimension": "Eligibility: exclusion criteria",
        "definition": (
            "All explicitly stated conditions or characteristics that disqualify a participant from enrollment "
            "or continued participation in the study. This includes safety-related exclusions (e.g., "
            "contraindications, relevant comorbidities, allergies, pregnancy where specified as exclusion, "
            "severe organ dysfunction), protocol- or confounding-related exclusions (e.g., concurrent trial "
            "participation, use of prohibited concomitant treatments, prior exposure to the study intervention "
            "beyond allowed limits), feasibility-related exclusions (e.g., inability or unwillingness to adhere "
            "to required visits, procedures, or follow-up), and history or condition-based exclusions indicated "
            "by phrases such as \"must not,\" \"excluded if,\" \"no history of,\" or \"not eligible if.\" Commonly "
            "appears under \"Exclusion criteria\" or within \"Eligibility\"/\"Participants\" sections in trial "
            "registries, protocols, and manuscripts."
        ),
    },
    {
        "dimension": "Intervention/treatment and control/placebo",
        "definition": (
            "The detailed specification of each study arm, including both experimental and comparator arms. "
            "This encompasses the name or identity of the intervention (e.g., drug, biologic, device, surgical "
            "procedure, behavioural or psychological program, digital/app-based intervention), the dose or "
            "intensity (e.g., strength, amount, frequency, duration of sessions), route or mode of "
            "administration (e.g., oral, IV, subcutaneous, inhaled, implanted, online, in-person, telephone), "
            "schedule and timing (including dosing schedules, titration, tapering, timing relative to baseline "
            "or randomisation), planned duration of administration, and a clear description of the control or "
            "comparator condition, such as placebo (including route and appearance), sham procedure, active "
            "control (with its own dose and schedule), or usual/standard care (with defining elements). It "
            "also includes any mandated co-interventions (e.g., background therapy) and any explicitly "
            "prohibited concomitant treatments. Commonly appears under headings such as \"Interventions,\" "
            "\"Study treatments,\" \"Arm description,\" \"Trial arms,\" or \"Treatment protocol.\""
        ),
    },
    {
        "dimension": "Etical approval: Number",
        "definition": (
            "The specific identifier(s) associated with ethical approval of the study, including ethics "
            "committee/IRB/REC/REB approval numbers, protocol codes linked to ethics approval, national or "
            "institutional reference numbers, and identifiers for substantial amendments where explicitly "
            "reported. These identifiers are typically presented in sections or statements labelled \"Ethics "
            "approval,\" \"Ethical considerations,\" \"IRB (Institutional Review Board) approval,\" or similar, "
            "and may appear in trial registries, protocols, and manuscripts."
        ),
    },
    {
        "dimension": "Ethical approval: Committee",
        "definition": (
            "The official name(s) of the ethics body or bodies responsible for reviewing and approving the "
            "study, such as Institutional Review Boards (IRBs), Research Ethics Committees (RECs), Research "
            "Ethics Boards (REBs), hospital or university ethics committees, and national or regional competent "
            "authorities performing ethics review. Names often include the institution and country or region "
            "(e.g., \"University X Research Ethics Committee, Country Y\") and are typically reported in "
            "ethics-related sections or statements in trial registries, protocols, and manuscripts."
        ),
    },
    {
        "dimension": "Ethics approval: Date",
        "definition": (
            "The calendar date on which ethics approval was granted for the primary study protocol, and, where "
            "explicitly reported, the dates on which major amendments were approved. Dates may be presented in "
            "full (e.g., \"15 March 2022\") or in partial formats (e.g., month/year) and are usually mentioned "
            "alongside the ethics committee name or approval number in sections titled \"Ethics approval,\" "
            "\"Ethical considerations,\" or similar in trial registries, protocols, and manuscripts."
        ),
    },
    {
        "dimension": "Sample Size",
        "definition": (
            "The numerical specification of the number of participants associated with the study, including "
            "planned or target total sample size, planned or target per-arm sample size (where reported), and "
            "actual numbers enrolled, randomized, allocated, or analyzed, overall and by arm (where reported). "
            "Expressions may include phrases such as \"target sample size,\" \"planned enrollment,\" "
            "\"anticipated enrollment,\" \"a total of N participants,\" \"we aimed to recruit,\" or \"N participants "
            "were randomized/analyzed,\" and typically appear in \"Sample size,\" \"Methods,\" \"Participants,\" "
            "\"Study design,\" \"Statistical methods,\" enrollment fields, or CONSORT-style flow diagrams."
        ),
    },
    {
        "dimension": "Date recruitment started",
        "definition": (
            "The planned or actual date on which enrollment of the first participant began. This may be "
            "expressed as an exact date (e.g., \"Recruitment started on 01 June 2021\") or a less specific time "
            "reference (e.g., \"Recruitment commenced in June 2021\") and is commonly found in fields or text "
            "labelled \"Study start date,\" \"Date of first enrollment,\" \"Recruitment,\" or within \"Methods\" and "
            "\"Study design\" sections in trial registries, protocols, and manuscripts."
        ),
    },
    {
        "dimension": "Primary Outcome(s)",
        "definition": (
            "All outcome measures explicitly designated as \"primary,\" \"main,\" or \"primary endpoint(s)\" that "
            "define the principal endpoints of interest for evaluating the intervention. A well-specified "
            "primary outcome typically includes: (a) the outcome domain or construct (e.g., overall survival, "
            "hospitalization, pain, HbA1c, depression severity), (b) the measurement instrument or operational "
            "definition (e.g., named clinical scale, laboratory test, imaging modality, diagnostic or response "
            "criteria, event definition algorithm), (c) the metric or scale (e.g., absolute value, change from "
            "baseline, proportion meeting a threshold, time-to-event, rate, composite index, including units "
            "where applicable), and (d) the assessment timepoint(s) (e.g., at 12 weeks, day 28, at discharge, "
            "6 months post-randomisation). Such information is typically reported in \"Primary outcome(s)\" or "
            "\"Endpoints\" fields, \"Outcomes\" or \"Endpoints\" sections, abstracts, and statistical analysis "
            "descriptions. Some primary outcomes may be composites of other primary or secondary outcomes."
        ),
    },
    {
        "dimension": "Secondary Outcome(s)",
        "definition": (
            "All outcome measures explicitly labelled as \"secondary,\" \"key secondary,\" or similar that are "
            "intended to provide supportive, exploratory, mechanistic, or safety-related information beyond "
            "the primary endpoints. A well-specified secondary outcome similarly includes: (a) the outcome "
            "domain or construct, (b) the measurement instrument or operational definition, (c) the metric or "
            "scale, and (d) the assessment timepoint(s). These outcomes may include safety endpoints, adverse "
            "events, laboratory parameters, quality of life, functional status, biomarker changes, intermediate "
            "clinical outcomes, or subgroup-specific effects, and are typically documented in \"Secondary "
            "outcome(s)\" fields and \"Secondary outcomes\" or \"Endpoints\" sections. Some secondary outcomes "
            "may be composites of other primary or secondary outcomes."
        ),
    },
    {
        "dimension": "Method of randomisation and allocation",
        "definition": (
            "The described method used to generate and implement the allocation of participants or clusters to "
            "study arms. This includes the sequence generation approach (e.g., computer-generated random "
            "numbers, random-number tables, permuted block randomisation with or without specified block "
            "sizes, stratified randomisation by site or baseline characteristics, minimization or dynamic "
            "allocation procedures), the allocation ratio between arms (e.g., 1:1, 2:1, 3:1:1), and, where "
            "described in the same context, the allocation concealment mechanism (e.g., central randomisation, "
            "secure web- or telephone-based systems such as IWRS/IVRS, pharmacy-controlled allocation, sealed "
            "opaque sequentially numbered envelopes). Common textual indicators include phrases such as "
            "\"participants were randomiz(s)ed using...,\" \"random sequence generated by...,\" \"block size...,\" "
            "\"stratified by...,\" and \"allocation was concealed using...,\" and this information typically "
            "appears in \"Randomiz(s)ation,\" \"Study design,\" or \"Methods\" sections and corresponding fields "
            "in trial documentation."
        ),
    },
]


DEFAULT_DIMENSION_SETS: dict[DimensionSet, list[Dimension]] = {
    "clinical_trials": CLINICAL_TRIALS_DEFAULT_DIMENSIONS,
    "general_preregistration": GENERAL_PREREGISTRATION_DEFAULT_DIMENSIONS,
}


def default_dimensions_for(dimension_set: DimensionSet) -> list[Dimension]:
    return [item.copy() for item in DEFAULT_DIMENSION_SETS[dimension_set]]


def default_dimension_sets() -> dict[str, list[Dimension]]:
    return {
        name: default_dimensions_for(name)
        for name in DEFAULT_DIMENSION_SETS
    }
