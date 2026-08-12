# GENIE Data Model (GDM)

![GENIE Data Model — Core Components](DataModelOverviewImage-1.jpg)

**The GENIE Data Model (GDM)** is an open, versioned clinical data standard for multi-institutional cancer research. It defines precisely how cancer patient information should be structured, coded, and labeled — so that data collected at Memorial Sloan Kettering, Dana-Farber Cancer Institute, and a community oncology network can be analyzed together without ever centralizing raw patient records.

The GDM is developed and maintained by [AACR Project GENIE](https://www.aacr.org/professionals/research/aacr-project-genie/) and is publicly released on an annual cadence. Each release is fully versioned, enabling reproducible research across years and cohorts.

---

## Why a Common Data Model?

Real-world cancer data is fragmented. Every institution records clinical information in its own way — different field names, different coding systems, different conventions. One site records patient sex as `M/F`; another uses `Male/Female`; a third uses NAACCR numeric codes. Tumor staging might follow AJCC 7th edition at one center and the 8th at a neighboring one. Treatment names appear as brand names, generic names, abbreviations, and free text — sometimes all in the same dataset.

This heterogeneity makes multi-site research slow and error-prone. The GDM solves this by establishing a **shared vocabulary**: exact field names, controlled coding systems (ICD-O-3, AJCC TNM, OncoTree, NCI Thesaurus), derivation rules, and privacy-preserving date conventions that every contributing site maps their data to. The result is a federated dataset spanning tens of thousands of patients across leading cancer centers — available for research at scale without moving or centralizing raw patient data.

---

## What Is AACR Project GENIE?

[AACR Project GENIE](https://www.aacr.org/professionals/research/aacr-project-genie/) (Genomics Evidence Neoplasia Information Exchange) is one of the world's largest publicly accessible real-world cancer registries. It aggregates genomic sequencing data and clinical records from patients treated at 19+ consortium institutions across North America, Europe, and Asia. Data is released publicly on [Synapse](https://www.synapse.org/) — enabling academic, clinical, and industry researchers to conduct analyses that no single institution could support on its own.

The GDM is the clinical data layer of GENIE: the standard that governs what structured clinical information gets collected alongside each patient's genomic data, how it is coded, and how it links across tables.

---

## Clinical Domains

The GDM organizes patient information across four clinical domains, each versioned and released together:

| Domain | What it captures | Why it matters |
|--------|-----------------|----------------|
| **Person** | Demographics, social determinants of health, comorbidities | Enables population-level equity analyses and covariate adjustment |
| **Diagnosis** | Cancer type, anatomic site, histology, TNM staging | Anchors every other table to a precisely characterized index cancer |
| **Treatment** | Longitudinal therapy history, lines of therapy, clinical trials | Enables real-world treatment pattern and response analyses |
| **Outcomes** | Overall survival, progression-free survival, response metrics | Supports time-to-event and comparative effectiveness studies |

---

## What's in This Repository

This repository is the authoritative source for GDM schemas, curation directives, and logical models.

### [GDM v1.0.1](GDM_v1.0.1/) — Breast Cancer Cohort

The first versioned public release, covering the GENIE BPC Breast Cancer (BrCa) cohort.

| File | Who it's for | Description |
|------|-------------|-------------|
| [`GDM_v1.0.1_DataDictionary.csv`](GDM_v1.0.1/GDM_v1.0.1_DataDictionary.csv) | Data teams | Every variable: name, type, allowed values, and required status |
| [`GDM_v1.0.1_CurationDirectives.pdf`](GDM_v1.0.1/GDM_v1.0.1_CurationDirectives.pdf) | Abstractors & curators | 259-page REDCap guide with source-field mappings, coding rules, and derivation logic for each variable |
| [`SPECIFICATION.md`](GDM_v1.0.1/SPECIFICATION.md) | All audiences | Human-readable narrative specification — tables, field descriptions, controlled vocabularies, and the reasoning behind key design decisions |
| [`GDM_v1.0.1_XML.xml`](GDM_v1.0.1/GDM_v1.0.1_XML.xml) | Systems integrators | Machine-readable XML logical model for tooling and validation |

#### Data Tables

The GDM v1.0.1 schema consists of two layers that together form a complete longitudinal picture of a patient's cancer journey.

**Core GENIE tables** are generated automatically from genomic submissions to the GENIE registry and distributed via [Sage Bionetworks](https://www.synapse.org/). They require no additional curation:

| Table | What it contains |
|-------|----------------|
| `clinical_patient` | One row per patient: demographics, vital status, years of contact |
| `clinical_sample` | One row per sequenced tumor: cancer type ([OncoTree](http://oncotree.mskcc.org) code), sample origin, sequencing panel |
| `mutations` | Somatic variant calls in standard [MAF format](https://docs.gdc.cancer.gov/Data/File_Formats/MAF_Format/): gene, chromosome, position, alleles, functional effect |
| `rna_seq` | Bulk RNA expression in TPM for 200+ cancer-relevant genes |
| `wsi_manifest` | Whole-slide pathology image registry: scanner, stain, magnification, file format |

**Extended GDM tables** are produced through structured EHR abstraction by trained clinical data curators using the REDCap-based curation instrument. They capture the longitudinal clinical context surrounding each genomic event:

| Table | What it contains |
|-------|----------------|
| `gdm_cancer_patient_information` | Patient-level root record: birth year, sex, race/ethnicity, vital status, hospice |
| `gdm_cancer_diagnosis` | Index cancer record: ICD-O-3 site and histology, AJCC TNM staging (clinical and pathologic), metastatic sites at diagnosis, and breast-specific biomarkers (ER/PR/HER2, Oncotype DX) |
| `gdm_clinical_visit` | Longitudinal oncology encounters: visit date, provider type, ECOG performance status, overall disease status |
| `gdm_drug_exposure` | Every line of systemic therapy: NCI Thesaurus-standardized drug names, start/end dates, route, dose, and reasons for discontinuation |
| `gdm_imaging` | Radiologic assessments: modality, body region, evidence of disease, and structured response categorization |
| `gdm_surgical_procedures` | Pathology reports: procedure type, surgical margins, neoadjuvant treatment status, and specimen-level biomarkers (Ki-67, ER/PR/HER2, axillary node counts) |
| `gdm_biomarkers` | Biomarker testing: PD-L1 assay results (TPS/CPS/IC scoring) and longitudinal serum tumor markers (CA15-3, CA27-29) |
| `gdm_tumor_sample_information` | NGS panel test records: sample class (tumor vs. liquid biopsy), collection and report dates, OncoTree code, GENIE submission status |

> **A note on dates:** To protect patient privacy, all dates in the extended GDM tables are stored as the number of days elapsed since the patient's birth rather than as calendar dates. This approach preserves every temporal relationship — time from diagnosis to surgery, duration of a treatment, interval between imaging studies — while making it impossible to re-identify a patient from dates alone.

---

## Logical Model Diagrams

The [`diagram/`](diagram/) directory contains SVG entity diagrams for the full GDM logical model, covering entities such as `patient`, `visit`, `cancer_condition`, `medication`, `therapeutic_procedure`, `imaging`, `pathology_report`, `genomic`, `laboratory_result`, `observation`, `death`, `cohort`, and more.

---

## License

The GDM is released under a [CC BY-NC-SA](license) license — free to use for non-commercial research with attribution. Patient data distributed through AACR Project GENIE is governed separately by the [GENIE Data Use Agreement](https://www.aacr.org/professionals/research/aacr-project-genie/aacr-project-genie-data/).
