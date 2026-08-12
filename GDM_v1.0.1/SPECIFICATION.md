# GDM v1.0.1 — Field Specification

This document is the human-readable companion to [`GDM_v1.0.1_CurationDirectives.pdf`](GDM_v1.0.1_CurationDirectives.pdf) and [`GDM_v1.0.1_DataDictionary.csv`](GDM_v1.0.1_DataDictionary.csv). It describes every table in the GDM v1.0.1 schema — what each field captures, its data type and units, whether it is required, and the full set of allowed values with their meaning.

**Source authority:** GDM_v1.0.1_CurationDirectives.pdf (Jennifer Hoppe, 2026-02-23) — the 259-page REDCap curation specification for AACR Project GENIE Breast Cancer (BPC BrCa).

## How to Read This Document

Each table section contains a field-by-field reference table with these columns:

| Column | Meaning |
|--------|---------|
| **Field** | The exact variable name as it appears in the data file |
| **Type** | Data type: `String` (text), `Float` (number), or `Integer` |
| **Units** | Unit of measurement where applicable (e.g. `Days`, `Years`, `Months`) |
| **Required** | Whether the field must be populated for a record to be valid |
| **Description / Controlled Vocabulary** | Plain-language description plus the complete list of allowed coded values |

**Coded values** appear throughout — for example, sex is stored as `1=Male, 2=Female` rather than as text. These codes follow established standards (NAACCR, AJCC, ICD-O-3, OncoTree) so that GENIE data is interoperable with other cancer registries and international datasets.

**All dates** in the curated tables are stored as the number of days elapsed since the patient's date of birth (indicated by a `Days` unit). For example, a patient born in 1965 whose cancer was diagnosed 18,250 days later was diagnosed at age 50. This convention preserves every clinically meaningful time interval — time from diagnosis to surgery, duration on treatment, imaging follow-up interval — while ensuring that no calendar date capable of re-identifying a patient is ever stored in the data.

---

## Two-Layer Architecture

The GDM v1.0.1 schema consists of two distinct data layers:

| Layer | Tables | How it is produced |
|-------|--------|--------------------|
| **Core GENIE (Tier1A)** | `clinical_patient`, `clinical_sample`, `mutations`, `rna_seq`, `wsi_manifest` | Generated automatically from genomic submissions; standardized and distributed by Sage Bionetworks via Synapse |
| **Extended GDM (Curated)** | `gdm_cancer_patient_information`, `gdm_cancer_diagnosis`, `gdm_clinical_visit`, `gdm_drug_exposure`, `gdm_imaging`, `gdm_surgical_procedures`, `gdm_biomarkers`, `gdm_tumor_sample_information` | Produced by trained clinical data curators abstracting from EHR records and tumor registry data using the REDCap curation instrument |

Every table is linked by a shared patient identifier: `PATIENT_ID` in the core tables and `patient_id_curated` in the extended tables. The format is `<CENTER>-P#####` (e.g. `DFCI-000183`, `MSK-P00042`).

---

## Core GENIE Tables (Tier1A)

These tables originate from the GENIE repository submission pipeline. Sites provide raw data; GENIE applies standardization before publication on Synapse.

---

### `clinical_patient`

One row per patient. Primary key for the entire data model.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `PATIENT_ID` | String | — | Yes | Unique patient identifier. Format: `<CENTER>-P#####` (e.g. `MSK-P00001`). Primary key; foreign key in all other tables. |
| `SEX` | String | — | Yes | Biological sex. Values: `Male`, `Female`, `Unknown`. |
| `PRIMARY_RACE` | String | — | Yes | Self-reported race. Values: `White`, `Black or African American`, `Asian`, `Other`, `Unknown`. |
| `ETHNICITY` | String | — | Yes | Self-reported ethnicity. Values: `Non-Spanish/non-Hispanic`, `Spanish/Hispanic`, `Unknown`. |
| `BIRTH_YEAR` | Float | — | Yes | 4-digit calendar year of birth (e.g. `1958`). |
| `CENTER` | String | — | Yes | GENIE center abbreviation (e.g. `MSK`, `DFCI`, `VICC`). |
| `YEAR_CONTACT` | Float | — | Yes | 4-digit calendar year of patient's first contact with the institution. |
| `INT_CONTACT` | Float | Months | Yes | Interval in whole months from birth to first contact. |
| `DEAD` | Float | — | Yes | Vital status flag: `1` = deceased, `0` = alive. |
| `YEAR_DEATH` | String | — | No | 4-digit calendar year of death. Populate only if `DEAD=1`; use string `Unknown` for living patients. |
| `INT_DOD` | String | Months | No | Interval in whole months from birth to date of death. Populate only if `DEAD=1`; use string `Unknown` for living patients. |

---

### `clinical_sample`

One row per sequenced tumor sample. Multiple samples per patient are allowed.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `SAMPLE_ID` | String | — | Yes | Unique sample identifier. Format: `<PATIENT_ID>-S##` (e.g. `MSK-P00001-S01`). Primary key. |
| `PATIENT_ID` | String | — | Yes | Foreign key to `clinical_patient.PATIENT_ID`. |
| `AGE_AT_SEQ_REPORT` | Float | Years | Yes | Patient age in whole years at sequencing report date. |
| `ONCOTREE_CODE` | String | — | Yes | OncoTree cancer type code (e.g. `BRCA`, `LUAD`, `PAAD`). See oncotree.mskcc.org for full vocabulary. |
| `CANCER_TYPE` | String | — | Yes | Broad cancer type label from OncoTree hierarchy (e.g. `Breast Invasive Carcinoma`). |
| `CANCER_TYPE_DETAILED` | String | — | Yes | Detailed cancer type label from OncoTree, including sample-type qualifier. |
| `SAMPLE_TYPE` | String | — | Yes | Tissue sample type. Values: `Primary`, `Metastasis`, `Recurrence`, `Unspecified`. |
| `SEQ_ASSAY_ID` | String | — | Yes | Sequencing panel identifier as registered in GENIE (e.g. `MSK-IMPACT468`, `DFCI-ONCOPANEL-3`). |
| `YEAR_CONTACT` | Float | — | Yes | 4-digit calendar year of first contact with institution. |
| `INT_CONTACT` | Float | Months | Yes | Interval in whole months from birth to first contact. |
| `CENTER` | String | — | Yes | GENIE center abbreviation. |
| `STAGE` | String | — | No | Pathologic stage at time of sequencing. Values: `I`, `II`, `III`, `IV`. |

---

### `mutations`

One row per somatic variant call. Follows MAF (Mutation Annotation Format) conventions.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `SAMPLE_ID` | String | Yes | Foreign key to `clinical_sample.SAMPLE_ID`. |
| `PATIENT_ID` | String | Yes | Foreign key to `clinical_patient.PATIENT_ID`. |
| `Hugo_Symbol` | String | Yes | HUGO gene symbol (e.g. `TP53`, `PIK3CA`, `KRAS`). |
| `Chromosome` | String | Yes | Chromosome identifier without `chr` prefix (e.g. `1`, `X`, `MT`). |
| `Start_Position` | Integer | Yes | Genomic start position (1-based). |
| `Reference_Allele` | String | Yes | Reference allele sequence. |
| `Tumor_Seq_Allele2` | String | Yes | Alternate (tumor) allele sequence. |
| `Variant_Classification` | String | Yes | MAF-standard mutation effect (e.g. `Missense_Mutation`, `Nonsense_Mutation`, `Frame_Shift_Del`, `Splice_Site`). |
| `t_alt_count` | Integer | Yes | Tumor alternate allele read count. |
| `t_depth` | Integer | Yes | Tumor total read depth at locus. |

---

### `rna_seq`

One row per sequenced sample with RNA-seq data. Contains transcript-per-million (TPM) expression values for 200+ genes.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `SAMPLE_ID` | String | Yes | Foreign key to `clinical_sample.SAMPLE_ID`. |
| `PATIENT_ID` | String | Yes | Foreign key to `clinical_patient.PATIENT_ID`. |
| `ONCOTREE_CODE` | String | Yes | OncoTree code for the tumor sample. |
| `<GENE>` (×200+) | Float | Yes | TPM expression value for each gene (e.g. `ACTB`, `GAPDH`, `TP53`, `KRAS`, `PIK3CA`, ...). |

**Note:** Sites providing FPKM (site_b) or raw counts (site_c) must convert to TPM before submission. See harmonization mapping.

---

### `wsi_manifest`

One row per whole-slide image file.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `SAMPLE_ID` | String | Yes | Foreign key to `clinical_sample.SAMPLE_ID`. |
| `PATIENT_ID` | String | Yes | Foreign key to `clinical_patient.PATIENT_ID`. |
| `SLIDE_ID` | String | Yes | Unique identifier for the slide. |
| `STAIN` | String | Yes | Staining method (e.g. `H&E`, `IHC`). |
| `MAGNIFICATION` | String | Yes | Objective magnification at scan (e.g. `20x`, `40x`). |
| `SCANNER` | String | Yes | Scanner make and model. |
| `FILE_FORMAT` | String | Yes | Image file format (e.g. `SVS`, `NDPI`, `TIFF`). |
| `FILE_SIZE_MB` | Float | Yes | File size in megabytes. |
| `TISSUE_TYPE` | String | Yes | Tissue type (e.g. `Tumor`, `Normal`, `Margin`). |

---

## Extended GDM Tables (Curated)

These tables are populated through structured EHR abstraction using the GDM REDCap curation instrument. All date fields are days-from-birth integers. The primary key `patient_id_curated` matches the GENIE `PATIENT_ID` format (`<CENTER>-P#####`).

---

### `gdm_cancer_patient_information`

One row per patient. Root table for all extended GDM data.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `patient_id_curated` | String | — | Yes | **PK.** Curated GENIE patient identifier (`<INSTITUTION>-<NUMERIC_ID>`, e.g. `GENIE-DFCI-000183`). Derived from MRN or study ID. |
| `birth_year_curated` | Float | — | Yes | 4-digit birth year (e.g. `1968`). Used for age and interval calculations. |
| `sex_curated` | String | — | Yes | Biological sex. NAACCR numeric codes: `1`=Male, `2`=Female, `3`=Other (intersex/DSD), `4`=Transsexual NOS, `5`=Transsexual natal male, `6`=Transsexual natal female, `9`=Not stated/Unknown. |
| `primary_race_curated` | String | — | Yes | Self-reported primary race. NAACCR numeric codes: `1`=White, `2`=Black/African American, `3`=American Indian/Aleutian/Eskimo, `4`=Chinese, `5`=Japanese, `6`=Filipino, `7`=Hawaiian, `8`=Korean, `20`=Asian Indian or Pakistani, `96`=Other Asian, `97`=Pacific Islander NOS, `98`=Other, `99`=Unknown. |
| `ethnicity_curated` | String | — | Yes | Spanish/Hispanic ethnicity. NAACCR codes: `0`=Non-Spanish/non-Hispanic, `1`=Mexican, `2`=Puerto Rican, `3`=Cuban, `4`=South or Central American, `5`=Other specified Spanish/Hispanic, `6`=Spanish NOS/Latino NOS, `7`=Spanish surname only, `9`=Unknown. |
| `dead_curated` | Float | — | Yes | Vital status: `1`=deceased, `0`=alive or unknown. Derived: set to `1` if `hybrid_death_int` is non-null. |
| `death_year_curated` | String | — | No | 4-digit calendar year of death. Populate only when `dead_curated=1`; use `Unknown` for living patients. |
| `death_date_int` | Float | Days | No | Date of death as days elapsed from birth. Populate only when `dead_curated=1`. Source: `hybrid_death_int`. |
| `death_source_curated` | String | — | No | Death confirmation source: `1`=Curated (clinical note), `2`=EHR structured field, `3`=Tumor Registry, `4`=NDI (National Death Index), `5`=Commercial Database, `6`=Other. |
| `lastalive_date_int` | Float | Days | Yes | Date of last known alive as days from birth. Used as censoring date for survivors. Source: `last_alive_int`. |
| `follow_up_date_clin_int` | Float | Days | Yes | Date of most recent clinical follow-up contact as days from birth. Source: `last_anyvisit_int` or `last_oncvisit_int`. |
| `hospice_yn` | String | — | No | Hospice documentation: `1`=Yes Hospice Admission, `2`=Yes Hospice Referral, `3`=No. |
| `hospice_admit_int` | Float | Days | No | Hospice admission date as days from birth. Populate only when `hospice_yn=1`. |

---

### `gdm_cancer_diagnosis`

One row per cancer diagnosis per patient (index and non-index cancers). FK: `patient_id_curated`.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `patient_id_curated` | String | — | Yes | **FK.** Foreign key to `gdm_cancer_patient_information`. |
| `dx_date_int` | Float | Days | Yes | Diagnosis date as days from birth. Source: `dob_ca_dx_days`. |
| `dx_age_yr` | Float | Years | Yes | Patient age in whole years at diagnosis. |
| `dx_institution` | String | — | No | Diagnosis facility: `1`=Internal Institution, `2`=External Institution. |
| `dx_confirmation` | String | — | No | Confirmation method: `1`=Positive histology, `2`=Positive cytology, `3`=Histology + immunophenotyping/genetics, `4`=Positive microscopic confirmation unspecified, `5`=Positive lab/marker, `6`=Direct visualization, `7`=Radiology/imaging, `8`=Clinical diagnosis only. |
| `dx_primary_site` | String | — | Yes | Primary anatomic site (ICD-O-3 C-code, e.g. `C50.8`=overlapping lesion of breast, `C50.9`=breast NOS). |
| `dx_morphology` | String | — | Yes | Histology and behavior (ICD-O-3 M-code, 5 digits, e.g. `85003`=Invasive carcinoma NST, `85223`=Infiltrating duct+lobular, `80723`=Squamous intraepithelial neoplasia III). |
| `dx_laterality` | String | — | No | Laterality (NAACCR codes): `0`=Not a paired site, `1`=Right, `2`=Left, `3`=Only one side unspecified, `4`=Bilateral, `9`=Paired site side not specified. |
| `dx_grade` | String | — | No | Histologic grade: `I`=Well differentiated, `II`=Moderately differentiated, `III`=Poorly differentiated, `IV`=Undifferentiated. |
| `dx_clin_t` | String | — | No | Clinical T category (AJCC): `T0`, `T1`, `T1a`, `T1b`, `T1c`, `T2`, `T3`, `T4`, `Tis`, `TX`. Source: NAACCR C-prefix codes stripped (e.g. `C1C`→`T1c`). |
| `dx_clin_n` | String | — | No | Clinical N category (AJCC): `N0`, `N1`, `N2`, `N3`, `NX`. Source: NAACCR C-prefix codes stripped. |
| `dx_clin_m` | String | — | No | Clinical M category (AJCC): `M0`, `M1`, `MX`. |
| `dx_clin_group_stage` | String | — | No | AJCC clinical stage group: `0`, `I`, `IA`, `IB`, `II`, `IIA`, `IIB`, `III`, `IIIA`, `IIIB`, `IIIC`, `IV`, `Unknown`. NAACCR abbreviated notation mapped to Roman numerals. |
| `dx_path_t` | String | — | No | Pathologic T category (AJCC): `T0`, `T1`, `T1mi`, `T1a`, `T1b`, `T1c`, `T2`, `T3`, `T4`, `T4b`, `T4d`, `Tis`, `TX`. Source: NAACCR P-prefix codes stripped. |
| `dx_path_n` | String | — | No | Pathologic N category (AJCC): `N0`, `N1`, `N1a`, `N1b`, `N2`, `N2a`, `N3`, `N3a`, `NX`. |
| `dx_path_m` | String | — | No | Pathologic M category (AJCC): `M0`, `M1`, `MX`. |
| `dx_path_group_stage` | String | — | No | AJCC pathologic stage group (same vocabulary as `dx_clin_group_stage`). |
| `ajcc_edition` | String | — | No | AJCC TNM edition: `5th`, `6th`, `7th`, `8th`. Extracted from NAACCR field with mixed formats (bare integers, verbose descriptions). |
| `dx_mets_bone` | Float | — | No | Bone metastasis at diagnosis: `0`=None, `1`=Yes, `88`=Not applicable, `99`=Unknown. |
| `dx_mets_brain` | Float | — | No | Brain/CNS metastasis at diagnosis: `0`=None, `1`=Yes, `88`=Not applicable, `99`=Unknown. |
| `dx_mets_liver` | Float | — | No | Liver metastasis at diagnosis: `0`=None, `1`=Yes, `88`=Not applicable, `99`=Unknown. |
| `dx_mets_lung` | Float | — | No | Lung/pulmonary metastasis at diagnosis: `0`=None, `1`=Yes, `88`=Not applicable, `99`=Unknown. |
| `dx_mets_lymph` | Float | — | No | Distant lymph node metastasis (excluding regional nodes): `0`=None, `1`=Yes, `88`=Not applicable, `99`=Unknown. |
| `dx_mets_other` | Float | — | No | Other distant metastasis (abdomen, adrenal, skin, pleura, peritoneum, etc.): `0`=None, `1`=Yes, `88`=Not applicable, `99`=Unknown. |
| `dx_ca_type` | String | — | No | Primary cancer type: `1`=Lung, `2`=Prostate, `3`=Stomach, `4`=Colorectal, `5`=Liver, `6`=Melanoma, `7`=Bladder, `8`=Kidney, `9`=Breast, `10`=Ovarian, `11`=Pancreas, `12`=CNS. |
| **Breast-specific fields** | | | | |
| `breast_ot_inv_dcis` | String | — | No | Invasive vs. DCIS: `1`=Invasive, `2`=DCIS. |
| `breast_ot_inv_score` | String | — | No | Oncotype DX recurrence score category: `1`=Actual score 0–100, `2`=XX4 (<11), `3`=XX5 (≥11), `4`=XX6 (not applicable – in situ), `5`=XX7 (ordered, not in chart), `6`=XX9 (not documented). |
| `breast_ot_inv_score_details` | Float | — | No | Exact Oncotype DX recurrence score (0–100). Populate only when `breast_ot_inv_score=1`. |
| `breast_ot_inv_risk` | String | — | No | Oncotype DX risk level: `0`=Low (0–17), `1`=Intermediate (18–30), `2`=High (≥31), `6`=Not applicable DCIS, `7`=Ordered not in chart, `8`=Not applicable not collected, `9`=Not documented. |
| `breast_multigene_method` | String | — | No | Multigene assay: `1`=Mammaprint, `2`=PAM50/Prosigna, `3`=Breast Cancer Index, `4`=EndoPredict, `5`=Test type unknown, `6`=Multiple tests, `7`=Ordered not in chart, `8`=Not applicable, `9`=Not documented. |
| `breast_multigene_results` | String | — | No | Multigene result: `1`=Numeric score 00–99, `2`=X1 (100), `3`=X2 Low, `4`=X3 Moderate, `5`=X4 High, `6`=X7 ordered not in chart, `7`=X8 not applicable, `8`=X9 not documented. |
| `breast_er_summary` | String | — | No | ER receptor status: `0`=Negative (<1%), `1`=Positive (≥1%), `7`=Ordered not in chart, `9`=Not documented/not assessed. |
| `breast_pr_summary` | String | — | No | PR receptor status: `0`=Negative, `1`=Positive, `7`=Ordered not in chart, `9`=Not documented/not assessed. |
| `breast_her2_summary` | String | — | No | HER2 overall status (IHC+ISH combined): `0`=Negative/equivocal, `1`=Positive, `2`=Borderline, `7`=Ordered not in chart, `9`=Not documented/not assessed. |

---

### `gdm_clinical_visit`

One row per oncology clinic visit. FK: `patient_id_curated`.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `patient_id_curated` | String | — | Yes | **FK.** |
| `visit_date_int` | Float | Days | Yes | Visit date as days from birth. Source: `md_onc_visit_int`. |
| `visit_institution` | String | — | Yes | `1`=Internal Institution, `2`=External Institution. |
| `visit_provider` | String | — | No | Provider type: `1`=Surgical Oncologist, `2`=Medical Oncologist, `3`=Radiation Oncologist, `4`=Other Oncologist. |
| `visit_type` | String | — | No | Note type: `1`=Consultation Note (LOINC 11488-4), `3`=History and Physical (34117-2), `5`=Progress Note (11506-3), `9`=Other. |
| `visit_ca_yn` | Float | — | Yes | Cancer discussed in visit: `1`=Yes, `0`=No. |
| `visit_ca_status_overall` | String | — | Yes | Medical oncologist's cancer status assessment: `1`=Progressing/Worsening, `2`=Responding/Improving, `3`=Mixed, `4`=Stable, `5`=No Evidence of Disease, `9`=Not stated/Indeterminate. |
| `visit_ecog` | String | — | No | ECOG Performance Status: `0`=Fully active, `1`=Restricted in strenuous activity, `2`=Self-care capable unable to work >50% of day, `3`=Limited self-care confined >50% of day, `4`=Completely disabled, `9`=Not documented. |

---

### `gdm_drug_exposure`

One row per drug administration (wide-to-long pivot from regimen-level source data; up to 5 drugs per regimen). FK: `patient_id_curated`.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `patient_id_curated` | String | — | Yes | **FK.** |
| `trt_order_institution` | String | — | No | Order institution: `1`=Internal, `2`=External. |
| `trt_admin_institution` | String | — | No | Administration institution: `1`=Internal, `2`=External. |
| `trt_trial_yn` | Float | — | No | Clinical trial: `1`=Yes, `0`=No. |
| `trt_ongoing_yn` | Float | — | No | Still ongoing: `1`=Yes, `0`=No. |
| `trt_start_date_int` | Float | Days | Yes | Drug start date as days from birth. Source: `drugs_startdt_int_1..5`. |
| `trt_end_date_int` | Float | Days | No | Drug end/discontinuation date as days from birth. Source: `dx_dru_enddt_int_1..5`. |
| `trt_drug` | String | — | Yes | Standardized NCI Thesaurus drug name (parenthetical synonyms stripped). Examples: `Doxorubicin`, `Cyclophosphamide`, `Paclitaxel`, `Tamoxifen`, `Trastuzumab`, `Letrozole`, `Anastrozole`, `Capecitabine`, `Palbociclib`. |
| `trt_drug_route` | String | — | No | Route of administration: `1`=Intravenous, `2`=Oral, `3`=Intramuscular, `4`=Subcutaneous, `5`=Topical, `6`=Intrathecal, `7`=Intrapleural, `8`=Intra-arterial. |
| `trt_dose` | Float | — | No | Dose amount (numeric, units in `trt_dose_unit`). Not captured in most source data; leave null if unavailable. |
| `trt_dose_unit` | String | — | No | Dose unit: `1`=mg, `2`=mL, `3`=g, `4`=Units, `5`=micrograms, `6`=mEq, `7`=kg, `8`=mg/m², `9`=mg/kg. |
| `trt_freq` | String | — | No | Frequency: `1`=Daily, `2`=Weekly, `3`=Every 2 weeks, `4`=Every 3 weeks, `5`=Every 4 weeks/Monthly, `6`=Every 6 weeks, `7`=Every 8 weeks, `8`=Every 12 weeks, `9`=Other. |
| `trt_modification` | String | — | No | Regimen modification: `1`=No modification, `2`=Dose reduction, `3`=Schedule change, `4`=Drug substitution, `5`=Other modification. |
| `trt_discontinuation` | String | — | No | Discontinuation status: `1`=No discontinuation, `2`=Temporary, `3`=Permanent. Source: `drugs_dc_ynu` (Yes→3, No→1, Unknown→null). |
| `trt_discontinuation_reason` | String | — | No | Reason for discontinuation (populate when `trt_discontinuation`=2 or 3): `1`=Disease progression, `2`=Adverse events, `3`=Patient withdrawal, `4`=Death, `5`=Loss to follow-up, `6`=Protocol violation, `7`=Treatment completion as planned, `8`=Comorbidity. |

---

### `gdm_imaging`

One row per radiologic imaging study. FK: `patient_id_curated`.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `patient_id_curated` | String | — | Yes | **FK.** |
| `imaging_date_int` | Float | Days | Yes | Imaging procedure date as days from birth. Source: `image_scan_int`. |
| `imaging_report_date_int` | Float | Days | No | Radiology report finalization date as days from birth. |
| `imaging_institution` | String | — | Yes | `1`=Internal Institution, `2`=External Institution. |
| `imaging_modality` | String | — | Yes | Imaging type: `1`=PET, `2`=CT, `3`=PET/CT, `4`=MRI, `5`=PET/MRI, `6`=FDG-PET, `7`=Bone scan, `8`=Mammogram. |
| `imaging_body_location` | String | — | No | Body region(s) imaged (multi-select): `1`=Brain, `2`=Head and Neck, `3`=Spine, `4`=Chest/Thorax, `5`=Abdomen, `6`=Pelvis, `7`=Extremities upper/arms, `8`=Extremities lower/legs, `9`=Whole body, `10`=Other. Source: checkbox columns `image_scansite___1..8`. |
| `imaging_eod` | String | — | No | Radiologic evidence of disease (EOD): `1`=Evidence of disease present, `2`=No evidence of disease, `3`=Not mentioned/indeterminate. |
| `imaging_ca_yn` | Float | — | Yes | Primary cancer site described in report: `1`=Yes, `0`=No. Source: `image_ca`. |
| `imaging_ca_status_overall` | String | — | Yes | Overall cancer status from radiology report: `1`=Stable Disease, `2`=Progression (local only), `3`=Progression (distant only), `4`=Overall Progression (local+distant), `5`=Response (local), `6`=Response (distant), `7`=Overall Response (local+distant), `8`=No Evidence of Disease, `9`=Indeterminate/Not stated. |
| `imaging_mets_yn` | Float | — | No | Metastatic disease described: `1`=Yes, `0`=No. |

---

### `gdm_surgical_procedures`

One row per surgical pathology report. FK: `patient_id_curated`.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `patient_id_curated` | String | — | Yes | **FK.** |
| `proc_date_int` | Float | Days | Yes | Surgical procedure date as days from birth. Source: `path_proc_int`. |
| `path_report_date_int` | Float | Days | No | Final pathology report sign-out date as days from birth. |
| `proc_institution` | String | — | Yes | `1`=Internal Institution, `2`=External Institution. |
| `proc_neoadjuvant_yn` | Float | — | No | Neoadjuvant therapy prior to this procedure: `1`=Yes, `0`=No. Derived by cross-referencing drug exposure start dates. |
| `path_type` | String | — | Yes | Pathology discipline: `1`=Surgical pathology, `2`=Cytopathology, `3`=Hematopathology, `9`=Other. |
| `proc_type_breast` | String | — | No | Breast procedure type (multi-select): `1`=Lumpectomy/Partial mastectomy, `2`=Simple/Total mastectomy, `3`=Modified radical mastectomy, `4`=Radical mastectomy, `5`=Sentinel lymph node biopsy, `6`=Axillary lymph node dissection, `7`=Core needle biopsy, `8`=Excisional biopsy, `9`=Other. |
| `proc_margins` | String | — | No | Surgical margin status: `1`=Negative/clear, `2`=Positive (tumor at ink), `3`=Close (<2mm), `4`=Not applicable (biopsy only), `9`=Not documented. |
| `proc_specimen_num` | Float | — | No | Number of distinct surgical specimens submitted (integer 1–30). |
| **Breast biomarker fields (per specimen)** | | | | |
| `breast_er_range` | String | — | No | ER% range: `0`=Negative/< 1%, `1`=Exact percent (see `_details`), `2`=R10 (1–10%), `3`=R20 (11–20%), ... `11`=R100 (91–100%), `12`=Test done% not stated, `13`=Not applicable, `14`=Not documented. |
| `breast_er_range_details` | Float | — | No | Exact ER percent (1–100). Populate when `breast_er_range=1`. |
| `breast_er_summary` | String | — | No | ER summary: `0`=Negative (<1%), `1`=Positive (≥1%), `7`=Ordered not in chart, `9`=Not documented. |
| `breast_pr_range` | String | — | No | PR% range (same codes as `breast_er_range`). |
| `breast_pr_range_details` | Float | — | No | Exact PR percent (1–100). |
| `breast_pr_summary` | String | — | No | PR summary: `0`=Negative, `1`=Positive, `7`=Ordered not in chart, `9`=Not documented. |
| `breast_her2_summary` | String | — | No | HER2 status from surgical specimen: `0`=Negative/equivocal, `1`=Positive, `2`=Borderline, `7`=Ordered not in chart, `9`=Not documented. |
| `breast_ki67` | String | — | No | Ki-67 category: `1`=Actual value 0.0–100.0% (see `_details`), `2`=Test done% not stated, `3`=Not applicable, `4`=Not documented. |
| `breast_ki67_details` | Float | — | No | Exact Ki-67 percentage (0.0–100.0). Populate when `breast_ki67=1`. |
| `breast_ln_pos_axil` | String | — | No | Positive axillary lymph nodes: `0`=All negative, `1`=Actual count 1–99 (see `_details`), `2`=X1 (≥100), `3`=X5 (positive count unspecified), `4`=X6 (positive on aspiration/core), `5`=X8 (not applicable), `6`=X9 (not documented). |
| `breast_ln_pos_axil_details` | Float | — | No | Exact positive axillary node count (1–99). |

---

### `gdm_biomarkers`

One row per biomarker test event per patient. Covers PD-L1 testing and serum tumor markers (CA15-3, CA27-29). FK: `patient_id_curated`.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `patient_id_curated` | String | — | Yes | **FK.** |
| `ini1_testing` | Float | — | No | INI1/SMARCB1 IHC testing performed: `1`=Yes, `0`=No. |
| `ini1_result` | String | — | No | INI1 result (when `ini1_testing=1`): `1`=Intact/Retained, `2`=Loss, `3`=Equivocal, `99`=Unknown. |
| `pdl1_testing` | String | — | No | PD-L1 testing: `1`=0 tests (none done), `2`=Yes (at least one test done), `3`=No testing reported. Source: `data_clinical_sample.txt`. |
| `pdl1_assay` | String | — | No | PD-L1 antibody clone: `1`=22C3 (Dako/Merck), `2`=28-8 (Dako/BMS), `3`=SP142 (Ventana/Roche), `4`=SP263 (Ventana/AZ), `5`=E1L3N (Cell Signaling), `88`=Other, `99`=Unknown. |
| `pdl1_score_type` | String | — | No | PD-L1 score type: `1`=TPS (Tumor Proportion Score), `2`=CPS (Combined Positive Score), `3`=IC (Immune Cell Score), `88`=Other, `99`=Unknown. |
| `pdl1_value` | Float | — | No | Numeric PD-L1 result value. |
| `pdl1_interpretation` | String | — | No | PD-L1 interpretation: `1`=Positive, `2`=Negative, `3`=High, `4`=Low, `5`=Indeterminate, `99`=Not stated. |
| `tm_type` | String | — | Yes | Serum tumor marker type. Values: `CA15-3`, `CA27-29` (source `CA2729` mapped to `CA27-29`). |
| `tm_num_result` | Float | — | Yes | Numeric tumor marker result value. |
| `tm_result_units` | String | — | Yes | Result units (typically `U/mL` for both CA15-3 and CA27-29). |
| `tm_date_int` | Float | Days | Yes | Specimen collection date as days from birth. Source: `tm_spec_collect_int`. |
| `tm_normal_range_lower` | Float | — | No | Lower bound of institutional normal reference range. |
| `tm_normal_range_upper` | Float | — | No | Upper bound of institutional normal reference range (typical upper limit of normal for CA27-29: 38 U/mL). |

---

### `gdm_tumor_sample_information`

One row per tumor panel test (NGS sequencing event). FK: `patient_id_curated`.

| Field | Type | Units | Required | Description / Controlled Vocabulary |
|-------|------|-------|----------|--------------------------------------|
| `patient_id_curated` | String | — | Yes | **FK.** |
| `sample_genie_yn` | Float | — | Yes | Sample sequenced and submitted to GENIE: `1`=Yes, `0`=No. Derived from presence of `cpt_genie_sample_id`. |
| `sample_coll_date_int` | Float | Days | No | Sample collection date as days from birth. Source: `dob_cpt_report_days`. |
| `sample_seq_report_date_int` | Float | Days | Yes | NGS report date as days from birth. Source: `dob_cpt_report_days`. |
| `sample_seq_report_year` | Float | — | Yes | 4-digit year of NGS report. Source: `cpt_seq_date`. |
| `sample_class_curated` | String | — | Yes | Sample class: `1`=Tumor (solid tumor, cytology, core needle, surgical resection), `2`=cfDNA (circulating tumor DNA / liquid biopsy). Source: `Sample Type` from `data_clinical_sample.txt`. |
| `sample_type_detailed_curated` | String | — | Yes | Anatomic origin: `1`=Primary tumor, `2`=Lymph node metastasis, `3`=Distant organ metastasis, `4`=Metastasis site unspecified, `5`=Local recurrence, `99`=Unknown. Source: `sample_type` from `data_clinical_sample.txt`. |
| `sample_cancer_type_detailed_curated` | String | — | Yes | OncoTree cancer type code for the sample (e.g. `BRCA`, `MDLC`, `ILC`, `IDC`). Source: `cpt_oncotree_code` from `cancer_panel_test_level_dataset.csv`. |

---

## Harmonization Mapping

The following rules transform heterogeneous site-specific source data into the unified GDM format. Sites A, B, and C each have different schemas; only B and C require transformation.

### Site B Mappings

Source system: internal EHR database with tables `patient_data`, `specimen_data`, `somatic_variants`, `gene_expression_fpkm`.

| Source Table | Source Column | GDM Table | GDM Column | Transform |
|---|---|---|---|---|
| `patient_data` | `patient_mrn` | `clinical_patient` | `PATIENT_ID` | Prefix with `DFCI-` |
| `patient_data` | `gender` | `clinical_patient` | `SEX` | `M`→`Male`, `F`→`Female` |
| `patient_data` | `race_ethnicity` | `clinical_patient` | `PRIMARY_RACE` | `Caucasian`→`White`, `African American`→`Black or African American` |
| `patient_data` | `vital_status` | `clinical_patient` | `DEAD` | `Deceased`→`1`, `Alive`→`0` |
| `specimen_data` | `specimen_id` | `clinical_sample` | `SAMPLE_ID` | Rename |
| `specimen_data` | `cancer_diagnosis_local` | `clinical_sample` | `ONCOTREE_CODE` | Local string → OncoTree lookup |
| `specimen_data` | `specimen_type` | `clinical_sample` | `SAMPLE_TYPE` | `Primary Tumor`→`Primary`, `Metastatic`→`Metastasis` |
| `somatic_variants` | `gene_name` | `mutations` | `Hugo_Symbol` | Rename |
| `somatic_variants` | `chr` | `mutations` | `Chromosome` | Strip `chr` prefix |
| `somatic_variants` | `mutation_effect` | `mutations` | `Variant_Classification` | Rename |
| `gene_expression_fpkm` | `FPKM_<GENE>` | `rna_seq` | `<GENE>` | FPKM → TPM conversion |

### Site C Mappings

Source system: clinical data warehouse with tables `pt_clinical`, `samples`, `variants`, `rnaseq_counts`.

| Source Table | Source Column | GDM Table | GDM Column | Transform |
|---|---|---|---|---|
| `pt_clinical` | `pt_id` | `clinical_patient` | `PATIENT_ID` | Rename |
| `pt_clinical` | `sex_cd` | `clinical_patient` | `SEX` | `male`→`Male`, `female`→`Female` |
| `pt_clinical` | `race_cd` | `clinical_patient` | `PRIMARY_RACE` | HL7 codes: `2106-3`→`White`, `2054-5`→`Black or African American`, `2028-9`→`Asian` |
| `pt_clinical` | `deceased_flag` | `clinical_patient` | `DEAD` | `Y`→`1`, `N`→`0` |
| `samples` | `dx_icd_o_code` | `clinical_sample` | `ONCOTREE_CODE` | ICD-O → OncoTree: `C50.9`→`BRCA`, `C34.1`→`LUAD/LUSC`, `C25.9`→`PAAD`, `C18.9`→`CRC`, `C61`→`PRAD`, `C56.9`→`OV`, `C71.9`→`GBM` |
| `samples` | `specimen_type_cd` | `clinical_sample` | `SAMPLE_TYPE` | `P`→`Primary`, `M`→`Metastasis`, `R`→`Recurrence`, `U`→`Unspecified` |
| `variants` | `CHROM` | `mutations` | `Chromosome` | Already compatible (no `chr` prefix) |
| `rnaseq_counts` | `<GENE>_raw_count` | `rna_seq` | `<GENE>` | Raw counts → TPM normalization required |

---

## FCP Schema Format Reference

All files in `fcp_schemas/` follow the Rhino FCP schema CSV format. Each file has exactly 9 rows:

| Row | Name | Content |
|-----|------|---------|
| 1 | Variable Name | Column identifier (used as the field name in the dataset) |
| 2 | Identifier | Marks which column(s) are the primary key (`Identifier`) |
| 3 | Description | Human-readable description including controlled vocabulary values and source field mappings |
| 4 | Type | Data type per column: `String`, `Float`, `Integer` |
| 5 | Type Parameters | Additional type constraints (usually empty) |
| 6 | Units | Units for numeric fields (e.g. `Days`, `Months`, `Years`, `U/mL`) |
| 7 | Contains Sensitive Data? | `True` or `False` |
| 8 | Permissions | Access permission level (typically `Default`) |
| 9 | Required | `True` if field is mandatory, `False` if optional |

---

## Key Coding Standards

| Standard | Usage in GDM |
|----------|-------------|
| **ICD-O-3 Topography** | `dx_primary_site` (C-codes, e.g. `C50.9`) |
| **ICD-O-3 Morphology** | `dx_morphology` (5-digit M-codes, e.g. `85003`) |
| **NAACCR** | Sex, race, ethnicity, laterality, and T/N/M stage codes |
| **AJCC TNM** | `dx_clin_t/n/m`, `dx_path_t/n/m`, `dx_clin/path_group_stage`, `ajcc_edition` |
| **OncoTree** | `ONCOTREE_CODE`, `sample_cancer_type_detailed_curated` |
| **NCI Thesaurus** | `trt_drug` (canonical drug names) |
| **CTCAE** | Adverse event toxicity grading (referenced in drug discontinuation context) |
| **LOINC** | Note types in `visit_type` (e.g. 11506-3 = Progress Note) |
| **Days-from-birth** | All date fields in extended GDM tables (`_int` suffix) |
