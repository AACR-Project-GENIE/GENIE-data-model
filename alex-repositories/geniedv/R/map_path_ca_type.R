#' Map path_ca_type codes to text labels
#'
#' @param x A numeric (or character) vector of `path_ca_type` codes.
#'
#' @returns A character vector of labels.
#' @export
#'
#' @examples
#' map_path_ca_type(c(51, 41, 13))
map_path_ca_type <- function(x) {
  path_ca_type_labels <- c(
    "1" = "Adrenocortical Carcinoma",
    "3" = "Anal Cancer",
    "5" = "Appendix Cancer",
    "6" = "Bile Duct Cancer",
    "7" = "Bladder Cancer",
    "11" = "Brain Cancer",
    "13" = "Breast Cancer",
    "14" = "Breast Sarcoma",
    "15" = "NET or Carcinoid",
    "17" = "Cervical Cancer",
    "97" = "Corpus Uteri Carcinoma and Carcinosarcoma",
    "99" = "Corpus Uteri Sarcoma",
    "19" = "Colon Cancer",
    "20" = "Colon/Rectum Cancer",
    "21" = "Esophagus Cancer",
    "201" = "Ewing Sarcoma",
    "23" = "Fallopian Tube Cancer",
    "25" = "Gallbladder Cancer",
    "205" = "Germ Cell Tumor",
    "27" = "GIST",
    "29" = "Head and Neck Cancer",
    "30" = "Mesothelioma",
    "35" = "Ill Defined/Cancer of Unknown Primary",
    "39" = "Liver Cancer",
    "41" = "Lung Cancer, NOS",
    "45" = "Melanoma",
    "47" = "Merkel Cell",
    "211" = "Neuroblastoma",
    "51" = "Non Small Cell Lung Cancer",
    "215" = "Osteosarcoma",
    "55" = "Ovarian Cancer",
    "57" = "Pancreatic Cancer",
    "58" = "Parathyroid Cancer",
    "59" = "Penis Cancer",
    "61" = "Peritoneum Cancer",
    "63" = "Placenta Cancer",
    "65" = "Prostate Cancer",
    "67" = "Rectum and Rectosigmoid Cancer",
    "69" = "Renal Kidney Cancer",
    "71" = "Renal Pelvis Cancer",
    "221" = "Retinoblastoma",
    "225" = "Rhabdomyosarcoma",
    "73" = "Scrotum Cancer",
    "75" = "Small Cell Lung Cancer",
    "77" = "Small Intestine Cancer",
    "95" = "Soft tissue sarcoma",
    "81" = "Stomach Cancer",
    "83" = "Testis Cancer",
    "85" = "Thymus Cancer",
    "87" = "Thyroid Cancer",
    "89" = "Uterus Cancer NOS",
    "91" = "Vagina Cancer",
    "93" = "Vulva Cancer",
    "231" = "Wilms Tumor",
    "301" = "Leukemia NOS",
    "315" = "Acute Lymphoblastic Leukemia",
    "317" = "Acute Myeloid Leukemia",
    "319" = "Chronic Lymphocytic Leukemia",
    "321" = "Chronic Myelogenous Leukemia",
    "303" = "Non Hodgkin's Lymphoma",
    "305" = "Hodgkin's Lymphoma",
    "307" = "Multiple Myeloma",
    "323" = "Plasmacytoma",
    "311" = "MDS Myelodysplastic syndrome or Myeloproliferative Neoplasm",
    "313" = "MGUS Monoclonal gammopathy of undetermined significance",
    "309" = "Other hematopoietic or lymphoid neoplasm",
    "1000" = "Other",
    "1010" = "Not stated",
    "1020" = "Unknown"
  )

  dplyr::recode(as.character(x), !!!path_ca_type_labels)
}
