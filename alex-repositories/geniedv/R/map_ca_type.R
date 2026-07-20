#' Map ca_type codes to text labels
#'
#' @param x A numeric (or character) vector of ca_type codes.
#'
#' @returns A character vector of cancer type labels.
#' @export
#'
#' @examples
#' map_ca_type(c(1, 13, 41, 1000))
map_ca_type <- function(x) {
  ca_type_labels <- c(
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
    "89" = "Uterus Cancer",
    "91" = "Vagina Cancer",
    "93" = "Vulva Cancer",
    "231" = "Wilms Tumor",
    "1000" = "Other"
  )

  dplyr::recode(as.character(x), !!!ca_type_labels)
}
