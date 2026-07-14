/*
Description: Examine assay information, fusions, and CNA information
Author: Haley Hunter-Zinck
Date 2022-01-06
*/

-- distinct alteration types in assay_information.yaml
SELECT DISTINCT alteration_types
FROM genie_release_public.assay_information
WHERE alteration_types IS NOT NULL 
	AND release = 10;
LIMIT 10;

-- distinct sequencing assay IDs in assay_information.yaml
SELECT DISTINCT seq_assay_id
FROM genie_release_public.assay_information
WHERE alteration_types LIKE '%structural%' 
	AND release = 10;

-- distinct sequencing assays IDs and alteration types for CNAs
SELECT DISTINCT seq_assay_id, alteration_types
FROM genie_release_public.assay_information
WHERE alteration_types LIKE '%cna%' 
	AND release = 10;

-- distinct gene names for fusion files
SELECT DISTINCT f.hugo_symbol
FROM genie_release_public.fusion AS f
	JOIN genie_release_public.sample AS s
		ON f.tumor_sample_barcode = s.sample_id
WHERE f.release = 10 AND s.release = 10 AND s.seq_assay_id = 'MSK-IMPACT341'
	AND (f.hugo_symbol IS NULL OR f.hugo_symbol = '30302_c.890')

-- sequencing assay IDs for a center with structural variants
SELECT DISTINCT s.seq_assay_id
FROM genie_release_public.fusion AS f
	JOIN genie_release_public.sample AS s
		ON f.tumor_sample_barcode = s.sample_id
WHERE f.release = 10 AND f.center = 'MSK'
LIMIT 10
