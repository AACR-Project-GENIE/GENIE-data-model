/* 
Description: Investigate why 'TNFRSF10B' and 'TNF' have no mutations in
  the GENIE v10 databse.
Author: Haley Hunter-Zinck
Date: September 7, 2021
Request: 
	source: GENIE discusssion forum
	Author: https://www.synapse.org/#!Profile:3432090 (@AlishaColl)
	url: https://www.synapse.org/#!Synapse:syn7222066/discussion/threadId=8196
*/

/*
-- are there assays covering 'TNFRSF10B' or 'TNF': DUKE-F1-T7 and VICC-01-T7 covering TNF
SELECT DISTINCT hugo_symbol, seq_assay_id
FROM genie_release_public.genomic_information
WHERE hugo_symbol IN ('TNFRSF10B', 'TNF')
	AND release = 10
--*/

/*
-- number of mutations recorded in genes 'TNFRSF10B' or 'TNF': 0
SELECT *
FROM genie_release_public.mutation
WHERE hugo_symbol = 'TNF'
	AND release = 10
*/

/*
-- number of samples sequenced with assays that cover TNF: 10,740
SELECT COUNT(*) AS n_sample
FROM genie_release_public.genomic_information as gi
	JOIN genie_release_public.sample AS sam
		ON gi.seq_assay_id = sam.seq_assay_id
WHERE hugo_symbol = 'TNF'
	AND gi.release = 10 AND sam.release = 10
--*/

/*
-- number of mutations recorded within TNF regions sequenced: 0
SELECT *
FROM genie_release_public.genomic_information AS gi
	JOIN genie_release_public.mutation AS mut
		ON mut.chromosome = gi.chromosome 
			AND mut.start_position >= gi.start_position
			AND mut.end_position <= gi.end_position
WHERE gi.hugo_symbol = 'TNF'
	AND gi.release = 10 AND mut.release = 10
*/

/*
-- number of mutations recorded associated with samples sequenced by TNF assays: 29,422
-- so these samples have recorded mutations at other locations, just not TNF
SELECT COUNT(*) AS n_sample_tnf
FROM genie_release_public.mutation AS mut
	JOIN genie_release_public.sample AS sam
		ON mut.tumor_sample_barcode = sam.sample_id
WHERE mut.release = 10 AND sam.release = 10
	AND sam.seq_assay_id IN ('DUKE-F1-T7', 'VICC-01-T7')
--*/

/*
-- is there CNA data covering these regions?  no
SELECT *
FROM genie_release_public.genomic_information AS gi
	JOIN genie_release_public.cna AS cna
		ON cna.chrom = gi.chromosome 
			AND CAST(cna.loc_start AS INTEGER) >= gi.start_position
			AND CAST(cna.loc_end AS INTEGER) <= gi.end_position
WHERE gi.hugo_symbol = 'TNF'
	AND gi.release = 10 AND cna.release = 10
*/

-- looking at VICC-01-07 gene panel, what about 'TNFRSF14' or other variations?: yes: "TNFAIP3", "TNFRSF11A", "TNFRSF14"
/*
SELECT hugo_symbol, COUNT(*) AS n_mutation
FROM genie_release_public.mutation
WHERE hugo_symbol LIKE 'TNF%'
	AND release = 10
GROUP BY hugo_symbol
ORDER BY COUNT(*) DESC
*/

/*
-- is there fusion data covering these regions?  yes, 3 for TNFRSF10B
SELECT hugo_symbol, COUNT(*)
FROM genie_release_public.fusion AS fus
WHERE fus.hugo_symbol IN ('TNF', 'TNFRSF10B')
	AND fus.release = 10 
GROUP BY fus.hugo_symbol
*/
/*
SELECT *
FROM genie_release_public.fusion AS fus
WHERE fus.hugo_symbol IN ('TNF', 'TNFRSF10B')
	AND fus.release = 10 
*/

