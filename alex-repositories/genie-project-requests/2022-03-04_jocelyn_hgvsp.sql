/*
Description: look at discrepancies between HGVSp_short and HGVSp. in 9.1
Author: Haley Hunter-Zinck
Date: 2022-03-04
*/

--/*
SELECT HGVSp, HGVSp_short
FROM genie_release_public.mutation
WHERE HGVSp IS NULL AND HGVSp_short IS NOT NULL AND HGVSp_short NOT LIKE '%splice%' AND HGVSp_short NOT LIKE '%*%'
	AND release = 9
LIMIT 10
--*/

/*
SELECT HGVSp, HGVSp_short
FROM genie_release_public.mutation
WHERE HGVSp IS NOT NULL AND HGVSp_short IS NULL 
	AND release = 9
LIMIT 10
*/

/*
SELECT center, COUNT(center) AS n
FROM genie_release_public.mutation
WHERE HGVSp IS NULL AND HGVSp_short IS NOT NULL AND HGVSp_short NOT LIKE '%splice%' 
	AND release = 9
GROUP BY center
*/