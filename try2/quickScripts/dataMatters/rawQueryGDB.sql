select
                        new_master_clusts.sample_no         as sampleNumber,
                        clust_no                        as clusterNumber,
                        rtrim(ltrim(species))           as species,
                        weight                          as weight,
                        DATEPART(yyyy, sample_date)     as year,
                        DATEPART(QUARTER, sample_date)  as qtr,
                        port_complex                    as portComplex,
                        gear_grp                        as gearGroup,
                        mark_cat                        as marketCategory,
                        live_fish                       as live

from new_master_samples inner join new_master_clusts
                        ON new_master_samples.sample_no=new_master_clusts.sample_no

where 
                        DATEPART(yyyy, sample_date) >= 1991     and 
                        DATEPART(yyyy, sample_date) <= 2001     and 
--                        check_me='0'                            and
                        live_fish='N'                           and
                        gear_grp in ('HKL', 'TWL', 'FPT', 'NET', 'MDT')
