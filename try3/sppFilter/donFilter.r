rm(list=ls())

#
library("RJDBC")
library("diffdf")
#
source('dataFunk.r')

#
#FUNCTIONS
#

#
getRawCALCOMData = function(mcat, minYear, maxYear, save=F){
        #mcat   : mcat number
        #minYear: start of modeled time period 
        #maxYear: end of modeled time period 
        #
        #value  : a raw data.frame called from the data source 
        #

        #
        #SQL
        #

        #
        writeLines('\n****** Query *******')
        #driver
        drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
        #connection
        ch = dbConnect(drv, 'jdbc:sqlserver://sql2016.psmfc.org\\calcom;databaseName=CALCOM', 'ngrunloh', 'calcom!PSMFC2022')
	#ch = dbConnect(drv, 'jdbc:sqlserver://161.55.235.186;databaseName=COMX', 'NGrunloh', '2Ac3$$COMXdb987!')
        #port sample data query
        raw = dbGetQuery(ch,
                sprintf("
                select
                        master_clusts.sample_no         as sampleNumber,
                        clust_no                        as clusterNumber,
                        rtrim(ltrim(species))           as species,
                        weight                          as weight,
                        DATEPART(yyyy, sample_date)     as year,
                        DATEPART(QUARTER, sample_date)  as qtr,
                        port_complex                    as portComplex,
                        gear_grp                        as gearGroup,
                        mark_cat                        as marketCategory,
                        live_fish                       as live
                
                from master_samples inner join master_clusts
                        ON master_samples.sample_no=master_clusts.sample_no
               
                where 
                        DATEPART(yyyy, sample_date) >= %d       and 
                        DATEPART(yyyy, sample_date) <= %d       and 
                        mark_cat=%d                             and
                        check_me='0'                            and
                        live_fish='N'                           and
                        gear_grp in ('HKL', 'TWL', 'FPT', 'NET', 'MDT')
                ", minYear, maxYear, mcat)
        )
        #landings data
        lands = dbGetQuery(ch,
                sprintf("
                select
                        mark_cat        as marketCategory, 
                        year            as year, 
                        quarter         as qtr,
                        gear_grp        as gearGroup, 
                        port_complex    as portComplex,
                        species         as species,
                        live            as live,
                        sum(pounds)     as comLands
                
                FROM [dbo].[COM_LANDS]   
                
                where 
                        year >= %d       and 
                        year <= %d       and 
                        mark_cat=%d      and
                        live='N'         and
                        gear_grp in ('HKL', 'TWL', 'FPT', 'NET', 'MDT')
                
                group by 
                        mark_cat,
                        year,
                        quarter,
                        gear_grp,
                        port_complex,
                        live,
                        species
                        
                ", minYear, maxYear, mcat)
        )

        #merge
        raw = merge(raw, lands, by=c('species', 'year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live'), all.x=T)
        raw$comLands[is.na(raw$comLands)] = 0

        #
        #SAVE
        #

        #       
        if( save ){
                #save a local version of data for future reference
                write.csv(raw, sprintf('%sCALCOMdata%sTo%s_%s.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4), Sys.Date()),
                        row.names=F,
                        quote=F
                )
                #raw = read.csv('./data83to90.csv', header=T)           
        }

        #
        writeLines('**** Complete ******\n')
        return( raw )
}

#
#MAIN
#

#COMX

#dat = getRawData(250, 1991, 2001, save=T)
comX7882 = read.csv("250data78To82_2018-06-08.csv")
comX8390 = read.csv("250data83To90_2018-06-08.csv")
comX9101 = read.csv("250data91To01_2024-10-28.csv")

#CALCOM

#
cal7882 = read.csv("250CALCOMdata78To82_2024-10-28.csv") #getRawCALCOMData(250, 1978, 1982, save=T)
cal8390 = read.csv("250CALCOMdata83To90_2024-10-28.csv") #getRawCALCOMData(250, 1983, 1990, save=T)
cal9101 = read.csv("250CALCOMdata91To01_2024-10-28.csv") #getRawCALCOMData(250, 1991, 2001, save=T)

#COMPARE

#
comp7882 = diffdf(comX7882, cal7882)
diff7882 = cal7882[unlist(comp7882$ExtRowsComp),]
#
comp8390 = diffdf(comX8390, cal8390)
diff8390 = cal8390[unlist(comp8390$ExtRowsComp),]
#
comp9101 = diffdf(comX9101, cal9101)
diff9101 = cal8390[unlist(comp9101$ExtRowsComp),]

















#
#JUNK
#



##driver
#drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
##connection
#ch = dbConnect(drv, 'jdbc:sqlserver://161.55.235.186;databaseName=COMX', 'NGrunloh', '2Ac3$$COMXdb987!')
##port sample data query
#comxRaw = dbGetQuery(ch,
#        sprintf("
#        select
#                master_clusts.sample_no         as sampleNumber,
#                clust_no                        as clusterNumber,
#                rtrim(ltrim(species))           as species,
#                weight                          as weight,
#                DATEPART(yyyy, sample_date)     as year,
#                DATEPART(QUARTER, sample_date)  as qtr,
#                port_complex                    as portComplex,
#                gear_grp                        as gearGroup,
#                mark_cat                        as marketCategory,
#                live_fish                       as live
#        
#        from master_samples inner join master_clusts
#                ON master_samples.sample_no=master_clusts.sample_no
#       
#        where 
#                DATEPART(yyyy, sample_date) >= %d       and 
#                DATEPART(yyyy, sample_date) <= %d       and 
#                check_me='0'                            and 
#                gear_grp in ('HKL', 'TWL', 'FPT', 'NET', 'MDT')
#        ", minYear, maxYear)
#)

#CALCOM




