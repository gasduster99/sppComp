/****** Script for SelectTopNRows command from SSMS  ******/
SELECT	[MARK_CAT]	as marketCategory,
		[YEAR]		as year,
		[QUARTER]	as qtr,
		[GEAR_GRP]	as gearGroup,
		[PORT_COMPLEX]	as portComplex,
		[SPECIES]	as species,
		[LIVE]		as live,
		sum(LBS)	as comLands
  
  FROM [grunloh_db].[dbo].[COM_LANDS]
  
  where 
		year >= 1991	and
		year <= 2001	and
		live = 'N'		and
		gear_grp in ('HKL', 'TWL', 'FPT', 'NET', 'MDT')

group by
		mark_cat,
		year,
		quarter,
		gear_grp,
		port_complex,
		live,
		SPECIES
