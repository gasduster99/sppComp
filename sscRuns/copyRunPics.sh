#mkdir 25019781982M1
#cp /media/nick/extraBig/25019781982M1/*.csv 25019781982M1/
#cp /media/nick/extraBig/25019781982M1/*.pdf 25019781982M1/
#cp -r /media/nick/extraBig/25019781982M1/marginal* 25019781982M1/
#cp -r /media/nick/extraBig/25019781982M1/year-species/ 25019781982M1/
#cp -r /media/nick/extraBig/25019781982M1/gear-year-species/ 25019781982M1/
#cp -r /media/nick/extraBig/25019781982M1/port-gear-qtr-year-species/ 25019781982M1/
#git add 25019781982M1/
#git commit -am "added 25019781982M1"
copyRun(){
	mkdir $1
	cp /media/nick/extraBig/$1/*.csv $1/
	cp /media/nick/extraBig/$1/*.pdf $1/
	cp -r /media/nick/extraBig/$1/marg* $1/
	cp -r /media/nick/extraBig/$1/species-year/ $1/
	cp -r /media/nick/extraBig/$1/species-gear-year/ $1/
	cp -r /media/nick/extraBig/$1/species-port-gear-year-qtr/ $1/
	git add $1/
	git commit -am "added $1"
}

modRun(){
	rm -r $1/marginal*
	rm -r $1/year-species/
	rm -r $1/gear-year-species/
	rm -r $1/port-gear-qtr-year-species/
	rm $1/sppMad*.pdf
}

##250 time models
modRun /media/nick/extraBig/25019781982M1/
##modRun 25019781982M1
#copyRun 25019781982M1
modRun /media/nick/extraBig/25019781982M2/
##modRun 25019781982M2
#copyRun 25019781982M2
modRun /media/nick/extraBig/25019781982M3/
##modRun 25019781982M3
#copyRun 25019781982M3
modRun /media/nick/extraBig/25019781982M4/
##modRun 25019781982M4
#copyRun 25019781982M4
modRun /media/nick/extraBig/25019781982M5/
##modRun 25019781982M5
#copyRun 25019781982M5
modRun /media/nick/extraBig/25019781982M6/
##modRun 25019781982M6
#copyRun 25019781982M6

#250 prior models
modRun /media/nick/extraBig/25019781982M4HC1/
#modRun 25019781982M4HC1
#copyRun 25019781982M4HC1
modRun /media/nick/extraBig/25019781982M4HC3/
#modRun 25019781982M4HC3
#copyRun 25019781982M4HC3
modRun /media/nick/extraBig/25019781982M4U4/
#modRun 25019781982M4U4
#copyRun 25019781982M4U4

#250 interaction models
modRun /media/nick/extraBig/25019781982M4HC3SG/
#modRun 25019781982M4HC1
copyRun 25019781982M4HC3SG
modRun /media/nick/extraBig/25019781982M4HC3SP/
#modRun 25019781982M4HC3
copyRun 25019781982M4HC3SP

##253 time models
modRun /media/nick/extraBig/25319781982M1/
##modRun 25319781982M1
#copyRun 25319781982M1
modRun /media/nick/extraBig/25319781982M2/
##modRun 25319781982M2
#copyRun 25319781982M2
modRun /media/nick/extraBig/25319781982M3/
##modRun 25319781982M3
#copyRun 25319781982M3
modRun /media/nick/extraBig/25319781982M4/
##modRun 25319781982M4
#copyRun 25319781982M4
modRun /media/nick/extraBig/25319781982M5/
##modRun 25319781982M5
#copyRun 25319781982M5
modRun /media/nick/extraBig/25319781982M6/
##modRun 25319781982M6
#copyRun 25319781982M6
#
##253 prior models
modRun /media/nick/extraBig/25319781982M4HC1/
##modRun 25319781982M4HC1
#copyRun 25319781982M4HC1
modRun /media/nick/extraBig/25319781982M4HC3/
##modRun 25319781982M4HC3
#copyRun 25319781982M4HC3
modRun /media/nick/extraBig/25319781982M4U4/
##modRun 25319781982M4U4
#copyRun 25319781982M4U4

#253 interaction models
#modRun /media/nick/extraBig/25319781982M4IGSG/
#modRun 25319781982M4IGSG
copyRun 25319781982M4IGSG
#modRun /media/nick/extraBig/25319781982M4IGSP/
#modRun 25319781982M4IGSP
copyRun 25319781982M4IGSP

#269 time models
modRun /media/nick/extraBig/26919781982M1/
#modRun 26919781982M1
#copyRun 26919781982M1
modRun /media/nick/extraBig/26919781982M2/
#modRun 26919781982M2
#copyRun 26919781982M2
modRun /media/nick/extraBig/26919781982M3/
#modRun 26919781982M3
#copyRun 26919781982M3
modRun /media/nick/extraBig/26919781982M4/
#modRun 26919781982M4
#copyRun 26919781982M4
modRun /media/nick/extraBig/26919781982M5/
#modRun 26919781982M5
#copyRun 26919781982M5
modRun /media/nick/extraBig/26919781982M6/
#modRun 26919781982M6
#copyRun 26919781982M6

#
#269 prior models
modRun /media/nick/extraBig/26919781982M4HC1/
#modRun 26919781982M4HC1
#copyRun 26919781982M4HC1
modRun /media/nick/extraBig/26919781982M4HC3/
#modRun 26919781982M4HC3
#copyRun 26919781982M4HC3
modRun /media/nick/extraBig/26919781982M4U4/
#modRun 26919781982M4U4
#copyRun 26919781982M4U4

#269 interaction models
#modRun /media/nick/extraBig/26919781982M4IGSG/
#modRun 26919781982M4IGSG
copyRun 26919781982M4IGSG
#modRun /media/nick/extraBig/26919781982M4IGSP/
#modRun 26919781982M4IGSP
copyRun 26919781982M4IGSP

