clean:
	lein clean
doo:
	lein doo phantom test
fig:
	lein figwheel
node:
	lein cljsbuild once
ghp:
	lein do clean, cljsbuild once min
