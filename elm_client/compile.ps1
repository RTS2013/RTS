$name=$args[0]
cls

elm --make `
	--only-js `
	--runtime=src/build/dependencies/elm-runtime.js `
	--build-dir=src/build `
	--cache-dir=cache `
	--src-dir=src `
	$name