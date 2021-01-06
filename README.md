# purescript-wordoku

This is a full halogen app which includes a sudoku solver, sudoku generator, and the option to map sudokus to colors or words. The wordoku variant asserts the additional constraint that the diagonal from the upper left to the lower right must form a complete, unique set. This diagonal then spells a 9 letter english word when solved.

## Dev
install dependencies:
```
bower install
```

build purescript (output in `/output/`)
```
pulp build
```

build deployable bundle (output in `/dist/`)
```
parcel index.html
```

## Deployment
`rm -rf ./dist` then run through all the build steps above, and push the `/dist/` directory to github. Netlify will automatically pick that up. DNS already points the sudoku.nathanielmay.com subdomain to the netlify app.