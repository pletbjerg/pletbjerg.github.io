# pletbjerg.github.io

# Development
All commands assume that you are in 
```
nix develop
```
## Watching the website
```
cabal run pletbjerg-github-io.cabal -- watch
```


## Starting a hoogle server
To start a hoogle server, type 
```
hoogle server --local
```

## CSS
The CSS is the default from Pandocs
```
pandoc -D html
```
and
```
pandoc --print-default-data-file=templates/styles.html -D templates/styles.html
```
