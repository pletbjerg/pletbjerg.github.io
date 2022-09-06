# pletbjerg.github.io

# Development
All commands assume that you are in 
```
nix develop
```
## Building the website.
Execute the following command.
```
cabal run pletbjerg-github-io.cabal
```

## Cleaning the website.
Execute the following command.
```
cabal run pletbjerg-github-io.cabal -- clean
```


## Starting a hoogle server
To start a hoogle server, type 
```
hoogle server --local
```

## Pandoc template defaults
The HTML default template from Pandocs is given by the following command.
```
pandoc -D html
```
Moreover, the CSS default template is given by the following command.
```
pandoc --print-default-data-file=templates/styles.html -D templates/styles.html
```
