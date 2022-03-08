# Dotfiles

## Git

Globally ignore files that may not be appropriate to put in a project
gitignore, like Mac's .DS_Store file.

`git config --global core.excludesfile ~/.gitignore`

## Playing nice with network share

### Don't write .DS_Store when navigating network share in Finder

`defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool TRUE`
