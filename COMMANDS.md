# Usefull hints

## Symlinks
### Linux
The point is to have central `dotfiles/` folde with all of the config and
create links to folder where software is expecting .dotfiles.

```sh
# linux symbolic link
ln -s ~/dotfiles/init.el /home/krumm/.emacs.d/init.el
```
### Windows
- Without any extra options, mklink creates a symbolic link to a file. The below command creates a symbolic, or “soft”, link at Link pointing to the file Target :
`mklink Link Target`
- Use /D when you want to create a soft link pointing to a directory. like so:
`mklink /D Link Target`
- Use /Hwhen you want to create a hard link pointing to a file:
`mklink /H Link Target`
- Use /J to create a hard link pointing to a directory, also known as a directory junction:
`mklink /J Link Target`
`mklink /J C:\LinkToFolder C:\Users\Name\OriginalFolder`

## Magit
```
```
		
