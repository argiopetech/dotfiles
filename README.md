# My .files
** USE AT YOUR OWN RISK!!! **

These are the basic configuration files I use on my local machines. They are subject to change rapidly and without warning, potentially breaking other things. I highly recommend that you review all files prior to using any of them.

## make-links.sh

Since having a git repo in ${HOME} is a bit of a pain in the prompt (see `files/bash_prompt`), I've put them in a separate directory. I clone this repository on new machines as `~/.dotfiles`.

In order to create links to the files in `files/`, I've created a short script that first creates the directories needed with `mkdir -p`, then links all the files in `files/` with a `.` prefix.

`make-links.sh` probably doesn't clobber anything (it should complain about, but not touch, any existing files), but, as with the rest of the repo, "use at your own risk".