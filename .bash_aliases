# MISCELLANOUS
alias mous='bash <(curl -s http://www.nicknorton.net/mousewheel.sh)'
alias off='shutdown -P now'
alias smi='watch -n 0.5 nvidia-smi'
alias cl='clear'
# listing files | directories
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias em='emacs -nw'

# SSH
alias ev='eval `ssh-agent`'
# alias ad='ssh-add ~/.ssh/' # moved to .bashrc as function ad()

# GIT
alias gs='git status'
alias gc='git commit -m "genrl commit"'
alias gp='git push origin main'
alias ga='git add'

# DOCKER
alias dp='docker ps'
alias dpa='docker ps -a'
alias di='docker images'
alias dri='docker rmi -f'
alias drm='docker rm'
alias db='docker build -t'

