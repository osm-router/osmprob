#!/bin/sh
SESSION="osmprob"

tmux -2 new-session -d -s $SESSION
cd ./R/
tmux new-window -t $SESSION:1 -k -n R
tmux split-window -h
tmux send-keys -t $SESSION:1 'R' C-m

cd ../src/
tmux new-window -t $SESSION:2 -n C++
tmux select-window -t $SESSION:1

tmux attach -t $SESSION
