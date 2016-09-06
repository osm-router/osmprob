#!/bin/sh
SESSION="osmprob"

tmux -2 new-session -d -s $SESSION
tmux new-window -t $SESSION:1 -k -n R
tmux send-keys -t $SESSION:1 'vim runTest.R' C-m

cd ./src/
tmux new-window -t $SESSION:2 -n C++
tmux send-keys -t $SESSION:2 'vim Graph.cpp' C-m
tmux select-window -t $SESSION:1

tmux attach -t $SESSION
