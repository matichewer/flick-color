#!/bin/bash


# Path of backup file
PATH_DB="${HOME}/Git/flick-color/pengines_server/apps/proylcc/backup.txt"

# Save backup content in a variable
DB=$(<${PATH_DB})

# Push backup in Prolog
/usr/bin/tmux send-keys -t flick-color-prolog "${DB}" Enter

