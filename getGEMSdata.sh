#!/bin/bash

# Set local and remote paths
remote_name="gems:Data/"
local_path="data/"

rclone copy --progress "$remote_name" "$local_path"
