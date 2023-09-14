#!/bin/bash

mute500="${MUTE_500:-True}"

env_file=$(realpath -s env.yaml)

b-correspondent_env_file=$(realpath -s b-correspondent_env)

# db_user, db_pass, db, minio_access_key, minio_secret_key
declare -a keysmap

idx=0
while IFS= read -r line || [[ -n "$line" ]]; do
    keysmap[idx]=$line
    (( idx++ ))
done < "${b-correspondent_env_file}"

echo 'launch server..'
. /home/nix/.nix-profile/etc/profile.d/nix.sh && \
  nix-shell deploy.nix \
    --log-format bar-with-logs \
    --verbose \
    --command \
    "$PWD/bin/b-correspondent \
        --cfg_path deploy/config.yaml \
        --path_to_katip deploy \
        --path_to_jwk deploy/jwk.txt \
        --print_cfg y \
        --env_path $env_file \
        --mute500 $mute500 \
        --b_correspondent_db_user ${keysmap[0]} \
        --b_correspondent_db_pass ${keysmap[1]} \
        --b_correspondent_database ${keysmap[2]} \
        --minio_access_key ${keysmap[3]} \
        --minio_secret_key ${keysmap[4]}"