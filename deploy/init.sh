#!/bin/bash

mute500="${MUTE_500:-True}"

env_file=$(realpath -s env.yaml)

bcorrespondent_env_file=$(realpath -s bcorrespondent_env)
front_env_file=$(realpath -s front_env)

# db_user, db_pass, db, minio_access_key, minio_secret_key
declare -a keysmap

idx=0
while IFS= read -r line || [[ -n "$line" ]]; do
    keysmap[idx]=$line
    (( idx++ ))
done < "${bcorrespondent_env_file}"

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
        --path_to_symmetric_base deploy/twofish128.txt \
        --path_to_ed448_base deploy/ed448.txt \
        --print_cfg y \
        --env_path $env_file \
        --minio_access_key ${keysmap[3]} \
        --minio_secret_key ${keysmap[4]} \
        --front_env_file_path $front_env_file \
        --b_correspondent_db_user ${keysmap[0]} \
        --b_correspondent_db_pass ${keysmap[1]} \
        --b_correspondent_database ${keysmap[2]}"