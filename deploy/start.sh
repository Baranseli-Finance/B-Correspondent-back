#!/bin/bash

# gh_key, pg_pass, pg_master_pass, pg_admin email, pg_admin psss, minio_user, minio_pass
declare -a keysmap

idx=0
while IFS= read -r line || [[ -n "$line" ]]; do
    keysmap[idx]=$line
    (( idx++ ))
done < "$1"

sha_front=$(curl -L \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ${keysmap[0]}"\
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/Baranseli-Finance/B-Correspondent-front/commits/master \
  | jq -r '.sha')

sha_back=$(curl -L \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ${keysmap[0]}"\
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/Baranseli-Finance/B-Correspondent-back/commits/master \
  | jq -r '.sha')

echo "back sha --> $sha_front"
echo "front sha --> $sha_back"

cat <<EOT >> .env
  DBUSER=sonny
  DATABASE=b-correspondent
  DBPASS=${keysmap[1]}
  DBPOSTGRESPASS=${keysmap[2]}
  BACK_TAG=master_${sha_back}
  FRONT_TAG=master_${sha_front}
  PGADMINEMAIL=${keysmap[3]}
  PGADMINPASS=${keysmap[4]}
  MINIO_USER=${keysmap[5]}
  MINIO_PASS=${keysmap[6]}
  ELASTICPASS=${keysmap[7]}
  KIBANA_SYSTEM_PASSWORD=${keysmap[8]}
  LOGSTASH_INTERNAL_PASSWORD=${keysmap[9]}
EOT

cp ~/ssl/front/b-correspondent.crt ./deploy/nginx/ssl/front/b-correspondent.crt
cp ~/ssl/front/b-correspondent.key ./deploy/nginx/ssl/front/b-correspondent.key
cp ~/ssl/back/b-correspondent.crt ./deploy/nginx/ssl/back/b-correspondent.crt
cp ~/ssl/back/b-correspondent.key ./deploy/nginx/ssl/back/b-correspondent.key
cp ~/ssl/global.pass ./deploy/nginx/ssl/global.pass

exec docker-compose --profile setup --profile log --profile database --profile main up -d