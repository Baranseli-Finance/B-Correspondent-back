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

tag_front=$(curl -L \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ${keysmap[0]}"\
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/Baranseli-Finance/B-Correspondent-front/tags \
  | jq -r '.[0].name')

sha_back=$(git log -n 1 --pretty=format:"%H")

tag_back=$(git describe --tags --abbrev=0)

if [ -z "$sha_back" ]
then
     back=master_$sha_back
else
     back=$tag_back
fi

if [ -z "$sha_front" ]
then
     front=master_$sha_front
else
     front=$tag_front
fi

echo "back sha --> $back"
echo "front sha --> $front"

cat <<EOT >> .env
  DBUSER=sonny
  DATABASE=b-correspondent
  DBPASS=${keysmap[1]}
  DBPOSTGRESPASS=${keysmap[2]}
  BACK_TAG=${back}
  FRONT_TAG=${front}
  PGADMINEMAIL=${keysmap[3]}
  PGADMINPASS=${keysmap[4]}
  MINIO_USER=${keysmap[5]}
  MINIO_PASS=${keysmap[6]}
EOT

cp ~/ssl/front/b-correspondent.crt ./deploy/nginx/ssl/front/b-correspondent.crt
cp ~/ssl/front/b-correspondent.key ./deploy/nginx/ssl/front/b-correspondent.key
cp ~/ssl/back/b-correspondent.crt ./deploy/nginx/ssl/back/b-correspondent.crt
cp ~/ssl/back/b-correspondent.key ./deploy/nginx/ssl/back/b-correspondent.key
cp ~/ssl/global.pass ./deploy/nginx/ssl/global.pass

exec docker-compose up -d