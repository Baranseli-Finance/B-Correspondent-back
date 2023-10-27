#!/bin/bash
# ============LICENSE_START====================================================
#  Copyright (C) 2021. Nordix Foundation. All rights reserved.
# =============================================================================
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0
# ============LICENSE_END======================================================

b-correspondent_env_file=$(realpath -s b-correspondent_env)

# db_user, db_pass, db, minio_access_key, minio_secret_key
declare -a keysmap

idx=0
while IFS= read -r line || [[ -n "$line" ]]; do
    keysmap[idx]=$line
    (( idx++ ))
done < "${b-correspondent_env_file}"

/liquibase/liquibase \
    --driver=org.postgresql.Driver \
    --url=jdbc:postgresql://db:5432/${keysmap[3]} \
    --changeLogFile=changelog/changelog.xml \
    --username=${keysmap[0]} \
    --password=${keysmap[1]} \
    --log-level info \
    update