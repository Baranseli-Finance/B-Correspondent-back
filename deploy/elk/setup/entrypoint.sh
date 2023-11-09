#!/usr/bin/env bash

set -eu
set -o pipefail

source "${BASH_SOURCE[0]%/*}"/lib.sh

log 'Waiting for availability of Elasticsearch. This can take several minutes.'

declare -i exit_code=0
wait_for_elasticsearch || exit_code=$?

if ((exit_code)); then
	case $exit_code in
		6)
			suberr 'Could not resolve host. Is Elasticsearch running?'
			;;
		7)
			suberr 'Failed to connect to host. Is Elasticsearch healthy?'
			;;
		28)
			suberr 'Timeout connecting to host. Is Elasticsearch healthy?'
			;;
		*)
			suberr "Connection to Elasticsearch failed. Exit code: ${exit_code}"
			;;
	esac

	exit $exit_code
fi

sublog 'Elasticsearch is running'


curl -u elastic -XPUT 'localhost:9200/_security/user/sonny'  -H 'Content-Type: application/json' -d '{ "password" : "'"KIBANA_SYSTEM_PASSWORD"'", "roles" : [ "kibana_system" ] }'

curl -u elastic -XPOST 'localhost:9200/security/role/logstash_writer' \
 -H 'Content-Type: application/json' \
 -d ' \
{ \
  "cluster": ["manage_index_templates", "monitor", "manage_ilm"], \
  "indices": [ \
    { \
      "names": [ "logstash-*" ], \
      "privileges": ["write","create","create_index","manage","manage_ilm"]  \
    } \
  ] \
}'

curl -u elastic -XPOST 'localhost:9200/_xpack/security/user/logstash_internal' -d '{"password" : "'"$LOGSTASH_INTERNAL_PASSWORD"'", "roles" : [ "logstash_writer" ] }'
