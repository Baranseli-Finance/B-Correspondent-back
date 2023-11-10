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

elastic_cred=$ELASTIC_PASSWORD
kibana_cred=$KIBANA_SYSTEM_PASSWORD
logstash_cred=$LOGSTASH_INTERNAL_PASSWORD

curl -u "elastic:$elastic_cred" -XPUT '192.168.22.12:9200/_security/user/sonny' \
 -H 'Content-Type: application/json' \
 -d @- << EOF 
{ "password" : "$kibana_cred", 
  "roles" : [ "kibana_system" ]
}
EOF

curl -u "elastic:$elastic_cred" -XPUT '192.168.22.12:9200/_security/role/logstash_writer' \
 -H 'Content-Type: application/json' \
 -d @- << EOF
{ 
  "cluster": ["manage_index_templates", "monitor", "manage_ilm"],
  "indices": [
    {
      "names": [ "logstash-*" ],
      "privileges": ["write","create","create_index","manage","manage_ilm"]
    }
  ]
}
EOF

curl -u "elastic:$elastic_cred" -XPUT '192.168.22.12:9200/_security/user/logstash_internal' \
  -H 'Content-Type: application/json' \
  -d @- << EOF 
{"password" : "$logstash_cred", 
 "roles" : [ "logstash_writer" ]
}
EOF