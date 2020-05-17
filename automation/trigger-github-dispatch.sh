#!/usr/bin/env bash

set -e  # Fail fast

# Trigger trigger a Github "dispatch event"
#
# Citation:
#   - http://www.btellez.com/posts/triggering-github-actions-with-webhooks.html
#   - https://developer.github.com/v3/repos/#create-a-repository-dispatch-event

owner=easoncxz
repo=weibo-export

if [ $# -lt 1 ]
then
    echo "Usage: $0 event_type [client_payload]"
    echo
    echo '  For client_payload, you might want "trigger_ci_run".'
    echo '  See .github/workflows/deploy.yml for details.'
    exit 1
else
    event_type="$1"

    if [ $# -lt 2 ]
    then
        curl -X POST "https://api.github.com/repos/$owner/$repo/dispatches" \
            -H 'Accept: application/vnd.github.everest-preview+json' \
            -H "Authorization: token $EASONCXZ_GITHUB_OAUTH_TOKEN" \
            --data "{\"event_type\": \"$event_type\"}"
    else
        client_payload="$2"
        curl -X POST "https://api.github.com/repos/$owner/$repo/dispatches" \
            -H 'Accept: application/vnd.github.everest-preview+json' \
            -H "Authorization: token $EASONCXZ_GITHUB_OAUTH_TOKEN" \
            --data "{\"event_type\": \"$event_type\", \"client_payload\": $client_payload }"
    fi

fi
