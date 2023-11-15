#!/usr/bin/env bash

boop() (
  local last="$?";

  if [ $last -eq 0 ]; then
    sfx good
  else
    sfx bad
  fi

  $(exit "$last")
)

function get_event() (
  spur_api "/private/events/$2/body" \
    --api private \
    --env "$1" \
    --accept text
)

function reprocess_agenda_item() (
  spur_api "/private/agenda/reprocess/$2" \
    -X POST \
    --api private \
    --env "$1" \
    --accept text
)

function invalidate_events() {
  EVENT_IDS=()
  ENV=""
  REASON=""

  while [ "$1" != "" ]; do
    case "$1" in
      --reason)
        shift
        REASON="$1"
        ;;

      --env)
        shift
        ENV="$1"
        ;;

      *)
        EVENT_IDS+=("$1")
        ;;
    esac
    shift
  done

  POST_JSON=$(
    jq -n -c -M \
      --arg "reason" "$REASON" \
      --arg "eventIds" "$EVENT_IDS" \
      '{"reason": $reason, "eventIds": ($eventIds | split(" ") | map(tonumber))}'
  )

  spur_api /private/events/invalidate \
    --verbose \
    -X POST \
    --api private \
    --env "$ENV" \
    --content-type json \
    --accept json \
    -d "$POST_JSON"
}
