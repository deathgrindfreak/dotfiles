api_prod() {
   curl -X 'GET' \
       "https://coreapi.heb.com$1" \
       -H "accept: $2" \
       -H "Spur-Authorization: token $KPS_PROD_PRIVATE" \
       -H "apikey: $KPS_PROD_API_TOKEN"
}

api_json_prod() {
   api_prod "$1" "application/json"
}

api_text_prod() {
   api_prod "$1" "text/plain;charset=utf-8"
}

supplier_raw_events_prod() {
   api_json_prod "/spur/private/suppliers/$1/raw-events"
}

supplier_event_body_prod() {
   api_text_prod "/spur/private/events/$1/body"
}

diff_xml() {
   spur_api "/peoplesoft-next-gen-trace/$2/next-gen-xml" \
       --api private \
       --env "$1" \
       --accept xml \
       | xmllint --format - \
       > "/tmp/next-gen-$2.xml"

   spur_api "/peoplesoft-next-gen-trace/$2/legacy-xml" \
       --api private \
       --env "$1" \
       --accept xml \
       | xmllint --format - \
       > "/tmp/legacy-$2.xml"

   colordiff -u "/tmp/legacy-$2.xml" "/tmp/next-gen-$2.xml"
}
