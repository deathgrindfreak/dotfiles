total_test_ids() (
    grep flowId staging.out \
        | sed 's/[^{]*\(.*\)/\1/g' \
        | jq -r ".flowId" \
        | sort \
        | uniq \
        | grep -v null
)

pass_test_ids() (
    grep flowId "$1" \
        | sed 's/[^{]*\(.*\)/\1/g' \
        | jq -r 'select(.state == "Success") | .flowId' \
        | sort \
        | uniq \
        | grep -v null
)

fail_test_ids() (
    grep flowId "$1" \
        | sed 's/[^{]*\(.*\)/\1/g' \
        | jq -r 'select(.state == "FAIL") | .flowId' \
        | sort \
        | uniq \
        | grep -v null
)

fail_test_ids_inline() (
    fail_test_ids "$1" | paste -sd ' ' -
)
