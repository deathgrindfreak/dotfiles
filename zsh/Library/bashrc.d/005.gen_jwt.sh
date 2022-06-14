gen_jwt() (
    local admin
    local tenant
    tenant="jcb"

    local sub
    sub="jcbell"

    set -- $(getopt "as:t:" -- "$*")

    while [ $# -ge 1 ]; do
        case "$1" in
            -a) admin=true ;;
            -t) tenant=$2 ;;
            -s) sub=$2 ;;
        esac
        shift
    done

    aws s3 cp s3://secrets-vix-prod/jwt/alpha_private.pem /tmp/dev_vendor.pem &> /dev/null

    jwtgen -a RS256 \
           -p /tmp/dev_vendor.pem \
           -c sub="$sub" \
           -c tenant="${tenant}" \
           "${admin:+-c vix='admin'}" \
           -h 'kid=vix:alpha' \
           -e 99999999 \
           -v \
        | tail -1 > /tmp/token
    echo "Done."
)
