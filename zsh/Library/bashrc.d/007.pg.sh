db_name() (
    DB_NAME=''

    case "$1" in
    prod)
        DB_NAME="pspur-db"
        ;;
    preprod)
        DB_NAME="rspur-db"
        ;;
    uat)
        DB_NAME="uspur-db"
        ;;
    cert)
        DB_NAME="cspur-db"
        ;;
    esac
    echo "$DB_NAME"
)

db_port() (
    PORT=''

    case "$1" in
    prod)
        PORT="5436"
        ;;
    preprod)
        PORT="5435"
        ;;
    uat)
        PORT="5434"
        ;;
    cert)
        PORT="5433"
        ;;
    esac
    echo "$PORT"
)

spur_db() (
    local env="$1"
    psql -h localhost -p "$(db_port ${env})" -U spurdbuser "$(db_name ${env})"
)

spur_db_pgcli() (
    local env="$1"
    pgcli -h localhost -p "$(db_port ${env})" -U spurdbuser "$(db_name ${env})"
)

hung_queries() (
    ssh_host="$(ssh_hostname)"
    db_name="${1:=logging}"

    echo "SSH: ${VIX_ARX_USERNAME}@${ssh_host}"

    ssh -o CheckHostIP=no \
        -o ForwardAgent=yes \
        "${VIX_ARX_USERNAME}@${ssh_host}" \
        "db-${db_name}" < ~/src/hung.sql
)

kill_hung_queries() (
    ssh_host="$(ssh_hostname)"
    db_name="${1:=logging}"

    echo "SSH: ${VIX_ARX_USERNAME}@${ssh_host}"

    ssh -o CheckHostIP=no \
        -o ForwardAgent=yes \
        "${VIX_ARX_USERNAME}@${ssh_host}" \
        "db-${db_name}" < ~/src/kill_hung.sql
)
