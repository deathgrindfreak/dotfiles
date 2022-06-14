source ~/.vix/arx_env.sh || { echo "Missing ~/.vix/arx_env.sh"; exit 1; }

ssh_hostname() (
    SSH_HOSTNAME=''

    case "$VIX_REGION" in
    ue1)
        SSH_HOSTNAME="arx.veracitydoc.com"
        ;;
    ue2)
        SSH_HOSTNAME="arx.ue2.veracitydoc.com"
        ;;
    uw2)
        SSH_HOSTNAME="arx.uw2.veracitydoc.com"
        ;;
    cc1)
        SSH_HOSTNAME="arx.cc1.veracitydoc.com"
        ;;
    ec1)
        SSH_HOSTNAME="arx.ec1.veracitydoc.com"
        ;;
    esac
    echo "$SSH_HOSTNAME"
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
