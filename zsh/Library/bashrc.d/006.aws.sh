region() {
    case "$1" in
        ue1)
            export AWS_REGION=us-east-1
            export VIX_REGION=ue1
            ;;
        ue2)
            export AWS_REGION=us-east-2
            export VIX_REGION=ue2
            ;;
        uw2)
            export AWS_REGION=us-west-2
            export VIX_REGION=uw2
            ;;
        cc1)
            export AWS_REGION=ca-central-1
            export VIX_REGION=cc1
            ;;
        ec1)
            export AWS_REGION=eu-central-1
            export VIX_REGION=ec1
            ;;
        "")
            echo "AWS_REGION = $(echo $AWS_REGION)"
            echo "VIX_REGION = $(echo $VIX_REGION)"
    esac
}

complete -W 'ue1 ue2 uw2 cc1 ec1' region

# tunnel into various veracity dbs
db_tunnel() (
    case "$1" in
        ops)
            ssh -L 5439:ops.cby3nvbeul0v.us-east-1.rds.amazonaws.com:5432 arxops
            ;;
        singularity)
            ssh -L 5436:singularity.db.us-east-1.vix:5432 arx
            ;;
        singularity-ro)
            ssh -L 5439:prod-cluster.cluster-ro-cby3nvbeul0v.us-east-1.rds.amazonaws.com:5432 arx
            ;;
        logging)
            ssh -L 5437:logging.db.us-east-1.vix:5432 arx
            ;;
        entity)
            ssh -L 5438:entity.db.us-east-1.vix:5432 arx
            ;;
        singularity_ue2)
            ssh -L 5436:singularity.czrkquu3kfvh.us-east-2.rds.amazonaws.com:5432 arxue2
            ;;
        logging_ue2)
            ssh -L 5437:logging.czrkquu3kfvh.us-east-2.rds.amazonaws.com:5432 arxue2
            ;;
        entity_ue2)
            ssh -L 5438:entity.czrkquu3kfvh.us-east-2.rds.amazonaws.com:5432 arxue2
            ;;
        singularity_uw2)
            ssh -L 5436:singularity.ctcc4agocbsg.us-west-2.rds.amazonaws.com:5432 arxuw2
            ;;
        logging_uw2)
            ssh -L 5437:logging.ctcc4agocbsg.us-west-2.rds.amazonaws.com:5432 arxuw2
            ;;
        entity_uw2)
            ssh -L 5438:entity.ctcc4agocbsg.us-west-2.rds.amazonaws.com:5432 arxuw2
            ;;
        singularity_cc1)
            ssh -L 5436:singularity.caslz9qhomz1.ca-central-1.rds.amazonaws.com:5432 arxcc1
            ;;
        logging_cc1)
            ssh -L 5437:logging.caslz9qhomz1.ca-central-1.rds.amazonaws.com:5432 arxcc1
            ;;
        entity_cc1)
            ssh -L 5438:entity.caslz9qhomz1.ca-central-1.rds.amazonaws.com:5432 arxcc1
            ;;
        *)
            echo "Couldn't find db for $1"
            ;;
    esac
)

complete -W 'ops singularity singularity-ro entity logging singularity_ue2 entity_ue2 logging_ue2 singularity_uw2 entity_uw2 logging_uw2 singularity_cc1 entity_cc1 logging_cc1' db_tunnel

# AWS credential switcher
creds() {
    case "$1" in
        prod)
            export AWS_PROFILE=vixprod
            ;;
        dev)
            export AWS_PROFILE=vixdev
            ;;
        personal)
            export AWS_PROFILE=personal
            ;;
        *)
            echo "Current profile = $AWS_PROFILE"
            ;;
    esac
 }

kill_instance() (
    aws autoscaling terminate-instance-in-auto-scaling-group --region "$1" --instance-id "$2" --should-decrement-desired-capacity
)
