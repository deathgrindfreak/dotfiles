pr() (
    open "https://github.com/VeracityInnovations/$(pwd | xargs -I {} basename '{}')/pull/new/$(git symbolic-ref --short HEAD)"
)

gh() (
    local base_url="https://gitlab.com/heb-engineering/teams/enterprise/sco/spur/"
    case "$1" in
        ""|.) open "${base_url}/$(pwd | xargs -I {} basename '{}')" ;;
        *) open "${base_url}/$1" ;;
    esac
)

git_vix() ( git clone git@gitlab.com:heb-engineering/teams/enterprise/sco/"$1".git )
