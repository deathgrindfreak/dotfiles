pr() (
    open "https://github.com/VeracityInnovations/$(pwd | xargs -I {} basename '{}')/pull/new/$(git symbolic-ref --short HEAD)"
)

gh() (
    case "$1" in
        ""|.) open "https://github.com/VeracityInnovations/$(pwd | xargs -I {} basename '{}')" ;;
        *) open "https://github.com/VeracityInnovations/$1" ;;
    esac
)

git_vix() ( git clone git@github.com:VeracityInnovations/"$1".git )

list_vix_repos() (
    local api_key
    api_key="$(jq -r '.githubToken' < ~/.vix/cli-config.json)"
    for page in {1..7}; do
        curl -s -u deathgrindfreak:"$api_key" https://api.github.com/orgs/VeracityInnovations/repos\?per_page\=100\&page\="$page" \
            | jq -r "map(.name)[]"
    done
)

list_outdated_vix_deps() (
    yarn outdated --json \
        | jq -r 'select(.type == "table")
                    | .data.body[]
                    | select(.[0] | match("vix"))
                    | select(.[4] == "dependencies")
                    | .[0]'
)

list_outdated_vix_dev_deps() (
    yarn outdated --json \
        | jq -r 'select(.type == "table")
                    | .data.body[]
                    | select(.[0] | match("vix"))
                    | select(.[4] == "devDependencies")
                    | .[0]'
)

update_vix_deps() (
    for dep in $(list_outdated_vix_deps); do { yarn add "$dep" } done
    for dep in $(list_outdated_vix_dev_deps); do { yarn add --dev "$dep" } done
)
