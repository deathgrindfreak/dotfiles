export PATH="$PATH:$HOME/Library/scripts/bin"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

export PATH="$PATH:$HOME/build/stack/:$HOME/.local/bin:$HOME/Library/Haskell/bin:$HOME/.cabal/bin"

# ConTeXt install path
export PATH="$PATH:$HOME/context/tex/texmf-osx-64/bin"

# Add terraform plan and apply
export PATH="$PATH:$HOME/src/provision-ops/bin"

# brew Cellar
export PATH="$PATH:/usr/local/Cellar/"

export PATH="$PATH:$HOME/.emacs.d/bin"

export PATH="$PATH:/Applications/Emacs.app/"

# J language
export PATH="$PATH:/Applications/j903/bin/"

export PATH="$PATH:${HOME}/Library/Python/3.9/bin"

# z command
. $HOME/bin/z/z.sh

# default browser
export BROWSER=firefox

# default editor
export EDITOR='emacsclient -c -nw'

# history settings
export HISTCONTROL=ignoredups
export HISTSIZE=500000
export HISTFILESIZE="$HISTSIZE"

# pkg-config path
export PKG_CONFIG_PATH=/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib/i386-linux-gnu/pkgconfig/:/usr/include/

# chicken scheme
export CSC_OPTIONS="-I/usr/include/iup"

# add support for ctrl+o to open selected file in VS Code
export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(code {})+abort'"

export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig"

# lisp programs
export PATH="$PATH:$HOME/dotfiles/lisp/bin"
export MANPATH="$MANPATH:$HOME/dotfiles/lisp/man"

export PROMPT_COMMAND='if [ "$(id -u)" -ne 0 ]; then echo "$(date "+%Y-%m-%d.%H:%M:%S") $(pwd) $(history 1)" >> ~/.logs/bash-history-$(date "+%Y-%m-%d").log; fi'

export GTAGSLABEL=pygments

export PGPASSFILE="$HOME/.pgpass"

export SECRETS_DIR="$HOME/src/supplier-core/secrets"
export SECRETS_SCRIPTS_DIR="${SECRETS_DIR}/script-config"

export KPS_PROD_API_TOKEN="$(yq '.spurClientConfig.apiHubKey_enc' ${SECRETS_SCRIPTS_DIR}/kps-prod.dec.yml)"
export KPS_PROD_INTEGRATION="$(yq '.spurClientConfig.spurIntegrationToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-prod.dec.yml)"
export KPS_PROD_PRIVATE="$(yq '.spurClientConfig.spurPrivateToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-prod.dec.yml)"
export KPS_PROD_PUBLIC="$(yq '.spurClientConfig.spurPublicToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-prod.dec.yml)"

export KPS_PREPROD_API_TOKEN="$(yq '.spurClientConfig.apiHubKey_enc' ${SECRETS_SCRIPTS_DIR}/kps-preprod.dec.yml)"
export KPS_PREPROD_INTEGRATION="$(yq '.spurClientConfig.spurIntegrationToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-preprod.dec.yml)"
export KPS_PREPROD_PRIVATE="$(yq '.spurClientConfig.spurPrivateToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-preprod.dec.yml)"
export KPS_PREPROD_PUBLIC="$(yq '.spurClientConfig.spurPublicToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-preprod.dec.yml)"

export KPS_UAT_API_TOKEN="$(yq '.spurClientConfig.apiHubKey_enc' ${SECRETS_SCRIPTS_DIR}/kps-uat.dec.yml)"
export KPS_UAT_INTEGRATION="$(yq '.spurClientConfig.spurIntegrationToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-uat.dec.yml)"
export KPS_UAT_PRIVATE="$(yq '.spurClientConfig.spurPrivateToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-uat.dec.yml)"
export KPS_UAT_PUBLIC="$(yq '.spurClientConfig.spurPublicToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-uat.dec.yml)"

export KPS_CERT_API_TOKEN="$(yq '.spurClientConfig.apiHubKey_enc' ${SECRETS_SCRIPTS_DIR}/kps-cert.dec.yml)"
export KPS_CERT_INTEGRATION="$(yq '.spurClientConfig.spurIntegrationToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-cert.dec.yml)"
export KPS_CERT_PRIVATE="$(yq '.spurClientConfig.spurPrivateToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-cert.dec.yml)"
export KPS_CERT_PUBLIC="$(yq '.spurClientConfig.spurPublicToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-cert.dec.yml)"

export KPS_DEV_API_TOKEN="$(yq '.spurClientConfig.apiHubKey_enc' ${SECRETS_SCRIPTS_DIR}/kps-dev.dec.yml)"
export KPS_DEV_INTEGRATION="$(yq '.spurClientConfig.spurIntegrationToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-dev.dec.yml)"
export KPS_DEV_PRIVATE="$(yq '.spurClientConfig.spurPrivateToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-dev.dec.yml)"
export KPS_DEV_PUBLIC="$(yq '.spurClientConfig.spurPublicToken_enc' ${SECRETS_SCRIPTS_DIR}/kps-dev.dec.yml)"

export KPS_LOCAL_API_TOKEN="$(yq '.spurClientConfig.apiHubKey_enc' ${SECRETS_SCRIPTS_DIR}/local.dec.yml)"
export KPS_LOCAL_INTEGRATION="$(yq '.spurClientConfig.spurIntegrationToken_enc' ${SECRETS_SCRIPTS_DIR}/local.dec.yml)"
export KPS_LOCAL_PRIVATE="$(yq '.spurClientConfig.spurPrivateToken_enc' ${SECRETS_SCRIPTS_DIR}/local.dec.yml)"
export KPS_LOCAL_PUBLIC="$(yq '.spurClientConfig.spurPublicToken_enc' ${SECRETS_SCRIPTS_DIR}/local.dec.yml)"

export KPS_PSPUR_DB_PASS="$(yq '.env.KPS_PSPUR_DB_PASS_enc' ${SECRETS_DIR}/gcloud.dec.yaml)"
export KPS_USPUR_DB_PASS="$(yq '.env.KPS_USPUR_DB_PASS_enc' ${SECRETS_DIR}/gcloud.dec.yaml)"
