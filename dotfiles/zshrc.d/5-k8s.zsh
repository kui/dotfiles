if kubectl completion zsh &> /dev/null; then
    source <(kubectl completion zsh)
    alias kg="kubectl get"
    alias kgp="kubectl get pods"
    alias kgs="kubectl get services"
    alias kgd="kubectl get deployments"
    alias kd="kubectl describe"
    alias kr="kubectl rollout"
    alias kl="kubectl logs"
fi

if kustomize completion zsh &> /dev/null; then
    source <(kustomize completion zsh)
fi

if minikube completion zsh &> /dev/null; then
    source <(minikube completion zsh)
fi

if helm completion zsh &> /dev/null; then
    source <(helm completion zsh)
fi

source_if_exist ~/golang/src/github.com/bonnefoa/kubectl-fzf/kubectl_fzf.plugin.zsh
