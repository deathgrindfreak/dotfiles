#!/usr/bin/env sh

generate_docker_memory_plot() {
    echo "Saving memory data for $1 to $2"
    while true; do
        docker stats --no-stream \
            | grep "$1" \
            | awk '
                {
                  if(index($4, "GiB")) {
                    gsub("GiB","",$4); print $4 * 1000
                  } else {
                    gsub("MiB","",$4); print $4
                  }
                }
            ' >> "$2"
    done
}

plot_docker_stats() {
    gnuplot docker-stats.gnu
}
