#!/bin/bash

psql_host=$1
psql_port=$2
db_name=$3
psql_user=$4
psql_password=$5

if [ "$#" -ne 5 ]; then
    echo "Illegal number of parameters"
    exit 1
fi

lscpu_out=$(lscpu)

hostname=$(hostname -f)
cpu_number=$(echo "$lscpu_out" | grep "^CPU(s):" | awk '{print $2}' | xargs)

# CPU architecture
cpu_architecture=$(echo "$lscpu_out" | grep "Architecture:" | awk '{print $2}' | xargs)

# CPU model
cpu_model=$(echo "$lscpu_out" | grep "Model name:" | cut -d ':' -f2 | xargs)

# CPU MHz
cpu_mhz=$(echo "$lscpu_out" | grep "CPU max MHz:" | awk '{print $4}' | xargs)

# L2 Cache
l2_cache=$(echo "$lscpu_out" | grep "L2 cache:" | awk '{print $3}' | xargs)

# Total memory in MB
total_mem=$(vmstat --unit M | tail -1 | awk '{print $4}')

# Current timestamp in `2019-11-26 14:40:19` format
timestamp=$(date '+%Y-%m-%d %H:%M:%S')



insert_stmt="INSERT INTO host_info(hostname, cpu_number, cpu_architecture, cpu_model, cpu_mhz, L2_cache, total_mem, timestamp)
VALUES('$hostname', '$cpu_number', '$cpu_architecture', '$cpu_model', '$cpu_mhz', '$l2_cache', '$total_mem', '$timestamp');"

export PGPASSWORD=$psql_password
psql -h $psql_host -p $psql_port -d $db_name -U $psql_user -c "$insert_stmt"
exit $?
