# Introduction
This project is designed to monitor and log hardware specifications and usage metrics for a set of Linux servers. The project automates data collection and storage using shell scripts, PostgreSQL for data storage, and Docker for isolation. Key technologies include Bash scripting for automation, Git for version control, and Crontab for automation. This solution ensures easy deployment and scalability across multiple Linux hosts.

# Quick Start
## Step 1: Create a container using the psql_docker bash script
./psql_docker.sh create username password

## Step 2: Start the PostgreSQL container
./psql_docker.sh start

## Create the tables using the ddl script (located in sql/)
psql -U username -f ddl.sql

## Insert hardware specs
./host_info.sh host port database username password 

## Insert real-time hardware usage metrics 
./host_usage.sh host port database username password


# Implementation
## Architecture
TODO

## Scripts
`psql_docker.sh`

Purpose: Manages the PostgreSQL container

Usage: ./psql_docker.sh start|stop|create


`host_info.sh`

Purpose: Collects static hardware information and stores it in host_info table

Usage: ./host_info.sh host port database username password


`host_usage.sh`

Purpose: Collects real-time usage metrics and stores them in the host_usage table

Usage: ./host_usage.sh host port database username password


`sql/ddl.sql`

Purpose: Create the host_info and host_usage tables if they don't exist

Usage: psql -U username -f ddl.sql

## Database Modeling
### host_info
`id` SERIAL ⭐

`hostname` VARCHAR UNIQUE 

`cpu_numer` INT2

`cpu_architecture` VARCHAR

`cpu_model` VARCHAR

`cpu_mhz` FLOAT8

`l2_cache` INT4

`timestamp` TIMESTAMP

`total_mem` INT4


### host_usage
`timestamp` TIMESTAMP

`host_id` SERIAL FK

`memory_free` INT4

`cpu_idle` INT2

`cpu_kernel` INT2

`disk_io` INT4

`disk_available` INT4


## Tests
Each script was thoroughly tested on a local environment and Linux VMs
DDL Script : Exectued on an empty database to ensure tables are created correctly
Shell Scripts: Validated by running them with known inputs and comparing database entires

## Deployment
The project is deployed using:
GitHub: For Sharing Code.
Git: For version control.
Docker: To containerize the PostgreSQL database for easy setup.
Crontab: To automate the periodic execution of the host_usage.sh script

## Improvements
1. Adding support for hardware updates in the host_info
2. Developing a web dashboard for real-time visualization of hardware metrics.
3. Enhance error handling
