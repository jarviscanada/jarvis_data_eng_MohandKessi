# Introduction
This project is designed to monitor and log hardware specifications and usage metrics for a set of Linux servers. The project automates data collection and storage using shell scripts, PostgreSQL for data storage, and Docker for isolation. Key technologies include Bash scripting for automation, Git for version control, and Crontab for automation. This solution ensures easy deployment and scalability across multiple Linux hosts.

# Quick Start
```
# Create a container using the psql_docker bash script
./psql_docker.sh create username password

# Start the PostgreSQL container
./psql_docker.sh start

# Create the tables using the ddl script (located in sql/)
psql -U username -f ddl.sql

# Insert hardware specs
./host_info.sh host port database username password 

# Insert real-time hardware usage metrics 
./host_usage.sh host port database username password
```

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
| **Column**           | **Data Type**   | **Description**                      | **Constraints**       |
|-----------------------|-----------------|--------------------------------------|-----------------------|
| `id`                 | SERIAL          | Unique identifier for the host       | ⭐ Primary Key        |
| `hostname`           | VARCHAR         | Hostname of the machine              | UNIQUE               |
| `cpu_numer`          | INT2            | Number of CPUs                       |                       |
| `cpu_architecture`   | VARCHAR         | CPU architecture (e.g., x86, ARM)    |                       |
| `cpu_model`          | VARCHAR         | Model name of the CPU                |                       |
| `cpu_mhz`            | FLOAT8          | CPU clock speed in MHz               |                       |
| `l2_cache`           | INT4            | Size of L2 cache in KB               |                       |
| `timestamp`          | TIMESTAMP       | Time of host data collection         |                       |
| `total_mem`          | INT4            | Total memory                         |                       |

### host_usage
| **Column**         | **Data Type**   | **Description**                  | **Constraints**       |
|---------------------|-----------------|----------------------------------|-----------------------|
| `timestamp`        | TIMESTAMP       | Time of data collection          |                       |
| `host_id`          | SERIAL          | Identifier for the host          | FK (Foreign Key)      |
| `memory_free`      | INT4            | Free memory in bytes             |                       |
| `cpu_idle`         | INT2            | CPU idle time percentage         |                       |
| `cpu_kernel`       | INT2            | CPU kernel time percentage       |                       |
| `disk_io`          | INT4            | Disk I/O operations count        |                       |
| `disk_available`   | INT4            | Available disk space in bytes    |                       |




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
