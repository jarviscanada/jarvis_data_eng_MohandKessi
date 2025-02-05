# Introduction
Grep is a Java application that recursively searches for a given regex pattern in files within a specified directory. It reads lines from all files, filters the lines matching the pattern, and writes the results to an output file. This implementation utilizes Java Streams and Lambda expressions to enhance readability and performance.
# Quick Start
java -cp out ca.jrvs.apps.grep.JavaGrepLambdaImp "pattern" "searchDirectory" "outFile"

# Implemenation
## Pseudocode
process():
  list all files in the root directory
  initialize an empty list for matched lines
  for each file in the list:
    read lines from the file
    for each line:
      if line matches regex pattern:
        add to matched lines list
  write matched lines to the output file


## Performance Issue
This implementation reads all lines from each file into memory, which can cause memory overflow, when processing large files. To mitigate this, we could read and process files line-by-line using BufferedREader to reduce memory consumption.

# Test
To manually test the application:
1. Prepare sample files with different content types, including small and large files.
2. Run the application with various regex patterns to match different scenarios.
3. Test edge cases, such as empty files and binary files.
4. Compare the output file with expected results to ensure accuracy.
5. Measure performance when processing large files to detect potential bottlenecks.


# Deployment
### Using Docker
1. Pull the Docker image.
```bash
docker pull volta808/grep
```
2. Run the container, three arguments are needed: the pattern to search, the directory and the output file .
```bash
docker run --rm \
-v `pwd`/data:/data -v `pwd`/log:/log \
${docker_user}/grep .*Romeo.*Juliet.* /data /log/grep.out
```
# Improvement
1. Precompile regex pattern: Reduce redundant Pattern.compile() calls.
2. Buffered writing: improves efficiency when writing large output files.
3. Error handling: Handle exceptions instead of stopping the process.
