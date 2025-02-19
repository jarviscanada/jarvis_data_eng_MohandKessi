package ca.jrvs.apps.grep;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class JavaGrepLambdaImp implements JavaGrep {

  private String regex;
  private String rootPath;
  private String outFile;

  @Override
  public void process() throws IOException {
    List<File> files = listFiles(getRootPath());
    List<String> matchedLines = files.stream()
        .flatMap(file -> {
          try {
            return readLines(file).stream();
          } catch (Exception e) {
            throw new RuntimeException("Error reading file: " + file.getAbsolutePath(), e);
          }
        })
        .filter(this::containsPattern)
        .collect(Collectors.toList());

    // Write matching lines to the output file
    writeToFile(matchedLines);
  }

  @Override
  public List<File> listFiles(String rootDir) {
    Path rootPath = Paths.get(rootDir);
    if (!Files.exists(rootPath) || !Files.isDirectory(rootPath)) {
      System.err.println("Error: Root directory does not exist or is not a directory: " + rootDir);
      return Collections.emptyList();
    }

    try (Stream<Path> paths = Files.walk(rootPath)) {
      return paths
          .filter(Files::isRegularFile)
          .map(Path::toFile)
          .collect(Collectors.toList());
    } catch (IOException e) {
      throw new RuntimeException("Error listing files in directory: " + rootDir, e);
    }
  }

  @Override
  public List<String> readLines(File inputFile) {
    try (Stream<String> lines = Files.lines(inputFile.toPath())) {
      return lines.collect(Collectors.toList());
    } catch (IOException e) {
      throw new RuntimeException("Error reading file: " + inputFile.getAbsolutePath(), e);
    }
  }

  @Override
  public boolean containsPattern(String line) {
    return Pattern.compile(regex, Pattern.CASE_INSENSITIVE)
        .matcher(line)
        .find();
  }

  @Override
  public void writeToFile(List<String> lines) throws IOException {
    Files.write(Paths.get(outFile), lines);
  }

  @Override
  public String getRegex() {
    return regex;
  }

  @Override
  public void setRegex(String regex) {
    this.regex = regex;
  }

  @Override
  public String getRootPath() {
    return rootPath;
  }

  @Override
  public void setRootPath(String rootPath) {
    this.rootPath = rootPath;
  }

  @Override
  public String getOutFile() {
    return outFile;
  }

  @Override
  public void setOutFile(String outFile) {
    this.outFile = outFile;
  }

  public static void main(String[] args) {
    if (args.length != 3) {
      throw new IllegalArgumentException("USAGE: JavaGrepLambdaImp regex rootPath outFile");
    }

    JavaGrepLambdaImp grepApp = new JavaGrepLambdaImp();
    grepApp.setRegex(args[0]);
    grepApp.setRootPath(args[1]);
    grepApp.setOutFile(args[2]);

    try {
      grepApp.process();
    } catch (IOException e) {
      System.err.println("Error processing files: " + e.getMessage());
    }
  }
}