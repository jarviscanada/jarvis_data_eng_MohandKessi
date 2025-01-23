package ca.jrvs.apps.grep;


import com.sun.org.slf4j.internal.Logger;
import com.sun.org.slf4j.internal.LoggerFactory;
import org.apache.log4j.BasicConfigurator;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

public class JavaGrepImp implements JavaGrep {
    final Logger logger = LoggerFactory.getLogger(JavaGrep.class);

    private String regex;
    private String rootPath;
    private String outFile;

    public static void main(String[] args) {
        if (args.length != 3) {
            throw new IllegalArgumentException("USAGE: JavaGrep regex rootPath outFile");
        }

        // Use default logger config
        BasicConfigurator.configure();

        JavaGrepImp javaGrepImp = new JavaGrepImp();
        javaGrepImp.setRegex(args[0]);
        javaGrepImp.setRootPath(args[1]);
        javaGrepImp.setOutFile(args[2]);
        try {
            javaGrepImp.process();
        } catch (Exception ex) {
            javaGrepImp.logger.error("Error: Unable to process", ex);
        }
    }



    public String getRegex() {
        return regex;
    }

    public void setRegex(String regex) {
        this.regex = regex;
    }

    @Override
    public void process() throws IOException {
        List<String> matchedLines = new ArrayList<String>();
        for(File file : listFiles(getRootPath())){
            List<String> listedStrings = readLines(file);
            for(String text : listedStrings){
                if(containsPattern(text)){
                    matchedLines.add(text);
                }
            }
        }
        writeToFile(matchedLines);
    }

    @Override
    public List<File> listFiles(String rootDir) {
        File rootFile = new File(rootDir);
        if (!rootFile.exists() || !rootFile.isDirectory()) {
            logger.error("Invalid root directory: " + rootDir);
            return Collections.emptyList(); // Return an empty list if the directory is invalid
        }

        File[] filesList = rootFile.listFiles();
        List<File> resultList = new ArrayList<>();
        if (filesList != null) {
            for (File file : filesList) {
                if (file.isDirectory()) {
                    resultList.addAll(listFiles(file.getAbsolutePath()));
                } else {
                    resultList.add(file);
                }
            }
        }
        return resultList;
    }


    @Override
    public List<String> readLines(File inputFile) {
        //Return empty list if the inputFIle is null
        if(!inputFile.exists()) {
            return Collections.emptyList();
        }

        List<String> lines = new ArrayList<String>();
        try(BufferedReader br = new BufferedReader(new FileReader(inputFile))){
            String line;
            while((line = br.readLine()) != null){
                lines.add(line);
            }
        }catch(IOException e){
            logger.error("Error reading file : " + inputFile.getAbsolutePath(), e);
        }
        return lines;
    }

    @Override
    public boolean containsPattern(String line) {
        Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
        return pattern.matcher(line).find();
    }

    @Override
    public void writeToFile(List<String> lines) throws IOException {
        BufferedWriter bw = new BufferedWriter(new FileWriter(outFile));
        for(String line : lines){
            bw.write(line + System.lineSeparator());
        }
        bw.close();
    }

    public String getRootPath() {
        return rootPath;
    }

    public void setRootPath(String rootPath) {
        this.rootPath = rootPath;
    }

    public String getOutFile() {
        return outFile;
    }

    public void setOutFile(String outFile) {
        this.outFile = outFile;
    }
}
