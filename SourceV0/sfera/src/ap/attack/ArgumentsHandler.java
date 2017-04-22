/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ap.attack;

import java.util.ArrayList;
import java.util.Arrays;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

/**
 *
 * @author mmaouche
 */
public class ArgumentsHandler {

    private String[][] optionValues = {
        {"o", "output", "path"},
        {"kd", "known-data", "path"},
        {"ud", "unknown-data", "path"},
        {"d", "dataset", "path"},
        {"p", "proportion", "0 < value < 1"},
        {"c", "cell-size", "value (meters)"},};
    private String workdir = "";
    private String kd = "";
    private String ud = "";
    private String dataset = "";
    private String proportion = "";
    private String cellSize = "";

    private String[] args = {""};

    public ArgumentsHandler(String[] args) {
        this.args = args;
    }

    public void printAttributes() {
        System.out.println("Attributes : workdir=" + workdir + " , kd=" + kd + " , ud=" + ud + " , dataset=" + dataset + " , proportion=" + proportion + " , cellSize=" + cellSize);
    }

    public void addOption(Options options, String name, String longOpt, String argName) {
        options.addOption(Option.builder(name)
                .longOpt(longOpt)
                .argName(argName)
                .hasArg()
                .valueSeparator()
                .build());
    }

    public void parseArguments(String[] args) {
        System.out.println("Args : \n -------------------------------------------------");
        for (String a : args) {
            System.out.println(a);
        }

        Options options = createOptions();
        CommandLineParser parser = new DefaultParser();
        try {
            // parse the command line arguments
            CommandLine line = parser.parse(options, args);
            if (line.hasOption("o")) {
                workdir = line.getOptionValue("o");
            }
            if (line.hasOption("kd")) {
                kd = line.getOptionValue("kd");
            }
            if (line.hasOption("ud")) {
                ud = line.getOptionValue("ud");
            }
            if (line.hasOption("d")) {
                dataset = line.getOptionValue("d");
            }
            if (line.hasOption("p")) {
                proportion = line.getOptionValue("p");
            }
            if (line.hasOption("c")) {
                cellSize = line.getOptionValue("c");
            }

        } catch (ParseException exp) {
            // oops, something went wrong
            System.out.println("Parsing failed.  Reason: " + exp.getMessage());
        }

    }

    public void parseArguments() {
        this.parseArguments(this.args);
    }

    public Options createOptions() {
        // create Options object
        Options options = new Options();
        for (String[] value : optionValues) {
            addOption(options, value[0], value[1], value[2]);
        }
        return options;
    }

    public String getWorkdir() {
        return workdir;
    }

    public String getKd() {
        return kd;
    }

    public String getUd() {
        return ud;
    }

    public String getDataset() {
        return dataset;
    }

    public String getProportion() {
        if(proportion.isEmpty()) return "";
        try {
            Double.parseDouble(proportion);
        } catch (NumberFormatException e) {
            System.out.println("Error : Wrong number format for -p/--proportion, got : " + proportion);
            System.exit(0);
        }
        return proportion;
    }

    public String getCellSize() {
        if(cellSize.isEmpty()) return "";
        try {
             Double.parseDouble(cellSize);
             
        } catch (NumberFormatException e) {
            System.out.println("Error : Wrong number format for -c/--cell-size, got : " + cellSize);
            System.exit(0);
        }
        return cellSize;
    }

    public ArrayList<String> getCommandLine() throws Exception {
        String json = "";
        String params = "";
        ArrayList<String> cmds = new ArrayList<>();
        cmds.add("run");
        if(!workdir.isEmpty()) cmds.addAll(Arrays.asList("-workdir",workdir));
        if(ud.isEmpty()){
            System.out.println("one Source JSON");
            json += "oneSource.json";
            if(!dataset.isEmpty()) params += "urlES="+dataset;
            else if(!kd.isEmpty()) params += "urlES="+kd;
            else throw new Exception("At least one dataset has to entered with -d path");
            if(!proportion.isEmpty()) params+= " endTrain=" + proportion + " startTest=" + proportion;
        }else{
            System.out.println("two Source JSON");
            if(!proportion.isEmpty()) System.out.println("-p option ignored since two datasets for train and test were already informed");
            json += "twoSource.json";
            params = " urlUD="+ ud;                 
            if(!kd.isEmpty()) params += " urlKD="+kd;
            else if(!dataset.isEmpty()) params += " urlKD="+dataset;
            else  throw new Exception("-ud option given but -kd was not informed");
        }
        if(!cellSize.isEmpty()) params += " cellSize="+cellSize;
        cmds.add("-params");
        cmds.add(params);
        cmds.add(json);
    
        return cmds;
    }
}
