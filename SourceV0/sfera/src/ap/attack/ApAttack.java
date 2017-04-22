/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ap.attack;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import static java.lang.Thread.sleep;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.cli.*;

/**
 *
 * @author mmaouche
 */
public class ApAttack {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
       // try {
            ArgumentsHandler parser = new ArgumentsHandler(args);
            parser.parseArguments(args);
            parser.printAttributes();
         
            
            try {
            ArrayList<String> cmds = parser.getCommandLine();
            JarExecutor exec=  new JarExecutor();

            exec.executeJar("lib/accio.jar", cmds);
            String log =  exec.getExecutionLog();
            System.out.println(log);
            
            } catch (Exception ex) {
            ex.printStackTrace();
            }

    }

}
