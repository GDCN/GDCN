
import java.util.*;
import java.io.*;

/*
 * Program for running an other program that needs interactive input
 *
 * Usage: java TestInteractive <command> _START <input>
 * <command> may be multiple words
 * _START signals when command stops and input starts
 * <input> may be a string to input or a wait signal
 * Wait signals are written as _tX where X is time to wait in milliseconds
 *
 * Example usage: java TestInteractive more -u _START "hello world" _t1000 "1 second later" _t1000 end
 */
public class TestInteractive {

    public static void main(String[] args) throws IOException, InterruptedException {
	List<String> cmdList = new ArrayList<>();
	
	boolean initState = true;
	Process proc = null;

	OutputStream out = null; 
	BufferedWriter writer = null;
	
	for (int i = 0; i < args.length; i++) {
	    if (initState) {
		if (args[i].equals("_START")) {
		    ProcessBuilder pb = new ProcessBuilder(cmdList);
		    pb.redirectOutput(ProcessBuilder.Redirect.INHERIT);
		    proc = pb.start();
		    out = proc.getOutputStream();
		    writer = new BufferedWriter(new OutputStreamWriter(out));
		    initState = false;
		}
		else {
		    cmdList.add(args[i]);
		}
	    }
	    else {
		if (args[i].startsWith("_t")) {
		    int time = Integer.parseInt(args[i].substring(2));
		    Thread.sleep(time);
		}
		else {
		    writer.write(args[i]);
		    writer.newLine();
		    writer.flush();
		}
	    }
	}
	
	writer.write("\u001a");
	writer.close();
	out.close();
	proc.waitFor();

    }
}
