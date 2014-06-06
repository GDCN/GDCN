GDCN - General decentralised computation network
====
Distribute ALL the computations!

Latest version can be found on [GitHub](https://github.com/GDCN/GDCN).  
The program is platform independent but requires `java` and [`ghc`](http://www.haskell.org/ghc/) to be installed in beforehand.

## Compilation
Compilation is done with Maven to a runnable jar.

Build project (with tests):  
`mvn clean install`

Build projects but skip tests:  
`mvn -Dmaven.test.skip=true clean install`

or simply run:  
`build.sh`


## Running the program
After compilation, a runnable jar will appear in `GDCN_proj/target`. Run it with the command:  
`java -jar GDCN\_console\_*.jar [options]`

__Options__:  
`-nosplash`
for not printing the Splash screen

The script `run.sh` simply runs the jar in a new `screen`.

## Getting started
You can exit the program at all times by writing `stop` followed by `exit`. To see a list of all commands, type `help`.

The first time you run the program, you must type `install`. This will create a new directory `~/.gdcn/` which will contain all program files. If you want some of the files to be put in a custom location, you can edit `pathdata.prop` and restart the program.

In order to connect to any other node, you must first run `start`. You can also set a specific port to listen to by typing `start <port>` instead; default is `4001`. Be sure to run `stop` before exiting to properly close the port the program is listening to.

To connect to a different peer, you must know its IP-address. Either you can just connect to it by running `bootstrap <ip> <port>` or you can start working for it with `work <ip> <port>`. The command `autowork` will work continually for the specified peer until there are no tasks left to compute.

## Creating new tasks
Example computations can be found in the folder `GDCN_proj/dGDCN`. This folder mirrors the anticipated structure of `~/.gdcn`. Computations are released in groups at a time, called a `job`. A single computation is called a `task`.

One of the example computations is a simple Prime calculation found in: `jobs/Job1`. Each task is written as a `.json` in `Job1/tasks/` which contains information about which Haskell module the task should run and what resource files it will use as input data. Any Haskell modules should be located in `Job1/code` and input files in `Job1/resources`. For efficient validation, a quality function `quality.hs` should also be written that can evaluate the quality of a given result.

The location of jobs can be customized by editing the `pathdata.prop` in `~/.gdcn`.

To load a job into the program, after running `start`, type `push <jobname>`. In this example `push Job1`. The necessary files will be uploaded so that connecting peers can find them. Now, you can just wait for connecting workers to start working on your computations.

_The ability to set custom values for minimal replication and minimal reputation for each job is an upcoming feature. Currently, you would have to edit the default values in code in `se.chalmers.gdcn.replica.ReplicaManagerBuilder`._

