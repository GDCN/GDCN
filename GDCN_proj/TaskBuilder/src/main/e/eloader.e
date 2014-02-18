
# Simple E program for running an algorithm module

def args := interp.getArgs()
if (args.size() != 1) {
    throw("usage: eloader.e modulename")
}

def algorithm := <import>[args[0]]
def result := algorithm.run()

# Other way of presenting results than print?
print(result)