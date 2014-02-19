
# Simple E program for running an algorithm module

def args := interp.getArgs()
if (args.size() == 0 || args.size() > 2) {
    throw("usage: eloader.e modulename [input file]")
}

def algorithm := <import>[args[0]]

if (args.size() == 2) {
    def result := algorithm.run()
} else {
    def input := <file>[[args[1]]].deepReadOnly()
    def result := algorithm.run(input)
}

# Other way of presenting results than print?
print(result)