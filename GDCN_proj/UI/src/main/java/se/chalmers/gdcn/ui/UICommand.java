package se.chalmers.gdcn.ui;

import se.chalmers.gdcn.communicationToUI.WordInterface;

import java.util.List;

/**
* Created by Leif on 2014-02-17.
*/
public interface UICommand {
    public void execute(List<String> args);
    public WordInterface getWord();
}
