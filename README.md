# castle-editor-tutorial

## Abstract

A Castle Editor tutorial, that shows how to create and use Castle User Interface.

## Set up environment

For this tutorial we shall be using Lazarus IDE. We shan't use GUI provided by Lazarus, therefore this is not a requirement, just a personal preference. You can easily follow this tutorial in any other IDE capable of editing FreePascal code.

Before we start:

* Make sure that Lazarus + FPC are properly installed.

* Make sure Castle Game Engine packages are installed, compiled and working properly. We shall be using `castle_base.lpk` and `castle_window.lpk` packages. Try compiling a few examples from `examples` folder to make sure they're working

* Make sure that `alternative_castle_window_based_on_lcl.lpk` is _**NOT**_ installed in Lazarus, as it will conflict with window package we shall be using.

* Make sure system environment variables required for Castle Game Engine are set up properly.

* Make sure Castle Editor is compiled and ready to use.

## Creating a base project

@michaliskambi, _I've never ever been able to setup the environment variables correctly, and generating a new project from a template never worked for me neither on Linux or on Windows. Therefore I'm cheating here - I'm creating the project in Lazarus and copy files from Castle Editor templates. Please, double-check if it makes sense._

Run Castle Editor and select "New Project":

![New project](images/new-project.png)

In this tutorial we'll be using the most basic template - "Empty". Write a name for the project and press "Create Project" button. Note that the name cannot start with a number. And it's best to avoid non-ASCII characters or spaces in project name or path.

![Create new project](images/new-project-create.png)

This will create a simple dummy project we can start filling with our game-specific code and assets.

Let's open freshly created `ButtonClickerGame.lpi` in Lazarus. We can immediately compile and run it. As it is a template, it's a fully functional minimalistic project, that will show us an empty dark gray window with a lone FPS counter. No, Empty project is not actually empty, but it contains:

- `ButtonClickerGame.lpi` - Lazarus project file. It is created and maintained automatically by Lazarus.

- `ButtonClickerGame.lpr` - a program initialization file, that was automatically created for us by Castle Editor and we don't need to change anything in there, maybe only update the proper game version as we would be working on it. You can open it in Lazarus or any text editor. `lpr` stands for "Lazarus project" but in everything else it's equal to a normal `pas` file.

- `CastleEngineManifest.xml` - a Castle Game Engine manifest file, that describes some important settings for the project that control how the project is compiled, packaged and distributed. For now we don't need to change anything inside.

- `gameinitialize.pas` - pascal unit for initialization of the game.

- `gamestatemain.pas` - pascal unit for Main State of the User Interface. It describes how this State should work.

- `data` folder, that contains game data. For now it's almost empty and contains only:

- - `CastleSettings.xml` - settings of our game project.

- - `state_main.castle-user-interface` - design of the Main State. We can edit it with Castle Editor.

- - `README.txt` - short reminder of what the `data` folder is and where to get additional information. We don't need this file and it can be deleted right away.

Before we finish this part, let's also update some settings in Lazarus for convenience of our future work.

Firs of all, we may want to have all the project units listed in Project Inspector. We can do that either by simply dragging them from the folder, or by pressing "Add" button.

![Project inspector](images/project-inspector.png)

Note, that the required packages `castle-base` and `castle-window` are already included as dependencies of our project.

