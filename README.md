# Tutorial: Creating a simple clicker game in Castle Game Engine and Castle Editor

## Abstract

This tutorial is aimed at new users, who have some minor knowledge of Pascal and general concepts of programming and game development. It will cover all steps necessary from creation and setting up of the project, to a small but finished game.

## Set up environment

For this tutorial we shall be using Lazarus IDE. We shan't use GUI provided by Lazarus, therefore this is not a requirement, just a personal preference. You can easily follow this tutorial in any other IDE capable of editing FreePascal code.

Before we start:

* Make sure that Lazarus + FPC are properly installed.

* Make sure Castle Game Engine packages are installed, compiled and working properly. We shall be using `castle_base.lpk` and `castle_window.lpk` packages. Try compiling a few examples from `examples` folder to make sure they're working

* Make sure that `alternative_castle_window_based_on_lcl.lpk` is _**NOT**_ installed in Lazarus, as it will conflict with window package we shall be using.

* Make sure system environment variables required for Castle Game Engine are set up properly.

* Make sure Castle Editor is compiled and ready to use.

## Creating a base project

@michaliskambi, _I've never ever been able to setup the environment variables correctly, and generating a new project from a template never worked for me neither on Linux nor on Windows. Therefore I'm cheating here - I'm creating the project in Lazarus and copy files from Castle Editor templates. Please, double-check if it makes sense._

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

If the Project Inspector is not visible on the screen, we may show it by going to Project -> Project Inspector menu:

![Project inspector menu](images/project-inspector-menu.png)

Note, that the required packages `castle-base` and `castle-window` are already included as dependencies of our project.

Next let's tweak a couple of Project Options, which can be found in Project -> Project Options:

![Project options menu](images/project-options-menu.png)

First of all, let's go to Project Options -> Miscellaneous and uncheck option "Main unit has Uses section containing all units of project". This will ensure our `lpr` file won't get spammed by all units we add to the game, and avoid compilation problems if we decide to rename our units at some point.

![Project options menu](images/project-options-miscellaneous.png)

Now let's go to any submenu of "Compiler Options" and click "..." button near "Build Modes: Default".

![Project options menu](images/project-options-compiler-options.png)

And there press the button "Create Debug and Release modes".

![Project options menu](images/project-options-build-modes.png)

Now our "Build Modes" screen should look like this:

![Project options menu](images/project-options-build-modes-2.png)

It is important to work on a project in "Debug" mode - which was conveniently autoselected for us already. Debug mode adds a lot of additional checks, that can really help with debugging the game, and also will report memory leaks if any.

Obviously, such additional checks slow down the program, so when we are satisfied with the result and are releasing the game to the world, we need to use "Release" mode, which will work faster.

Now we can close both "Build Modes" and "Project Options" windows by pressing OK (to save the changes) and we can go a bit deeper.

## Deleting a State

**State** is one of the game's view, like Main menu, Gameplay view, High scores, maybe some different States, like Inventory, Achievements, differnt gamemodes, etc.

As we noted above the "Empty" template is not actually empty - it already contains a Main State with a FPS label. We could have cleaned those up and reused, but we might want to use this opportunity to learn how to delete a state.

Simply deleting `state_main.castle-user-interface` and `gamestatemain.pas` is not enough, as they are referenced from within our game. Let's look more closely at our `gameinitialize.pas`. What we are interested in is the procedure `ApplicationInitialize`.

As you can see it will be called when the application (our game) will start up:

```Pascal
initialization
  ...
  Application.OnInitialize := @ApplicationInitialize;
```

That is this procedure contains everything that happens during the game startup - usually loading of game data and initialization of game States. Let's see what we have there right now (comments are removed for more compact view):

```Pascal
procedure ApplicationInitialize;
begin
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  StateMain := TStateMain.Create(Application);
  TUIState.Current := StateMain;
end;
```

So, first of all, we load `CastleSettings.xml` file by calling `Window.Container.LoadSettings`. It will set up our Window parameters. We could do this here by manually setting `Window` properties that we need to change from default, but having a `CastleSettings` file is usually more convenient and flexible. Moreover, it allows Castle Editor to know more about how our game should look like.

The next line `StateMain := TStateMain.Create(Application);` creates a singleton for `TStateMain` - the class that manages our Main State. Inside `GameStateMain` unit you can see that the class API (`interface`), `implementation` and also declaration of the singleton variable:

```Pascal
var
  StateMain: TStateMain; 
```

Next we set our `StateMain` as the `Current` state by calling `TUIState.Current := StateMain;`. 

We shall create our own states in the very same way, but for now we are going to delete this state. This means we need to delete files `state_main.castle-user-interface` and `gamestatemain.pas` and remove the state initialization from `gameinitialize.pas`.

Now our `ApplicationInitialize` will look like:

```Pascal
procedure ApplicationInitialize;
begin
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');
end;
```

and we also should adjust our `uses` section to remove the reference to `GameStateMain` unit. That would be just changing:

```Pascal
uses SysUtils,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleFilesUtils,
  CastleUIControls, CastleApplicationProperties,
  GameStateMain;
```

into

```Pascal
uses SysUtils,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleFilesUtils,
  CastleUIControls, CastleApplicationProperties;
```

Note that we also silently deleted a few other unused units from `uses` section, when you create a project from `Empty` template you may have a little more units there, but Lazarus will notify you in "Messages" window, that some units aren't used in the project:

![Unused units](images/unused-units.png)

It's very reasonable to check "Messages" from time to time to see if Lazarus hints us to change something, usually it's quite useful.

Now it's time to physically delete the files `state_main.castle-user-interface` and `gamestatemain.pas` - with any convenient file explorer.

Note, that in case we've added `gamestatemain.pas` to our Project Inspector earlier, it will now complain that the file is missing:

![Missing file](images/missing-file.png)

It won't trigger an error or any other trouble, but we also should remove it by right clicking it and selecting "Remove":

![Missing file](images/missing-file-remove.png)

Of course we can also use "Remove" button which is located above.

Now let's try to compile and run our project. We have a perfectly clean, truly "Empty" window. In case we missed some of the references to the deleted unit, we may run into compilation errors which in this case should be easy to fix, but after going through all the process above - we're all good and ready to start working!

## Creating a Main Menu State

### Create empty State

Make sure Castle Engine is running and our project is open. If not, then it's easy to open it by:

![Recent projects](images/recent-projects.png)

After the project opens we see the Castle Editor Designer window:

![Empty designer window](images/designer-empty.png)

Let's create our new state. There are many different user interface elements we can use as a root, but usually it's most convenient to have either Empty rectangle as root, or a specific user interface we are going to use. In this case let's create a State based on "Image (TCastleImageControl)" - so that we shall have an image for background.

![Create a new state](images/designer-create-state.png)

Now our window should look like this:

![Empty state](images/designer-empty-state.png)

### Create background image

On the left we see a "Hierarchy" - this is the tree of User Interface elements that are present within our State currently. For now it's only `ImageControl`. Let's select it by left-clicking in Hierarchy. Now on the right side we have a way to edit the properties of our user interface element:

![Image control](images/image-control-basic.png)

Let's change the `Name` field to "BackgroundImage" and select the actual image to display by clicking the `URL` field and then the button with three dots:

![Url field](images/url-field.png)

It will open the `data` folder of our project prompting us to select an image. All of our game data should be stored in this folder, so let's use our file manager to drop some assets there. For now let's add `images` subfolder and drop our background image there. Then our open window will look like this:

![Open image](images/open-image.png)

Let's select the background image and press "Open". We immediately see the image in our State:

![Open image result](images/open-image-result.png)

However, it doesn't seem to be exactly what we want - to behave as a background image, i.e. to fill the entire screen. To improve this behavior we need to go to "All" tab in properties of our ImageControl and set FullSize property to true:

![Full size image](images/full-size.png)

This will make our background image stretch horizontally and vertically to fill the entire parent User Interface. As this is our topmost user interface - it'll fill the entire window, which is exactly what we want here. Now our background behaves as we intend it to:

![Background image](images/background-image-result.png)

### Save State

As Castle Editor doesn't support backups and autosaving yet, we should get a habit of saving our work often - and it's a good habit anyway. To do this we press Design -> Save (or Save As):

![Save state](images/save-state-menu.png)

Let's name our state "MainMenu" and press "Save":

![Save state](images/save-state.png)

### Create Logo image

In simple words, Main Menu is a game logo + a set of buttons that would allow the player to start the game, change options, see credits, etc.

So first of all we add a logo of our game. The process is very similar to the one that we've been following when creating the empty State, but this time instead of creating a new State, we use Design -> Add User Interface Component like this:

![Add User Interface Component](images/add-user-interface.png)

And select an "Image (TCastleImageControl)". In the same way as we did with the background now we pick `images/buttonclickerlogo.png` as the image file. And our image is ready in our user interface:

![Title added](images/title-added.png)

Now we should position it in the upper center of our design. To do that we use "Layout" tab of the Castle Editor:

![Layout tab](images/layout-tab.png)

Let's set `HorizontalAnchorParent` (i.e. to which side of the Parent (currently that is `BackgroundImage`) we align our interface element) and HorizontalAnchorSelf (which side of this user interface element is aligned) both to `hpMiddle` (which stands for "horizontal position - Middle). And `VerticalAnchorParent` and `VerticalAnchorSelf` to `vpTop` (which stands for "vertical position - Top"). Now we should have something like:

![UI alignment](images/alignment.png)

Note, that the values we've changed from the default ones are highlighted in bold and dark red color.

Now we have a small detail - our logo is glued to the top of the design, which is not nice. We need to add a gap, which we can do by changing `VerticalAnchorDelta` property of our logo image. As we are moving the image _down_ we have to specify a _negative value_ here, let's say "-50". Now our image has some free space around it in the design:

![UI alignment](images/alignment2.png)

The same way if we'd wanted to move the image horizontally from the anchor, we'd have had to modify `HorizontalAnchorDelta` property.

Let's rename this logo image into `LogoImage` and we're done here.

### Create menu buttons

Next we need to create the actual menu buttons. This is a vertically-aligned set of similar items, which should make use of automatic arrangement, so that we shall have no problems adding/removing the buttons in future. This part is covered by "Vertical Group" UI element. We can add it by Design -> Add User Interface Component -> Vertical Group (TCastleVerticalGroup). Note, that first we should select the User Interface Component to which we add it - and it should be our `BackgroundImage`.

![Vertical Group](images/vertical-group.png)

Note, the item appearing in our design hierarchy:

![Vertical Group](images/vertical-group-hierarchy.png)

Now, we already know how to change this component's name to `MenuGroup`. Let's align it to the center of our design in a similar way how we aligned the logo, but this time using `hpMiddle` and `vpMiddle` anchors:

![Vertical Group](images/vertical-group-alignment.png)

Now it's time to add our menu buttons to the Vertical Group. Select `MenuGroup` and add a button by Design -> Add User Interface Component -> Button (TCastleButton) and name it `StartGameButton`:

![Start button](images/button-start.png)

Now before adding other buttons, let's fix its design. First of all let's assign a proper `Caption` to the button: `Start`.

![Start button](images/button-start-caption.png)

Then we want to have some nicely designed image for the button instead of standard box button image. This can be done in "All" tab by modifying "CustomBackground" properties:

![Start button image](images/button-start-custom-background.png)

First we should tick the "CustomBackground" checkbox and next let's click `CustomBackgroundNormal`:

![Start button image](images/button-start-custom-background-normal.png)

It's easy to see, that it looks very similar to how we were working with `TCastleImageControl` as both are using `TCastleImage` class underneath. For now we are interested in `URL` property - let's add an image here just like we did before for background and logo. The same way let's add URLs of images to `CustomBackgroundPressed` (this will be the image displayed when user presses the button) and `CustomBackgroundFocused` (the image displayed when the user hovers the mouse pointer over the button).

Now the button should look like this:

![Start button image](images/button-start-custom-background-small.png)

However, the button is too small, let's go to "Layout" tab and uncheck the `AutoSize` property:

![Start button image](images/auto-size-property.png)

Now the button fits in a 100x100 square:

![Start button image](images/button-start-custom-background-autosizeoff.png)

Now the button has size of `Width` and `Height` specified in the same tab. Let's change those values to 406x160:

![Start button image](images/button-start-custom-background-size.png)

Also let's change the font size, so that the text won't be so tiny. It can be done in "Basic" tab:

![Start button image](images/button-start-font-size.png)

Zero means "default" value, we'll learn to change it later. For now let's assign it to 80:

![Start button image](images/button-start-font-size-result.png)

Now, to avoid going through all this process again, we can just create copies of our already designed button. Let's select it and press Design -> Duplicate Component:

![Duplicate component](images/duplicate-component.png)

Now we have two identical buttons in the design. Let's create two more the same way, so that we have four buttons in total:

![Duplicate buttons](images/four-buttons.png)

Let's name the second button `OptionsButton` and set its caption "Options", then `CreditsButton` and set its caption "Credits" and finally `QuitButton` with "Quit" caption:

![Duplicate buttons](images/four-buttons-done.png)

And as a final touch let's add some spacing between the buttons by selecting `MenuGroup` and changing `Spacing` property in "Basic" tab:

![Duplicate buttons](images/four-buttons-done-spacing.png)

Let's set it to 20 and we're done with this part:

![Duplicate buttons](images/four-buttons-done-finish.png)


.......................................................

We would be aiming at mobile devices in Portrait orientation, which would define our design arrangement, but still the variety of mobile devices resolutions and aspect ratio is extremely high, and we have to make our interface flexible enough to fit most if not all of them.

Fortunately, Castle Game Engine does most of this dirty work for us.