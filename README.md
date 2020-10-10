# Tutorial: Creating a simple clicker game in Castle Game Engine and Castle Editor

## Abstract

This tutorial is aimed at new users, who have some minor knowledge of Pascal and general concepts of programming and game development. It will cover all steps necessary from creation and setting up of the project, to a small but finished game.

## Set up environment

For this tutorial we shall be using Lazarus IDE. We shan't use GUI provided by Lazarus, therefore this is not a requirement, just a personal preference. You can easily follow this tutorial in any other IDE capable of editing FreePascal code.

Before we start:

* Make sure that Lazarus + FPC are properly installed.

* Make sure Castle Game Engine packages are installed, compiled and working properly. We shall be using `castle_base.lpk` and `castle_window.lpk` packages.

* Make sure that `alternative_castle_window_based_on_lcl.lpk` is _**NOT**_ installed in Lazarus, as it will conflict with `castle_window.lpk` package we shall be using.

* Make sure the required libraries are available on system $PATH. Try compiling a few examples from `examples` folder to make sure they're working.

* Make sure system environment variables required for Castle Game Engine and Castle Editor are set up properly.

* Make sure Castle Editor is compiled and ready to use.

## Creating a base project

@michaliskambi, _I've never ever been able to setup the environment variables correctly, and generating a new project from a template never worked for me neither on Linux nor on Windows. Therefore I'm cheating here - I'm creating the project in Lazarus and copy files from Castle Editor templates. Please, double-check if it makes sense._

Run Castle Editor and select "New Project":

![New project](images/new-project.png)

In this tutorial we'll be using the most basic template - "Empty". Write a name for the project and press "Create Project" button. Note that the name cannot start with a number. And it's best to avoid non-ASCII characters or spaces in project name or path. Also it's a good idea to limit the path to the project to 255 symbols, which might cause bugs on some exotic platforms.

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

### Running the Project

...

## Deleting a State

**State** is one of the game's view, like Main menu, Gameplay view, High scores, maybe some different States, like Inventory, Achievements, different game modes, etc.

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
  CastleUIControls, CastleApplicationProperties, CastleUiState
  GameStateMain;
```

into

```Pascal
uses SysUtils,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleFilesUtils,
  CastleUIControls, CastleApplicationProperties, CastleUiState;
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

Let's create our new state. There are many different user interface elements we can use as a root, but usually it's most convenient to have either Empty rectangle as root, or a specific user interface we are going to use. In this case let's create a State based on "Image" - so that we shall have an image for background. That is: Design -> New User Interface (Chosen Root) -> Image (TCastleImageControl).

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

However, it doesn't seem to be exactly what we want - to behave as a background image, i.e. to fill the entire screen. To improve this behavior we need to go to "All" tab in properties of our ImageControl and set `FullSize` property to true:

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

Let's set `HorizontalAnchorParent` (i.e. to which side of the Parent (currently that is `BackgroundImage`) we align our interface element) and HorizontalAnchorSelf (which side of this user interface element is aligned) both to `hpMiddle` (which stands for "horizontal position - Middle"). And `VerticalAnchorParent` and `VerticalAnchorSelf` to `vpTop` (which stands for "vertical position - Top"). Now we should have something like:

![UI alignment](images/alignment.png)

Note, that the values we've changed from the default ones are highlighted in bold and dark red color.

Now we have a small detail - our logo is glued to the top of the design, which is not nice. We need to add a gap, which we can do by changing `VerticalAnchorDelta` property of our logo image. As we are moving the image _down_ we have to specify a _negative value_ here, let's say "-50". Now our image has some free space around it in the design:

![UI alignment](images/alignment2.png)

The same way if we'd wanted to move the image horizontally from the anchor, we'd have had to modify `HorizontalAnchorDelta` property.

Let's rename this logo image into `LogoImage` and we're done here.

Note, that as we never explicitly set the size of the image, it will have its native dimensions of 740x310.

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

Then we want to have some nicely designed image for the button instead of standard box button image. This can be done in "All" tab by modifying "Custom Background" properties:

![Start button image](images/button-start-custom-background.png)

First we should tick the `CustomBackground` checkbox and next let's click `CustomBackgroundNormal`:

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

Also let's change the `FontSize`, so that the text won't be so tiny. It can be done in "Basic" tab:

![Start button image](images/button-start-font-size.png)

Zero means "default" value, we'll learn to change it later. For now let's assign it to 60:

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

### Screen aspect ratio and orientation

Obviously we have a problem that the buttons overlap with the game title. This happens because our current view has proportions of Landscape orientation, but we are aiming at mobile devices in Portrait orientation, which would define our design arrangement, but still the variety of mobile devices resolutions and aspect ratio is extremely high, and we have to make our interface flexible enough to fit most if not all of them. Fortunately, Castle Game Engine does most of this dirty work for us.

There are several steps to fix this problem, but first of all, let's go to the settings file `CastleSettings.xml` and look inside. It contains the following lines:

```XML
<castle_settings>
  <ui_scaling
    mode="EncloseReferenceSize"
    reference_width="1600"
    reference_height="900"
  />
</castle_settings>
```

Let's set it to the ones specific to iPhone 6, 6s, 7 and 8 in Portrait orientation:

```XML
reference_width="750"
reference_height="1334"
```

This will tell the engine that it should consider that all the internal designs of the game are designed for 750x1334 screen. The actual values don't really matter, as we try to aim at diverse possible screen ratios, it's just something that the game engine will take as reference.

Note that setting `reference_width` and `reference_height` doesn't enforce the actual screen size, so in case of Desktop application we should do it manually. Let's open `GameInitialize` unit and add the following lines in the very end of `initialization` section, right before the `end.`:

```Pascal
{$ifndef CASTLE_IOS}
  {$ifndef ANDROID}
    Window.Height := Application.ScreenHeight * 5 div 6;
    Window.Width := Window.Height * 750 div 1334;
  {$endif}
{$endif}
```

Here we set the `Window.Height` based on current monitor resolution `Application.ScreenHeight` - to fit almost all the screen height, and then scale `Window.Width` accordingly to our 750x1334 reference screen size.

`{$ifndef CASTLE_IOS}...{$endif}` and `{$ifndef ANDROID}...{$endif}` are compiler directives that tell the Free Pascal compiler not to set `Window.Width` and `Window.Height` for Android and iOS devices.

Now we can try and compile the game to see that our window is now has a correct shape in Portrait orientation.

Let's also add one important touch here, so that when we would want to compile for mobile platforms, our game will tell the device that it wants to run only in Portrait mode. Let's open `CastleEngineManifest.xml` and add a line `screen_orientation="portrait"` somewhere in `project` tag, e.g. after `caption` field. So now it will look something like this:

```XML
<project name="ButtonClickerGame"
  standalone_source="ButtonClickerGame.lpr"
  game_units="GameInitialize"
  qualified_name="io.castleengine.ButtonClickerGame"
  caption="Button Clicker"
  screen_orientation="portrait"
>
</project>
```

Now let's close our project and reopen it again in Castle Editor, so that our changes to `CastleSettings.xml` would take effect. Let's open our MainMenu design again by choosing Design -> Open, selecting `MainMenu.castle-user-interface` file and clicking "Open". Now the Main Menu immediately looks better even though Castle Editor window is still in Landscape orientation:

![Menu looks better](images/menu-looking-better.png)

However, the buttons are still covering our logo a bit, and there is too much asymmetric free space below. This happened because we aligned our Vertical Group exactly at the center of the screen. There are many different ways to fix this issue, but in the current design we can cheat a bit.

As we already have the Vertical Group, let's just add our logo to it, so that it will be arranged the same way as the buttons. To do that, just click the `LogoImage` in the Hierarchy and drag it into the `MenuGroup`:

![Menu looks better](images/menu-looking-better2.png)

Now the whole Logo + Menu buttons construction is properly aligned to the screen center vertically and horizontally, but they aren't aligned to each other. It's fixed by selecting `MenuGroup` and going into "All" tab:

![Alignment](images/menu-looking-better-alignment.png)

Let's edit `Align` property and change it from `hpLeft` (which means all the elements in the Vertical Group are aligned by their left border) to `hpMiddle` (which means all the elements in the Vertical Group are aligned to middle). And everything looks cool now:

![Menu looks better](images/menu-looking-better-result.png)

Now we can try changing the screen aspect ratio by dragging these separators:

![Separators](images/separators.png)

And we see that the design remains consistent no matter what screen resolution or aspect ratio we pick:

![Menu looks better](images/menu-looking-better3.png)

![Menu looks better](images/menu-looking-better4.png)

Now, why don't we use this feature to make our Castle Editor window layout more convenient? Let our game window indeed be something Portrait-like. Let's drag the lower separator straight to the bottom, we shan't be using anything from below in this tutorial:

![Editor arrangement](images/editor-arrangement.png)

### Load the Main Menu in the game

Now as we have a ready Main Menu design, we need to use it in the game. Unfortunately at the moment it's still not possible to do that automatically and will require us to return to Lazarus.

First of all we need to create a unit for our new Game State. It's done by clicking File -> New Unit:

![New unit](images/file-new-unit.png)

Now we've got an empty pascal unit with the first line containing its name: `unit Unit1;`. Let's change it to `unit GameStateMainMenu;` and save the file.

Let's add `CastleUiState` to `uses` section, like this:

```Pascal
uses
  Classes, SysUtils, CastleUiState;
```

And create a dummy TStateMainMenu class right behind the `uses` section:

```Pascal
type
  TStateMainMenu = class(TUiState)
  end;
```

We should also create a singleton variable for this class, by adding right after the class definition:

```Pascal
var
  StateMainMenu: TStateMainMenu;
```

Now we need to create a `Start` procedure that will be run when this state is activated. This is a `virtual` procedure, present in ancestor class, therefore we have to `override` it and use `inherited` inside to call the inherited code from the ancestor which is in turn required for the state to operate properly. Now our class declaration will look like this:

```Pascal
type
  TStateMainMenu = class(TUiState)
  public
    procedure Start; override;
  end;
```

And we have to _implement_ this procedure in `Implementation` section below:

```Pascal
implementation

procedure TStateMainMenu.Start;
begin
  inherited;
  //here will be our code
end;
```

Next we have to load the specific design into our State:

```Pascal
procedure TStateMainMenu.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/MainMenu.castle-user-interface', FreeAtStop, UiOwner);
end;
```

Here we use `InsertUserInterface` procedure to insert our newly designed `MainMenu.castle-user-interface` design into this State. Here `castle-data:/` is a protocol that enables us to reference the `data` folder in a safe cross-platform way, so that it'll work on Desktop and iPhone equally well.

`FreeAtStop` is a `TComponent` class that will free every unit assigned to it when the State will call `Stop`.

`UiOwner` is a container for our design, that we can use to get references to its specific components.

Last but not least we should create our `StateMainMenu` class and set it as current. We can do it in the unit `gameinitialize` - in `ApplicationInitialize` procedure by adding two lines:

```Pascal
StateMainMenu := TStateMainMenu.Create(Application);
TUiState.Current := StateMainMenu;
```

We also have to add the unit of the state to `gameinitialize`'s `uses` section:

```Pascal
uses SysUtils,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleFilesUtils,
  CastleUIControls, CastleApplicationProperties,
  GameStateMainMenu;
```

Save, compile and run. Now we have our MainMenu successfully loaded in the game. More than that, the buttons react to mouse cursor hovering over them, and clicking.

![Game window](images/game-window-basic.png)

### Adding click events

Now we have to make those buttons actually perform some function. To do that we first have to "find" the button in the loaded design. Let's go back to `GameStateMainMenu` unit. First of all, let's create the variables for the buttons in our class, we'll also need to add `CastleControls` unit to the `uses` section:

```Pascal
uses
  Classes, SysUtils, CastleUiState, CastleControls;

type
  TStateMainMenu = class(TUiState)
  private
    StartGameButton, OptionsButton, CreditsButton, QuitButton: TCastleButton;
  public
    procedure Start; override;
  end;
```

Next we'll need to add `CastleComponentSerialize` unit to our `uses` section, let's add it after `implementation`. It will allow us to "find" the buttons by their names in the loaded design by calling:

```Pascal
implementation
uses
  CastleComponentSerialize;

procedure TStateMainMenu.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/MainMenu.castle-user-interface', FreeAtStop, UiOwner);
  StartGameButton := UiOwner.FindRequiredComponent('StartGameButton') as TCastleButton;
  OptionsButton := UiOwner.FindRequiredComponent('OptionsButton') as TCastleButton;
  CreditsButton := UiOwner.FindRequiredComponent('CreditsButton') as TCastleButton;
  QuitButton := UiOwner.FindRequiredComponent('QuitButton') as TCastleButton;
end;
```

Next we have to create dummy procedures that will be assigned to the buttons' click events:

```Pascal
type
  TStateMainMenu = class(TUiState)
  private
    StartGameButton, OptionsButton, CreditsButton, QuitButton: TCastleButton;
    procedure ClickStart(Sender: TObject);
    procedure ClickOptions(Sender: TObject);
    procedure ClickCredits(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    procedure Start; override;
  end;
```

And their corresponding implementation:

```
procedure TStateMainMenu.ClickStart(Sender: TObject);
begin
end;

procedure TStateMainMenu.ClickOptions(Sender: TObject);
begin
end;

procedure TStateMainMenu.ClickCredits(Sender: TObject);
begin
end;

procedure TStateMainMenu.ClickQuit(Sender: TObject);
begin
end;
```

Lastly let's assign our buttons `OnClick` event to these procedures in the end of `Start`:

```Pascal
StartGameButton.OnClick := @ClickStart;
OptionsButton.OnClick := @ClickOptions;
CreditsButton.OnClick := @ClickCredits;
QuitButton.OnClick := @ClickQuit;
```

Now we may immediately implement the Quit button behavior by adding `CastleWindow` to `uses` section and calling `Application.MainWindow.Close` in the button click event:

```Pascal
procedure TStateMainMenu.ClickQuit(Sender: TObject);
begin
  Application.MainWindow.Close;
end;
```

Now we can compile and test if this button is working as expected. We cannot implement the other buttons behavior yet, as we need to create corresponding game states.

For now let's add one more quality of life detail. On mobile platform we usually don't need "Quit" button, as the apps are closed by the operation system. So we should simply disable it in this case. We can do it by adding:

```Pascal
{$ifdef CASTLE_IOS}QuitButton.Enabled := false;{$endif}
{$ifdef ANDROID}QuitButton.Enabled := false;{$endif}
  ```

In a similar way as we did that when we forced the Window size on Desktop platforms.

### Set up Default font

Before we proceed, we need to improve how our fonts look. We shall be using two fonts in this game - the default one and the font for captions and buttons. Let's set up the Default font first. It can be done by adding font information to `CastleSettings.xml`:

```XML
<castle_settings>
  <ui_scaling
    mode="EncloseReferenceSize"
    reference_width="750"
    reference_height="1334"
  />
  <default_font>
    <regular
      url="fonts/CatV_6x12_9.ttf"
      size="40"
      anti_aliased="true"
    />
  </default_font>
</castle_settings>
```

This will make `CatV_6x12_9.ttf` a default font for the game, with `size` 40. We can immediately see that it looks better:

![New default font look](images/better-font.png)

### Load and use a custom font

However, it's still blurry (because we requested font size 60 for the buttons, and the loaded font size is 40 - much smaller) and it doesn't fit well into the design. Therefore, we'll be using a different font for button captions. There is no way to do that through Castle Editor yet, so let's load the font by code. In order to be easily able to reference the loaded alternative font, let's create a new unit and call it `GameFont` and make it have the following content:

```Pascal
unit GameFont;

{$mode objfpc}{$H+}

interface

uses SysUtils,
  CastleFonts;

var
  CartoonFont60: TTextureFont;

procedure LoadFonts;

implementation

procedure LoadFonts;
begin
  CartoonFont60 := TTextureFont.Create('castle-data:/fonts/Big_Bottom_Cartoon.ttf', 60, true);
end;

finalization
  FreeAndNil(CartoonFont60);
end.
```

This unit creates a variable CartoonFont60 with an alternative font `Big_Bottom_Cartoon.ttf` of size "60". We shall load it by calling `LoadFonts` procedure, and it will be automatically freed upon the game end by calling `FreeAndNil(CartoonFont60)` in `finalization` section.

Now let's add this unit to `GameInitialize` `uses` section and call `LoadFonts;` before creating `StateMainMenu`.

The last step remaining is to assign this font to our buttons. To do this, add `GameFont` to the `uses` section of `GameStateMainMenu` and in the `Start` procedure write:

```Pascal
StartGameButton.CustomFont := CartoonFont60;
OptionsButton.CustomFont := CartoonFont60;
CreditsButton.CustomFont := CartoonFont60;
QuitButton.CustomFont := CartoonFont60;
```

And now our design finally looks almost as it was anticipated:

![New custom font](images/better-font2.png)

### Colored button text

Still the black color is not good here, so let's go back to Castle Editor and change it to one of the design colors. Let's select one of the buttons, open the tab "All":

![Button text color](images/button-text-color.png)

Check the `CutsomTextColorUse` checkbox and paste "162D40" into `CutsomTextColor` field above to get:

![Button text color](images/button-text-color2.png)

Let's repeat this step for all buttons, save the design and run the game from Lazarus:

![Final view of Main Menu](images/main-menu-final.png)

## Creating a Game State

### Start designing Game State

Now, finally we can start working on the game. Let's note here, that it is actually better to start working on the gameplay from the beginning, and only after that create other UI states, like Main Menu. This way we shall have a working prototype quite soon, that can be easily tested and some adjustments can be made as early as possible. However, as in our case the game is expected to be extra simple and still creating a game is more complex than creation of a simple UI - we did the first part so now we can concentrate more on the game itself, than on using the Castle Editor.

But of course, we shall be creating the game screen in Castle Editor. Let's start by creating a new Design by doing the same way as we did for Main Menu: Design -> New User Interface (Chosen Root) -> Image (TCastleImageControl). Again, let's call this image `BackgroundImage` and load our background image in "Basic" tab and make it `FullScreen` in "All" tab.

Finally let's save this design as `Game.castle-user-interface`.

Next, again as our design is quite simple, let's reuse the cheat we've done in Main Menu and create a one large Vertical Group for all our design: Design -> Add User Interface Component -> Vertical Group (TCastleVerticalGroup). Let's name it `GameGroup` and set `Spacing` to 20. Let's again align it to the center by setting `HorizontalAnchorParent` and `HorizontalAnchorSelf` to `hpMiddle` and `VerticalAnchorDelta` and `VerticalAnchorSelf` to `vpMiddle` in "Layout" tab. Also let's set `Alignment` to `hpMiddle` at "All" tab.

Now what we want to do is to have our UI organized in the following way:

- Score: 99999

- Gameplay area with 3x4 buttons

- High Score: 99999

### Design score area

Let's start with creating a "Score" area. If we create a "Label" right away, as it is aligned to the center (remember `Alignment` in GameGroup is `hpMiddle`) it will shift around when the score increases. So we have to create a "container" for it, so that we can strictly fix its position horizontally. So, let's add an "Empty Rectangle" by Design -> Add User Interface Component -> Empty Rectangle (TCastleUserInterface). Let's name it `ScoreArea`. Note, that it's not mandatory to name our UI elements - Castle Editor already provides us valid and unique names for them, however it's a good habit to learn.

By default it has dimensions 100x100. Let's make it 650x100, like this:

![Empty rectangle width](images/empty-rectangle-width.png)

Now we have a rigid container which size won't change. Let's add two labels to it by Design -> Add User Interface Component -> Label (TCastleLabel). Be sure to select `GameGroup` after adding the first label before adding the second one - as the first label will be automatically selected, and the second label will be added as its child. This is not a problem, but it's better to keep things clean.

![Score area](images/score-area.png)

Let's name our labels `ScoreText` and `ScoreLabel`. Let's set `Caption`s to "Score:" and "9999999". Let's set color of both to "162D40" and font size "60" for the first one and "80" for the second. Now our labels properties "Basic" tabs look like this:

![Label's properties](images/scoretext-properties.png)

![Label's properties](images/scorelabel-properties.png)

Let's go to Layout tab and set `VerticalAnchorParent` and `VerticalAnchorSelf` to `vpMiddle` for both labels. Let's leave `HorizontalAnchorParent` and `HorizontalAnchorSelf` at default `hpLeft` and change `HorizontalAnchorDelta` to "100" and "320" correspondingly. Now our design looks like this:

![Labels anchors](images/score-label-anchors.png)

### Design gameplay area

Now, the gameplay area is a grid 3x4 consisting of "buttons". There's no "Grid" component for Castle Game Engine ready out-of-the box, but actually we don't really need it here. Let's select `GameGroup` create a Vertical Group and call it `GameplayArea`, also setting `Alignment` to `hpMiddle` again.

Let's add a "Horizontal Group" to it and call it `GameplayRow1`.

Let's add an image to it, calling it `Button11` and load "button.png" file by using `URL` property. Now it looks like this:

![Adding button](images/adding-button-image.png)

Before adding another image, let's add a Label to this one, name it "Label11", set its `Caption` to "999", `Font` to "80" and `Color` to "162D40". Finally, let's go into "Layout" tab and set its anchors: `VerticalAnchorParent`:`vpMiddle`; `VerticalAnchorSelf`:`vpMiddle`; `HorizontalAnchorParent`:`hpMiddle`; `HorizontalAnchorSelf`:`hpMiddle`. Now our button looks like this:

![designing-clicker-button](images/designing-clicker-button.png)

Yes, you might have already noticed, that we say "button" but actually we've created an "Image". We'll know why we did that a bit later.

For now let's create two copies of our button by selecting and duplicating (Design -> Duplicate Component) it. Now our design looks like this:

![duplicating-buttons](images/duplicating-buttons.png)

As we can see the buttons are too close to each other. We could have done this by selecting `GameplayRow` and set `Spacing` to "50". However, it's not the best idea to go this way. Let's rather increase the size of the buttons, so that the Player will not have "dead areas" where clicking/tapping the gameplay field will not trigger any events. Yes, that means we'll learn to rework a ready design.

Surprisingly, it's usually easier to delete the duplicated components and duplicate them again later, than to apply some significant change to every component. Let's delete our `Button12` and `Button13` by Design -> Delete Component.

Let's add an Empty Rectangle (Design -> Add User Interface Component -> Empty Rectangle (TCastleUserInterface)) to our `GameplayArea` and call it `ButtonGroup11`. If you peek into our `data` folder, our image has dimensions of 176x185 and Our screen has reference width 750. To let 3 buttons cover it completely, our "clickable area" should have size 750/3 = 250x250, so, let's set `ButtonGroup11` `Width` and `Height` to "250" in "Layout" tab.

Now let's drag `Button11` into `ButtonGroup11`. Let's go to "Layout" tab, and set `VerticalAnchorDelta` and `HorizontalAnchorDelta` to zero and `HorizontalAnchorParent` and `HorizontalAnchorSelf` to `hpMiddle`. Now our design looks like:

![button-group](images/button-group.png)

Now let's duplicate our `ButtonGroup11` two times. Note that Castle Editor sometimes "remembers" names of already deleted components and may try to call the new ones with insequential numbers. To fix that we have to save and re-open the design:

![duplicate button-group](images/duplicating-buttongroups.png)

Now, let's duplicate the `GameplayRow1` three times to get four rows in total

Note that Castle Editor named our rows correctly, but other elements - `ButtonGroup`s, `Button`s and `Label`s are simply keep naming consequently. Let's change their names so that the first digit would correspond to row number and the second one - column number. We'll have to start from the bottom, as e.g. name "21" is already taken by some of the elements lower in the hierarchy. Now our hierarchy looks like this:

![Hierarchy after duplications](images/gameplay-hierarchy.png)

### Adding High Score label

Finally, let's duplicate our `ScoreArea` and name the copy `HighScoreArea`. Let's drag it to the very bottom of our `GameGroup` hierarchy. Hint, we can simply drag it onto `GameGroup` element in the Hierarchy until a gray triangle shows up on the right - this will avoid us having to drag it all along and trying to snap to the end of the group:

![Hierarchy after duplications](images/dragging-high-score.png)

Let's set `Height` of the `HighScoreArea` to "50", name its children as `HighScoreText` and `HighScoreLabel`, set the corresponding font sizes to "30" and "40" and `HorizontalAnchorDelta`s to "160" and "360" to accommodate the new font sizes. Let's change `HighScoreText` caption to "High Score:".

![Gameplay design result](images/gameplay-result.png)

### Loading multiple different font sizes

Before we proceed, note that our numbers are blurry. This happened because again we've used the font significantly larger than the one we've requested in `CastleSettings.XML`. Let's go back to it and add a line `sizes_at_load="40 80"` to `<default_font>`, like this:

```XML
<default_font>
  <regular
    url="fonts/CatV_6x12_9.ttf"
    size="40"
    anti_aliased="true"
    sizes_at_load="40 80"
  />
</default_font>
```

This will instruct Castle Game Engine to load font sizes "40" and "80" for our default font.

Note, that every loaded font size is a texture, which occupies some decent amount of memory. So, normally, the quantity of the font sizes loaded should be limited to 2-4. Also it'd be a bad idea to load a huge size font, as the texture size will be too big. There is a way to optimize this behavior by specifying only specific limited set of characters that should be loaded (e.g. load only numbers in case this font size is used only for showing game score), but it is outside of the scope of this tutorial.

Let's also go to our `GameFont` unit and ask it to load different font sizes for our fancy font. We already have `CartoonFont60` there, so let's create `CartoonFont30` the same way:

```Pascal
var
  CartoonFont60: TTextureFont;
  CartoonFont30: TTextureFont;

procedure LoadFonts;

implementation

procedure LoadFonts;
begin
  CartoonFont60 := TTextureFont.Create('castle-data:/fonts/Big_Bottom_Cartoon.ttf', 60, true);
  CartoonFont30 := TTextureFont.Create('castle-data:/fonts/Big_Bottom_Cartoon.ttf', 30, true);
end;

finalization
  FreeAndNil(CartoonFont60);
  FreeAndNil(CartoonFont30);
end.
```

Now if we reload our project in Castle Editor, everything looks crisp:

![Gameplay design result](images/gameplay-result-crisp.png)

### Start Game State

Now it's time to start integrating the new State into our game. Let's create a new unit in Lazarus and call it `GameStateGame`. Let's crate a yet empty `class` `TStateGame` with a yet empty `Start` procedure and a singleton variable for our class:

```Pascal
uses
  Classes, SysUtils,
  CastleUiState, CastleControls;

type
  TStateGame = class(TUiState)
  private
  public
    procedure Start; override;
  end;

var
  StateGame: TStateGame;

implementation
uses
  CastleComponentSerialize,
  GameFont;

procedure TStateGame.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/Game.castle-user-interface', FreeAtStop, UiOwner);
end;
```

Let's also create our state in `GameInitialize` by adding `GameStateGame` to `uses` section and inside `ApplicationInitialize` after `LoadFonts` add a line:

```Pascal
StateGame := TStateGame.Create(Application);
```

Now let's also show this state if in Main Menu the Player clicks the button "Start". To do this, let's go to `GameStateMainMenu` unit and add `GameStateGame` to `uses` section and in `ClickStart` let's set `StateGame` as current:

```Pascal
procedure TStateMainMenu.ClickStart(Sender: TObject);
begin
  TUiState.Current := StateGame;
end;
```

If we did everything correctly, now our state should show in the game after we press "Start" button. As simple as that.

![State Game is working](images/state-game-first-steps.png)

### Parse Score and High Score labels

Let's start from easier task, which we already had some experience with. Let's find our 4 labels in the design that correspond to Score and High Score. This is done the same way as we made it with buttons. We have to create references for them in our `TStateGame` class and `FindRequiredComponent` in `Start` like this:

```Pascal
type
  TStateGame = class(TUiState)
  private
    ScoreText, ScoreLabel, HighScoreText, HighScoreLabel: TCastleLabel;
  public
    procedure Start; override;
  end;
...
procedure TStateGame.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/Game.castle-user-interface', FreeAtStop, UiOwner);
  ScoreText := UiOwner.FindRequiredComponent('ScoreText') as TCastleLabel;
  ScoreLabel := UiOwner.FindRequiredComponent('ScoreLabel') as TCastleLabel;
  HighScoreText := UiOwner.FindRequiredComponent('HighScoreText') as TCastleLabel;
  HighScoreLabel := UiOwner.FindRequiredComponent('HighScoreLabel') as TCastleLabel;
end;
```

Let's also use `CartoonFont60` and `CartoonFont30` for the `ScoreText` and `HighScoreText` labels the same way as we did with the `Button`s in the Main Menu. Let's add these two lines to `Start`:

```Pascal
ScoreText.CustomFont := CartoonFont60;
HighscoreText.CustomFont := CartoonFont30;
```

As we can see:

![Wrong font position](images/wrong-font-position.png)

The `ScoreText` and `HighScoreText` are now incorrectly positioned in the design (because there is no way to set and preview the second font in Castle Editor yet). Let's fix this by going back to Castle Editor. For `ScoreText` let's change `HorizontalAnchorDelta` from "100" to "50" and `VerticalAnchorDelta` from "0" to "11". And for `HighScoreText` change `HorizontalAnchorDelta` from "160" to "110" and `VerticalAnchorDelta` from "0" to "5". Now it looks much better:

![Wrong font position fixed](images/wrong-font-position-fixed.png)

### Parse game field

Finally we're getting closer to making an actual game. Let's parse the designed elements for gameplay field. We could do it the same way as we did for buttons of the Main Menu and labels for score and high score. However, as we have 12 "buttons" here and we shall need to operate them conveniently, let's create a construction that will contain all the information we need about the "button" in one place:

```Pascal
type
  TGamePad = record
    Group: TCastleUserInterface;
    Image: TCastleImageControl;
    Caption: TCastleLabel;
  end;
```

Here we're using a minimalistic `record` data type for simplicity reasons. Often when creating such complex UI elements, we would want to create a `class` that would manage them appropriately. However, in our case we only need to keep them together, and avoiding memory management here is favorable.

Note, that unlike other UI elements, `TCastleUserInterface` is located inside `CastleUiControls`, so we have to add this unit to the `uses` section.

Now we can have a (again minimalistic) static 2-dimensional array to hold our 12 `TGamePad`s inside `TStateGame`:

```Pascal
type
  TStateGame = class(TUiState)
  private
    GamePads: array[1..3, 1..4] of TGamePad;
    ScoreText, ScoreLabel, HighScoreText, HighScoreLabel: TCastleLabel;
  ...
  end;
```

And finally in `Start` we "find" all those 12 pads in our design. Of course we could do it the same way as we did with labels and buttons, but here it'll be much more efficient to make a loop that will cycle through all 3x4 elements and assign them to corresponding array elements:

```Pascal
procedure TStateGame.Start;
var
  UiOwner: TComponent;
  X, Y: Integer;
begin
  ...
  for X := 1 to 3 do
    for Y := 1 to 4 do
    begin
      GamePads[X, Y].Group := UiOwner.FindRequiredComponent('ButtonGroup' + Y.ToString + X.ToString) as TCastleUserInterface;
      GamePads[X, Y].Image := UiOwner.FindRequiredComponent('Button' + Y.ToString + X.ToString) as TCastleImageControl;
      GamePads[X, Y].Caption := UiOwner.FindRequiredComponent('Label' + Y.ToString + X.ToString) as TCastleLabel;
    end;
end;
```

### Responsive buttons

Now it's time to talk a bit about the gameplay. So, as we know, we're making a clicker game, where the Player is supposed to click buttons as fast as possible. That means, that the buttons must be very responsive to Player's click or touch.

On the other hand, `OnClick` event, we've used in Main Menu state is not a responsive event. We would want the clicker button to react instantly to Player's actions - that is when the User `Press`es the button, but `OnClick` fires when the user `Press`es and `Release`s the button. This way we'll have to create "our own button" that reacts to `Press` event.

We could have done that by creating a child of `TCastleUserInterface` (or any other UI element, e.g. `TCastleImagecontrol`) and override its `Press` function. But in current simple situation, Castle Game Engine already has this solution ready for us - `OnPress` event, that works very similarly to `OnClick`.

Let's create a `private` callback for this event inside `TStateGame`:

```Pascal
procedure ButtonPress(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
```

Note that `TInputPressRelease` is inside `CastleKeysMouse`, so we have to add this unit to `uses` section too. This procedure will receive three parameters: `Sender` which will be the object that was clicked, we'll need its `Name` to determine which exactly button was clicked, `Event` - a thorough description of the `Press` `Event` that happened and if this event was already `Handled` by some other UI element (we won't need it in this game).

Next, let's assign `OnPress` event to this procedure for our "ButtonGroup"s. In the loop we need just to add one line:

```Pascal
GamePads[X, Y].Group := UiOwner.FindRequiredComponent('ButtonGroup' + Y.ToString + X.ToString) as TCastleUserInterface;
GamePads[X, Y].Group.OnPress := @ButtonPress;
GamePads[X, Y].Image := UiOwner.FindRequiredComponent('Button' + Y.ToString + X.ToString) as TCastleImageControl;
GamePads[X, Y].Caption := UiOwner.FindRequiredComponent('Label' + Y.ToString + X.ToString) as TCastleLabel;
```

And finally let's implement an empty `ButtonPress` procedure somewhere in `implementation` section.

```Pascal
procedure TStateGame.ButtonPress(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
begin
end;
```

### Buttons reaction to clicks

Let's go on with implementing `ButtonPress` procedure. There are more efficient way to handle this, but here we'll go with a simple and straightforward solution - we'll determine which button was clicked by its Name, like this:

```Pascal
procedure TStateGame.ButtonPress(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
var
  ThisGamePad: ^TGamePad;
begin
  if Event.EventType = itMouseButton then
  begin
    case Sender.Name of
      'ButtonGroup11': ThisGamePad := @GamePads[1, 1];
      'ButtonGroup12': ThisGamePad := @GamePads[2, 1];
      'ButtonGroup13': ThisGamePad := @GamePads[3, 1];
      'ButtonGroup21': ThisGamePad := @GamePads[1, 2];
      'ButtonGroup22': ThisGamePad := @GamePads[2, 2];
      'ButtonGroup23': ThisGamePad := @GamePads[3, 2];
      'ButtonGroup31': ThisGamePad := @GamePads[1, 3];
      'ButtonGroup32': ThisGamePad := @GamePads[2, 3];
      'ButtonGroup33': ThisGamePad := @GamePads[3, 3];
      'ButtonGroup41': ThisGamePad := @GamePads[1, 4];
      'ButtonGroup42': ThisGamePad := @GamePads[2, 4];
      'ButtonGroup43': ThisGamePad := @GamePads[3, 4];
      else
        raise Exception.Create('Unexpected Button name: ' + Sender.Name);
    end;
    ThisGamePad^.Caption.Caption := '!!!';
  end;
end;
```

Here we've done a lot of things, let's explain what they mean line-by-line:

- `ThisGamePad: ^TGamePad;` is a pointer to our `TGamePad` construction.

- `if Event.EventType = itMouseButton then` checks what exactly was `Press`ed. It can be a key on the keyboard, a mouse wheel or a mouse button. We proceed only in case a mouse button was pressed. This can be either left or right mouse button - we don't really care. This can also be a touch on mobile devices. Note that on mobile devices we can receive multiple touches simultaneously for different UI elements - and we don't have to worry about this, Castle Game Engine will take care of all of them and will send an `OnPress` callback for each.

- `case Sender.Name of` cycles through possible values of `Sender.Name` and in the next lines determines actions that should be taken in each specific case. For example `'ButtonGroup22': ThisGamePad := @GamePads[2, 2];` means that in case `Sender.Name` is equal to `ButtonGroup22` then `ThisGamePad` pointer should point (`@` symbol) at `GamePads[2, 2]`.

- `else raise Exception.Create('Unexpected Button name: ' + Sender.Name);` is a sanity check, that we didn't forget how we've called our buttons in Castle Editor and didn't make any typos.

- And finally `ThisGamePad^.Caption.Caption := '!!!';` is a temporary measure to provide us some feedback that the button was detected and clicked successfully. It replaces the `Caption` of the button from "999" originally determined in the Castle Editor into "!!!" so that we can easily see that exactly the button we've clicked reacted to our click and we've made no errors.

Yes, surely now we should compile our game and try it out!

![Testing OnPress event](images/gameplay-onpress-testing.png)

### Update event

Now, next thing that we should do - is to have something happen on the screen as the time passes. That is something should happen every frame, which is handled by `Update` event, which we can `override`:

```Pascal
type
  TStateGame = class(TUiState)
  ...
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;
```

And add its implementation, empty for now:

```Pascal
procedure TStateGame.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;
```

Again, as we `override` some virtual method, we have to call `inherited` to make sure that the parent `class`es run their `Update` code properly.

### Adding gameplay elements

Let's think a bit about the anticipated gameplay:

- Start the button "from zero".

- The button "grow"s until it's "ripe".

- When the button is "ripe" it can be "harvested" by the player.

- If some button becomes "overripe", then the game is over.

Next, let's define 2 constants, that will govern our game pace: time for the button to "grow"->"ripe" and "ripe"-"overripe".

```Pascal
const
  GrowTime = 2;
  RipeTime = GrowTime + 10;
```

The gameplay will be accelerating and the buttons will ripe and overripe much faster plus each button will "grow" with a randomly different speed, therefore these values are only for reference - but an "average button at the start of the game" will take 2 seconds to "grow" and 10 seconds to "ripe". So, let's add the relative variables for each of our "buttons":

```Pascal
type
  TGamePad = record
    Speed: Single;
    Ripeness: Single;
    Score: Integer;
    Group: TCastleUserInterface;
    Image: TCastleImageControl;
    Caption: TCastleLabel;
  end;
```

Here:

- `Speed` will define how quickly will our button grow and ripe relative to an "average button" - a random number from `0.5` to `1.5`.

- `Ripeness` is the current progress of this button (0..GrowTime - button is growing; GrowTime..RipeTime - button is ripening; if this value is larger than RipeTime, the game is over).

- `Score` conveniently stored button's score, calculated based on it's `Ripeness` - the more ripe the button, the larger the score is, will start at "100" and grow up to "999". This way the later the Player presses the button, the more score he/she will get, encouraging risky gameplay.

Also, let's introduce global game pace in `TStateGame`:

```Pascal
type
  TStateGame = class(TUiState)
  private
    GamePace: Single;
    GameScore: Integer;
    GameRunning: Boolean;
  ...
  end;
```

Here:

- `GamePace` - current speed of the game. The longer the player plays, the faster the game becomes.

- `GameScore` - current Player's score.

- `GameRunning` - answers the question is the game currently running? The game starts running and stops after "Game Over".

Now let's set the initial values for each of these new variables in our `Start` procedure:

```Pascal
procedure TStateGame.Start;
var
  UiOwner: TComponent;
  X, Y: Integer;
begin
  inherited;
  ...
  Randomize;
  for X := 1 to 3 do
    for Y := 1 to 4 do
    begin
      GamePads[X, Y].Group := UiOwner.FindRequiredComponent('ButtonGroup' + Y.ToString + X.ToString) as TCastleUserInterface;
      GamePads[X, Y].Group.OnPress := @ButtonPress;
      GamePads[X, Y].Image := UiOwner.FindRequiredComponent('Button' + Y.ToString + X.ToString) as TCastleImageControl;
      GamePads[X, Y].Caption := UiOwner.FindRequiredComponent('Label' + Y.ToString + X.ToString) as TCastleLabel;
      GamePads[X, Y].Ripeness := 0.0;
      GamePads[X, Y].Speed := 0.5 + Random;
    end;
  GamePace := 1.0;
  GameScore := 0;
  GameRunning := true;
end;
```

And finally in `Update` we cycle through all of our buttons and update them properly:

```Pascal
procedure TStateGame.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  X, Y: Integer;
begin
  inherited;
  if GameRunning then
  begin
    for X := 1 to 3 do
      for Y := 1 to 4 do
      begin
        GamePads[X, Y].Ripeness += SecondsPassed * GamePace * GamePads[X, Y].Speed;
      end;
    GamePace += SecondsPassed / 120;
  end;
  ScoreLabel.Caption := GameScore.ToString;
end;
```

Here:

- `if GameRunning then` checks if the game is running, and doesn't update `GamePads` or `GamePace` otherwise.

- `GamePads[X, Y].Ripeness += SecondsPassed * GamePace * GamePads[X, Y].Speed;` increases `Ripeness` of this button by amount of `SecondsPassed` multiplied by `GamePace` multiplied by this specific button grow `GamePads[X, Y].Speed`.

- `GamePace += SecondsPassed / 120;` makes the game accelerate with time, i.e. at the beginning the game speed is 1.0, in 2 minutes the game speed will be 2.0 and in 4 minutes - 3.0.

- `ScoreLabel.Caption := GameScore.ToString;` sets `Caption` of our label that represents the game score to current Player's score.

Next thing to do, we should update our visuals, based on how "ripe" the button is. Let's do this by working inside the loop in `Update`:

```Pascal
for X := 1 to 3 do
  for Y := 1 to 4 do
  begin
    GamePads[X, Y].Ripeness += SecondsPassed * GamePace * GamePads[X, Y].Speed;
    if GamePads[X, Y].Ripeness < GrowTime then
    begin
      GamePads[X, Y].Score := 0;
      GamePads[X, Y].Caption.Exists := false;
      GamePads[X, Y].Image.Color := Vector4(GamePads[X, Y].Ripeness / GrowTime, 1.0, 0.0, 1.0);
    end else
    if GamePads[X, Y].Ripeness <= RipeTime then
    begin
      GamePads[X, Y].Score := 100 + Trunc(899 * (GamePads[X, Y].Ripeness - GrowTime) / (RipeTime - GrowTime));
      GamePads[X, Y].Caption.Exists := true;
      GamePads[X, Y].Caption.Caption := GamePads[X, Y].Score.ToString;
      GamePads[X, Y].Image.Color := Vector4(1.0, 1.0 - (GamePads[X, Y].Ripeness - GrowTime) / (RipeTime - GrowTime), 0.0, 1.0);
    end else
    begin
      //GameOver
      GamePads[X, Y].Score := -1;
      GamePads[X, Y].Caption.Exists := false;
      GamePads[X, Y].Image.Color := Vector4(1.0, 0.0, 0.0, 1.0);
      GameRunning := false;
    end;
  end;
```

Here:

- First, we update `Ripeness` as we already did before.

- Next we check if `Ripeness` is less than `GrowTime` and if it is, the button is only growing - it's not ripe yet. In this case we set the `Score` of the button to zero, we don't show the button Caption by setting `Exists := false` of the label and we change the color of the button by modifying it's `Color` property - and assigning it result of `Vector4` function as in `Vector4(Red, Green, Blue, Alpha)` - in this case it goes from green to yellow color. Note that to use `Vector4` function we'll need to include `CastleVectors` unit in `uses` section. As our button image is grayscale, this will be the button's color. However, what it does exactly is multiplies the color of every pixel of the image by this value, i.e. creating a tint for the image.

- If the first condition is not met, then we check if the `Ripeness` is still less than `RipeTime`. In this case the button is "ripe" and we can harvest it to get some score. Here we show the score label by setting its `Exists := true` and its caption to current `Score` for this button which is calculated above. And finally we set the button's color progressively from yellow to red depending on `Ripeness`.

- Finally, if neither of the above conditions are met, we have a "Game Over". We'll put more work into this situation later, for now we'll just make this button have no caption and pure red color.

Let's compile our project and run it:

![Testing Update event](images/gameplay-update-testing.png)

### Clicking those buttons!

We're very close. Now we want to be able to harvest those ripe buttons and add their score to player's score. Let's do this by modifying `TStateGame.ButtonPress` procedure:

```Pascal
procedure TStateGame.ButtonPress(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
var
  ThisGamePad: ^TGamePad;
begin
  if Event.EventType = itMouseButton then
  begin
    if GameRunning then
    begin
      case Sender.Name of
        ...
      end;
      if ThisGamePad^.Score > 0 then
      begin
        GameScore += ThisGamePad^.Score;
        ThisGamePad^.Score := 0;
        ThisGamePad^.Ripeness := 0.0;
        ThisGamePad^.Speed := 0.5 + Random;
      end else
      if ThisGamePad^.Score = 0 then
        GamePace += 0.25;
    end;
  end;
end;
```

Here:

- `if GameRunning then` checks if the game is currently running, and doesn't allow clicking GamePads if the game is lost.

- If `Score` of the clicked Pad is greater than zero, we add this `Score` to `GameScore` and reset the pad, so that it'll start growing again from zero.

- If `Score` is equal to zero (i.e. this button is not "ripe" yet, as we set it in `Update`) - we "punish" the Player by significantly accelerating the `GamePace` (equivalent of stealing 30 seconds from gameplay time).

Let's compile and now we can finally start playing this game!

![Testing the gameplay for the first time](images/gameplay-testing.png)

### Replacing an image runtime

Now before going back to gameplay, let's add a tiny touch to how our game looks. Let's replace the button image for something obviously broken if the Player loses the game. To do this we'll need to load an image with "broken button" and replace it runtime if the game is lost.

First of all, let's add `CastleImages` unit to our `uses` section and in `TStateGame` add the button image:

```Pascal
type
  TStateGame = class(TUiState)
  private
    BrokenButton: TCastleImage;
  ...
  end;
```

We could have loaded the `BrokenButton` in `Start`, but that'll make the image loaded every time the Player start playing the game, while the image itself never changes. So, let's go a bit harder but more efficient way. By loading the image in `constructor`. Of course, we'll need to remember to `FreeAndNil` it in `destructor`. Let's add `constructor` and `destructor` to our class, keeping in mind that they are `virtual` so that we have to `override` them and call parent's code by `inherited`:

```Pascal
type
  TStateGame = class(TUiState)
  ...
  public
    ...
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
```

With their corresponding `implementation`:

```Pascal
constructor TStateGame.Create(AOwner: TComponent);
begin
  inherited;
  BrokenButton := LoadImage('castle-data:/images/buttons/button_broken.png', [TRGBAlphaImage]);
end;

destructor TStateGame.Destroy;
begin
  FreeAndNil(BrokenButton);
  inherited;
end;
```

Here we load the `BrokenButton` image by calling `LoadImage` providing it the image URL and expected image type. `TRGBAlphaImage` means that the image is RGB with Alpha (transparency). And in `destructor` we free the memory occupied by the image by calling `FreeAndNil(BrokenButton);`.

Finally in our `Update` we add a line `GamePads[X, Y].Image.Image := BrokenButton;` to "GameOver" section:

```Pascal
//GameOver
GamePads[X, Y].Score := -1;
GamePads[X, Y].Caption.Exists := false;
GamePads[X, Y].Image.Image := BrokenButton;
GamePads[X, Y].Image.OwnsImage := false;
GamePads[X, Y].Image.Color := Vector4(1.0, 0.0, 0.0, 1.0);
GameRunning := false;
```

Note, that we also set `OwnsImage := false;` so that Castle Game Engine won't free this image, when this `TCastleImageControl` will be `destroy`ed - as the whole idea is that we want to keep this image in memory until the game quits, and images are created and destroyed every time the Player starts or finishes the game.

This replaces the image of `TCastleImageControl` with our new image of a broken button:

![Replacing the image runtime](images/gameplay-broken-button.png)

### Loading and Saving High Score

Before we start implementing Game Over state, let's do one more small but important thing - learn to save and load some information about our game. We'll start from "High Score". There are multiple ways we can do this, but in our simple case (and in many complex cases) we're perfectly good with using a ready solution from `CastleConfig` unit, which takes care of storing our game data in a very safe and cross-platform way.

First, let's "load" the configuration. To do this, let's add `CastleConfig` to `uses` section of `GameInitialize` and in `ApplicationInitialize` add a line somewhere around `LoadFonts`:

```Pascal
UserConfig.Load;
```

As simple as that. Now our information (whatever it is) will be properly loaded from game configuration file. Note, that in case this file doesn't exist (e.g. when the Player runs the game for the first time) there will be no information in `UserConfig`.

Now in `GameStateGame` we again have to add `CastleConfig` in `uses` section of `implementation`. And in the end of `Start` procedure add the line:

```Pascal
HighScoreLabel.Caption := UserConfig.GetValue('high_score', 0).ToString;
```

I.e. here we set the `Caption` of our `HighScoreLabel` to the value of `'high_score'` configuration entry stored in our `UserConfig`. And in case no value is stored, we have this value set to `0`.

And finally, we have to set the High Score when the game is over:

```Pascal
begin
  //GameOver
  if UserConfig.GetValue('high_score', 0) < GameScore then
  begin
    UserConfig.SetValue('high_score', GameScore);
    UserConfig.Save;
  end;
  GamePads[X, Y].Score := -1;
  ...
  GameRunning := false;
end;
```

Done! Now our High Score is automatically assigned when the Player finishes the game and is safely stored in game configuration.

![High Score saved](images/gameplay-high-score.png)

### Fix a possible bug with Update order

Now, before we proceed, let's make our code a bit more "bullet-proof". The problem here is that `Update` event can come at different moments on different platforms. Therefore it'd be a very good idea to `Update` our game view before the first frame is rendered, so that the Player won't see our placeholders for a fraction of a second. Therefore we'd want to `Update` our UI in the end of `Start` procedure.

To make it simple, let's make a small hack. It's unsafe to do this way in a general case, but in our simple game there isn't anything that can go wrong here. So let's just add `UnusedBooleanVariable: Boolean = false;` to `var` section of `Start` and `Update(0, UnusedBooleanVariable);` in the end of `Start` procedure - after every UI item was found and all variables have proper values.

## Creating Game Over Popup

Currently the game simply stops when one button becomes "overripe". Of course we'd want to show some nice screen that would show the Player current game score and congratulate with getting the High Score. Obviously, we go back to Castle Editor to design such a screen. However, it may be a good idea to make this State not as a "normal" State we've been making till now, but something more like a popup - so that the gameplay field will still be visible underneath plus add some nice fly-in effect for our design.

### Designing a Popup

Let's create a new State, but this time we'll choose a Color Rectangle as a root: Design -> New User Interface (Chosen Root) -> Color Rectangle (TCastleRectnagleControl). Let's save our Design and name it `GameOver.castle-user-interface`. Note, that unlike "Image", "Color Rectangle" was automatically set to `FullSize`. Let's name it `BackgroundColor`.

Let's add an "Empty Rectangle" by Design -> Add User Interface Component -> Empty Rectangle (TCastleUserInterface). Let's name it `GameOverPoup` and position in the center of the screen (`HorizontalAnchorParent` and `HorizontalAnchorSelf` set to `hpMiddle`; `VerticalAnchorParent` and `VerticalAnchorSelf` set to `vpMiddle`). Let's also set `AutoSizeToChildren` property to `true` (i.e. check the corresponding checkbox) - this will make our `GameOverPoup` scale together with its content.

Again, let's add inside a "Vertical Group" by Design -> Add User Interface Component -> Vertical Group (TCastleVerticalGroup). Name it `GameOverGroup`, set `Align` to `hpMiddle` and `Spacing` to "50".

Let's add two images inside. Let's name one `GameOverImage` and load a `gameover.png` image URL. The second one `HighScoreImage` with `highscore.png` image URL. Note, that now they are displayed on top of one another, however in-game only one of them will be visible. We can try how it looks by enabling/disabling `Exists` property of the images.

Let's add two labels `ScoreTextLabel` and `ScoreValueLabel` with `FontSize` "60" and "80" correspondingly. Let's change `Caption` of the first one to "Your Score:" and of the second one - "9999999". `Color` of both: `162D40`.

Finally let's a button `PlayAgainButton`. Let's make their design equal to those of Main Menu buttons (check up above): `FontColor`: "60"; `CustomBackground`: checked; set proper images for `CustomBackgroundFocused`, `CustomBackgroundNormal` and `CustomBackgroundPressed`; `Width`: "550"; `Height`: "160"; `AutoSize`: unchecked; `Caption`: "Play Again"; `CustomTextColorUse`: checked; `CustomTextColor`: "162D40".

Let's duplicate this button and name it `MainMenuButton` with `Caption`: "Main Menu".

Now our State design looks like:

![Game Over popup](images/designing-gameover-popup.png)

### Creating a State for Game Over Popup

The very same way as we already did twice, let's make a State for Game Over Popup. Let's create a unit `GameStateGameOver` with the following contents:

```Pascal
unit GameStateGameOver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUiState, CastleControls, CastleUiControls;

type
  TStateGameOver = class(TUiState)
  private
    BackgroundColor: TCastleRectangleControl;
    GameOverPopup: TCastleUserInterface;
    GameOverImage, HighScoreImage: TCastleImageControl;
    ScoreTextLabel, ScoreValueLabel: TCastleLabel;
    PlayAgainButton, MainMenuButton: TCastleButton;
    procedure ClickPlayAgain(Sender: TObject);
    procedure ClickMainMenu(Sender: TObject);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateGameOver: TStateGameOver;

implementation
uses
  CastleComponentSerialize,
  CastleVectors,
  GameFont, GameStateGame, GameStateMainMenu;

procedure TStateGameOver.Start;
var
  UiOwner: TComponent;
  UnusedBooleanVariable: Boolean = false;
begin
  inherited;
  InsertUserInterface('castle-data:/GameOver.castle-user-interface', FreeAtStop, UiOwner);
  BackgroundColor := UiOwner.FindRequiredComponent('BackgroundColor') as TCastleRectangleControl;
  GameOverPopup := UiOwner.FindRequiredComponent('GameOverPopup') as TCastleUserInterface;
  GameOverImage := UiOwner.FindRequiredComponent('GameOverImage') as TCastleImageControl;
  HighScoreImage := UiOwner.FindRequiredComponent('HighScoreImage') as TCastleImageControl;
  ScoreTextLabel := UiOwner.FindRequiredComponent('ScoreTextLabel') as TCastleLabel;
  ScoreValueLabel := UiOwner.FindRequiredComponent('ScoreValueLabel') as TCastleLabel;
  PlayAgainButton := UiOwner.FindRequiredComponent('PlayAgainButton') as TCastleButton;
  MainMenuButton := UiOwner.FindRequiredComponent('MainMenuButton') as TCastleButton;
  ScoreTextLabel.CustomFont := CartoonFont60;
  PlayAgainButton.CustomFont := CartoonFont60;
  MainMenuButton.CustomFont := CartoonFont60;
  PlayAgainButton.OnClick := @ClickPlayAgain;
  MainMenuButton.OnClick := @ClickMainMenu;
  Update(0, UnusedBooleanVariable);
end;

procedure TStateGameOver.ClickPlayAgain(Sender: TObject);
begin
  TUiState.Current := StateGame;
end;

procedure TStateGameOver.ClickMainMenu(Sender: TObject);
begin
  TUiState.Current := StateMainMenu;
end;

procedure TStateGameOver.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
end;

end.
```

We've already been through all of this process twice, so let's not waste any time. Just a short summary of what we did here:

- We've created `TStateGameOver` and it's singleton variable `StateGameOver`.

- We've parsed the State Design in `Start` procedure and assigned fonts and `OnClick` events.

- We've implemented button clicks that will change `TUiState.Current` to either `StateGame` or `StateMainMenu` depending on Player's choice.

- We've created an empty `Update` procedure to take care of animations.

The same way as before let's create our State in `GameInitialize` adding `GameStateGameOver` to `uses` section and `StateGameOver := TStateGameOver.Create(Application);` somewhere in `ApplicationInitialize`.

### Showing Game Over Popup

Now, as this is a popup, we'd want to show it without hiding the underlying state. Let's go into unit `GameStateGame`, add `GameStateGameOver` to `uses` section in `implementation` and inside `TStateGame.Update` find our block that corresponds to "Game Over" state. In the end of this block, when everything was updated correctly, let's add `TUiState.Push(StateGameOver);` like this:

```Pascal
begin
  //GameOver
  ...
  GamePads[X, Y].Image.Color := Vector4(1.0, 0.0, 0.0, 1.0);
  if GameRunning then
    TUiState.Push(StateGameOver);
  GameRunning := false;
end;
```

We've also added `if GameRunning then` condition to avoid possible rare bug in case two popups will fire simultaneously.

Save, compile and run. We've got our State Game Over running with both buttons already useful:

![Game Over state - first results](images/gameover-first-results.png)

### Passing Data to Game Over Popup

Now, let's pass some data to the Popup, so that it will show relevant image and score. Let's add two `public` variables to `TStateGameOver`:

```Pascal
type
  TStateGameOver = class(TUiState)
  ...
  public
    Score: Integer;
    HighScore: Boolean;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;
```

In `Start` let's add somewhere near the end, before calling `Update` hack:

```Pascal
ScoreValueLabel.Caption := Score.ToString;
HighScoreImage.Exists := HighScore;
GameOverImage.Exists := not HighScore;
```

This way we set the `Caption` of `ScoreValueLabel` to value of `Score` and set visibility of `HighScoreImage` and `GameOverImage` based on boolean variable `HighScore`. Now in `GameStateGame` in the block relating to "Game Over" let's set these variables to current Player's results:

```Pascal
StateGameOver.Score := GameScore;
if UserConfig.GetValue('high_score', 0) < GameScore then
begin
  UserConfig.SetValue('high_score', GameScore);
  UserConfig.Save;
  StateGameOver.HighScore := true;
end else
  StateGameOver.HighScore := false;
```

![Passing data to a State](images/passing-data-to-state.png)

### Animating Game Over Popup

As we've already noted, we'll do the animations through `Update` procedure in `GameStateGameOver`. We'll be animating two objects: color of `BackgroundColor` and position of `GameOverPopup`. First let's add a `const` with animation time somewhere in `interface` section:

```Pascal
const
  AnimationDuration = 0.3;
```

This specifies the animation duration - 0.3 seconds. Now in `TStateGameOver` let's create a `private` variable:

```Pascal
type
  TStateGameOver = class(TUiState)
  private
    AnimationTime: Single;
    ...
  end;
```

Set it to zero in `Start`:

```Pascal
procedure TStateGameOver.Start;
var
  UiOwner: TComponent;
  UnusedBooleanVariable: Boolean = false;
begin
  inherited;
  ...
  AnimationTime := 0;
  Update(0, UnusedBooleanVariable);
end;
```

Now let's add `SecondsPassed` to it in `Update` and set parameters of our two animated objects based on this value:

```Pascal
procedure TStateGameOver.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  AnimationTime += SecondsPassed;
  if AnimationTime < AnimationDuration then
  begin
    BackgroundColor.Color := Vector4(0.57, 0.80, 0.92, 0.9 * AnimationTime/AnimationDuration);
    GameOverPopup.VerticalAnchorDelta := -0.5 * 1334 * (1.0 - AnimationTime/AnimationDuration);
  end else
  begin
    BackgroundColor.Color := Vector4(0.57, 0.80, 0.92, 0.9);
    GameOverPopup.VerticalAnchorDelta := 0;
  end;
end;
```

Here we increase `AnimationTime` every frame. If it's less than `AnimationDuration` we slowly increase alpha (opacity) of `BackgroundColor` and set it to a cyan tint. Also we're changing `GameOverPopup.VerticalAnchorDelta` which is its vertical position - moving it from below the screen to central position (where `VerticalAnchorDelta` is zero).

If `AnimationTime >= AnimationDuration` we just set the final values for the variables. Now our Game Over screen looks like this:

![Animated Game Over](images/animated-gameover.png)

## Audio

Let's note that the game is already quite playable at current stage. Once we shall have added sound and music to the game, we'll be done with most critically important game features and left with only some polishing to do.

### Audio repository

There are several ways of working with audio in Castle Game Engine. Also there are several possible audio backends to be used. We'll stick with simplest and default solution - through using audio repository. This approach suggests us creating a special "audio registry" in XML format that would describe how our sounds should be called from the game.

Let's go to our `data` folder and create `audio` subfolder, and after dropping our sound files inside, create a file named `index.xml` with the following content:

```XML
<?xml version="1.0"?>

<sounds>
</sounds>
```

This is our minimal (and empty) sound repository registry. Now let's go into `GameInitialize` unit, add `CastleSoundEngine` to `uses` section and in `ApplicationInitialize` load it somewhere near `LoadFonts;`:

```Pascal
SoundEngine.RepositoryURL := 'castle-data:/audio/index.xml';
```

This will instruct Castle Game Engine's SoundEngine to load our `index.xml` file which will be immediately ready to use.

### Music Information

Let's go back to our `index.xml` file and start filling it up with useful information. Let's add information about our two music tracks inside:

```XML
<sounds>
  <sound name="menu_music" url="music/Curious_Critters_CC_BY_Matthew_Pablo.ogg" stream="true" />
  <sound name="game_music" url="music/Casual_game_track_CC_BY_Alexandr_Zhelanov.ogg" stream="true" />
</sounds>
```

Here:

- `sound` is a keyword, telling that this is a description of a sound file.

- `name` is the name of the sound or music track. We'll use it to call our sound or music from the game.

- `url` is an optional field, that specifies exact filename to be loaded for this sound.

- `stream` will tell the `SoundEngine` not to load our whole music track, but stream it from HDD/SDD or other source. That's a good idea for music files, which tend to be large and may take a lot of time to load, but a bad idea for short sound files that must be immediately available without any possible delays.

### Playing Music

Let's now play our music in game. Let's go to our `GameStateMainMenu` unit and add `CastleSoundEngine` to `uses` section. Now somewhere in `Start` let's add a line:

```Pascal
SoundEngine.LoopingChannel[0].Sound := SoundEngine.SoundFromName('menu_music');
```

Which instructs to loop a track named `menu_music` on channel #0. As simple as that. We can now compile and run and see that the music is already working!

Let's now go into `GameStateGame`, again add `CastleSoundEngine` to `uses` section and a similar line somewhere in `Start` procedure:

```Pascal
SoundEngine.LoopingChannel[0].Sound := SoundEngine.SoundFromName('menu_music');
```

And it also works without any problems. We can also test and see that the music is switched when we go from Main Menu State into Game State and back.

### Button clicking sounds

Let's add our sounds to the registry in the very same way as we did with the music:

```XML
<sounds>
  <sound name="menu_music" url="music/Curious_Critters_CC_BY_Matthew_Pablo.ogg" stream="true" />
  <sound name="game_music" url="music/Casual_game_track_CC_BY_Alexandr_Zhelanov.ogg" stream="true" />

  <sound name="click_01" url="clicks/01-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_02" url="clicks/02-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_03" url="clicks/03-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_04" url="clicks/04-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_05" url="clicks/05-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_06" url="clicks/06-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_07" url="clicks/07-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_08" url="clicks/08-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_09" url="clicks/09-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_10" url="clicks/10-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_11" url="clicks/11-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_12" url="clicks/12-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_13" url="clicks/13-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_14" url="clicks/14-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_15" url="clicks/15-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_16" url="clicks/16-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_17" url="clicks/17-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_18" url="clicks/18-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_19" url="clicks/19-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_20" url="clicks/20-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_21" url="clicks/21-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
  <sound name="click_22" url="clicks/22-319419__johnthewizar__selection-sounds_CC0_by_johnthewizar.wav" />
</sounds>
```

As we've already noted, we aren't using `stream` feature here. Also using uncompressed `wav` format instead of `ogg`. We'd want all of these 22 click sounds to play randomized in our game, when we click game pads. We could do that by providing the `SoundEngine` with the proper sound names, but it can already do this for us. Let's add a special `alias` feature here:

```XML
<sounds>
  ...
  <alias name="click">
    <target name="click_01" />
    <target name="click_02" />
    <target name="click_03" />
    <target name="click_04" />
    <target name="click_05" />
    <target name="click_06" />
    <target name="click_07" />
    <target name="click_08" />
    <target name="click_09" />
    <target name="click_10" />
    <target name="click_11" />
    <target name="click_12" />
    <target name="click_13" />
    <target name="click_14" />
    <target name="click_15" />
    <target name="click_16" />
    <target name="click_17" />
    <target name="click_18" />
    <target name="click_19" />
    <target name="click_20" />
    <target name="click_21" />
    <target name="click_22" />
  </alias>
</sounds>
```

This means, that when we call sound named `"click"` from the game `SoundEngine` will randomly play any of the sound that we've specified as `target`s here.

### Accelerate and Game Over sounds

The very same way we can add an "acceleration" sound when the Player clicked an empty button:

```XML
<sounds>
  ...
  <sound name="accelerate" url="clicks/349311__newagesoup__uplifter-sweep-10s-three-tones_CC0_by_newagesoup.wav" />
</sounds>
```

Now in our `GameStateGame` unit we can navigate to `ButtonPress` procedure and at the point where we process the result of user click add two lines to play `"click"` or `"accelerate"` sound depending on which button was clicked:

```Pascal
if ThisGamePad^.Score > 0 then
begin
  SoundEngine.Sound(SoundEngine.SoundFromName('click'));
  GameScore += ThisGamePad^.Score;
  ThisGamePad^.Score := 0;
  ThisGamePad^.Ripeness := 0.0;
  ThisGamePad^.Speed := 0.5 + Random;
end else
if ThisGamePad^.Score = 0 then
begin
  GamePace += 0.25;
  SoundEngine.Sound(SoundEngine.SoundFromName('accelerate'));
end;
```

Here `SoundEngine.Sound` plays a sound which is in turn found "by name" using `SoundEngine.SoundFromName`. We can now test and see that it's working.

The same way we add a "game over" sound:

```XML
<sounds>
  ...
  <sound name="game_over" url="clicks/245646__unfa__cartoon-pop-distorted_CC0_by_unfa.wav" />
</sounds>
```

And play it in `Update`:

```Pascal
begin
  //GameOver
  SoundEngine.Sound(SoundEngine.SoundFromName('game_over'));
  ...
end;
```

### The rest of UI sounds

Let's quickly add some other sounds for UI events:

```XML
<sounds>
  ...
  <sound name="ui_click_1" url="ui/click_001_CC0_by_Kenney.wav" />
  <sound name="ui_click_2" url="ui/click_002_CC0_by_Kenney.wav" />
  <sound name="ui_click_3" url="ui/click_003_CC0_by_Kenney.wav" />
  <alias name="ui_click">
    <target name="ui_click_1" />
    <target name="ui_click_2" />
    <target name="ui_click_3" />
  </alias>

  <sound name="start_game_1" url="ui/confirmation_001_CC0_by_Kenney.wav" />
  <sound name="start_game_2" url="ui/confirmation_002_CC0_by_Kenney.wav" />
  <sound name="start_game_3" url="ui/confirmation_004_CC0_by_Kenney.wav" />
  <alias name="start_game">
    <target name="start_game_1" />
    <target name="start_game_2" />
    <target name="start_game_3" />
  </alias>

  <sound name="quit_1" url="ui/switch_001_CC0_by_Kenney.wav" />
  <sound name="quit_2" url="ui/switch_002_CC0_by_Kenney.wav" />
  <sound name="quit_3" url="ui/switch_003_CC0_by_Kenney.wav" />
  <sound name="quit_4" url="ui/switch_004_CC0_by_Kenney.wav" />
  <sound name="quit_5" url="ui/switch_005_CC0_by_Kenney.wav" />
  <sound name="quit_6" url="ui/switch_006_CC0_by_Kenney.wav" />
  <sound name="quit_7" url="ui/switch_007_CC0_by_Kenney.wav" />
  <alias name="quit">
    <target name="quit_1" />
    <target name="quit_2" />
    <target name="quit_3" />
    <target name="quit_4" />
    <target name="quit_5" />
    <target name="quit_6" />
    <target name="quit_7" />
  </alias>
</sounds>
```

Let's put `SoundEngine.Sound(SoundEngine.SoundFromName('start_game'));` inside `ClickStart` in `GameStateMainMenu` and inside `ClickPlayAgain` in `GameStateGameOver` (don't forget to add `CastleSoundEngine` to `uses` section). Let's add `SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));` into `ClickOptions`, `ClickCredits` in `GameStateMainMenu`. And finally `SoundEngine.Sound(SoundEngine.SoundFromName('quit'));` to `ClickQuit` in `GameStateMainMenu` and `ClickMainMenu` in `GameStateGameOver`.

Let's note that now the music plays a bit too loud to properly hear `ui_click` sound. Let's leave it like that, we'll change default music volume when we'll be making Options.

### Vibration

Some platform, mobile devices first of all, can also vibrate. Normally, we shouldn't overuse this feature. Let's add some vibration in case when the player clicks an empty button (and the game accelerates) and when losing the game.

Let's add `CastleOpenDocument` to `uses` section of `GameStateGame`. Next, navigate to `ButtonPress` and add `Vibrate(100);` near the line where we play the sound. This will tell our device to vibrate for 100 milliseconds:

```Pascal
begin
  GamePace += 0.25;
  SoundEngine.Sound(SoundEngine.SoundFromName('accelerate'));
  Vibrate(100);
end;
```

And in `Update` add `Vibrate(500);` to vibrate for 0.5 seconds in case the game is over:

```Pascal
begin
  //GameOver
  Vibrate(500);
  SoundEngine.Sound(SoundEngine.SoundFromName('game_over'));
  ...
end;
```

Note, that it's safe to call this function on platforms that don't support vibration, but nothing will happen.

## State Options

The Player might want to fine-tune some game settings for more convenient gameplay. There isn't really much to tweak in our simple game, but we might want to let Player change the volume of the music and sound.

### Designing State Options

Let's create a new Design, the very same way as we did for Main Menu and Game States, with `TCastleImagecontrol` as root element, call it `BackgroundImage` and load our regular background image into it. Save the design as `Options.castle-user-interface`. Again, let's add a `TCastleVerticalGroup` to the design naming it `OptionsGroup` and centering it on screen "Layout" tab, and setting `Alignment` to `hpMiddle`.

Let's add a `TCastleHorizontalGroup` named `VolumeGroup` inside. Let's add a `TCastleLabel` named `VolumeText` with caption "Volume:" and color "162D40". Let's add a button with `CustomBackgroun` and `CustomBackgroundNormal`: "smallbutton_cyan.png"; `CustomBackgroundFocused`: "smallbutton_green.png"; `CustomBackgroundPressed`: "smallbutton_yellow.png"; `AutoSizeToChildren`: "false"; `CustomTextColorUse`: "true"; `CustomTextColor`: "162D40"; `Width`: "76"; `Height`: "80"; `Name`: "ButtonVolume0"; `Caption`: "0%".

As a child of `ButtonVolume0` let's add a `TCastleImageControl` with name "SelectedVolume0" and URL pointing to "selected_circle.png", let's center it in "Layout" tab. So it'll now look like this:

![Volume button design](images/volume-button-design.png)

Now, it's just to duplicate our button 4 times so that we have 5 buttons:

![Volume buttons](images/volume-buttons.png)

Now let's duplicate our whole `VolumeGroup` and name it `MusicGroup`. Also let's rename all buttons and images correspondingly:

![Volume and music options](images/volume-and-music-options.png)

Note that here we've also done a small trick and `MusicText` `Caption` is set to "Music: " (notice space in the end) - just to avoid alignment problems quick and dirty thanks to using monospace font.

Let's duplicate our `MusicGroup` and call it `VibrationGroup`, doing the very same renames, but only leave 2 buttons with captions changed into "OFF" and "ON":

![Volume, music and vibration options](images/volume-music-vibration-options.png)

Let's add an Empty Rectangle to `OptionsGroup` that would work as a separator here. And in the end add a button with equal design like Main Menu buttons:

![Options design](images/options-design.png)

### Adding State Options to the Game

Again, we're quickly going through the already known process of implementing a `GameStateOptions` unit. Let's make a short summary of its content:

```Pascal
unit GameStateOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls;

type
  TStateOptions = class(TUiState)
  private
    ButtonVolume0, ButtonVolume1, ButtonVolume2, ButtonVolume3, ButtonVolume4,
      ButtonMusic0, ButtonMusic1, ButtonMusic2, ButtonMusic3, ButtonMusic4,
      ButtonVibration0, ButtonVibration1, BackButton: TCastleButton;
    SelectedVolume0, SelectedVolume1, SelectedVolume2, SelectedVolume3,
      SelectedVolume4, SelectedMusic0, SelectedMusic1, SelectedMusic2,
      SelectedMusic3, SelectedMusic4, SelectedVibration0, SelectedVibration1:
      TCastleImageControl;
    VibrationGroup: TCastleHorizontalGroup;
    procedure UpdateButtonsSelection;
    procedure SetVolume(const Volume: Single);
    procedure SetMusic(const Music: Single);
    procedure ClickVolume0(Sender: TObject);
    procedure ClickVolume1(Sender: TObject);
    procedure ClickVolume2(Sender: TObject);
    procedure ClickVolume3(Sender: TObject);
    procedure ClickVolume4(Sender: TObject);
    procedure ClickMusic0(Sender: TObject);
    procedure ClickMusic1(Sender: TObject);
    procedure ClickMusic2(Sender: TObject);
    procedure ClickMusic3(Sender: TObject);
    procedure ClickMusic4(Sender: TObject);
    procedure ClickVibrationOff(Sender: TObject);
    procedure ClickVibrationOn(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateOptions: TStateOptions;

implementation
uses
  CastleComponentSerialize,
  CastleSoundEngine, CastleConfig,
  GameFont, GameStateMainMenu;

procedure TStateOptions.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/Options.castle-user-interface', FreeAtStop, UiOwner);
  ButtonVolume0 := UiOwner.FindRequiredComponent('ButtonVolume0') as TCastleButton;
  ButtonVolume1 := UiOwner.FindRequiredComponent('ButtonVolume1') as TCastleButton;
  ButtonVolume2 := UiOwner.FindRequiredComponent('ButtonVolume2') as TCastleButton;
  ButtonVolume3 := UiOwner.FindRequiredComponent('ButtonVolume3') as TCastleButton;
  ButtonVolume4 := UiOwner.FindRequiredComponent('ButtonVolume4') as TCastleButton;
  ButtonMusic0 := UiOwner.FindRequiredComponent('ButtonMusic0') as TCastleButton;
  ButtonMusic1 := UiOwner.FindRequiredComponent('ButtonMusic1') as TCastleButton;
  ButtonMusic2 := UiOwner.FindRequiredComponent('ButtonMusic2') as TCastleButton;
  ButtonMusic3 := UiOwner.FindRequiredComponent('ButtonMusic3') as TCastleButton;
  ButtonMusic4 := UiOwner.FindRequiredComponent('ButtonMusic4') as TCastleButton;
  ButtonVibration0 := UiOwner.FindRequiredComponent('ButtonVibration0') as TCastleButton;
  ButtonVibration1 := UiOwner.FindRequiredComponent('ButtonVibration1') as TCastleButton;
  BackButton := UiOwner.FindRequiredComponent('BackButton') as TCastleButton;
  SelectedVolume0 := UiOwner.FindRequiredComponent('SelectedVolume0') as TCastleImageControl;
  SelectedVolume1 := UiOwner.FindRequiredComponent('SelectedVolume1') as TCastleImageControl;
  SelectedVolume2 := UiOwner.FindRequiredComponent('SelectedVolume2') as TCastleImageControl;
  SelectedVolume3 := UiOwner.FindRequiredComponent('SelectedVolume3') as TCastleImageControl;
  SelectedVolume4 := UiOwner.FindRequiredComponent('SelectedVolume4') as TCastleImageControl;
  SelectedMusic0 := UiOwner.FindRequiredComponent('SelectedMusic0') as TCastleImageControl;
  SelectedMusic1 := UiOwner.FindRequiredComponent('SelectedMusic1') as TCastleImageControl;
  SelectedMusic2 := UiOwner.FindRequiredComponent('SelectedMusic2') as TCastleImageControl;
  SelectedMusic3 := UiOwner.FindRequiredComponent('SelectedMusic3') as TCastleImageControl;
  SelectedMusic4 := UiOwner.FindRequiredComponent('SelectedMusic4') as TCastleImageControl;
  SelectedVibration0 := UiOwner.FindRequiredComponent('SelectedVibration0') as TCastleImageControl;
  SelectedVibration1 := UiOwner.FindRequiredComponent('SelectedVibration1') as TCastleImageControl;

  ButtonVolume0.OnClick := @ClickVolume0;
  ButtonVolume1.OnClick := @ClickVolume1;
  ButtonVolume2.OnClick := @ClickVolume2;
  ButtonVolume3.OnClick := @ClickVolume3;
  ButtonVolume4.OnClick := @ClickVolume4;
  ButtonMusic0.OnClick := @ClickMusic0;
  ButtonMusic1.OnClick := @ClickMusic1;
  ButtonMusic2.OnClick := @ClickMusic2;
  ButtonMusic3.OnClick := @ClickMusic3;
  ButtonMusic4.OnClick := @ClickMusic4;
  ButtonVibration0.OnClick := @ClickVibrationOff;
  ButtonVibration1.OnClick := @ClickVibrationOn;

  BackButton.OnClick := @ClickBack;
  BackButton.CustomFont := CartoonFont60;

  VibrationGroup := UiOwner.FindRequiredComponent('VibrationGroup') as TCastleHorizontalGroup;
  {$ifndef CASTLE_IOS}
    {$ifndef ANDROID}
      VibrationGroup.Exists := false;
    {$endif}
  {$endif}

  UpdateButtonsSelection;
end;

procedure TStateOptions.UpdateButtonsSelection;
var
  VolumeLevel, MusicLevel: Integer;
begin
  VolumeLevel := Round(UserConfig.GetFloat('volume', 1.0) * 4);
  SelectedVolume0.Exists := VolumeLevel = 0;
  SelectedVolume1.Exists := VolumeLevel = 1;
  SelectedVolume2.Exists := VolumeLevel = 2;
  SelectedVolume3.Exists := VolumeLevel = 3;
  SelectedVolume4.Exists := VolumeLevel = 4;
  MusicLevel := Round(UserConfig.GetFloat('music', 0.5) * 4);
  SelectedMusic0.Exists := MusicLevel = 0;
  SelectedMusic1.Exists := MusicLevel = 1;
  SelectedMusic2.Exists := MusicLevel = 2;
  SelectedMusic3.Exists := MusicLevel = 3;
  SelectedMusic4.Exists := MusicLevel = 4;
  SelectedVibration0.Exists := not UserConfig.GetValue('vibration', true);
  SelectedVibration1.Exists := UserConfig.GetValue('vibration', true);
end;

procedure TStateOptions.SetVolume(const Volume: Single);
begin
  SoundEngine.Volume := Volume;
  UserConfig.SetFloat('volume', Volume);
  UserConfig.Save;
  UpdateButtonsSelection;
end;

procedure TStateOptions.SetMusic(const Music: Single);
begin
  SoundEngine.LoopingChannel[0].Volume := Music;
  UserConfig.SetFloat('music', Music);
  UserConfig.Save;
  UpdateButtonsSelection;
end;

procedure TStateOptions.ClickVolume0(Sender: TObject);
begin
  SetVolume(0.0);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVolume1(Sender: TObject);
begin
  SetVolume(0.25);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVolume2(Sender: TObject);
begin
  SetVolume(0.5);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVolume3(Sender: TObject);
begin
  SetVolume(0.75);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVolume4(Sender: TObject);
begin
  SetVolume(1.0);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic0(Sender: TObject);
begin
  SetMusic(0.0);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic1(Sender: TObject);
begin
  SetMusic(0.25);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic2(Sender: TObject);
begin
  SetMusic(0.5);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic3(Sender: TObject);
begin
  SetMusic(0.75);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic4(Sender: TObject);
begin
  SetMusic(1.0);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVibrationOff(Sender: TObject);
begin
  UserConfig.SetValue('vibration', false);
  UserConfig.Save;
  UpdateButtonsSelection;
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVibrationOn(Sender: TObject);
begin
  UserConfig.SetValue('vibration', true);
  UserConfig.Save;
  UpdateButtonsSelection;
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickBack(Sender: TObject);
begin
  TUiState.Current := StateMainMenu;
  SoundEngine.Sound(SoundEngine.SoundFromName('quit'));
end;

end.
```

On the first hand it may look huge, but most of it is very simple and similar to what we've already done before:

- We create `TStateOptions` class and corresponding variable `StateOptions`.

- We implement `Start` procedure where we "find" all the components in the design we're going to use from the code.

- We implement click events for the buttons like `procedure ClickVolume0` and assign them to the corresponding buttons in `Start`.

- We play sounds when user clicks buttons.

- We find `VibrationGroup` and hide it in case our platform is not `CASTLE_IOS` or `ANDROID`.

- We store the user-selected vibration preference by `UserConfig.GetValue` and `UserConfig.SetValue`, like we did with High Score.

- Similarly we `UserConfig.GetFloat` and `UserConfig.SetFloat` values for music and sounds volumes (through `SoundEngine.Volume` which sets overall sound and music volume and `SoundEngine.LoopingChannel[0].Volume` which controls only music volume) and `Save` the corresponding values in `UserConfig`.

- In `UpdateButtonsSelection` we set `Exists` only for those images that correspond to the selected level of music.

- We get `BackButton` and change it's font from default to fancy one; it's click event will set `TUiState.Current := StateMainMenu;` - i.e. will bring the Player back to the Main Menu.

That's all we needed here. Almost nothing new, the same things we did when we were implementing other UI states, but in larger quantity and sometimes performing some different function.

That's how our Options State now looks on Desktop:

![Options in game](images/options-in-game.png)

### Using the new Options

First of all, we have to create our `StateOptions` in `GameInitialize`. Let's add `GameStateOptions` in `uses` section, and create the state by `StateOptions := TStateOptions.Create(Application);` in `ApplicationInitialize`.

As we are already changing the volumes for `SoundEngine` it's a good idea to set them to the values stored in `UserConfig`:

```Pascal
SoundEngine.Volume := UserConfig.GetFloat('volume', 1.0);
SoundEngine.LoopingChannel[0].Volume := UserConfig.GetFloat('music', 0.5);
```

Note, that both here and in `GameStateOptions` we propose 0.5 default music volume.

Yes, that's all about sound. Now, the final touch - in `GameStateGame` let's call `Vibrate` only in case it's enabled in `UserConfig`. In `ButtonPress` we set:

```Pascal
if UserConfig.GetValue('vibration', true) then
  Vibrate(100);
```

And in `Update` - where our GameOver state happens:

```Pascal
if UserConfig.GetValue('vibration', true) then
  Vibrate(500);
```

## State Credits

It is respectful and often legally required to mention the people who worked on a game and whose assets (like graphics, music, sounds) were used in it. So, again, let's create a special - this time very simple - state Credits for this purpose.

### Design State Credits

We already know how to create designs in Castle Editor, so let's not go into any detail and just get straight into our result:

![State Credits design](images/credits-design.png)

The only thing worth mentioning here is `CastleGameEngineLabel`. First of all, this label has multiple lines. Large labels can be edited by editing `Text` property (press button with three dots) instead of `Caption`:

![Editing large labels](images/credits-multiline-label.png)

And the second one is we set this label as a child of Castle Game Engine logo and align `hpLeft` of the label to `hpRight` of Parent with `HorizontalAnchorDelta` of 20. This way the label starts exactly 20 pixels to the right of the parent (in this case - logo).

In order for the lines of multiline text don't stick together too tightly, we can increase `LineSpacing` from "2" to "10" in "All" tab:

![Line spacing](images/credits-line-spacing.png)

Everything else is something that we've already done before - labels, images and empty rectangles packed inside a `TCastleVerticalGroup`.

### Implement State Credits in the Game

Let's create a unit `GameStateCredits` in Lazarus, as we usually do:

```Pascal
unit GameStateCredits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls, CastleKeysMouse;

type
  TStateCredits = class(TUiState)
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateCredits: TStateCredits;

implementation
uses
  CastleComponentSerialize,
  CastleSoundEngine,
  GameStateMainMenu;

procedure TStateCredits.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/Credits.castle-user-interface', FreeAtStop, UiOwner);
end;

function TStateCredits.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  SoundEngine.Sound(SoundEngine.SoundFromName('quit'));
  TUiState.Current := StateMainMenu;
end;

end.
```

The only "new" thing here is `function Press` which is called when the Player presses mouse button, keyboard key or taps the screen. It's very similar to the `OnPress` event we've used when implementing `GameStateGame`. And in the function itself we simply send the Player back to `StateMainMenu` - upon pressing anything. Also note the new syntax of calling `inherited;`:

```Pascal
Result := inherited;
```

Which sets the `Result` of our `function` to the `Result` of the `function` inherited from the Parent class.

Also note, that we actually don't even process anything in `Start` - this state is "static" just a single unchanging screen.

Now, as usually let's add `GameStateCredits` to `uses` section of `GameInitialize` and create our State in `ApplicationInitialize`:

```Pascal
StateCredits := TStateCredits.Create(Application);
```

And again add `GameStateCredits` to `uses` section of `StateMainMenu` and in `ClickCredits` set `StateCredits` as `Current`:

```Pascal
procedure TStateMainMenu.ClickCredits(Sender: TObject);
begin
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
  TUiState.Current := StateCredits;
end;
```

All done!

## State Tutorial

Another very small and simple state to create is a short information for the Player on how to play our game. Again it's so small and simple and so similar to the State Credits we've just made, that we won't go into any detail here. It's just a small but important quality-of-life feature for our Game.

### Design State Tutorial

We know everything now! Let's just make it without any additional words:

![Tutorial design](images/tutorial-design.png)

Still one note to be made: we've changed some of the labels `Alignment` property that can align the text left (`hpLeft`) or right (`hpRight`):

![Text alignment on Label](images/text-alignment-on-label.png)

And edited images `Color` property, the same way as we do it in-game, to emulate how the button will look.

### Implement State Tutorial

Implementation of this state is almost identical to `StateCredits`:

```Pascal
unit GameStateTutorial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls, CastleKeysMouse;

type
  TStateTutorial = class(TUiState)
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateTutorial: TStateTutorial;

implementation
uses
  CastleComponentSerialize,
  CastleSoundEngine,
  GameStateGame;

procedure TStateTutorial.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/Tutorial.castle-user-interface', FreeAtStop, UiOwner);
end;

function TStateTutorial.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  SoundEngine.Sound(SoundEngine.SoundFromName('start_game'));
  TUiState.Current := StateGame;
end;

end.
```

Exactly the same way we add `GameStateTutorial` to `uses` section of `GameInitialize` and create the `TStateTutorial` class in `ApplicationInitialize`:

```Pascal
StateTutorial := TStateTutorial.Create(Application);
```

The only thing left to do - is now to show the tutorial before going into `StateGame` state from the `StateMainMenu`. We simply have to replace `GameStateGame` for `GameStateTutorial` in `uses` section of `GameStateMainMenu` and replace `StateGame` with `StateTutorial` in `ClickStart`.

All done! Now we can compile, run and see that our Tutorial is displayed when we start a new game.

## Splash Screen

### Creating a Splash Screen Image

Our game also needs a more prominent "Splash Screen" than a minimal "Loading..." text provided to us by Castle Game Engine. We have to note that loading of the game, especially on slow mobile devices, can take relatively significant amount of time, therefore the splash screen is not a regular "State" with which we had some experience before. In the very beginning we can't even count that it's possible to read files, as the corresponding permissions may have not yet been given to our game - that is we can't read the Splash Screen image from a file.

There is a solution. We can have the splash screen as a part of our code. This way it will be absolutely robust and will load lighting-fast and will be ready to display as soon as anything can be displayed. Therefore we have to convert an image to Pascal code. This can be done by Castle Game Engine's tool called `image-to-pascal`. It can be found in `tools` folder. You can compile it using Lazarus by double-clicking the `image-to-pascal.lpi` file and pressing F9 or Run.

There is one catch though, it is a command-line app, not a GUI one. So we have to use console. On Windows it can be done by pressing `Win+R` and typing in `cmd`. Then navigate to the drive and folder where `image-to-pascal` is located. This might be not trivial if you've never had experience using console. Let's imagine our Castle Game Engine installation is in "D:\castle-engine" folder. Then we have to do it this way:

```Shell
C:\Users\Username\>D:
```

This will change our current drive to "D:" and then:

```Shell
D:\>cd castle-engine\tools\image-to-pascal
```

Which will make `image-to-pascal` folder current.

Now let's specify the following parameters for `image-to-pascal`:

```Shell
D:\castle-engine\tools\image-to-pascal\>image-to-pascal SplashScreen SplashImage.png
```

This will create `SplashScreen` unit containing the whole `SplashImage.png`.

You can find the original `SplashImage.png` image for Button Clicker inside `source_images` directory in `game` folder of this tutorial.

The command above will generate two files: `splashscreen.pas` which contains the image definition and `splashscreen.image_data` which contains the image data. Let's copy or move those to our game folder.

On Linux system the shell commands are very similar, however, Linux users usually know how to deal with those.

### Using Splash Screen in the Game

Let's add `SplashScreen` and `CastleColors` to `uses` section of `GameInitialize` and in `initialization` add:

```Pascal
initialization
  ApplicationProperties.ApplicationName := 'ButtonClickerGame';

  if IsLibrary then
    InitializeLog;

  Theme.LoadingBackgroundColor := HexToColor('6fbee4');
  Theme.Images[tiLoading] := SplashImage;
  Theme.OwnsImages[tiLoading] := false;
  Theme.LoadingImageForWindowHeight := 1334;
  ...
```

Here we modify default values of the `Theme` - to set background color (`LoadingBackgroundColor`) for our Splash Screen (that would be color of everything beyond our image). `HexToColor` is a useful function from `CastleColors` that converts color in HEX notation (often used by graphic designers) into `TCastleColor` - that is a color usable in the program.

Next we set one of `Theme`'s images namely `tiLoading` which is shown as a Splash Screen while the application is loading. As our generated image file automatically frees the image for us, Theme should not "own" this image.

And finally we suggest `Theme` the resolution for which the image has been created. This doesn't work as good as UI scaling we've been using so far, so we should take some caution that our Splash Screen image would look fine in different aspect ratios.

![Splash Screen](images/splashscreen.png)

## Achievements

It sometimes happens, that there are last-minute requested changes before the release. Let's emulate this situation and learn one more cool feature of Castle Editor.

Let's add Achievements to our game. No, we won't be publishing our game on Apple Store or Google Play Store, it's certainly outside of the scope of this tutorial. But let's make an internal system for handling some Achievements to motivate the Player reach the new heights.

We'll show Achievements both in a special Achievements page, accessible from Main Menu and in Game Over screen in case Player has reached a new achievement. That means that we might be better to reuse every achievement design, so that we won't have to draw them again and again.

### Creating an achievement

In our minimalistic case we could have made it in a simpler way, but let's use this opportunity to learn to use `TCastleDesign` which allows us to reuse designed cuhnks of the UI in different UI states. Let's create our `Achievement1` Design, with Empty Rectangle as Root: Design -> New User Interface (Empty Rectangle as Root) and name it `Root`.

Let's go to "All" tab and uncheck `FullSize` property. Then set `Width`: "712" and `Height`:"200". Note that these values don't matter in this case and we're using them only for consistency.

Inside let's add an image `AchievementImage` place a `ScoreLabel` with text "50k" over it, and also as a child of this image add a `TCastleVerticalGroup` named `AchievementInfoGroup` and having its `HorizontalAnchorSelf` `hpLeft` anchored to `HorizontalAnchorParent` `hpRight` with `HorizontalAnchorDelta` of 20 pixels.

Inside this group have two labels - `AchievementCaption` with larger font that will hold the caption of the achievement, and `AchievementDescription` which will have the description of the achievement. Don't forget to set color of all labels to `162D40`.

![Achievement design](images/achievement-design.png)

### Creating 5 achievements

Now let's save our design as `achievement1.castle-user-interface` and let's make 5 similar files following the next pattern:

| Filename       | ScoreLabel   | AchievementCaption      | AchievementDescription   |
| :------------: | :----------: | :---------------------: | :----------------------: |
| achievement1   | 50k          | Nice!                   | Keep it up!              |
| achievement2   | 100k         | Very Good!              | You're doing great!      |
| achievement3   | 200k         | Remarkable!             | Now that's some skill!   |
| achievement4   | 350k         | Magnificent!            | A result to be proud of! |
| achievement5   | 500k         | Unbelievable!           | Are you serious?! WOW!!! |

We can do that by editing the same file and (not forgetting to) saving it under a different name, or by making several copies of the file and editing them separately.

Let's note that this is not the optimal way to deal with achievements. Most likely we'd rather have some sort of a database of achievements and corresponding images, but for this tutorial let's stick with a less flexible path, that will show us how to use blocks in designing other states.

### Designing State Achievements

First of all, we'd like to have a separate page where the Player can see all the achievements to be proud of. This will be a normal state very similar to `StateMainMenu` or `StateCredits`. Let's create a new Design using Image (TCastleImageControl) as Root. As usually let's add a `TCastleVerticalGroup` named `AchievementsGroup` to this group.

On the top line let's show current High Score, the same way we did in `StateGame`. We won't go into detail here, we already know how to do that.

And now for the new part - inserting another Design into this one.

There's a special UI element that allows us to load another Castle User Interface as a Component in this Design: `TCastleDesign`. Let's add it to our `AchievementsGroup`: Design -> Add User Interface Component -> Design (Reference Another castle-user-interface File) (TCastleDesign). Let's name it `DesignAchievement1`.

Now we have a familiar `URL` field. Let's click the three dots button again and pick our `achievement1.castle-user-interface`.

Unfortunately it can't scale automatically to fit the underlying design, so we'll have to manually set `Width`:"712" and `Height`:"200" to match those of the design we load. The same way we add the rest of 5 achievements:

![State Achievements design](images/state-achievements-design.png)

### Adding Achievements to the Game

### Adding Achievements to Main Menu

### Adding Achievements to Game Over

## Happy End!

