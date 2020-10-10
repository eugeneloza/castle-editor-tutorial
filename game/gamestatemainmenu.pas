unit GameStateMainMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls;

type
  TStateMainMenu = class(TUiState)
  private
    StartGameButton, OptionsButton, CreditsButton, QuitButton,
      AchievementsButton: TCastleButton;
    procedure ClickStart(Sender: TObject);
    procedure ClickOptions(Sender: TObject);
    procedure ClickAchievements(Sender: TObject);
    procedure ClickCredits(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation
uses
  CastleComponentSerialize,
  CastleWindow, CastleSoundEngine,
  GameFont, GameStateTutorial, GameStateOptions, GameStateCredits,
  GameStateAchievements;

procedure TStateMainMenu.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/MainMenu.castle-user-interface', FreeAtStop, UiOwner);
  StartGameButton := UiOwner.FindRequiredComponent('StartGameButton') as TCastleButton;
  OptionsButton := UiOwner.FindRequiredComponent('OptionsButton') as TCastleButton;
  AchievementsButton := UiOwner.FindRequiredComponent('AchievementsButton') as TCastleButton;
  CreditsButton := UiOwner.FindRequiredComponent('CreditsButton') as TCastleButton;
  QuitButton := UiOwner.FindRequiredComponent('QuitButton') as TCastleButton;
  StartGameButton.OnClick := @ClickStart;
  OptionsButton.OnClick := @ClickOptions;
  AchievementsButton.OnClick := @ClickAchievements;
  CreditsButton.OnClick := @ClickCredits;
  QuitButton.OnClick := @ClickQuit;
  StartGameButton.CustomFont := CartoonFont60;
  OptionsButton.CustomFont := CartoonFont60;
  AchievementsButton.CustomFont := CartoonFont60;
  CreditsButton.CustomFont := CartoonFont60;
  QuitButton.CustomFont := CartoonFont60;
  {$ifdef CASTLE_IOS}QuitButton.Enabled := false;{$endif}
  {$ifdef ANDROID}QuitButton.Enabled := false;{$endif}
  SoundEngine.LoopingChannel[0].Sound := SoundEngine.SoundFromName('menu_music');
end;

procedure TStateMainMenu.ClickStart(Sender: TObject);
begin
  SoundEngine.Sound(SoundEngine.SoundFromName('start_game'));
  TUiState.Current := StateTutorial;
end;

procedure TStateMainMenu.ClickOptions(Sender: TObject);
begin
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
  TUiState.Current := StateOptions;
end;

procedure TStateMainMenu.ClickAchievements(Sender: TObject);
begin
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
  TUiState.Current := StateAchievements;
end;

procedure TStateMainMenu.ClickCredits(Sender: TObject);
begin
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
  TUiState.Current := StateCredits;
end;

procedure TStateMainMenu.ClickQuit(Sender: TObject);
begin
  SoundEngine.Sound(SoundEngine.SoundFromName('quit'));
  Application.MainWindow.Close;
end;

end.

