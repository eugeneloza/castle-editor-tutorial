unit GameStateMainMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls;

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

var
  StateMainMenu: TStateMainMenu;

implementation
uses
  CastleComponentSerialize,
  CastleWindow,
  GameFont;

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
  StartGameButton.OnClick := @ClickStart;
  OptionsButton.OnClick := @ClickOptions;
  CreditsButton.OnClick := @ClickCredits;
  QuitButton.OnClick := @ClickQuit;
  StartGameButton.CustomFont := ButtonFont;
  OptionsButton.CustomFont := ButtonFont;
  CreditsButton.CustomFont := ButtonFont;
  QuitButton.CustomFont := ButtonFont;
  {$ifdef CASTLE_IOS}QuitButton.Enabled := false;{$endif}
  {$ifdef ANDROID}QuitButton.Enabled := false;{$endif}
end;

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
  Application.MainWindow.Close;
end;

end.

