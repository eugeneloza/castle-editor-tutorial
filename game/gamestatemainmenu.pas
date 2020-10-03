unit GameStateMainMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls;

type
  TStateMainMenu = class(TUiState)
  private
    StartGameButton, OptionsButton, CreditsButton, QuitButton: TCastleButton;
  public
    procedure Start; override;
  end;

var
  StateMainMenu: TStateMainMenu;

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

end.

