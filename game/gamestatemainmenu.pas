unit GameStateMainMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState;

type
  TStateMainMenu = class(TUiState)
    procedure Start; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

procedure TStateMainMenu.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/MainMenu.castle-user-interface', FreeAtStop, UiOwner);
end;

end.

