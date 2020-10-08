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
  TUiState.Current := StateMainMenu;
end;

end.

